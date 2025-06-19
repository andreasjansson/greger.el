;;; greger-diff.el --- Diff and undiff functions for greger -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Andreas Jansson

;; Author: Andreas Jansson <andreas@jansson.me.uk>
;; Version: 0.1.0
;; URL: https://github.com/andreasjansson/greger.el
;; SPDX-License-Identifier: MIT

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:
;; Provides diff and undiff functions for greger that can generate and parse
;; unified diff format with full context, allowing perfect reconstruction of
;; both original and new content.

;;; Code:

(defun greger-diff--convert-faces-for-tree-sitter ()
  "Convert 'face text properties to 'font-lock-face for tree-sitter compatibility."
  (let ((pos (point-min)))
    (while (setq pos (next-single-property-change pos 'face))
      (when-let ((face (get-text-property pos 'face)))
        (let ((end (next-single-property-change pos 'face nil (point-max))))
          (put-text-property pos end 'font-lock-face face))))
    ;; Mark as fontified to prevent re-fontification
    (add-text-properties (point-min) (point-max) '(fontified t))))

(defun greger-diff--clean-and-hide-metadata (diff-string)
  "Delete diff headers and make 'No newline' messages invisible in DIFF-STRING.
Deletes file headers (--- and +++), hunk headers (@@), but keeps 'No newline' messages invisible."
  (with-temp-buffer
    (insert diff-string)
    
    ;; Delete file headers (--- and +++ lines)
    (goto-char (point-min))
    (while (re-search-forward "^\\(---\\|\\+\\+\\+\\) .*\n" nil t)
      (replace-match ""))
    
    ;; Delete hunk headers (@@ lines)
    (goto-char (point-min))
    (while (re-search-forward "^@@.*@@.*\n" nil t)
      (replace-match ""))
    
    ;; Make "No newline at end of file" messages invisible (don't delete them)
    (goto-char (point-min))
    (while (re-search-forward "^\\\\ No newline at end of file$" nil t)
      (let ((line-start (line-beginning-position))
            (line-end (line-end-position)))
        ;; Make the entire line invisible, including the newline if present
        (put-text-property line-start 
                          (if (< line-end (point-max)) (1+ line-end) line-end)
                          'invisible t)))
    
    (buffer-string)))

(defun greger-diff-fontify-string (diff-string)
  "Apply diff-mode fontification to DIFF-STRING and return fontified string.
Uses a temporary buffer to apply diff-mode fontification, then converts
face properties to font-lock-face for tree-sitter compatibility.
Also makes diff metadata lines (headers, hunk headers, 'No newline' messages) invisible."
  (if (string-empty-p (string-trim diff-string))
      diff-string
    (condition-case err
        (with-temp-buffer
          (insert diff-string)
          (delay-mode-hooks (diff-mode))
          (font-lock-fontify-buffer)
          ;; Convert 'face to 'font-lock-face for tree-sitter compatibility
          (greger-diff--convert-faces-for-tree-sitter)
          ;; Hide metadata lines (headers and "No newline" messages)
          (greger-diff--hide-metadata-lines (buffer-string)))
      (error
       ;; If fontification fails, return the original string
       (message "greger-diff: fontification failed: %s" (error-message-string err))
       diff-string))))

(defun greger-diff--clean-diff-output (diff-str)
  "Clean up diff output by removing timestamps but preserving newline indicators."
  (with-temp-buffer
    (insert diff-str)
    (goto-char (point-min))
    
    ;; Remove timestamps from header lines
    (while (re-search-forward "^\\(---\\|\\+\\+\\+\\) \\([^\t]*\\)\t.*$" nil t)
      (replace-match "\\1 \\2" nil nil))
    
    (buffer-string)))

(defun greger-diff-strings (original-str new-str)
  "Return a diff string using the system `diff` command with full context.
This creates a unified diff that can be reconstructed with `greger-diff-undiff-strings`."
  (let ((temp-dir (make-temp-file "greger-diff-" t))
        (original-file nil)
        (new-file nil))
    (unwind-protect
        (progn
          (setq original-file (expand-file-name "original" temp-dir))
          (setq new-file (expand-file-name "new" temp-dir))
          
          ;; Write strings to temporary files
          (with-temp-file original-file
            (insert original-str))
          (with-temp-file new-file
            (insert new-str))
          
          ;; Run diff with full context and custom labels
          (with-temp-buffer
            (let ((exit-code (call-process "diff" nil t nil
                                         "-U" "1000000"  ; Very large context
                                         "--label" "original-content"
                                         "--label" "new-content"
                                         original-file new-file)))
              ;; diff returns 0 when files are identical, 1 when they differ
              (cond
               ((zerop exit-code)
                ;; Files are identical, create a special marker diff
                (let ((lines (split-string original-str "\n")))
                  ;; Remove the last empty line if the string doesn't end with newline
                  (when (and lines (string= (car (last lines)) ""))
                    (setq lines (butlast lines)))
                  (let* ((line-count (length lines))
                         (has-no-newline (not (string-suffix-p "\n" original-str)))
                         (identical-diff (concat
                                          (format "--- original-content\n+++ new-content\n@@ -1,%d +1,%d @@\n"
                                                  line-count line-count)
                                          (mapconcat (lambda (line) (concat " " line))
                                                     lines "\n")
                                          (when has-no-newline "\n\\ No newline at end of file"))))
                    (greger-diff-fontify-string identical-diff))))
               ((eq exit-code 1)
                ;; Files differ, clean up the output and apply fontification
                (let ((clean-diff (greger-diff--clean-diff-output (buffer-string))))
                  (greger-diff-fontify-string clean-diff)))
               (t
                (error "diff command failed with exit code %d" exit-code))))))
      
      ;; Cleanup
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

(defun greger-diff-undiff-strings (unified-diff-str)
  "Extract original and new strings from a unified diff string.
Handles unified diff format created by the `diff` command.
Returns a cons cell (ORIGINAL-STR . NEW-STR)."
  ;; Handle empty diff (identical files)  
  (if (string= "" (string-trim unified-diff-str))
      (cons "" "")

    (let ((lines (split-string unified-diff-str "\n"))
          (original-lines '())
          (new-lines '())
          (in-hunk nil)
          (orig-no-newline nil)
          (new-no-newline nil))
    
      (dolist (line lines)
        (cond
         ;; Skip header lines
         ((string-match "^\\(---\\|\\+\\+\\+\\|@@\\)" line)
          (when (string-match "^@@" line)
            (setq in-hunk t)))
         
         ;; Process hunk content
         (in-hunk
          (cond
           ;; Handle "No newline" messages
           ((string-match "^\\\\ No newline" line)
            ;; The "No newline" message applies to the last processed line
            ;; We need to track which file it applies to based on context
            (setq orig-no-newline t new-no-newline t))
           
           ;; Process normal lines
           ((> (length line) 0)
            (let ((prefix (substring line 0 1))
                  (content (substring line 1)))
              (cond
               ;; Unchanged line
               ((string= prefix " ")
                (push content original-lines)
                (push content new-lines)
                (setq orig-no-newline nil new-no-newline nil))
               
               ;; Deleted line
               ((string= prefix "-")
                (push content original-lines)
                (setq orig-no-newline nil))
               
               ;; Added line
               ((string= prefix "+")
                (push content new-lines)
                (setq new-no-newline nil))
               
               ;; Handle lines without prefix (context)
               ((not (member prefix '("-" "+")))
                (push line original-lines)
                (push line new-lines)
                (setq orig-no-newline nil new-no-newline nil)))))))))
      
      ;; Build the final strings
      (let ((orig-str (if original-lines 
                          (string-join (nreverse original-lines) "\n")
                        ""))
            (new-str (if new-lines 
                         (string-join (nreverse new-lines) "\n")
                       "")))
        ;; Add trailing newlines unless explicitly marked as having no newline
        (unless (or orig-no-newline (string= orig-str ""))
          (setq orig-str (concat orig-str "\n")))
        (unless (or new-no-newline (string= new-str ""))
          (setq new-str (concat new-str "\n")))
        ;; Strip text properties to return clean strings
        (cons (substring-no-properties orig-str)
              (substring-no-properties new-str))))))

(provide 'greger-diff)
;;; greger-diff.el ends here
