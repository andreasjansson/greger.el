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
Deletes diff headers (file and hunk headers) and makes 'No newline' messages invisible."
  (if (string-empty-p (string-trim diff-string))
      diff-string
    (condition-case err
        (with-temp-buffer
          (insert diff-string)
          (delay-mode-hooks (diff-mode))
          (font-lock-fontify-buffer)
          ;; Convert 'face to 'font-lock-face for tree-sitter compatibility
          (greger-diff--convert-faces-for-tree-sitter)
          ;; Delete headers and hide "No newline" messages
          (greger-diff--clean-and-hide-metadata (buffer-string)))
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

(defun greger-diff--get-syntax-highlighted-content (content filename)
  "Apply syntax highlighting to CONTENT based on FILENAME, preserving context."
  (if (string-empty-p (string-trim content))
      content
    (condition-case err
        (with-temp-buffer
          (insert content)
          ;; Determine major mode from filename
          (let ((buffer-file-name filename))
            (set-auto-mode)
            ;; Apply syntax highlighting
            (font-lock-fontify-buffer)
            ;; Convert face properties to font-lock-face for compatibility
            (let ((pos (point-min)))
              (while (setq pos (next-single-property-change pos 'face))
                (when-let ((face (get-text-property pos 'face)))
                  (let ((end (next-single-property-change pos 'face nil (point-max))))
                    (put-text-property pos end 'font-lock-face face)))))
            (buffer-string)))
      (error
       ;; If syntax highlighting fails, return original content
       content))))

(defun greger-diff--apply-syntax-to-diff-content (diff-string filename)
  "Apply syntax highlighting to diff content while preserving diff highlighting."
  (with-temp-buffer
    (insert diff-string)
    
    ;; Extract just the content lines to build the full original and new content
    (let ((original-lines '())
          (new-lines '()))
      
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((line-start (line-beginning-position))
               (line-end (line-end-position))
               (line-content (buffer-substring line-start line-end)))
          
          (when (> (length line-content) 0)
            (let ((prefix (substring line-content 0 1))
                  (content (if (> (length line-content) 1)
                              (substring line-content 1)
                            "")))
              (cond
               ;; Context line - goes to both original and new
               ((string= prefix " ")
                (push content original-lines)
                (push content new-lines))
               ;; Removed line - only in original
               ((string= prefix "-")
                (push content original-lines))
               ;; Added line - only in new
               ((string= prefix "+")
                (push content new-lines)))))
          
          (forward-line 1)))
      
      ;; Now we have the full original and new content - apply syntax highlighting
      (let* ((original-content (string-join (nreverse original-lines) "\n"))
             (new-content (string-join (nreverse new-lines) "\n"))
             (highlighted-original (greger-diff--get-syntax-highlighted-content original-content filename))
             (highlighted-new (greger-diff--get-syntax-highlighted-content new-content filename)))
        
        ;; Now reconstruct the diff with syntax highlighting
        (erase-buffer)
        (let ((orig-lines (split-string highlighted-original "\n"))
              (new-lines (split-string highlighted-new "\n"))
              (diff-lines (split-string diff-string "\n"))
              (orig-pos 0)
              (new-pos 0))
          
          (dolist (diff-line diff-lines)
            (when (> (length diff-line) 0)
              (let ((prefix (substring diff-line 0 1)))
                (cond
                 ;; Context line
                 ((string= prefix " ")
                  (when (< orig-pos (length orig-lines))
                    (insert " " (nth orig-pos orig-lines) "\n")
                    (setq orig-pos (1+ orig-pos))
                    (setq new-pos (1+ new-pos))))
                 ;; Removed line  
                 ((string= prefix "-")
                  (when (< orig-pos (length orig-lines))
                    (insert "-" (nth orig-pos orig-lines) "\n")
                    (setq orig-pos (1+ orig-pos))))
                 ;; Added line
                 ((string= prefix "+")
                  (when (< new-pos (length new-lines))
                    (insert "+" (nth new-pos new-lines) "\n")
                    (setq new-pos (1+ new-pos))))
                 ;; Other lines (invisible markers, etc.) - copy as-is
                 (t
                  (insert diff-line "\n")))))))
        
        ;; Apply diff highlighting to the reconstructed diff
        (goto-char (point-min))
        (delay-mode-hooks (diff-mode))
        (font-lock-fontify-buffer)
        ;; Convert face properties to font-lock-face
        (greger-diff--convert-faces-for-tree-sitter)
        
        (buffer-string)))))

(defun greger-diff-strings (original-str new-str filename)
  "Return a diff string using the system `diff` command with full context.
Applies syntax highlighting based on FILENAME before diffing.
This creates a unified diff that can be reconstructed with `greger-diff-undiff-strings`."
  (let ((temp-dir (make-temp-file "greger-diff-" t))
        (original-file nil)
        (new-file nil))
    (unwind-protect
        (progn
          (setq original-file (expand-file-name "original" temp-dir))
          (setq new-file (expand-file-name "new" temp-dir))
          
          ;; Write plain strings to temporary files
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
                                          (when has-no-newline "\n\\ No newline at end of file")))
                         (diff-fontified (greger-diff-fontify-string identical-diff)))
                    ;; Apply syntax highlighting on top of diff highlighting
                    (greger-diff--apply-syntax-to-diff-content diff-fontified filename))))
               ((eq exit-code 1)
                ;; Files differ, clean up the output and apply fontification
                (let* ((clean-diff (greger-diff--clean-diff-output (buffer-string)))
                       (diff-fontified (greger-diff-fontify-string clean-diff)))
                  ;; Apply syntax highlighting on top of diff highlighting
                  (greger-diff--apply-syntax-to-diff-content diff-fontified filename)))
               (t
                (error "diff command failed with exit code %d" exit-code))))))
      
      ;; Cleanup
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

(defun greger-diff-undiff-strings (unified-diff-str)
  "Extract original and new strings from a unified diff string.
Handles unified diff format created by the `diff` command, including diffs
without headers (file/hunk headers deleted).
Returns a cons cell (ORIGINAL-STR . NEW-STR)."
  ;; Handle empty diff (identical files)  
  (if (string= "" (string-trim unified-diff-str))
      (cons "" "")

    (let ((lines (split-string unified-diff-str "\n"))
          (original-lines '())
          (new-lines '())
          (in-hunk nil)
          (orig-no-newline nil)
          (new-no-newline nil)
          (has-headers nil)
          (last-operation nil)) ; Track what the last operation was for "No newline" handling
    
      ;; Check if the diff has headers (to determine processing mode)
      (dolist (line lines)
        (when (string-match "^\\(---\\|\\+\\+\\+\\|@@\\)" line)
          (setq has-headers t)))
      
      ;; If no headers found, assume we're directly in hunk content
      (unless has-headers
        (setq in-hunk t))
      
      (dolist (line lines)
        (cond
         ;; Skip header lines if they exist
         ((string-match "^\\(---\\|\\+\\+\\+\\|@@\\)" line)
          (when (string-match "^@@" line)
            (setq in-hunk t)))
         
         ;; Process hunk content
         (in-hunk
          (cond
           ;; Handle "No newline" messages
           ((string-match "^\\\\ No newline" line)
            ;; Apply "No newline" based on the last operation
            (cond
             ((eq last-operation 'deleted)
              (setq orig-no-newline t))
             ((eq last-operation 'added)
              (setq new-no-newline t))
             ((eq last-operation 'context)
              ;; For context lines, both sides don't have newlines
              (setq orig-no-newline t new-no-newline t))
             (t
              ;; Fallback: if we can't determine, assume it applies to both
              (setq orig-no-newline t new-no-newline t))))
           
           ;; Process normal lines
           ((> (length line) 0)
            (let ((prefix (substring line 0 1))
                  (content (substring line 1)))
              (cond
               ;; Unchanged line
               ((string= prefix " ")
                (push content original-lines)
                (push content new-lines)
                (setq last-operation 'context))
               
               ;; Deleted line
               ((string= prefix "-")
                (push content original-lines)
                (setq last-operation 'deleted))
               
               ;; Added line
               ((string= prefix "+")
                (push content new-lines)
                (setq last-operation 'added))
               
               ;; Handle lines without prefix (context)
               ((not (member prefix '("-" "+")))
                (push line original-lines)
                (push line new-lines)
                (setq last-operation 'context)))))))))
      
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
