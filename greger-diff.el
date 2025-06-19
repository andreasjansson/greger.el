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
          
          ;; Run diff with full context (use a large number for context lines)
          (with-temp-buffer
            (let ((exit-code (call-process "diff" nil t nil
                                         "-U" "1000000"  ; Very large context
                                         original-file new-file)))
              ;; diff returns 0 when files are identical, 1 when they differ
              (cond
               ((zerop exit-code)
                ;; Files are identical, create a special marker diff
                (format "--- IDENTICAL\n+++ IDENTICAL\n@@ -0,0 +0,0 @@\n%s"
                        (mapconcat (lambda (line) (concat " " line))
                                   (split-string original-str "\n")
                                   "\n")))
               ((eq exit-code 1)
                ;; Files differ, return normal diff
                (buffer-string))
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
  (when (string= "" (string-trim unified-diff-str))
    (return '("" . "")))
  
  (let ((lines (split-string unified-diff-str "\n"))
        (original-lines '())
        (new-lines '())
        (in-hunk nil))
    
    (dolist (line lines)
      (cond
       ;; Skip header lines
       ((string-match "^\\(---\\|\\+\\+\\+\\|@@\\)" line)
        (when (string-match "^@@" line)
          (setq in-hunk t)))
       
       ;; Process hunk content
       (in-hunk
        (cond
         ;; Skip "No newline" messages
         ((string-match "^\\\\ No newline" line)
          nil)
         
         ;; Process normal lines
         ((> (length line) 0)
          (let ((prefix (substring line 0 1))
                (content (substring line 1)))
            (cond
             ;; Unchanged line
             ((string= prefix " ")
              (push content original-lines)
              (push content new-lines))
             
             ;; Deleted line
             ((string= prefix "-")
              (push content original-lines))
             
             ;; Added line
             ((string= prefix "+")
              (push content new-lines))
             
             ;; Handle lines without prefix (context)
             ((not (member prefix '("-" "+")))
              (push line original-lines)
              (push line new-lines)))))))))
    
    ;; Return cons cell, removing empty strings from ends
    (let ((orig-str (string-join (nreverse original-lines) "\n"))
          (new-str (string-join (nreverse new-lines) "\n")))
      ;; Clean up trailing newlines that diff might add
      (cons (string-trim-right orig-str "\n")
            (string-trim-right new-str "\n")))))

(provide 'greger-diff)
;;; greger-diff.el ends here
