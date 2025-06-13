;;; batch-fix.el --- Batch fix greger-tools-execute calls

(defun simple-fix ()
  "Simple search and replace for greger-tools-execute calls."
  (interactive)
  (goto-char (point-min))
  ;; Just replace the function calls one pattern at a time
  (while (search-forward "(greger-tools-execute \"" nil t)
    (let ((start (match-beginning 0)))
      (when (looking-back "(greger-tools-execute \"" nil)
        (goto-char start)
        (when (looking-at "(greger-tools-execute\\s-+\"\\([^\"]+\\)\"")
          (replace-match "(greger-tools-execute :tool-name \"\\1\"" nil nil)))))
  
  ;; Now fix the args parameter
  (goto-char (point-min))
  (while (search-forward ":tool-name " nil t)
    (when (looking-at "\"[^\"]+\"\\s-+\\('([^']+)\\)")
      (replace-match "\"\\&\" :args \\1" nil nil)))
  
  (message "Applied simple fixes"))

;; Load the file and run the fix
(find-file "test/test-greger-tools.el")
(simple-fix)
(save-buffer)
