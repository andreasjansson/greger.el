;;; fix-tool-calls.el --- Fix greger-tools-execute calls to use keyword arguments

(defun fix-greger-tools-execute-calls ()
  "Fix greger-tools-execute calls to use keyword arguments."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward 
            "(greger-tools-execute\\s-+\"\\([^\"]+\\)\"\\s-+\\(.*?\\)\\s-+(lambda\\s-+(\\([^)]*\\))\\s-+\\(.*?\\))\\s-+\\([^)]+\\))"
            nil t)
      (let ((tool-name (match-string 1))
            (args (match-string 2))
            (lambda-args (match-string 3))
            (lambda-body (match-string 4))
            (buffer (match-string 5)))
        (replace-match
         (format "(greger-tools-execute :tool-name \"%s\" :args %s :callback (lambda (%s) %s) :buffer %s)"
                 tool-name args lambda-args lambda-body buffer)))))
  (message "Fixed greger-tools-execute calls"))

;; Apply to the test file
(with-current-buffer (find-file "test/test-greger-tools.el")
  (fix-greger-tools-execute-calls)
  (save-buffer))

;;; fix-tool-calls.el ends here
