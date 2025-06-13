;;; fix-tests.el --- Fix greger-tools-execute calls in tests

(defun fix-greger-tools-execute-calls ()
  "Fix greger-tools-execute calls to use keyword arguments."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward 
            "(greger-tools-execute \"\\([^\"]+\\)\"\\s-+\\([^,)]+\\)\\s-+(lambda\\s-+([^)]+)\\s-+([^)]+))\\s-+\\([^)]+\\))"
            nil t)
      (let ((tool-name (match-string 1))
            (args (match-string 2))
            (callback-body (match-string 3))
            (buffer (match-string 4)))
        (replace-match
         (format "(greger-tools-execute :tool-name \"%s\" :args %s :callback (lambda (r e) %s) :buffer %s)"
                 tool-name args callback-body buffer)))))
  (message "Fixed greger-tools-execute calls"))

;; Run the function
(with-current-buffer (find-file "test/test-greger-tools.el")
  (fix-greger-tools-execute-calls)
  (save-buffer))

;;; fix-tests.el ends here
