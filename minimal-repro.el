;;; minimal-repro.el --- Minimal reproduction of Emacs 29.4 vs 30.1 difference

;; Test buffer modification during font-lock fontification

(defun test-font-lock-buffer-modification ()
  "Test if buffer modification during font-lock works the same in both versions."
  (let ((test-content "ORIGINAL CONTENT TO REPLACE"))
    (with-temp-buffer
      ;; Set up the buffer
      (insert "Before: " test-content " After")
      
      ;; Define a font-lock function that modifies buffer content
      (setq-local font-lock-keywords
                  `((,test-content . (lambda (end)
                                       (let ((start (match-beginning 0))
                                             (inhibit-read-only t))
                                         (message "Font-lock function called, modifying buffer...")
                                         (goto-char start)
                                         (delete-region start end)
                                         (insert "REPLACED CONTENT")
                                         nil)))))
      
      ;; Enable font-lock
      (font-lock-mode 1)
      (font-lock-fontify-buffer)
      
      (message "Final buffer content: '%s'" (buffer-string))
      (message "Buffer length: %d" (buffer-size))
      
      ;; Return the buffer content for comparison
      (buffer-string))))

(message "Testing buffer modification during font-lock...")
(let ((result (test-font-lock-buffer-modification)))
  (message "Result: %s" result))
