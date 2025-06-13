;;; debug-greger-buffer-call.el --- Test greger-buffer function call

(add-to-list 'load-path ".")

(require 'greger)

(message "Starting greger-buffer test...")

(condition-case err
    (progn
      (message "Creating greger buffer...")
      (greger)
      (message "Greger buffer created successfully")
      
      (message "Current buffer: %s" (current-buffer))
      (message "Buffer content before: %S" (buffer-string))
      
      ;; Add a simple message
      (goto-char (point-max))
      (insert "Test message")
      (message "Added test message")
      (message "Buffer content after insert: %S" (buffer-string))
      
      ;; Now call greger-buffer - this is where the segfault likely happens
      (message "About to call greger-buffer...")
      (greger-buffer)
      (message "greger-buffer call completed")
      
      ;; Wait a moment to see if there's any response
      (sit-for 2)
      (message "Final buffer content: %S" (buffer-string))
      
      ;; Clean up
      (kill-buffer (current-buffer)))
  (error (message "Error in greger-buffer test: %s" err)))

(message "greger-buffer test completed")

;;; debug-greger-buffer-call.el ends here
