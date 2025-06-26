;;; Debug cache control issue

(load "greger-client.el")

(defun test-cache-control ()
  (let ((messages '(((role . "user") (content . "Hello"))
                    ((role . "assistant") 
                     (content (((type . "thinking") (thinking . "Let me think..."))
                               ((type . "text") (text . "Response text"))))))))
    
    (message "BEFORE:")
    (message "Messages: %S" messages)
    
    (greger-client--add-cache-control messages)
    
    (message "AFTER:")
    (message "Messages: %S" messages)
    
    ;; Check what got modified
    (let* ((assistant-msg (cadr messages))
           (content-blocks (alist-get 'content assistant-msg))
           (thinking-block (car content-blocks))
           (text-block (cadr content-blocks)))
      (message "Thinking block: %S" thinking-block)
      (message "Text block: %S" text-block)
      (message "Thinking has cache_control: %S" (assq 'cache_control thinking-block))
      (message "Text has cache_control: %S" (assq 'cache_control text-block)))))

(test-cache-control)
