;;; test-thinking.el --- Test thinking implementation

(require 'greger)
(require 'json)

;; Test that thinking parameter is properly added to request
(let ((greger-thinking-budget 0)
      (model 'claude-sonnet-4-20250514)
      (dialog '(((role . "user") (content . "Hello"))))
      (tools '())
      (server-tools '()))
  
  (let ((request-data (greger-client--build-data model dialog tools server-tools nil)))
    (let ((parsed-json (json-read-from-string request-data)))
      (message "Request has thinking: %s" (assq 'thinking parsed-json))
      (when (assq 'thinking parsed-json)
        (let ((thinking-config (alist-get 'thinking parsed-json)))
          (message "Thinking config: %s" thinking-config)
          (message "Thinking type: %s" (alist-get 'type thinking-config))
          (message "Thinking budget: %s" (alist-get 'budget_tokens thinking-config))))
      
      ;; Test max_tokens includes thinking budget
      (let ((max-tokens (alist-get 'max_tokens parsed-json)))
        (message "Max tokens: %s (should be %s)" max-tokens (+ 8000 greger-thinking-budget))))))

;;; test-thinking.el ends here
