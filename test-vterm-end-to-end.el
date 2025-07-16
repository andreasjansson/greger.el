;;; Test vterm implementation end-to-end with actual greger.el

(add-to-list 'load-path ".")
(add-to-list 'load-path "~/.emacs.d/elpa/vterm-20241218.331")

(require 'greger-stdlib)
(require 'greger)
(require 'vterm)

(defun test-vterm-end-to-end ()
  "Test the complete end-to-end vterm behavior exactly as it would work in greger."
  (interactive)
  (message "=== TESTING VTERM END-TO-END ===")
  
  ;; Create a buffer that simulates the greger chat buffer
  (let ((test-buffer (get-buffer-create "*test-greger-chat*")))
    (with-current-buffer test-buffer
      (erase-buffer)
      ;; Insert a mock tool result structure
      (insert "<function_calls>\n")
      (insert "<invoke name=\"shell-command\">\n")
      (insert "<parameter name=\"command\">ls -la</parameter>\n")
      (insert "<parameter name=\"use-vterm\">true</parameter>\n")
      (insert "</invoke>\n")
      (insert "</function_calls>\n\n")
      (insert "<function_results>\n")
      (insert "</function_results>\n")
      
      ;; Set up treesit mode (simplified)
      (setq buffer-read-only nil)
      
      ;; Create a mock state
      (let ((mock-state (make-hash-table :test 'equal)))
        (puthash :buffer test-buffer mock-state)
        (puthash :live-chat-buffer test-buffer mock-state)
        
        ;; Mock the greger--find-tool-result-content-node function
        (defun greger--find-tool-result-content-node (tool-id)
          "Mock function to find tool result content node."
          (with-current-buffer test-buffer
            (goto-char (point-min))
            (when (search-forward "<function_results>" nil t)
              (let ((start (point))
                    (end (if (search-forward "</function_results>" nil t)
                           (match-beginning 0)
                         (point-max))))
                ;; Return a mock node-like structure
                (list :start start :end end)))))
        
        ;; Mock treesit functions
        (defun treesit-node-start (node)
          (plist-get node :start))
        
        (defun treesit-node-end (node)
          (plist-get node :end))
        
        (defun greger--maybe-save-excursion (&rest body)
          (apply #'progn body))
        
        ;; Find the tool result content area
        (goto-char (point-min))
        (search-forward "<function_results>")
        (let ((content-start (point))
              (content-end (progn (search-forward "</function_results>") (match-beginning 0))))
          
          (message "Content area: %d to %d" content-start content-end)
          (message "Initial content: '%s'" (buffer-substring content-start content-end))
          
          ;; Test the streaming function directly
          (let ((tool-id "test-tool"))
            (message "Testing streaming callbacks...")
            
            ;; Simulate streaming calls
            (greger--append-tool-result-text mock-state tool-id "VTERM_FULL_REPLACE:")
            (message "After empty replace: '%s'" (buffer-substring content-start content-end))
            
            (greger--append-tool-result-text mock-state tool-id "VTERM_FULL_REPLACE:Hello World")
            (message "After 'Hello World': '%s'" (buffer-substring content-start content-end))
            
            (greger--append-tool-result-text mock-state tool-id "VTERM_FULL_REPLACE:Final Output")
            (message "After 'Final Output': '%s'" (buffer-substring content-start content-end))
            
            ;; Show the final buffer state
            (message "=== FINAL BUFFER STATE ===")
            (message "%s" (buffer-string))
            (message "=== END BUFFER STATE ===")
            
            ;; Test if the content is clean
            (let ((final-content (buffer-substring content-start content-end)))
              (if (string-match "VTERM_FULL_REPLACE:" final-content)
                  (message "❌ FAIL: VTERM_FULL_REPLACE markers still in buffer")
                (message "✅ PASS: Clean buffer without markers")))))))))

;; Run the test
(test-vterm-end-to-end)
