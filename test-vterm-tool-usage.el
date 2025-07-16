;;; Test vterm implementation with actual tool usage simulation

(add-to-list 'load-path ".")
(add-to-list 'load-path "~/.emacs.d/elpa/vterm-20241218.331")

(require 'greger-stdlib)
(require 'vterm)

(defvar test-tool-output "")

(defun test-streaming-callback (text)
  "Simulate the streaming callback that processes tool output."
  (message "=== STREAMING CALLBACK ===")
  (message "Received: %s" (prin1-to-string text))
  
  ;; Simulate what greger.el does with streaming
  (if (string-prefix-p "VTERM_FULL_REPLACE:" text)
      (let ((vterm-content (substring text (length "VTERM_FULL_REPLACE:"))))
        (message "VTERM full replace with content: %s" (prin1-to-string vterm-content))
        (setq test-tool-output vterm-content))
    (progn
      (message "Regular streaming append: %s" (prin1-to-string text))
      (setq test-tool-output (concat test-tool-output text)))))

(defun test-final-callback (output error)
  "Simulate the final callback."
  (message "=== FINAL CALLBACK ===")
  (message "Final output: %s" (prin1-to-string output))
  (message "Error: %s" error)
  (setq test-tool-output output))

(defun test-vterm-tool-usage ()
  "Test the vterm tool usage simulation."
  (interactive)
  (message "=== TESTING VTERM TOOL USAGE ===")
  (setq test-tool-output "")
  
  ;; This simulates exactly what happens when the shell-command tool is used
  (greger-stdlib--shell-command
   "ls -la"                              ; command
   #'test-final-callback                 ; callback
   nil                                   ; working-directory
   nil                                   ; timeout
   nil                                   ; enable-environment
   t                                     ; use-vterm
   #'test-streaming-callback             ; streaming-callback
   '(:safe-shell-commands ("ls") :allow-all-shell-commands t)) ; metadata
  
  (message "Command submitted, waiting for completion...")
  
  ;; Wait for completion
  (let ((wait-count 0))
    (while (and (string= test-tool-output "") (< wait-count 100))
      (sleep-for 0.1)
      (setq wait-count (1+ wait-count)))
    
    (message "=== FINAL TOOL OUTPUT ===")
    (message "What would appear in tool result:")
    (message "%s" test-tool-output)
    (message "=== END TOOL OUTPUT ===")
    
    (if (string= test-tool-output "")
        (message "❌ Test failed - no output received")
      (if (string-match "VTERM_FULL_REPLACE:" test-tool-output)
          (message "❌ Test failed - VTERM_FULL_REPLACE markers in output")
        (message "✅ Test passed - clean output received")))))

;; Run the test
(test-vterm-tool-usage)
