;;; Test vterm implementation simulating greger.el behavior

(add-to-list 'load-path ".")
(add-to-list 'load-path "~/.emacs.d/elpa/vterm-20241218.331")

(require 'greger-stdlib)
(require 'vterm)

(defvar test-tool-result "")

(defun test-greger-streaming (text)
  "Simulate greger.el streaming behavior."
  (message "=== GREGER STREAMING ===")
  (message "Processing: %s" (substring (prin1-to-string text) 0 (min 100 (length (prin1-to-string text)))))
  
  ;; Simulate what greger.el does
  (if (string-prefix-p "VTERM_FULL_REPLACE:" text)
      (let ((vterm-content (substring text (length "VTERM_FULL_REPLACE:"))))
        (message "Full replace with content length: %d" (length vterm-content))
        (setq test-tool-result vterm-content))
    (progn
      (message "Regular append: %s" (substring text 0 (min 50 (length text))))
      (setq test-tool-result (concat test-tool-result text)))))

(defun test-final-callback (output error)
  "Final callback."
  (message "=== FINAL CALLBACK ===")
  (message "Final output length: %d" (length output))
  (message "Error: %s" error)
  (when output
    (setq test-tool-result output)))

(defun test-vterm-greger-behavior ()
  "Test the complete vterm + greger behavior."
  (interactive)
  (message "=== TESTING VTERM + GREGER BEHAVIOR ===")
  (setq test-tool-result "")
  
  ;; Test with ls command
  (greger-stdlib--shell-command
   "ls -la | head -10"
   #'test-final-callback
   nil                                   ; working-directory
   nil                                   ; timeout
   nil                                   ; enable-environment
   t                                     ; use-vterm
   #'test-greger-streaming               ; streaming-callback
   '(:safe-shell-commands ("ls") :allow-all-shell-commands t))
  
  (message "Command submitted...")
  
  ;; Wait for completion
  (let ((wait-count 0)
        (last-length 0))
    (while (and (< wait-count 100) 
                (or (string= test-tool-result "")
                    (not (equal (length test-tool-result) last-length))))
      (setq last-length (length test-tool-result))
      (sleep-for 0.1)
      (setq wait-count (1+ wait-count)))
    
    (message "=== FINAL RESULT ===")
    (message "Tool result length: %d" (length test-tool-result))
    (message "First 200 chars of result:")
    (message "%s" (substring test-tool-result 0 (min 200 (length test-tool-result))))
    (message "...")
    (message "Last 200 chars of result:")
    (let ((start (max 0 (- (length test-tool-result) 200))))
      (message "%s" (substring test-tool-result start)))
    
    (if (string-match "VTERM_FULL_REPLACE:" test-tool-result)
        (message "❌ FAIL: VTERM_FULL_REPLACE markers still in result")
      (message "✅ PASS: Clean result without markers"))))

;; Run the test
(test-vterm-greger-behavior)
