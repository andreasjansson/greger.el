;;; Test to reproduce vterm output issues

(add-to-list 'load-path ".")
(add-to-list 'load-path "~/.emacs.d/elpa/vterm-20241218.331")

(require 'greger-stdlib)
(require 'vterm)

(defvar test-output "")
(defvar test-error nil)
(defvar test-completed nil)

(defun test-vterm-ls ()
  "Test the vterm ls command and capture all output."
  (interactive)
  (setq test-output "")
  (setq test-error nil)
  (setq test-completed nil)
  
  (message "Starting vterm ls test...")
  
  (greger-stdlib--shell-command
   "ls"                                    ; command
   (lambda (output error)                  ; callback
     (setq test-output output)
     (setq test-error error)
     (setq test-completed t)
     (message "=== FINAL CALLBACK ===")
     (message "Output length: %d" (length (or output "")))
     (message "Error: %s" error)
     (message "First 200 chars: %s" (substring (or output "") 0 (min 200 (length (or output ""))))))
   nil                                     ; working-directory
   nil                                     ; timeout
   nil                                     ; enable-environment
   t                                       ; use-vterm
   (lambda (text)                          ; streaming-callback
     (message "=== STREAMING: %s ===" (prin1-to-string text)))
   '(:safe-shell-commands ("ls") :allow-all-shell-commands t)) ; metadata
  
  (message "Command submitted, waiting for completion...")
  
  ;; Wait for completion
  (let ((wait-count 0))
    (while (and (not test-completed) (< wait-count 100))
      (sleep-for 0.1)
      (setq wait-count (1+ wait-count)))
    
    (if test-completed
        (progn
          (message "=== TEST COMPLETED ===")
          (message "Final output length: %d" (length test-output))
          (message "Final error: %s" test-error)
          (message "=== FULL OUTPUT ===")
          (message "%s" test-output)
          (message "=== END OUTPUT ==="))
      (message "Test timed out after 10 seconds"))))

;; Run the test
(test-vterm-ls)
