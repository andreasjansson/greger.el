;;; Test various vterm commands

(add-to-list 'load-path ".")
(add-to-list 'load-path "~/.emacs.d/elpa/vterm-20241218.331")

(require 'greger-stdlib)
(require 'vterm)

(defun test-vterm-command (command description)
  "Test a vterm command and print results."
  (message "=== Testing %s ===" description)
  (let ((test-output "")
        (test-error nil)
        (test-completed nil))
    
    (greger-stdlib--shell-command
     command
     (lambda (output error)
       (setq test-output output)
       (setq test-error error)
       (setq test-completed t))
     nil                                     ; working-directory
     nil                                     ; timeout
     nil                                     ; enable-environment
     t                                       ; use-vterm
     nil                                     ; streaming-callback
     '(:safe-shell-commands ("ls" "echo" "date" "pwd") :allow-all-shell-commands t)) ; metadata
    
    ;; Wait for completion
    (let ((wait-count 0))
      (while (and (not test-completed) (< wait-count 50))
        (sleep-for 0.1)
        (setq wait-count (1+ wait-count))))
    
    (if test-completed
        (progn
          (message "✅ %s completed successfully" description)
          (message "Output: %s" (substring test-output 0 (min 100 (length test-output))))
          (message "Error: %s" test-error))
      (message "❌ %s timed out" description))))

;; Test various commands
(test-vterm-command "echo 'Hello World'" "echo command")
(test-vterm-command "date" "date command")
(test-vterm-command "pwd" "pwd command")
(test-vterm-command "ls -la | head -5" "pipe command")
(test-vterm-command "echo 'Colors: \\e[31mRed\\e[0m \\e[32mGreen\\e[0m \\e[34mBlue\\e[0m'" "colored output")

(message "=== All tests completed ===")
