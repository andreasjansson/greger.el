#!/usr/bin/env emacs --script
;; Test script to verify vterm functionality is working

;; Load the necessary files
(add-to-list 'load-path "/Users/andreas/projects/greger.el")
(require 'greger-stdlib)

;; Set up permissions for shell commands
(setq greger-allow-all-shell-commands t)

;; Test function
(defun test-vterm-command ()
  "Test vterm shell command execution"
  (let ((test-completed nil)
        (test-result nil)
        (test-error nil))
    
    ;; Test callback function
    (defun test-callback (result error)
      (setq test-completed t)
      (setq test-result result)
      (setq test-error error)
      (message "CALLBACK: result=%S error=%S" result error))
    
    ;; Test streaming callback
    (defun test-streaming (text)
      (message "STREAMING: %s" text))
    
    ;; Run the vterm command
    (message "Testing vterm command: echo 'Hello vterm world!'")
    (greger-stdlib--run-shell-command-with-vterm
     "echo 'Hello vterm world!'"
     default-directory
     #'test-callback
     10
     nil
     #'test-streaming)
    
    ;; Wait for completion
    (let ((counter 0))
      (while (and (not test-completed) (< counter 100))
        (sleep-for 0.1)
        (setq counter (1+ counter))))
    
    ;; Report results
    (if test-completed
        (progn
          (message "TEST COMPLETED!")
          (message "Result: %S" test-result)
          (message "Error: %S" test-error))
      (message "TEST FAILED: Command did not complete within timeout"))))

;; Test regular shell command function
(defun test-regular-shell-command ()
  "Test regular shell command for comparison"
  (let ((test-completed nil)
        (test-result nil)
        (test-error nil))
    
    ;; Test callback function
    (defun test-callback (result error)
      (setq test-completed t)
      (setq test-result result)
      (setq test-error error)
      (message "REGULAR CALLBACK: result=%S error=%S" result error))
    
    ;; Run the regular command
    (message "Testing regular command: echo 'Hello regular world!'")
    (greger-stdlib--shell-command
     "echo 'Hello regular world!'"
     #'test-callback
     default-directory
     10
     nil
     nil
     nil
     '(:allow-all-shell-commands t))
    
    ;; Wait for completion
    (let ((counter 0))
      (while (and (not test-completed) (< counter 100))
        (sleep-for 0.1)
        (setq counter (1+ counter))))
    
    ;; Report results
    (if test-completed
        (progn
          (message "REGULAR TEST COMPLETED!")
          (message "Result: %S" test-result)
          (message "Error: %S" test-error))
      (message "REGULAR TEST FAILED: Command did not complete within timeout"))))

;; Run the tests
(message "=== STARTING VTERM TESTS ===")
(test-regular-shell-command)
(message "")
(test-vterm-command)
(message "=== TESTS COMPLETE ===")
