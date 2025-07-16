#!/usr/bin/env emacs --script
;; Test vterm function directly

;; Load the necessary files
(add-to-list 'load-path "/Users/andreas/projects/greger.el")
(load-file "greger-stdlib.el")

;; Initialize package system for vterm
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Try to install vterm
(unless (package-installed-p 'vterm)
  (package-refresh-contents)
  (package-install 'vterm))

;; Test the function
(let ((test-completed nil)
      (test-result nil)
      (test-error nil))
  
  (defun test-callback (result error)
    (setq test-completed t)
    (setq test-result result)
    (setq test-error error)
    (message "CALLBACK: result=%S error=%S" result error))
  
  (defun test-streaming (text)
    (message "STREAMING: %s" text))
  
  (message "Testing vterm function directly...")
  
  (if (fboundp 'greger-stdlib--run-shell-command-with-vterm)
      (progn
        (message "Function found! Testing...")
        (condition-case err
            (greger-stdlib--run-shell-command-with-vterm
             "echo 'Hello from vterm!'"
             default-directory
             #'test-callback
             10
             nil
             #'test-streaming)
          (error (message "ERROR calling function: %S" err)))
        
        ;; Wait for completion
        (let ((counter 0))
          (while (and (not test-completed) (< counter 100))
            (sleep-for 0.1)
            (setq counter (1+ counter))))
        
        (if test-completed
            (progn
              (message "SUCCESS: Test completed!")
              (message "Result: %S" test-result)
              (message "Error: %S" test-error))
          (message "TIMEOUT: Test did not complete")))
    (message "FAILURE: Function not found")))
