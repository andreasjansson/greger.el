#!/usr/bin/env emacs --script
;; Test the fixed vterm function

;; Load the necessary files
(add-to-list 'load-path "/Users/andreas/projects/greger.el")
(require 'greger-stdlib)
(setq greger-allow-all-shell-commands t)

;; Test the fixed function
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
  
  (message "Testing fixed vterm function...")
  
  ;; Test with vterm disabled first
  (message "Testing regular shell command...")
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
    (while (and (not test-completed) (< counter 50))
      (sleep-for 0.1)
      (setq counter (1+ counter))))
  
  (if test-completed
      (message "Regular command result: %S" test-result)
    (message "Regular command failed"))
  
  ;; Reset for vterm test
  (setq test-completed nil test-result nil test-error nil)
  
  ;; Test with vterm enabled
  (message "Testing vterm shell command...")
  (condition-case err
      (greger-stdlib--shell-command
       "echo 'Hello vterm world!'"
       #'test-callback
       default-directory
       10
       nil
       t  ; use-vterm = true
       #'test-streaming
       '(:allow-all-shell-commands t))
    (error (message "VTERM ERROR: %S" err)))
  
  ;; Wait for completion
  (let ((counter 0))
    (while (and (not test-completed) (< counter 50))
      (sleep-for 0.1)
      (setq counter (1+ counter))))
  
  (if test-completed
      (message "Vterm command result: %S" test-result)
    (message "Vterm command failed or timed out")))
