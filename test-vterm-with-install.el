#!/usr/bin/env emacs --script
;; Test vterm with package installation

;; Load the necessary files
(add-to-list 'load-path "/Users/andreas/projects/greger.el")

;; Initialize package system
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Try to install vterm
(unless (package-installed-p 'vterm)
  (package-refresh-contents)
  (package-install 'vterm))

;; Load greger-stdlib
(require 'greger-stdlib)
(setq greger-allow-all-shell-commands t)

;; Test the function with vterm available
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
  
  (message "Testing vterm function with vterm available...")
  
  ;; Test with vterm enabled
  (condition-case err
      (greger-stdlib--shell-command
       "echo 'Hello vterm world!'"   ; command
       #'test-callback               ; callback
       default-directory             ; working-directory
       10                           ; timeout
       nil                          ; enable-environment
       t                            ; use-vterm = true
       #'test-streaming             ; streaming-callback
       '(:allow-all-shell-commands t)) ; metadata
    (error (message "VTERM ERROR: %S" err)))
  
  ;; Wait for completion
  (let ((counter 0))
    (while (and (not test-completed) (< counter 100))
      (sleep-for 0.1)
      (setq counter (1+ counter))))
  
  (if test-completed
      (progn
        (message "SUCCESS: Vterm command completed!")
        (message "Result: %S" test-result)
        (message "Error: %S" test-error))
    (message "FAILURE: Vterm command failed or timed out")))
