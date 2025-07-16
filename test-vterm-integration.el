#!/usr/bin/env emacs --script
;; Test vterm integration with greger shell command tool

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

;; Allow all shell commands for testing
(setq greger-allow-all-shell-commands t)

;; Test 1: Basic command with vterm
(message "=== Test 1: Basic command with vterm ===")
(let ((test-completed nil)
      (test-result nil)
      (test-error nil))
  
  (defun test-callback (result error)
    (setq test-completed t)
    (setq test-result result)
    (setq test-error error))
  
  (defun test-streaming (text)
    (when (string-prefix-p "VTERM_FULL_REPLACE:" text)
      (message "✅ VTERM streaming working")))
  
  (greger-stdlib--shell-command
   "echo 'Hello from vterm!'"
   #'test-callback
   default-directory
   10
   nil
   t ; use-vterm = true
   #'test-streaming
   nil)
  
  ;; Wait for completion
  (let ((counter 0))
    (while (and (not test-completed) (< counter 100))
      (sleep-for 0.1)
      (setq counter (1+ counter))))
  
  (if test-completed
      (progn
        (message "✅ Test completed successfully")
        (message "Result: %S" test-result)
        (if test-error
            (message "❌ Error: %S" test-error)
          (message "✅ No errors")))
    (message "❌ Test timed out")))

;; Test 2: Command with colors
(message "\n=== Test 2: Command with colors ===")
(let ((test-completed nil)
      (test-result nil)
      (test-error nil))
  
  (defun test-callback (result error)
    (setq test-completed t)
    (setq test-result result)
    (setq test-error error))
  
  (defun test-streaming (text)
    nil) ; Silent
  
  (greger-stdlib--shell-command
   "printf '\\033[31mRed text\\033[0m and \\033[32mGreen text\\033[0m'"
   #'test-callback
   default-directory
   10
   nil
   t ; use-vterm = true
   #'test-streaming
   nil)
  
  ;; Wait for completion
  (let ((counter 0))
    (while (and (not test-completed) (< counter 100))
      (sleep-for 0.1)
      (setq counter (1+ counter))))
  
  (if test-completed
      (progn
        (message "✅ Color test completed")
        (let ((result-str (if (stringp test-result) test-result (substring-no-properties test-result))))
          (message "Result: %S" result-str)
          (if (get-text-property 0 'font-lock-face test-result)
              (message "✅ Colors preserved (font-lock-face properties found)")
            (message "❓ Colors may not be preserved (no font-lock-face properties)"))))
    (message "❌ Color test timed out")))

(message "\n=== Integration tests completed ===")
(message "To use vterm in greger, add 'use-vterm: true' to your shell-command tool calls.")
