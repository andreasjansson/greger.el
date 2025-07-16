#!/usr/bin/env emacs --script
;; Comprehensive test for vterm fixes

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

;; Test 1: Basic output with potential first character issue
(message "=== Test 1: Basic output ===")
(let ((test-completed nil)
      (test-result nil)
      (test-error nil))
  
  (defun test-callback (result error)
    (setq test-completed t)
    (setq test-result result)
    (setq test-error error))
  
  (defun test-streaming (text)
    nil) ; Silent streaming
  
  (if (fboundp 'greger-stdlib--run-shell-command-with-vterm)
      (progn
        (condition-case err
            (greger-stdlib--run-shell-command-with-vterm
             "echo 'Test123'"
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
            (let ((result-str (if (stringp test-result) test-result (substring-no-properties test-result))))
              (message "Result: %S" result-str)
              (if (string-prefix-p "Test123" result-str)
                  (message "✅ First character NOT cut off")
                (message "❌ First character may be cut off")))
          (message "TIMEOUT: Test did not complete")))
    (message "FAILURE: Function not found")))

;; Test 2: Empty lines preservation
(message "\n=== Test 2: Empty lines preservation ===")
(let ((test-completed nil)
      (test-result nil)
      (test-error nil))
  
  (defun test-callback (result error)
    (setq test-completed t)
    (setq test-result result)
    (setq test-error error))
  
  (defun test-streaming (text)
    nil) ; Silent streaming
  
  (condition-case err
      (greger-stdlib--run-shell-command-with-vterm
       "printf 'Line1\\n\\nLine3\\n\\nLine5'"
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
      (let ((result-str (if (stringp test-result) test-result (substring-no-properties test-result))))
        (message "Result: %S" result-str)
        (let ((lines (split-string result-str "\n")))
          (message "Lines: %S" lines)
          (if (and (= (length lines) 5)
                   (string= (nth 0 lines) "Line1")
                   (string= (nth 1 lines) "")
                   (string= (nth 2 lines) "Line3")
                   (string= (nth 3 lines) "")
                   (string= (nth 4 lines) "Line5"))
              (message "✅ Empty lines are preserved correctly")
            (message "❌ Empty lines are not preserved correctly"))))
    (message "TIMEOUT: Test did not complete")))

;; Test 3: Form feed character filtering
(message "\n=== Test 3: Form feed character filtering ===")
(let ((test-completed nil)
      (test-result nil)
      (test-error nil))
  
  (defun test-callback (result error)
    (setq test-completed t)
    (setq test-result result)
    (setq test-error error))
  
  (defun test-streaming (text)
    nil) ; Silent streaming
  
  (condition-case err
      (greger-stdlib--run-shell-command-with-vterm
       "printf 'Before\\fAfter'"
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
      (let ((result-str (if (stringp test-result) test-result (substring-no-properties test-result))))
        (message "Result: %S" result-str)
        (let ((result-bytes (string-to-list result-str)))
          (if (memq 12 result-bytes) ; 12 is form feed
              (message "❌ Form feed character is NOT filtered out")
            (message "✅ Form feed character is filtered out correctly"))))
    (message "TIMEOUT: Test did not complete")))

(message "\n=== Tests completed ===")
