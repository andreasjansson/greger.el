#!/usr/bin/env emacs --script
;; Test the exact vitest command that's showing issues

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

;; Create metadata to allow all shell commands for testing
(setq test-metadata '(:allow-all-shell-commands t))

;; Test the exact vitest command
(message "=== Testing exact vitest command ===")
(let ((test-completed nil)
      (test-result nil)
      (test-error nil)
      (streaming-messages '()))
  
  (defun test-callback (result error)
    (setq test-completed t)
    (setq test-result result)
    (setq test-error error))
  
  (defun test-streaming (text)
    (push text streaming-messages)
    (when (< (length streaming-messages) 3)
      (message "STREAMING: %S" (substring text 0 (min 100 (length text))))))
  
  (greger-stdlib--shell-command
   "npx vitest run test-sa.test.js"
   #'test-callback
   (expand-file-name "~/r8/turbopuffer-search")
   60  ; 60 second timeout
   nil
   t ; use-vterm = true
   #'test-streaming
   test-metadata)
  
  ;; Wait for completion
  (let ((counter 0))
    (while (and (not test-completed) (< counter 600)) ; 60 second timeout
      (sleep-for 0.1)
      (setq counter (1+ counter))))
  
  (if test-completed
      (progn
        (message "✅ Test completed")
        (message "Error: %S" test-error)
        
        ;; Analyze the result
        (message "\n--- Result Analysis ---")
        (let ((result-str (if (stringp test-result) test-result test-result)))
          (message "Result length: %d" (length result-str))
          (message "First 100 chars: %S" (substring result-str 0 (min 100 (length result-str))))
          
          ;; Check first few characters
          (message "\n--- First 10 Characters ---")
          (dotimes (i (min 10 (length result-str)))
            (let ((char (aref result-str i))
                  (props (text-properties-at i result-str)))
              (message "Char %d: %c (byte %d) props: %S" i char char props)))
          
          ;; Look for color properties
          (message "\n--- Color Properties Check ---")
          (let ((has-colors nil)
                (color-positions '()))
            (dotimes (i (length result-str))
              (let ((props (text-properties-at i result-str)))
                (when (plist-get props 'font-lock-face)
                  (setq has-colors t)
                  (push i color-positions))))
            (if has-colors
                (message "✅ Found colors at positions: %S" (reverse color-positions))
              (message "❌ No color properties found")))
          
          ;; Check for line positioning issues
          (message "\n--- Line Analysis ---")
          (let ((lines (split-string result-str "\n")))
            (message "Number of lines: %d" (length lines))
            (dotimes (i (min 3 (length lines)))
              (message "Line %d: %S" i (nth i lines)))))
        
        ;; Analyze streaming messages
        (message "\n--- Streaming Analysis ---")
        (let ((vterm-messages (seq-filter (lambda (msg) (string-prefix-p "VTERM_FULL_REPLACE:" msg)) streaming-messages)))
          (message "Number of VTERM streaming messages: %d" (length vterm-messages))
          (when vterm-messages
            (let ((first-vterm (car (last vterm-messages))))
              (message "First VTERM message: %S" (substring first-vterm 0 (min 200 (length first-vterm))))))))
    (message "❌ Test timed out")))

(message "\n=== Vitest test completed ===")
