#!/usr/bin/env emacs --script
;; Test to diagnose the positioning issue

(add-to-list 'load-path "/Users/andreas/projects/greger.el")
(load-file "greger-stdlib.el")

(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(unless (package-installed-p 'vterm)
  (package-refresh-contents)
  (package-install 'vterm))

(setq test-metadata '(:allow-all-shell-commands t))

;; Test the vitest command with raw buffer analysis
(let ((test-completed nil)
      (test-result nil)
      (test-error nil))
  
  (defun test-callback (result error)
    (setq test-completed t)
    (setq test-result result)
    (setq test-error error))
  
  (defun test-streaming (text)
    ;; Only show first few streaming messages
    nil)
  
  (greger-stdlib--shell-command
   "npx vitest run test-sa.test.js"
   #'test-callback
   (expand-file-name "~/r8/turbopuffer-search")
   30
   nil
   t
   #'test-streaming
   test-metadata)
  
  (let ((counter 0))
    (while (and (not test-completed) (< counter 300))
      (sleep-for 0.1)
      (setq counter (1+ counter))))
  
  (when test-completed
    (message "First line of result: %S" 
             (car (split-string test-result "\n")))
    (message "First 5 chars: %S" 
             (substring test-result 0 (min 5 (length test-result))))
    (message "Character before first: %S" 
             (if (> (length test-result) 0)
                 (let ((first-char (aref test-result 0)))
                   (format "First char: %c (byte %d)" first-char first-char))
               "Empty result"))))

(message "Position test complete")
