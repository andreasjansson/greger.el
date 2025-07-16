#!/usr/bin/env emacs --script
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

(let ((test-completed nil)
      (test-result nil))
  
  (defun test-callback (result error)
    (setq test-completed t)
    (setq test-result result))
  
  (greger-stdlib--shell-command
   "echo 'Hello World'"
   #'test-callback
   default-directory
   10
   nil
   t
   (lambda (text) nil)
   test-metadata)
  
  (let ((counter 0))
    (while (and (not test-completed) (< counter 100))
      (sleep-for 0.1)
      (setq counter (1+ counter))))
  
  (when test-completed
    (message "Result: %S" test-result)
    (message "First char: %c (byte %d)" (aref test-result 0) (aref test-result 0))
    (message "Second char: %c (byte %d)" (aref test-result 1) (aref test-result 1))))
