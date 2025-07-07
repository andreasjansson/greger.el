#!/usr/bin/env emacs --script

;; Test to isolate exactly where treesit-buffer-root-node is called

(add-to-list 'load-path ".")
(require 'greger)

;; Override treesit-buffer-root-node to log when it's called
(defvar original-treesit-buffer-root-node (symbol-function 'treesit-buffer-root-node))

(defun treesit-buffer-root-node (&optional parser)
  (message "TRACE: treesit-buffer-root-node called from: %s" 
           (mapconcat 'identity (butlast (split-string (backtrace-to-string (backtrace)) "\n")) "\n  "))
  (funcall original-treesit-buffer-root-node parser))

(message "Testing with traced treesit-buffer-root-node...")

(with-temp-buffer
  (insert "# SYSTEM\n\nYou are an expert coding agent.\n\n# USER\n\n<eval>\n<")
  
  (message "About to enable greger-mode...")
  (greger-mode)
  
  (message "Greger-mode enabled"))
