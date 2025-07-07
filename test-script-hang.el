#!/usr/bin/env emacs --script

;; Test script to reproduce the font-lock hang

(add-to-list 'load-path ".")
(require 'greger)

;; Enable font-lock mode explicitly
(setq font-lock-global-modes t)

;; Create a buffer with the problematic content
(with-temp-buffer
  (insert "# SYSTEM\n\nYou are an expert coding agent.\n\n# USER\n\n<eval>")
  
  ;; Enable greger-mode
  (greger-mode)
  
  ;; Force font-lock to be enabled
  (font-lock-mode 1)
  (font-lock-ensure)
  
  ;; Show initial parse tree
  (let ((root-node (treesit-buffer-root-node)))
    (message "Initial parse tree: %s" (treesit-node-string root-node)))
  
  ;; Go to end and add newline
  (goto-char (point-max))
  (insert "\n")
  
  ;; Force font-lock processing
  (font-lock-flush)
  (font-lock-ensure)
  
  ;; Show parse tree after newline
  (let ((root-node (treesit-buffer-root-node)))
    (message "Parse tree after newline: %s" (treesit-node-string root-node)))
  
  ;; Now insert < which should trigger the hang
  (message "About to insert < character...")
  (let ((start-time (current-time)))
    (insert "<")
    
    ;; Force font-lock processing - this might hang
    (font-lock-flush)
    (font-lock-ensure)
    
    (let ((elapsed (time-subtract (current-time) start-time)))
      (message "Successfully processed < in %s seconds" elapsed))
    
    ;; Show final parse tree
    (let ((root-node (treesit-buffer-root-node)))
      (message "Final parse tree: %s" (treesit-node-string root-node)))
    
    (message "Buffer content:\n%s" (buffer-string))))

(message "Test completed without hanging")
