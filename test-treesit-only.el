#!/usr/bin/env emacs --script

;; Test script to isolate tree-sitter parsing issue

(add-to-list 'load-path ".")
(require 'greger)

(message "Testing tree-sitter parsing with problematic content...")

(with-temp-buffer
  (insert "# SYSTEM\n\nYou are an expert coding agent.\n\n# USER\n\n<eval>\n<")
  
  ;; Only test tree-sitter parsing without enabling greger-mode
  (message "Creating tree-sitter parser...")
  (treesit-parser-create 'greger)
  
  (message "Getting root node...")
  (let ((root-node (treesit-buffer-root-node)))
    (message "Root node: %s" root-node)
    
    (message "Getting node string...")
    (let ((node-string (treesit-node-string root-node)))
      (message "Parse tree: %s" node-string))))

(message "Tree-sitter test completed")
