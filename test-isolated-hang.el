#!/usr/bin/env emacs --script

;; Test script to isolate the font-lock hang

(add-to-list 'load-path ".")
(require 'greger)

;; Create a buffer with the problematic content
(with-temp-buffer
  (insert "# SYSTEM\n\nYou are an expert coding agent.\n\n# USER\n\n<eval>\n<")
  
  ;; Enable greger-mode
  (greger-mode)
  
  ;; Test tree-sitter parsing without font-lock
  (message "Testing tree-sitter parsing...")
  (let ((root-node (treesit-buffer-root-node)))
    (message "Parse tree: %s" (treesit-node-string root-node)))
  
  ;; Test if specific font-lock rules are causing the issue
  (message "Testing font-lock rules...")
  
  ;; Disable specific font-lock features to isolate the problem
  (setq-local treesit-font-lock-feature-list
              '((tool-tags tool-syntax-highlighting)  ; Remove eval-tags
                (headers folding comments tool-result-syntax)
                (error)))
  
  ;; Re-enable font-lock
  (font-lock-mode -1)
  (font-lock-mode 1)
  (font-lock-ensure)
  
  (message "Font-lock with eval-tags disabled completed")
  
  ;; Now test with eval-tags enabled
  (message "Testing with eval-tags enabled...")
  (setq-local treesit-font-lock-feature-list
              '((tool-tags tool-syntax-highlighting eval-tags)
                (headers folding comments tool-result-syntax)
                (error)))
  
  (font-lock-mode -1)
  (font-lock-mode 1)
  
  ;; This might hang
  (message "About to call font-lock-ensure with eval-tags...")
  (font-lock-ensure)
  
  (message "Font-lock with eval-tags enabled completed")
  (message "Buffer content:\n%s" (buffer-string)))

(message "Test completed without hanging")
