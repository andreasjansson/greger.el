#!/usr/bin/env emacs --script
;;; test-defensive-parsing.el --- Test defensive parsing wrapper

(require 'greger)

(defvar greger--treesit-timeout 2.0
  "Timeout in seconds for tree-sitter operations.")

(defun greger--safe-treesit-buffer-root-node (&optional parser)
  "Safely get tree-sitter root node with timeout protection."
  (let ((start-time (current-time)))
    (catch 'timeout
      (with-timeout (greger--treesit-timeout
                     (throw 'timeout 'timeout))
        (treesit-buffer-root-node parser)))))

(defun greger--detect-problematic-content ()
  "Detect content patterns that might cause tree-sitter to hang."
  (save-excursion
    (goto-char (point-min))
    ;; Look for <eval> followed by newline and < without proper closing
    (re-search-forward "<eval>\\s-*\n\\s-*<[^/>]" nil t)))

;; Test the defensive approach
(with-temp-buffer
  (insert "# SYSTEM\n\nYou are an expert coding agent.\n\n# USER\n\n<eval>\n<")
  
  (greger-mode)
  
  (message "Testing problematic content detection...")
  (if (greger--detect-problematic-content)
      (message "Problematic content detected - avoiding tree-sitter parsing")
    (message "Content appears safe"))
  
  (message "Testing safe tree-sitter parsing...")
  (let ((result (greger--safe-treesit-buffer-root-node)))
    (if (eq result 'timeout)
        (message "Tree-sitter parsing timed out safely")
      (message "Tree-sitter parsing completed: %s" result))))

(message "Defensive parsing test completed")
