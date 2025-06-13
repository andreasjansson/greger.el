;;; debug-treesitter-parsing.el --- Test tree-sitter parsing functions individually

(add-to-list 'load-path ".")

(require 'greger-parser)

(message "Testing tree-sitter parsing functions individually...")

;; Create a test buffer like greger does
(let ((test-buffer (generate-new-buffer "*test-greger*")))
  (with-current-buffer test-buffer
    (insert "# SYSTEM\n\nYou are a helpful assistant.\n\n# USER\n\nTest message"))
  
  (message "Test buffer created with content: %S" 
           (with-current-buffer test-buffer (buffer-string)))

  ;; Test 1: greger-parser-markdown-buffer-to-dialog
  (condition-case err
      (progn
        (message "Testing greger-parser-markdown-buffer-to-dialog...")
        (let ((result (greger-parser-markdown-buffer-to-dialog test-buffer)))
          (message "greger-parser-markdown-buffer-to-dialog result: %S" result)))
    (error (message "Error with greger-parser-markdown-buffer-to-dialog: %s" err)))

  ;; Test 2: greger-parser-find-safe-shell-commands-in-buffer
  (condition-case err
      (progn
        (message "Testing greger-parser-find-safe-shell-commands-in-buffer...")
        (let ((result (greger-parser-find-safe-shell-commands-in-buffer test-buffer)))
          (message "greger-parser-find-safe-shell-commands-in-buffer result: %S" result)))
    (error (message "Error with greger-parser-find-safe-shell-commands-in-buffer: %s" err)))

  ;; Test 3: Try creating parser directly in the buffer context
  (condition-case err
      (progn
        (message "Testing direct parser creation in buffer context...")
        (with-current-buffer test-buffer
          (let* ((parser (treesit-parser-create 'greger))
                 (root-node (treesit-parser-root-node parser)))
            (message "Direct parser creation successful: %s" parser)
            (message "Root node: %s" root-node)
            (message "Root node type: %s" (treesit-node-type root-node)))))
    (error (message "Error with direct parser creation: %s" err)))

  ;; Clean up
  (kill-buffer test-buffer))

(message "Tree-sitter parsing test completed")

;;; debug-treesitter-parsing.el ends here
