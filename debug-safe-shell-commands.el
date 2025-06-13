;;; debug-safe-shell-commands.el --- Test safe shell commands extraction

(add-to-list 'load-path ".")

(require 'greger-parser)

(message "Testing safe shell commands extraction...")

;; Create a test buffer with and without safe shell commands
(let ((test-buffer-simple (generate-new-buffer "*test-simple*"))
      (test-buffer-with-commands (generate-new-buffer "*test-with-commands*")))
  
  ;; Simple buffer without safe shell commands
  (with-current-buffer test-buffer-simple
    (insert "# SYSTEM\n\nYou are a helpful assistant.\n\n# USER\n\nTest message"))
  
  ;; Buffer with safe shell commands (if that's how they're supposed to be formatted)
  (with-current-buffer test-buffer-with-commands
    (insert "# SYSTEM\n\nYou are a helpful assistant.\n\n<safe_shell_commands>\nls\necho hello\n</safe_shell_commands>\n\n# USER\n\nTest message"))
  
  (message "Test buffers created")
  
  ;; Test with simple buffer first
  (condition-case err
      (progn
        (message "Testing safe shell commands extraction with simple buffer...")
        (greger-parser-activate-tree-sitter)
        (with-current-buffer test-buffer-simple
          (let* ((parser (treesit-parser-create 'greger))
                 (root-node (treesit-parser-root-node parser)))
            (message "Parser created successfully")
            (message "Root node: %s" root-node)
            (message "Root node type: %s" (treesit-node-type root-node))
            
            ;; Test treesit-search-subtree function specifically
            (message "Testing treesit-search-subtree...")
            (let ((safe-commands-node (treesit-search-subtree root-node "safe_shell_commands")))
              (message "safe_shell_commands node: %s" safe-commands-node)))))
    (error (message "Error with simple buffer safe shell commands test: %s" err)))
  
  ;; Test with buffer that has safe shell commands
  (condition-case err
      (progn
        (message "Testing safe shell commands extraction with commands buffer...")
        (with-current-buffer test-buffer-with-commands
          (let* ((parser (treesit-parser-create 'greger))
                 (root-node (treesit-parser-root-node parser)))
            (message "Parser created for commands buffer")
            (let ((safe-commands-node (treesit-search-subtree root-node "safe_shell_commands")))
              (message "safe_shell_commands node in commands buffer: %s" safe-commands-node)))))
    (error (message "Error with commands buffer test: %s" err)))
  
  ;; Now test the actual function
  (condition-case err
      (progn
        (message "Testing greger-parser-find-safe-shell-commands-in-buffer...")
        (let ((result (greger-parser-find-safe-shell-commands-in-buffer test-buffer-simple)))
          (message "Result: %S" result)))
    (error (message "Error with greger-parser-find-safe-shell-commands-in-buffer: %s" err)))
  
  ;; Clean up
  (kill-buffer test-buffer-simple)
  (kill-buffer test-buffer-with-commands))

(message "Safe shell commands test completed")

;;; debug-safe-shell-commands.el ends here
