;;; debug-treesitter.el --- Debug tree-sitter issues

(add-to-list 'load-path ".")

;; Check if tree-sitter is available
(message "Tree-sitter available: %s" (treesit-available-p))

;; Try to load the greger parser without requiring other modules first
(condition-case err
    (progn
      (message "Loading greger-parser...")
      (load-file "greger-parser.el")
      (message "greger-parser loaded successfully"))
  (error (message "Error loading greger-parser: %s" err)))

;; Try creating a simple tree-sitter parser for markdown
(condition-case err
    (when (treesit-available-p)
      (message "Attempting to create markdown parser...")
      (let ((parser (treesit-parser-create 'markdown)))
        (message "Markdown parser created successfully: %s" parser)))
  (error (message "Error creating markdown parser: %s" err)))

;;; debug-treesitter.el ends here
