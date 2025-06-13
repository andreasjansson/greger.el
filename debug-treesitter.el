;;; debug-treesitter.el --- Debug tree-sitter issues

(add-to-list 'load-path ".")

;; Check if tree-sitter is available
(message "Tree-sitter available: %s" (treesit-available-p))

;; Check what languages are available
(when (treesit-available-p)
  (message "Available languages: %s" (treesit-available-languages)))

;; Try to load the greger parser without requiring other modules first
(condition-case err
    (progn
      (message "Loading greger-parser...")
      (load-file "greger-parser.el")
      (message "greger-parser loaded successfully"))
  (error (message "Error loading greger-parser: %s" err)))

;; Check if markdown language is available
(when (treesit-available-p)
  (message "Markdown language available: %s" 
           (member 'markdown (treesit-available-languages))))

;; Try creating a simple tree-sitter parser for markdown
(condition-case err
    (when (and (treesit-available-p) 
               (member 'markdown (treesit-available-languages)))
      (message "Attempting to create markdown parser...")
      (let ((parser (treesit-parser-create 'markdown)))
        (message "Markdown parser created successfully: %s" parser)))
  (error (message "Error creating markdown parser: %s" err)))

;;; debug-treesitter.el ends here
