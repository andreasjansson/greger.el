;;; debug-greger-grammar.el --- Debug greger tree-sitter grammar loading

(add-to-list 'load-path ".")

;; Check if tree-sitter is available
(message "Tree-sitter available: %s" (treesit-available-p))
(message "Current directory: %s" default-directory)

;; Check what paths are in treesit-extra-load-path
(message "treesit-extra-load-path before: %s" treesit-extra-load-path)

;; Add the grammar directory to load path manually
(let ((grammar-dir (expand-file-name "grammar" default-directory)))
  (message "Grammar directory: %s" grammar-dir)
  (message "Grammar directory exists: %s" (file-exists-p grammar-dir))
  (message "Library file exists: %s" 
           (file-exists-p (expand-file-name "libtree-sitter-greger.dylib" grammar-dir)))
  
  (add-to-list 'treesit-extra-load-path grammar-dir)
  (message "treesit-extra-load-path after: %s" treesit-extra-load-path))

;; Try to check if greger language is ready
(condition-case err
    (progn
      (message "Checking if greger parser is ready...")
      (if (treesit-ready-p 'greger)
          (message "Greger parser is ready!")
        (message "Greger parser is NOT ready")))
  (error (message "Error checking greger parser: %s" err)))

;; Try creating a greger parser
(condition-case err
    (when (treesit-available-p)
      (message "Attempting to create greger parser...")
      (let ((parser (treesit-parser-create 'greger)))
        (message "Greger parser created successfully: %s" parser)))
  (error (message "Error creating greger parser: %s" err)))

;;; debug-greger-grammar.el ends here
