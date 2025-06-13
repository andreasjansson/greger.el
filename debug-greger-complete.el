;;; debug-greger-complete.el --- Complete test of greger grammar

(require 'treesit)

(message "Emacs version: %s" emacs-version)
(message "treesit-available-p: %s" (treesit-available-p))

;; Set up the grammar path like greger-parser does
(let ((grammar-dir (expand-file-name "grammar" default-directory)))
  (message "Grammar directory: %s" grammar-dir)
  (message "Grammar directory exists: %s" (file-exists-p grammar-dir))
  (message "Library file exists: %s" 
           (file-exists-p (expand-file-name "libtree-sitter-greger.dylib" grammar-dir)))
  
  (add-to-list 'treesit-extra-load-path grammar-dir)
  (message "treesit-extra-load-path: %s" treesit-extra-load-path))

;; Test treesit-ready-p
(condition-case err
    (progn
      (message "Testing treesit-ready-p for 'greger...")
      (let ((ready (treesit-ready-p 'greger)))
        (message "treesit-ready-p 'greger: %s" ready)))
  (error (message "Error with treesit-ready-p: %s" err)))

;; Test parser creation
(condition-case err
    (progn
      (message "Testing parser creation...")
      (let ((parser (treesit-parser-create 'greger)))
        (message "Parser created: %s" parser)
        ;; Test parsing some simple content
        (with-temp-buffer
          (insert "# USER\n\nHello world\n\n# ASSISTANT\n\nHi there!")
          (treesit-parser-set-buffer parser (current-buffer))
          (let ((root (treesit-parser-root-node parser)))
            (message "Root node: %s" root)
            (message "Root node type: %s" (treesit-node-type root))))))
  (error (message "Error with parser: %s" err)))

;;; debug-greger-complete.el ends here
