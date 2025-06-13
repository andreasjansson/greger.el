;;; debug-treesit-functions.el --- Check what treesit functions exist

;; Check what treesit functions are available
(message "treesit-available-p exists: %s" (fboundp 'treesit-available-p))
(message "treesit-ready-p exists: %s" (fboundp 'treesit-ready-p))
(message "treesit-parser-create exists: %s" (fboundp 'treesit-parser-create))
(message "treesit-language-available-p exists: %s" (fboundp 'treesit-language-available-p))

;; Try alternative functions
(when (treesit-available-p)
  (condition-case err
      (let ((parser (treesit-parser-create 'greger)))
        (message "Direct parser creation works: %s" (not (null parser))))
    (error (message "Direct parser creation failed: %s" err))))

;;; debug-treesit-functions.el ends here
