;;; debug-treesit-require.el --- Check treesit after requiring

;; Explicitly require treesit
(require 'treesit)

;; Check what treesit functions are available after requiring
(message "After requiring treesit:")
(message "treesit-available-p exists: %s" (fboundp 'treesit-available-p))
(message "treesit-ready-p exists: %s" (fboundp 'treesit-ready-p))
(message "treesit-parser-create exists: %s" (fboundp 'treesit-parser-create))
(message "treesit-language-available-p exists: %s" (fboundp 'treesit-language-available-p))

(message "treesit-available-p result: %s" (treesit-available-p))

;;; debug-treesit-require.el ends here
