;; Test the new approach
(add-to-list 'load-path ".")
(require 'greger)

(let ((test-file (expand-file-name "test-simple.greger")))
  (when (file-exists-p test-file)
    (find-file test-file)
    (greger-mode)
    
    ;; Test with folding mode disabled
    (setq greger-ui-folding-mode nil)
    (font-lock-flush)
    (font-lock-ensure)
    (message "=== FOLDING MODE DISABLED ===")
    (message "Should see tags normally")
    
    ;; Test with folding mode enabled
    (setq greger-ui-folding-mode t)
    (font-lock-flush)
    (font-lock-ensure)
    (message "=== FOLDING MODE ENABLED ===")
    (message "Should see arrow and folded content")))
