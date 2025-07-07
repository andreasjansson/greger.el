;; Test overlay cleanup
(add-to-list 'load-path ".")
(require 'greger)

(let ((test-file (expand-file-name "test-eval.greger")))
  (when (file-exists-p test-file)
    (find-file test-file)
    (greger-mode)
    
    ;; Start with folding mode enabled
    (setq greger-ui-folding-mode t)
    (font-lock-flush)
    (font-lock-ensure)
    (message "=== FOLDING MODE ENABLED ===")
    (message "Overlays count: %d" (length (overlays-in (point-min) (point-max))))
    
    ;; Toggle folding mode off
    (setq greger-ui-folding-mode nil)
    (font-lock-flush)
    (font-lock-ensure)
    (message "=== FOLDING MODE DISABLED ===")
    (message "Overlays count: %d" (length (overlays-in (point-min) (point-max))))
    
    ;; Toggle folding mode back on
    (setq greger-ui-folding-mode t)
    (font-lock-flush)
    (font-lock-ensure)
    (message "=== FOLDING MODE RE-ENABLED ===")
    (message "Overlays count: %d" (length (overlays-in (point-min) (point-max))))))
