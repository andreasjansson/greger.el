;; Test the final implementation
(add-to-list 'load-path ".")
(require 'greger)

(let ((test-file (expand-file-name "test-eval.greger")))
  (when (file-exists-p test-file)
    (find-file test-file)
    (greger-mode)
    
    ;; Test with folding mode disabled
    (setq greger-ui-folding-mode nil)
    (font-lock-flush)
    (font-lock-ensure)
    (message "=== FOLDING MODE DISABLED ===")
    (save-excursion
      (goto-char (point-min))
      (when (search-forward "<eval-result-abc123>" nil t)
        (let ((start (match-beginning 0))
              (end (match-end 0)))
          (message "Found tag at %d-%d: invisible=%s display=%s" 
                   start end
                   (get-text-property start 'invisible)
                   (get-text-property start 'display)))))
    
    ;; Test with folding mode enabled
    (setq greger-ui-folding-mode t)
    (font-lock-flush)
    (font-lock-ensure)
    (message "=== FOLDING MODE ENABLED ===")
    (save-excursion
      (goto-char (point-min))
      (when (search-forward "<eval-result-abc123>" nil t)
        (let ((start (match-beginning 0))
              (end (match-end 0)))
          (message "Found tag at %d-%d: invisible=%s display=%s" 
                   start end
                   (get-text-property start 'invisible)
                   (get-text-property start 'display)))))))
