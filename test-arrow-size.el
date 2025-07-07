;; Test that arrow is normal size
(add-to-list 'load-path ".")
(require 'greger)

(let ((test-file (expand-file-name "test-eval.greger")))
  (when (file-exists-p test-file)
    (find-file test-file)
    (greger-mode)
    
    ;; Test with folding mode enabled to see arrow
    (setq greger-ui-folding-mode t)
    (font-lock-flush)
    (font-lock-ensure)
    (message "=== FOLDING MODE ENABLED (arrow should be normal size) ===")
    (save-excursion
      (goto-char (point-min))
      (when (search-forward "<eval-result-abc123>" nil t)
        (let ((start (match-beginning 0)))
          (message "Start tag: font-lock-face=%s display=%s" 
                   (get-text-property start 'font-lock-face)
                   (get-text-property start 'display)))))
    
    ;; Test with folding mode disabled to see tag styling
    (setq greger-ui-folding-mode nil)
    (font-lock-flush)
    (font-lock-ensure)
    (message "=== FOLDING MODE DISABLED (tag should be small/gray) ===")
    (save-excursion
      (goto-char (point-min))
      (when (search-forward "<eval-result-abc123>" nil t)
        (let ((start (match-beginning 0)))
          (message "Start tag: font-lock-face=%s display=%s" 
                   (get-text-property start 'font-lock-face)
                   (get-text-property start 'display)))))))
