;; Test that eval result tags use tool tag face
(add-to-list 'load-path ".")
(require 'greger)

(let ((test-file (expand-file-name "test-eval.greger")))
  (when (file-exists-p test-file)
    (find-file test-file)
    (greger-mode)
    
    ;; Test with folding mode disabled to see tag styling
    (setq greger-ui-folding-mode nil)
    (font-lock-flush)
    (font-lock-ensure)
    (message "=== FOLDING MODE DISABLED ===")
    (save-excursion
      (goto-char (point-min))
      (when (search-forward "<eval-result-abc123>" nil t)
        (let ((start (match-beginning 0)))
          (message "Eval result start tag face: %s" 
                   (get-text-property start 'font-lock-face))))
      (when (search-forward "</eval-result-abc123>" nil t)
        (let ((start (match-beginning 0)))
          (message "Eval result end tag face: %s" 
                   (get-text-property start 'font-lock-face)))))))
