;; Test eval result tag styling
(add-to-list 'load-path ".")
(require 'greger)

(let ((test-file (expand-file-name "test-eval.greger")))
  (when (file-exists-p test-file)
    (find-file test-file)
    (greger-mode)
    
    ;; Test with folding mode disabled (tags should be styled and visible)
    (setq greger-ui-folding-mode nil)
    (font-lock-flush)
    (font-lock-ensure)
    (message "=== FOLDING MODE DISABLED (tags styled) ===")
    (save-excursion
      (goto-char (point-min))
      (when (search-forward "<eval-result-abc123>" nil t)
        (let ((start (match-beginning 0))
              (end (match-end 0)))
          (message "Start tag: invisible=%s display=%s face=%s" 
                   (get-text-property start 'invisible)
                   (get-text-property start 'display)
                   (get-text-property start 'font-lock-face))))
      (when (search-forward "</eval-result-abc123>" nil t)
        (let ((start (match-beginning 0))
              (end (match-end 0)))
          (message "End tag: invisible=%s display=%s face=%s" 
                   (get-text-property start 'invisible)
                   (get-text-property start 'display)
                   (get-text-property start 'font-lock-face)))))
    
    ;; Test with folding mode enabled (arrow for start, end hidden)
    (setq greger-ui-folding-mode t)
    (font-lock-flush)
    (font-lock-ensure)
    (message "=== FOLDING MODE ENABLED (arrow + hidden end) ===")
    (save-excursion
      (goto-char (point-min))
      (when (search-forward "<eval-result-abc123>" nil t)
        (let ((start (match-beginning 0))
              (end (match-end 0)))
          (message "Start tag: invisible=%s display=%s face=%s" 
                   (get-text-property start 'invisible)
                   (get-text-property start 'display)
                   (get-text-property start 'font-lock-face))))
      (when (search-forward "</eval-result-abc123>" nil t)
        (let ((start (match-beginning 0))
              (end (match-end 0)))
          (message "End tag: invisible=%s display=%s face=%s" 
                   (get-text-property start 'invisible)
                   (get-text-property start 'display)
                   (get-text-property start 'font-lock-face)))))))
