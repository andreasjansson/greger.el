;; Debug script for eval folding issues
(require 'greger)

;; Load the test file
(let ((test-file (expand-file-name "test-eval.greger")))
  (when (file-exists-p test-file)
    (find-file test-file)
    (greger-mode)
    
    ;; Enable folding mode
    (setq greger-ui-folding-mode t)
    
    ;; Check the current state
    (message "Folding mode: %s" greger-ui-folding-mode)
    (message "Current major mode: %s" major-mode)
    
    ;; Try to trigger fontification
    (font-lock-flush)
    (font-lock-ensure)
    
    ;; Look at the first eval result
    (goto-char (point-min))
    (when (search-forward "<eval-result-abc123>" nil t)
      (let ((start (match-beginning 0))
            (end (match-end 0)))
        (message "Found eval result tag at %d-%d" start end)
        (message "invisible property: %s" (get-text-property start 'invisible))
        (message "display property: %s" (get-text-property start 'display))))))
