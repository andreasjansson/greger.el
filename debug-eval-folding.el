;; Debug script for eval folding issues
(add-to-list 'load-path ".")
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
        (message "display property: %s" (get-text-property start 'display))
        
        ;; Check the content after the tag
        (let ((content-start end)
              (content-end (when (search-forward "</eval-result-abc123>" nil t)
                             (match-beginning 0))))
          (when content-end
            (message "Content from %d to %d: '%s'" content-start content-end 
                     (buffer-substring-no-properties content-start content-end))
            (message "Content display property: %s" (get-text-property content-start 'display))
            (message "Content invisible property: %s" (get-text-property content-start 'invisible))))))))
