;;; debug-eval-display.el --- Debug eval display issues

(defun debug-eval-display ()
  "Debug eval display issues in current buffer."
  (interactive)
  (when (not (derived-mode-p 'greger-mode))
    (error "Must be in greger-mode"))
  
  (message "=== EVAL DISPLAY DEBUG ===")
  (message "Folding mode: %s" greger-ui-folding-mode)
  (message "Font-lock mode: %s" font-lock-mode)
  (message "Font-lock level: %s" treesit-font-lock-level)
  
  ;; Force font-lock refresh
  (font-lock-flush)
  (font-lock-ensure)
  
  ;; Check what's at the first eval result position
  (let ((root (treesit-buffer-root-node)))
    (let ((eval-nodes (treesit-query-capture root '((eval) @eval))))
      (message "Found %d eval nodes" (length eval-nodes))
      
      (when eval-nodes
        (let* ((first-eval (cdr (car eval-nodes)))
               (eval-result (treesit-search-subtree first-eval "eval_result"))
               (eval-result-start-tag (treesit-search-subtree first-eval "eval_result_start_tag")))
          
          (when eval-result-start-tag
            (let* ((tag-start (treesit-node-start eval-result-start-tag))
                   (tag-end (treesit-node-end eval-result-start-tag))
                   (tag-text (treesit-node-text eval-result-start-tag))
                   (display-prop (get-text-property tag-start 'display))
                   (invisible-prop (get-text-property tag-start 'invisible))
                   (face-prop (get-text-property tag-start 'font-lock-face)))
              
              (message "First eval result start tag:")
              (message "  Position: %d-%d" tag-start tag-end)
              (message "  Text: '%s'" tag-text)
              (message "  Display property: %s" display-prop)
              (message "  Invisible property: %s" invisible-prop)
              (message "  Face property: %s" face-prop)
              
              ;; Check each character in the tag
              (message "  Character analysis:")
              (dotimes (i (min 5 (- tag-end tag-start)))
                (let* ((pos (+ tag-start i))
                       (char (char-after pos))
                       (disp (get-text-property pos 'display))
                       (inv (get-text-property pos 'invisible)))
                  (message "    %d: '%c' display=%s invisible=%s" 
                           pos char disp inv))))))
        
        ;; Check if folding function was called
        (message "Manually calling eval result folding function...")
        (when eval-nodes
          (let* ((first-eval (cdr (car eval-nodes)))
                 (eval-result (treesit-search-subtree first-eval "eval_result")))
            (when eval-result
              (greger-ui--eval-result-folding eval-result nil nil nil))))))))

(defun fix-eval-display ()
  "Try to fix eval display by refreshing font-lock."
  (interactive)
  (when (not (derived-mode-p 'greger-mode))
    (error "Must be in greger-mode"))
  
  (message "Fixing eval display...")
  
  ;; Clear all eval-related text properties first
  (save-excursion
    (goto-char (point-min))
    (while (< (point) (point-max))
      (let ((display-prop (get-text-property (point) 'display)))
        (when (and display-prop (stringp display-prop) (string-match-p "⇒" display-prop))
          (remove-text-properties (point) (1+ (point)) '(display nil))))
      (goto-char (1+ (point)))))
  
  ;; Force complete font-lock refresh
  (setq treesit-font-lock-level 4)
  (font-lock-mode -1)
  (font-lock-mode 1)
  (font-lock-flush (point-min) (point-max))
  (font-lock-ensure (point-min) (point-max))
  
  (message "Font-lock refreshed. Check if arrows appear now."))

;;; debug-eval-display.el ends here
