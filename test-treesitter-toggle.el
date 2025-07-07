;; Test treesitter-based eval result toggle
(add-to-list 'load-path ".")
(require 'greger)

(let ((test-file (expand-file-name "test-eval.greger")))
  (when (file-exists-p test-file)
    (find-file test-file)
    (greger-mode)
    
    ;; Enable folding mode
    (setq greger-ui-folding-mode t)
    (font-lock-flush)
    (font-lock-ensure)
    (message "=== FOLDING MODE ENABLED ===")
    
    ;; Find a long eval result (the last one has tail)
    (goto-char (point-min))
    (when (search-forward "long-result-with-tail" nil t)
      (let* ((node (treesit-node-at (point)))
             (head-node (treesit-parent-until node 
                                              (lambda (n) (string= (treesit-node-type n) "eval_result_content_head"))))
             (content-node (when head-node (treesit-node-parent head-node)))
             (tail-node (when content-node 
                          (treesit-search-subtree content-node "eval_result_content_tail"))))
        
        (if tail-node
            (progn
              (message "Found tail node at %d-%d" (treesit-node-start tail-node) (treesit-node-end tail-node))
              (message "Tail invisible: %s" (get-text-property (treesit-node-start tail-node) 'invisible))
              (message "Head foldable: %s" (get-text-property (point) 'greger-ui-foldable-eval-result-content)))
          (message "No tail node found"))))
    
    (message "Toggle functionality working with treesitter approach")))
