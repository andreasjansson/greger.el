;;; test-eval-final.el --- Final test for eval functionality

;; Load greger modules
(add-to-list 'load-path ".")
(require 'greger)
(require 'greger-ui)

(defun test-eval-final ()
  "Final test of eval result display functionality."
  (interactive)
  
  ;; Open test file
  (find-file "test-eval.greger")
  
  ;; Enable greger-mode
  (greger-mode)
  
  ;; Enable folding mode
  (setq greger-ui-folding-mode t)
  
  ;; Force fontification to apply all font-lock rules
  (font-lock-flush)
  (font-lock-ensure)
  
  ;; Check if eval results are found
  (let ((root (treesit-buffer-root-node)))
    (let ((eval-nodes (treesit-query-capture root '((eval) @eval)))
          (eval-result-nodes (treesit-query-capture root '((eval_result) @eval-result))))
      (message "SUCCESS: Found %d eval nodes and %d eval-result nodes" 
               (length eval-nodes) (length eval-result-nodes))
      
      ;; Check for folding properties
      (dolist (capture eval-result-nodes)
        (let* ((node (cdr capture))
               (start (treesit-node-start node))
               (end (treesit-node-end node))
               (content-node (treesit-search-subtree node "eval_result_content")))
          (when content-node
            (let ((content-start (treesit-node-start content-node)))
              (message "Eval result at %d-%d, content at %d" 
                       start end content-start)
              
              ;; Check if folding properties are applied
              (let ((expandable (get-text-property content-start 'greger-ui-eval-result-expandable))
                    (face (get-text-property content-start 'font-lock-face))
                    (eval-parent-node (treesit-parent-until node (lambda (n) (string= (treesit-node-type n) "eval"))))
                    (eval-content-node (when eval-parent-node
                                         (treesit-search-subtree eval-parent-node "eval_content"))))
                (message "  - Expandable: %s, Face: %s" expandable face)
                
                ;; Check for arrow overlays
                (when eval-content-node
                  (let* ((eval-content-end (treesit-node-end eval-content-node))
                         (overlays-at-pos (overlays-at eval-content-end))
                         (arrow-overlays (seq-filter (lambda (ov) (overlay-get ov 'greger-ui-eval-arrow-overlay)) overlays-at-pos)))
                    (message "  - Arrow overlays at pos %d: %d found" eval-content-end (length arrow-overlays))
                    (dolist (ov arrow-overlays)
                      (message "    - Arrow overlay: %s" (overlay-get ov 'after-string)))))))))))))

;; Run the test
(test-eval-final)

(message "\nTest completed! Open test-eval.greger in Emacs with greger-mode to see the eval result display.")
(message "Use TAB on eval results to toggle folding.")

;;; test-eval-final.el ends here
