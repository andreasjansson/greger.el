;;; debug-tree.el --- Debug tree structure

;; Load greger modules
(add-to-list 'load-path ".")
(require 'greger)

(defun print-tree-nodes (node indent)
  "Print all nodes in tree starting from NODE with INDENT."
  (let ((type (treesit-node-type node))
        (start (treesit-node-start node))
        (end (treesit-node-end node)))
    (message "%s%s (%d-%d)" 
             (make-string indent ? ) 
             type start end)
    
    ;; Print children
    (let ((child (treesit-node-child node 0)))
      (while child
        (print-tree-nodes child (+ indent 2))
        (setq child (treesit-node-next-sibling child))))))

;; Test function
(defun debug-tree-structure ()
  "Debug the tree structure of the test file."
  (find-file "test-eval.greger")
  (greger-mode)
  
  (let ((root (treesit-buffer-root-node)))
    (message "=== TREE STRUCTURE ===")
    (print-tree-nodes root 0))
  
  ;; Try to find eval nodes with different queries
  (message "\n=== EVAL NODE QUERIES ===")
  (condition-case err
      (let ((eval-query-result (treesit-query-capture root '((eval) @eval))))
        (message "eval query result: %s" eval-query-result))
    (error (message "eval query error: %s" err)))
  
  (condition-case err
      (let ((eval-result-query-result (treesit-query-capture root '((eval_result) @eval-result))))
        (message "eval_result query result: %s" eval-result-query-result))
    (error (message "eval_result query error: %s" err))))

;; Run the debug
(debug-tree-structure)

;;; debug-tree.el ends here
