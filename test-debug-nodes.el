;; Debug eval result node structure  
(add-to-list 'load-path ".")
(require 'greger)

(defun print-tree-recursive (node indent)
  "Print tree structure recursively."
  (let ((node-type (treesit-node-type node))
        (node-start (treesit-node-start node))
        (node-end (treesit-node-end node))
        (node-text (treesit-node-text node t)))
    (message "%s%s [%d-%d]: '%s'" 
             (make-string indent ?\ )
             node-type 
             node-start 
             node-end
             (if (> (length node-text) 30) 
                 (concat (substring node-text 0 30) "...")
               node-text)))
  
  ;; Print children
  (let ((child (treesit-node-child node 0)))
    (while child
      (print-tree-recursive child (+ indent 2))
      (setq child (treesit-node-next-sibling child)))))

(let ((test-file (expand-file-name "test-eval.greger")))
  (when (file-exists-p test-file)
    (find-file test-file)
    (greger-mode)
    
    (let ((root (treesit-buffer-root-node)))
      (message "=== EVAL RESULT NODE STRUCTURE ===")
      ;; Find all eval result nodes
      (let ((eval-results (list (treesit-search-subtree root "eval_result"))))
        (dolist (eval-result eval-results)
          (message "\n--- EVAL RESULT ---")
          (print-tree-recursive eval-result 0))))))
