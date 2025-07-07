;; Debug the actual tree structure
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
             (if (> (length node-text) 20) 
                 (concat (substring node-text 0 20) "...")
               node-text)))
  
  ;; Print children
  (let ((child (treesit-node-child node 0)))
    (while child
      (print-tree-recursive child (+ indent 2))
      (setq child (treesit-node-next-sibling child)))))

;; Load test file and analyze tree
(let ((test-file (expand-file-name "minimal-test.greger")))
  (when (file-exists-p test-file)
    (find-file test-file)
    (greger-mode)
    
    (let ((root (treesit-buffer-root-node)))
      (message "=== TREE STRUCTURE ===")
      (print-tree-recursive root 0)
      
      (message "\n=== SEARCHING FOR EVAL RESULT NODES ===")
      ;; Search for eval result nodes
      (let ((eval-results (treesit-search-subtree root "eval_result")))
        (if eval-results
            (progn
              (message "Found eval_result node: %s" eval-results)
              
              ;; Test the same search logic as the folding function
              (let ((start-tag-from-node (treesit-search-subtree eval-results "eval_result_start_tag"))
                    (end-tag-from-node (treesit-search-subtree eval-results "eval_result_end_tag")))
                (message "Start tag from eval_result node: %s" start-tag-from-node)
                (message "End tag from eval_result node: %s" end-tag-from-node)
                
                (when start-tag-from-node
                  (message "Start tag range: %d-%d" 
                           (treesit-node-start start-tag-from-node)
                           (treesit-node-end start-tag-from-node))
                  (message "Start tag text: '%s'"
                           (treesit-node-text start-tag-from-node t)))))
          (message "No eval_result nodes found!"))
        
        ;; Search for eval result start tags
        (let ((start-tags (treesit-search-subtree root "eval_result_start_tag")))
          (if start-tags
              (message "Found eval_result_start_tag: %s" start-tags)
            (message "No eval_result_start_tag nodes found!")))))))
