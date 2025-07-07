;; Debug treesit node boundaries
(add-to-list 'load-path ".")
(require 'greger)

;; Load the test file
(let ((test-file (expand-file-name "test-eval.greger")))
  (when (file-exists-p test-file)
    (find-file test-file)
    (greger-mode)
    
    ;; Enable folding mode
    (setq greger-ui-folding-mode t)
    
    ;; Force fontification
    (font-lock-flush)
    (font-lock-ensure)
    
    ;; Find the eval result node
    (goto-char (point-min))
    (when (search-forward "<eval-result-abc123>" nil t)
      (let* ((pos (match-beginning 0))
             (root (treesit-buffer-root-node))
             (node (treesit-node-at pos))
             (parent (treesit-node-parent node)))
        
        ;; Walk up the tree to find the eval result
        (while (and parent (not (string= (treesit-node-type parent) "eval_result")))
          (setq parent (treesit-node-parent parent)))
        
        (when parent
          (message "Found eval_result node: %s" (treesit-node-type parent))
          (message "Eval result range: %d-%d" (treesit-node-start parent) (treesit-node-end parent))
          
          ;; Find the start tag
          (let ((start-tag (treesit-search-subtree parent "eval_result_start_tag")))
            (when start-tag
              (message "Start tag range: %d-%d" (treesit-node-start start-tag) (treesit-node-end start-tag))
              (message "Start tag text: '%s'" 
                       (buffer-substring-no-properties (treesit-node-start start-tag) 
                                                       (treesit-node-end start-tag)))
              
              ;; Check what comes immediately after the start tag
              (let ((after-tag-pos (treesit-node-end start-tag)))
                (message "Character after tag at %d: '%c'" after-tag-pos (char-after after-tag-pos))
                (message "Next 5 chars: '%s'" 
                         (buffer-substring-no-properties after-tag-pos (min (+ after-tag-pos 5) (point-max))))))))))))  
