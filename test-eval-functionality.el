;;; test-eval-functionality.el --- Test eval functionality

;; Load greger modules
(add-to-list 'load-path ".")
(require 'greger)
(require 'greger-ui)

;; Test function
(defun test-eval-functionality ()
  "Test eval result display functionality."
  (interactive)
  
  ;; Open test file
  (find-file "test-eval.greger")
  
  ;; Enable greger-mode
  (greger-mode)
  
  ;; Enable folding mode
  (setq greger-ui-folding-mode t)
  
  ;; Force fontification
  (font-lock-flush)
  (font-lock-ensure)
  
  ;; Print some debug info
  (message "Buffer: %s" (buffer-name))
  (message "Major mode: %s" major-mode)
  (message "Folding mode: %s" greger-ui-folding-mode)
  
  ;; Test treesit functionality
  (when (treesit-ready-p 'greger)
    (message "Greger grammar is ready")
    (let ((root (treesit-buffer-root-node)))
      (message "Root node type: %s" (treesit-node-type root))
      
      ;; Find eval nodes
      (let ((eval-nodes (treesit-query-capture root '((eval) @eval))))
        (message "Found %d eval nodes" (length eval-nodes))
        
        ;; Find eval result nodes
        (let ((eval-result-nodes (treesit-query-capture root '((eval_result) @eval-result))))
          (message "Found %d eval result nodes" (length eval-result-nodes))))))
  
  ;; Switch to test buffer
  (switch-to-buffer (current-buffer))
  (message "Test completed. Check the buffer for eval result display."))

;; Run the test
(test-eval-functionality)

;;; test-eval-functionality.el ends here
