;;; test-reproduce-bug.el --- Test to reproduce the eval tag bug

;; This script reproduces the bug where typing < after <eval> causes Emacs to hang

(require 'greger)

(defun test-reproduce-eval-bug ()
  "Reproduce the eval tag bug by typing characters one by one."
  (interactive)
  
  ;; Open the test file
  (find-file "test-bug-reproduction.greger")
  
  ;; Enable greger-mode
  (greger-mode)
  
  ;; Force font-lock to be enabled and process the buffer
  (font-lock-ensure)
  
  ;; Go to the end of the buffer (after <eval>)
  (goto-char (point-max))
  
  ;; Add a newline first
  (insert "\n")
  (message "Inserted newline after <eval>")
  
  ;; Force font-lock to process the change
  (font-lock-flush)
  (font-lock-ensure)
  (sit-for 0.1)
  
  ;; Check tree-sitter state
  (let ((root-node (treesit-buffer-root-node)))
    (message "Tree-sitter root node: %s" root-node)
    (when root-node
      (message "Tree-sitter parse tree: %s" (treesit-node-string root-node))))
  
  ;; Now type < which should trigger the bug
  (message "About to insert < character...")
  (sit-for 0.5)
  
  ;; This should cause the hang - let's try to catch it
  (condition-case err
      (progn
        (insert "<")
        (message "Successfully inserted < - no hang occurred")
        
        ;; Force font-lock processing again
        (font-lock-flush)
        (font-lock-ensure)
        
        ;; Check tree-sitter state after inserting <
        (let ((root-node (treesit-buffer-root-node)))
          (message "Tree-sitter root node after <: %s" root-node)
          (when root-node
            (message "Tree-sitter parse tree after <: %s" (treesit-node-string root-node)))))
    (error
     (message "Error occurred: %s" err)))
  
  ;; Show the buffer content
  (message "Buffer content: %s" (buffer-string)))

;; Run the test
(test-reproduce-eval-bug)
