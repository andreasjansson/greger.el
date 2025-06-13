;;; debug-parser-activation.el --- Test parser activation sequence

(add-to-list 'load-path ".")

(require 'greger-parser)

(message "Testing parser activation sequence...")

;; Test 1: Call activation manually first
(condition-case err
    (progn
      (message "Step 1: Calling greger-parser-activate-tree-sitter...")
      (greger-parser-activate-tree-sitter)
      (message "Activation completed successfully"))
  (error (message "Error in activation: %s" err)))

;; Test 2: Check if greger is ready after activation
(condition-case err
    (progn
      (message "Step 2: Checking if greger parser is ready...")
      (let ((ready (treesit-ready-p 'greger)))
        (message "treesit-ready-p 'greger: %s" ready)))
  (error (message "Error checking readiness: %s" err)))

;; Test 3: Now try the buffer parsing
(let ((test-buffer (generate-new-buffer "*test-greger*")))
  (with-current-buffer test-buffer
    (insert "# SYSTEM\n\nYou are a helpful assistant.\n\n# USER\n\nTest message"))
  
  (condition-case err
      (progn
        (message "Step 3: Testing greger-parser-markdown-buffer-to-dialog after activation...")
        (let ((result (greger-parser-markdown-buffer-to-dialog test-buffer)))
          (message "Result: %S" result)))
    (error (message "Error with buffer parsing after activation: %s" err)))

  (kill-buffer test-buffer))

(message "Parser activation test completed")

;;; debug-parser-activation.el ends here
