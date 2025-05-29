;;; Test include tag fix

(require 'greger-parser)

;; Create a test file
(with-temp-file "test-include.txt"
  (insert "Hello from included file"))

;; Test basic include tag
(let ((test-input "## USER:\n\n<include>test-include.txt</include>"))
  (message "Testing include tag...")
  (let ((result (greger-parser-parse-dialog test-input t)))
    (message "Parse result: %S" result)
    (if (and result (> (length result) 0))
        (message "✓ Include tag test passed")
      (message "✗ Include tag test failed"))))

;; Clean up
(delete-file "test-include.txt")

(message "Include tag fix test complete")
