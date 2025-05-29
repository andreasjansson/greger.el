;;; Test file for greger-parser state refactoring

(require 'greger-parser)

;; Test basic parsing
(let ((test-input "## USER:\n\nHello world\n\n## ASSISTANT:\n\nHi there!"))
  (message "Testing basic parse...")
  (let ((result (greger-parser-parse-dialog test-input)))
    (message "Parse result: %S" result)
    (if (= (length result) 2)
        (message "✓ Basic parsing test passed")
      (message "✗ Basic parsing test failed"))))

;; Test with debug
(let ((test-input "## USER:\n\nHello world"))
  (message "Testing debug parse...")
  (let ((result (greger-parser-parse-dialog test-input t)))
    (message "Debug parse result: %S" result)
    (if (= (length result) 1)
        (message "✓ Debug parsing test passed")
      (message "✗ Debug parsing test failed"))))

(message "Parser state refactoring tests complete")
