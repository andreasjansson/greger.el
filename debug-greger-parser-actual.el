;;; debug-greger-parser-actual.el --- Test actual greger-parser functions

(add-to-list 'load-path ".")

;; Load greger-parser which should set up everything correctly
(require 'greger-parser)

(message "greger-parser loaded successfully")

;; Test the actual parser functions
(condition-case err
    (progn
      (message "Testing greger-parser-markdown-to-dialog...")
      (let* ((test-markdown "# USER\n\nHello world\n\n# ASSISTANT\n\nHi there!")
             (result (greger-parser-markdown-to-dialog test-markdown)))
        (message "Parse result: %S" result)))
  (error (message "Error with greger-parser-markdown-to-dialog: %s" err)))

;; Test buffer-based parsing
(condition-case err
    (progn
      (message "Testing greger-parser-markdown-buffer-to-dialog...")
      (with-temp-buffer
        (insert "# USER\n\nTest message\n\n# ASSISTANT\n\nTest response")
        (let ((result (greger-parser-markdown-buffer-to-dialog (current-buffer))))
          (message "Buffer parse result: %S" result))))
  (error (message "Error with buffer parsing: %s" err)))

;;; debug-greger-parser-actual.el ends here
