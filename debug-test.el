;;; debug-test.el --- Debug the safe-shell-commands issue

(require 'greger-parser)

;; Enable debug
(setq greger-parser--global-debug t)

;; Test the specific failing case
(let ((markdown "## SYSTEM:

<safe-shell-commands>
ls -la
pwd
echo hello
</safe-shell-commands>"))
  (message "=== Testing markdown ===")
  (message "%s" markdown)
  (message "=== Parse result ===")
  (let ((result (greger-parser-parse-dialog markdown t)))
    (message "Messages: %S" (plist-get result :messages))
    (message "Metadata: %S" (plist-get result :metadata))))

(message "Debug test completed")
