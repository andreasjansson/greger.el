;;; test-metadata.el --- Quick test for metadata functionality

(require 'greger-parser)

;; Test basic safe-shell-commands parsing
(let ((markdown "## SYSTEM:

<safe-shell-commands>
ls -la
pwd
echo hello
</safe-shell-commands>"))
  (let ((result (greger-parser-parse-dialog markdown)))
    (message "Messages: %s" (plist-get result :messages))
    (message "Metadata: %s" (plist-get result :metadata))))

;; Test with system content
(let ((markdown "## SYSTEM:

You are helpful.

## USER:

Hello"))
  (let ((result (greger-parser-parse-dialog markdown)))
    (message "Messages: %s" (plist-get result :messages))
    (message "Metadata: %s" (plist-get result :metadata))))

(message "Test completed")
