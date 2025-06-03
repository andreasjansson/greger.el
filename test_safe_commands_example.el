;;; Test the safe shell commands auto-generation functionality

(add-to-list 'load-path ".")
(require 'greger-parser)

;; Test the exact scenario from the user's request
(let ((markdown "## SYSTEM:

<safe-shell-commands>
bash script/package-lint
bash script/test --verbose
bash script/test --verbose --file test/test-parser.el
bash script/test --verbose --file test/test-greger.el
bash script/test --verbose --file test/test-greger-lib-lsp.el
</safe-shell-commands>"))

  (let ((result (greger-parser-parse-dialog markdown)))
    (message "Messages: %S" (plist-get result :messages))
    (message "Metadata: %S" (plist-get result :metadata))

    ;; Should have one system message with auto-generated content
    (when (= 1 (length (plist-get result :messages)))
      (let* ((system-msg (car (plist-get result :messages)))
             (content (alist-get 'content system-msg)))
        (message "\nGenerated system message content:")
        (message "%s" content)))))
