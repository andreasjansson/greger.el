;;; Test combining system content with safe shell commands

(add-to-list 'load-path ".")
(require 'greger-parser)

;; Test combining existing system content with safe shell commands
(let ((markdown "## SYSTEM:

You are a helpful assistant for an Emacs Lisp project.

<safe-shell-commands>
bash script/package-lint
bash script/test --verbose
</safe-shell-commands>"))

  (let ((result (greger-parser-parse-dialog markdown)))
    (when (= 1 (length (plist-get result :messages)))
      (let* ((system-msg (car (plist-get result :messages)))
             (content (alist-get 'content system-msg)))
        (message "Combined system message content:")
        (message "%s" content)))))
