(add-to-list 'load-path ".")
(require 'greger-parser)

(let ((markdown "## SYSTEM:

You are a helpful assistant.

<safe-shell-commands>
ls
pwd
</safe-shell-commands>

Please be careful."))
  (let ((result (greger-parser-parse-dialog markdown)))
    (let ((system-content (alist-get 'content (car (plist-get result :messages)))))
      (message "Actual content: %S" system-content))))
