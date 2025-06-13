;;; debug-server-tools2.el --- Debug server tools registration after fix

(add-to-list 'load-path ".")
(require 'greger-tools)

;; Check what happens when we register with the new macro
(greger-register-server-tool 'test-web-search
  :type "web_search_20250305"
  :max_uses 5)

;; Check what got registered
(message "After registration: %S" greger-server-tools-registry)
(let ((tool-def (gethash "test-web-search" greger-server-tools-registry)))
  (message "Tool def: %S" tool-def)
  (message "Name field: %S" (alist-get 'name tool-def))
  (message "Type of name field: %S" (type-of (alist-get 'name tool-def))))

;;; debug-server-tools2.el ends here
