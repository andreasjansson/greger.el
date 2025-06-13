;;; debug-server-tools.el --- Debug server tools registration

(add-to-list 'load-path ".")
(require 'greger-tools)

;; Check initial state
(message "Initial registry: %S" greger-server-tools-registry)

;; Register a test server tool like the test does
(greger-register-server-tool 'test-web-search
  :type "web_search_20250305"
  :name "web_search"
  :max_uses 5
  :allowed_domains ["example.com" "trusteddomain.org"]
  :user_location ((type . "approximate")
                  (city . "San Francisco")
                  (region . "California")
                  (country . "US")
                  (timezone . "America/Los_Angeles")))

;; Check what got registered
(message "After registration: %S" greger-server-tools-registry)
(message "Looking up 'test-web-search: %S" (gethash 'test-web-search greger-server-tools-registry))
(message "Looking up \"test-web-search\": %S" (gethash "test-web-search" greger-server-tools-registry))

;; Try to get schemas
(condition-case err
    (let ((schemas (greger-server-tools-get-schemas '(test-web-search))))
      (message "Schemas: %S" schemas))
  (error (message "Error: %S" err)))

;;; debug-server-tools.el ends here
