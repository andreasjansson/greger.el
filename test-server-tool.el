;;; test-server-tool.el --- Test server tool registration

(require 'greger-tools)

;; Register a web search server tool
(greger-register-server-tool 'web_search
  :type "web_search_20250305"
  :name "web_search"
  :max_uses 5
  :allowed_domains ["example.com" "trusteddomain.org"]
  :user_location ((type . "approximate")
                  (city . "San Francisco")
                  (region . "California")
                  (country . "US")
                  (timezone . "America/Los_Angeles")))

;; Get all registered server tools as JSON
(let ((schemas (greger-server-tools-get-all-schemas)))
  (dolist (schema schemas)
    (princ schema)
    (princ "\n")))
