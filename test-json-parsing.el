#!/usr/bin/env emacs --script

(add-to-list 'load-path ".")
(require 'greger-parser)

(let ((content "[
  {
    \"type\": \"web_search_result\",
    \"url\": \"https://en.wikipedia.org/wiki/Claude_Shannon\",
    \"title\": \"Claude Shannon - Wikipedia\",
    \"encrypted_content\": \"EqgfCioIARgBIiQ3YTAwMjY1Mi1mZjM5LTQ1NGUtODgxNC1kNjNjNTk1ZWI3Y...\",
    \"page_age\": \"April 30, 2025\"
  }
]"))
  (message "Content to test: %s" content)
  (message "Regex match old: %s" (string-match-p "\"type\":\\s*\"web_search_result\"" content))
  (message "Regex match new: %s" (string-match-p "\"type\"[[:space:]]*:[[:space:]]*\"web_search_result\"" content))
  (message "Parsed content: %s" (greger-parser--parse-web-search-content content))
  (let ((result (greger-parser--create-server-tool-result-message "test_id" content)))
    (message "Result: %s" result)))
