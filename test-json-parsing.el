#!/usr/bin/env emacs --script

(add-to-list 'load-path ".")
(require 'greger-parser)

;; Test 1: Web search result (should be web_search_tool_result)
(let ((content "[
  {
    \"type\": \"web_search_result\",
    \"url\": \"https://en.wikipedia.org/wiki/Claude_Shannon\",
    \"title\": \"Claude Shannon - Wikipedia\",
    \"encrypted_content\": \"EqgfCioIARgBIiQ3YTAwMjY1Mi1mZjM5LTQ1NGUtODgxNC1kNjNjNTk1ZWI3Y...\",
    \"page_age\": \"April 30, 2025\"
  }
]"))
  (message "=== Test 1: Web search result ===")
  (message "Content: %s" content)
  (message "Parsed content: %s" (greger-parser--parse-json-content content))
  (let ((result (greger-parser--create-server-tool-result-message "test_id" content)))
    (message "Result: %s" result)))

;; Test 2: General JSON array (should be server_tool_result with parsed content)
(let ((content "[{\"title\": \"Weather in San Francisco\", \"url\": \"https://weather.com/sf\", \"content\": \"Sunny, 72°F\"}]"))
  (message "\n=== Test 2: General JSON array ===")
  (message "Content: %s" content)
  (message "Parsed content: %s" (greger-parser--parse-json-content content))
  (let ((result (greger-parser--create-server-tool-result-message "test_id2" content)))
    (message "Result: %s" result)))

;; Test 3: Plain string (should be server_tool_result with string content)
(let ((content "Sunny and warm today"))
  (message "\n=== Test 3: Plain string ===")
  (message "Content: %s" content)
  (message "Parsed content: %s" (greger-parser--parse-json-content content))
  (let ((result (greger-parser--create-server-tool-result-message "test_id3" content)))
    (message "Result: %s" result)))
