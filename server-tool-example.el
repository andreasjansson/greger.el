#!/usr/bin/env emacs --script
;;; server-tool-example.el --- Example of web search server tool integration

;; Add current directory to load path
(add-to-list 'load-path ".")

;; Load greger
(require 'greger-tools)
(require 'greger-parser)

;; Register a web search server tool
(greger-register-server-tool 'web_search
  :type "web_search_20250305"
  :name "web_search"
  :max_uses 5
  :allowed_domains ["anthropic.com" "example.com"]
  :user_location ((type . "approximate")
                  (city . "San Francisco")
                  (region . "California")
                  (country . "US")
                  (timezone . "America/Los_Angeles")))

;; Display the server tool definition as JSON
(princ "=== Server Tool Definition (JSON) ===\n")
(let ((schemas (greger-server-tools-get-schemas '(web_search))))
  (princ (car schemas))
  (princ "\n\n"))

;; Test parsing a conversation with server tool use
(princ "=== Testing Server Tool Parsing ===\n")
(let* ((markdown "## USER:

What's the weather in San Francisco?

## SERVER TOOL USE:

Name: web_search
ID: srvtoolu_123

### query

<tool.srvtoolu_123>
current weather San Francisco
</tool.srvtoolu_123>

## SERVER TOOL RESULT:

ID: srvtoolu_123

<tool.srvtoolu_123>
[{\"title\": \"Weather in San Francisco\", \"url\": \"https://weather.com/sf\", \"content\": \"Sunny, 72°F\"}]
</tool.srvtoolu_123>

## ASSISTANT:

The current weather in San Francisco is sunny and 72°F.")
       (parsed (greger-parser-parse-dialog markdown))
       (messages (plist-get parsed :messages)))

  (princ (format "Parsed %d messages:\n" (length messages)))
  (dolist (msg messages)
    (let ((role (alist-get 'role msg))
          (content (alist-get 'content msg)))
      (princ (format "- %s: " role))
      (if (stringp content)
          (princ (format "\"%s\"\n" content))
        (princ (format "%d content blocks\n" (length content))))))

  ;; Test roundtrip conversion
  (princ "\n=== Testing Roundtrip Conversion ===\n")
  (let ((regenerated-markdown (greger-parser-dialog-to-markdown messages)))
    (princ "Successfully converted back to markdown\n")
    (princ (format "Original length: %d characters\n" (length markdown)))
    (princ (format "Regenerated length: %d characters\n" (length regenerated-markdown)))))

(princ "\n=== Integration Complete! ===\n")
