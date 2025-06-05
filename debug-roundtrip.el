#!/usr/bin/env emacs --script

(add-to-list 'load-path ".")
(require 'greger-parser)

(let ((markdown "## USER:

Search for current weather in San Francisco

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
[
  {
    \"title\": \"Weather in San Francisco\",
    \"url\": \"https://weather.com/sf\",
    \"content\": \"Sunny, 72°F\"
  }
]
</tool.srvtoolu_123>

## ASSISTANT:

The current weather in San Francisco is sunny and 72°F."))
  (let ((dialog (greger-parser-parse-dialog-messages-only markdown)))
    (message "ORIGINAL DIALOG:")
    (pp dialog)
    (let ((roundtrip-markdown (greger-parser-dialog-to-markdown dialog)))
      (message "\nROUNDTRIP MARKDOWN:\n%s" roundtrip-markdown)
      (let ((roundtrip-dialog (greger-parser-parse-dialog-messages-only roundtrip-markdown)))
        (message "\nROUNDTRIP DIALOG:")
        (pp roundtrip-dialog)))))
