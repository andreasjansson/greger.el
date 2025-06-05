#!/usr/bin/env emacs --script

(add-to-list 'load-path ".")
(require 'greger-parser)

(let ((markdown "## USER:

What's 2+2?

## THINKING:

This is a simple arithmetic question. I can answer this directly without needing any tools.

## ASSISTANT:

2 + 2 = 4"))
  (let ((dialog (greger-parser-parse-dialog-messages-only markdown)))
    (message "ORIGINAL DIALOG:")
    (pp dialog)
    (let ((roundtrip-markdown (greger-parser-dialog-to-markdown dialog)))
      (message "\nROUNDTRIP MARKDOWN:\n%s" roundtrip-markdown)
      (let ((roundtrip-dialog (greger-parser-parse-dialog-messages-only roundtrip-markdown)))
        (message "\nROUNDTRIP DIALOG:")
        (pp roundtrip-dialog)))))
