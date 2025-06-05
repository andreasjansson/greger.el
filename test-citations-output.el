#!/usr/bin/env emacs --script

;; Add current directory to load path
(add-to-list 'load-path ".")

;; Load required modules
(require 'greger-parser)

;; Test markdown with citations
(let* ((markdown "## USER:

When was Claude Shannon born?

## SERVER TOOL USE:

Name: web_search
ID: srvtoolu_01WYG3ziw53XMcoyKL4XcZmE

### query

<tool.srvtoolu_01WYG3ziw53XMcoyKL4XcZmE>
claude shannon birth date
</tool.srvtoolu_01WYG3ziw53XMcoyKL4XcZmE>

## SERVER TOOL RESULT:

ID: srvtoolu_01WYG3ziw53XMcoyKL4XcZmE

<tool.srvtoolu_01WYG3ziw53XMcoyKL4XcZmE>
[
  {
    \"type\": \"web_search_result\",
    \"url\": \"https://en.wikipedia.org/wiki/Claude_Shannon\",
    \"title\": \"Claude Shannon - Wikipedia\",
    \"encrypted_content\": \"EqgfCioIARgBIiQ3YTAwMjY1Mi1mZjM5LTQ1NGUtODgxNC1kNjNjNTk1ZWI3Y...\",
    \"page_age\": \"April 30, 2025\"
  }
]
</tool.srvtoolu_01WYG3ziw53XMcoyKL4XcZmE>

## ASSISTANT:

Based on the search results, <cite>Claude Shannon was born on April 30, 1916, in Petoskey, Michigan</cite>

## CITATIONS:

### https://en.wikipedia.org/wiki/Claude_Shannon

Title: Claude Shannon - Wikipedia
Cited text: Claude Elwood Shannon (April 30, 1916 – February 24, 2001) was an American mathematician, electrical engineer, computer scientist, cryptographer and i...
Encrypted index: Eo8BCioIAhgBIiQyYjQ0OWJmZi1lNm..")
       ;; Parse the dialog
       (parsed (greger-parser-parse-dialog markdown))
       ;; Convert back to markdown
       (roundtrip (greger-parser-dialog-to-markdown (plist-get parsed :messages))))

  (princ "=== ORIGINAL MARKDOWN ===\n")
  (princ markdown)
  (princ "\n\n=== PARSED STRUCTURE ===\n")
  (princ (format "%S" parsed))
  (princ "\n\n=== ROUNDTRIP MARKDOWN ===\n")
  (princ roundtrip)
  (princ "\n"))
