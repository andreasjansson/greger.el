#!/usr/bin/env emacs --script

;; Add current directory to load path
(add-to-list 'load-path ".")

;; Load required modules
(require 'greger-parser)

;; Override the apply function to print results immediately
(defun greger-parser--apply-citations-to-messages (messages citations)
  "Apply CITATIONS to the last assistant message in MESSAGES that contains <cite> tags."
  (message "DEBUG: Apply citations called with %d citations" (length citations))
  ;; Find the last assistant message and apply citations to it
  (dolist (message messages)
    (when (string= "assistant" (alist-get 'role message))
      (message "DEBUG: Processing assistant message")
      (let ((content (alist-get 'content message)))
        (cond
         ;; String content - check for <cite> tags and process
         ((stringp content)
          (message "DEBUG: String content: %s" content)
          (when (string-match-p "<cite>" content)
            (let ((clean-content (greger-parser--remove-cite-tags content)))
              (setcdr (assq 'content message) clean-content))))
         ;; List content - process each content block
         ((listp content)
          (message "DEBUG: List content with %d blocks" (length content))
          (greger-parser--add-citations-to-content-blocks content citations)
          ;; Print the message structure immediately after processing
          (message "DEBUG: Message after citation processing: %S" message))))))

;; Test markdown with citations
(let* ((markdown "## USER:

When was Claude Shannon born?

## ASSISTANT:

Based on the search results, <cite>Claude Shannon was born on April 30, 1916, in Petoskey, Michigan</cite>

## CITATIONS:

### https://en.wikipedia.org/wiki/Claude_Shannon

Title: Claude Shannon - Wikipedia
Cited text: Claude Elwood Shannon (April 30, 1916 – February 24, 2001) was an American mathematician, electrical engineer, computer scientist, cryptographer and i...
Encrypted index: Eo8BCioIAhgBIiQyYjQ0OWJmZi1lNm..")
       ;; Parse the dialog
       (parsed (greger-parser-parse-dialog markdown)))

  (princ "=== FINAL PARSED STRUCTURE ===\n")
  (princ (format "%S" parsed))
  (princ "\n"))
