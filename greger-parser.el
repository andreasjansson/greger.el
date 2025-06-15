;;; greger-parser.el --- Parser for greger dialog format -*- lexical-binding: t -*-

;; Copyright (C) 2023 Andreas Jansson

;; Author: Andreas Jansson <andreas@jansson.me.uk>
;; Version: 0.1.0
;; URL: https://github.com/andreasjansson/greger.el
;; SPDX-License-Identifier: MIT

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:
;; Parses markdown-style dialog format with sections like # USER, # ASSISTANT, etc.
;; Handles tool use, thinking blocks, and complex content structures.

;;; Code:

(require 'treesit)
(require 'json)
(require 'cl-lib)
(require 'greger-web)

;; Section tag constants
(defconst greger-parser-system-tag "# SYSTEM")
(defconst greger-parser-user-tag "# USER")
(defconst greger-parser-assistant-tag "# ASSISTANT")
(defconst greger-parser-thinking-tag "# THINKING")
(defconst greger-parser-tool-use-tag "# TOOL USE")
(defconst greger-parser-server-tool-use-tag "# SERVER TOOL USE")
(defconst greger-parser-tool-result-tag "# TOOL RESULT")
(defconst greger-parser-web-search-tool-result-tag "# WEB SEARCH TOOL RESULT")

(add-to-list 'treesit-extra-load-path "/Users/andreas/projects/greger.el/greger-grammar")

;; Entrypoints

(defun greger-parser-markdown-to-dialog (text)
  "Parse greger conversation TEXT using tree-sitter and return structured dialog."
  (unless (treesit-ready-p 'greger)
    (error "Tree-sitter greger parser not available"))

  ;; Handle empty or whitespace-only input
  (let ((trimmed-text (string-trim text)))
    (if (string= trimmed-text "")
        '()
      (with-temp-buffer
        (insert text)
        (greger-parser-markdown-buffer-to-dialog (current-buffer))))))

(defun greger-parser-markdown-buffer-to-dialog (buffer)
  "Parse greger conversation BUFFER into structured dialog data."
  (with-current-buffer buffer
    (let* ((parser (treesit-parser-create 'greger))
           (root-node (treesit-parser-root-node parser)))
      (greger-parser--extract-dialog-from-node root-node))))

(defun greger-parser-dialog-to-markdown (dialog)
  "Convert DIALOG to markdown format."
  (if (null dialog)
      ""
    (mapconcat #'greger-parser--message-to-markdown dialog "\n\n")))

(defun greger-parser-find-safe-shell-commands-in-buffer (buffer)
  "Extract safe shell commands from BUFFER for validation.
Finds commands marked as safe for execution without user confirmation,
used by the shell command security system."
  (with-current-buffer buffer
    (let* ((parser (treesit-parser-create 'greger))
           (root-node (treesit-parser-root-node parser)))
      (greger-parser--extract-safe-shell-commands root-node))))

;; Tree-sitter-based markdown-to-dialog parsing

(defun greger-parser--extract-dialog-from-node (node)
  "Extract dialog entries from a tree-sitter NODE."
  (let ((raw-entries '()))
    ;; First extract all entries
    (dolist (child (treesit-node-children node))
      (let ((entry (greger-parser--extract-entry-from-node child)))
        (when entry
          (push entry raw-entries))))

    ;; Reverse after push
    (setq raw-entries (nreverse raw-entries))

    raw-entries))

(defun greger-parser--flush-assistant-content (content result)
  "Flush accumulated assistant CONTENT to RESULT list, returning updated result."
  (cons `((role . "assistant")
          (content . ,content))
        result))

(defun greger-parser--extract-entry-from-node (node)
  "Extract a single dialog entry from NODE."
  (let ((node-type (treesit-node-type node)))
    (cond
     ((string= node-type "untagged_text")
      (greger-parser--extract-user-from-untagged-text node))
     ((string= node-type "user")
      (greger-parser--extract-user node))
     ((string= node-type "assistant")
      (greger-parser--extract-assistant node))
     ((string= node-type "system")
      (greger-parser--extract-system node))
     ((string= node-type "thinking")
      (greger-parser--extract-thinking node))
     ((string= node-type "tool_use")
      (greger-parser--extract-tool-use node))
     ((string= node-type "server_tool_use")
      (greger-parser--extract-server-tool-use node))
     ((string= node-type "tool_result")
      (greger-parser--extract-tool-result node))
     ((string= node-type "web_search_tool_result")
      (greger-parser--extract-web-search-tool-result node))
     ;; citations are assistants too, but they don't have text
     ((string= node-type "citations")
      (greger-parser--extract-assistant node))
     (t nil))))

(defun greger-parser--extract-user-from-untagged-text (node)
  "Extract user entry from NODE."
  (let ((content (greger-parser--remove-two-trailing-newlines (treesit-node-text node t))))
    `((role . "user")
      (content . ,content))))

(defun greger-parser--extract-user (node)
  "Extract user entry from NODE."
  (let ((content (greger-parser--extract-text-content node)))
    `((role . "user")
      (content . ,content))))

(defun greger-parser--extract-system (node)
  "Extract system entry from NODE."
  (let* ((content (greger-parser--extract-text-content node))
         (safe-shell-commands (greger-parser--extract-safe-shell-commands node))
         (safe-shell-commands-text (greger-parser--safe-shell-commands-text safe-shell-commands)))

    (when safe-shell-commands-text
      (setq content (concat content "\n\n" safe-shell-commands-text)))

    `((role . "system")
      (content . ,content))))

(defun greger-parser--extract-safe-shell-commands (node)
  "Extract safe shell commands from NODE.
Use tree walking approach instead of `treesit-search-subtree' to avoid
segfaults when searching for safe_shell_commands nodes."
  (let ((safe-commands '()))
    (greger-parser--walk-tree
     node
     (lambda (n)
       (when (string= (treesit-node-type n) "safe_shell_commands")
         (let ((command-nodes (treesit-node-children n t)))
           (dolist (cmd-node command-nodes)
             (when (string= (treesit-node-type cmd-node) "shell_command")
               (push (treesit-node-text cmd-node t) safe-commands)))))))
    (nreverse safe-commands)))

(defun greger-parser--walk-tree (node callback)
  "Walk NODE tree calling CALLBACK on each node."
  (funcall callback node)
  (dolist (child (treesit-node-children node))
    (greger-parser--walk-tree child callback)))

(defun greger-parser--safe-shell-commands-text (commands)
  "Generate descriptive text for safe shell COMMANDS list."
  (when commands
    (concat "# Safe shell commands

You can run arbitrary shell commands with the shell-command tool, but the following are safe shell commands that will run without requiring user confirmation:

"
            (mapconcat (lambda (cmd) (format "* `%s`" cmd)) commands "\n"))))

(defun greger-parser--extract-thinking (node)
  "Extract thinking entry from NODE."
  (let ((content (greger-parser--extract-text-content node))
        (signature (greger-parser--extract-signature node)))
    `((role . "assistant")
      (content . (((type . "thinking")
                   (signature . ,signature)
                   (thinking . ,content)))))))

(defun greger-parser--extract-signature (node)
  "Extract thinking signature from NODE."
  (let* ((signature-node (treesit-search-subtree node "thinking_signature"))
         (value-node (treesit-node-child-by-field-name signature-node "value"))
         (signature (treesit-node-text value-node t)))
    signature))

(defun greger-parser--extract-tool-use (node)
  "Extract tool use entry from NODE."
  (let* ((name-node (treesit-search-subtree node "name"))
         (name (greger-parser--extract-value name-node))
         (id-node (treesit-search-subtree node "id"))
         (id (greger-parser--extract-value id-node))
         (tool-param-nodes (treesit-filter-child node (lambda (n) (string= (treesit-node-type n) "tool_param"))))
         (params '()))
    (dolist (tool-param-node tool-param-nodes)
      (push (greger-parser--extract-tool-param tool-param-node) params))
    (setq params (nreverse params))
    `((role . "assistant")
      (content . (((type . "tool_use")
                   (id . ,id)
                   (name . ,name)
                   (input . ,params)))))))

(defun greger-parser--extract-server-tool-use (node)
  "Extract tool use entry from NODE."
  (let* ((name-node (treesit-search-subtree node "name"))
         (name (greger-parser--extract-value name-node))
         (id-node (treesit-search-subtree node "id"))
         (id (greger-parser--extract-value id-node))
         (tool-param-nodes (treesit-filter-child node (lambda (n) (string= (treesit-node-type n) "tool_param"))))
         (params '()))
    (dolist (tool-param-node tool-param-nodes)
      (push (greger-parser--extract-tool-param tool-param-node) params))
    (setq params (nreverse params))
    `((role . "assistant")
      (content . (((type . "server_tool_use")
                   (id . ,id)
                   (name . ,name)
                   (input . ,params)))))))

(defun greger-parser--extract-value (node)
  "Extract the value field from tree-sitter NODE."
  (let ((child (treesit-node-child-by-field-name node "value")))
    (when child
      (string-trim (treesit-node-text child t)))))

(defun greger-parser--extract-tool-param (node)
  "Parse tool parameter NODE to extract name-value pair."
  (let* ((name-node (treesit-search-subtree node "name"))
         (name (intern (treesit-node-text name-node t)))
         (value-node (treesit-search-subtree node "value"))
         (value (greger-parser--extract-tool-content value-node)))
    `(,name . ,value)))

(defun greger-parser--extract-tool-content (node)
  "Extract the value field from tree-sitter NODE."
  (let* ((value-node (treesit-node-child-by-field-name node "value"))
         (value (treesit-node-text value-node t)))

    (greger-parser--convert-value value)))

(defun greger-parser--convert-param-value (value)
  "Convert VALUE to appropriate type for tool parameters.
Recognizes numbers, booleans, JSON arrays/objects, and plain strings."
  (if (string-match "^[0-9]+$" value)
      (string-to-number value)
    value))

(defun greger-parser--convert-value (str)
  "Convert STR to appropriate Elisp value."
  (let ((trimmed (string-trim str)))
    (cond
     ((string= trimmed "true") t)
     ((string= trimmed "false") nil)
     ((string-match-p "\\`-?[0-9]+\\'" trimmed)
      (string-to-number trimmed))
     ((string-match-p "\\`-?[0-9]*\\.[0-9]+\\'" trimmed)
      (string-to-number trimmed))
     ((and (string-prefix-p "[" trimmed) (string-suffix-p "]" trimmed))
      (greger-parser--parse-json-array trimmed))
     ((and (string-prefix-p "{" trimmed) (string-suffix-p "}" trimmed))
      (greger-parser--parse-json-object trimmed))
     (t (greger-parser--remove-single-leading-and-trailing-newline str)))))

(defun greger-parser--parse-json-array (str)
  "Parse JSON array STR."
  (condition-case nil
      (json-read-from-string str)
    (error str)))

(defun greger-parser--parse-json-object (str)
  "Parse JSON object STR."
  (condition-case nil
      (let ((parsed (json-read-from-string str)))
        (mapcar (lambda (pair)
                  (cons (intern (symbol-name (car pair))) (cdr pair)))
                parsed))
    (error str)))

(defun greger-parser--extract-tool-result (node)
  "Extract tool result entry from NODE."
  (let ((id nil)
        (content nil))
    (dolist (child (treesit-node-children node))
      (let ((child-type (treesit-node-type child)))
        (cond
         ((string= child-type "id")
          (setq id (string-trim (greger-parser--extract-value child))))
         ((string= child-type "content")
          (setq content (greger-parser--extract-tool-content child))))))
    `((role . "user")
      (content . (((type . "tool_result")
                   (tool_use_id . ,id)
                   (content . ,content)))))))

(defun greger-parser--extract-web-search-tool-result (node)
  "Extract server tool result entry from NODE."
  (let ((id nil)
        (content nil))
    (dolist (child (treesit-node-children node))
      (let ((child-type (treesit-node-type child)))
        (cond
         ((string= child-type "id")
          (setq id (greger-parser--extract-value child)))
         ((string= child-type "content")
          (setq content (greger-parser--extract-tool-content child))))))
    `((role . "assistant")
      (content . (((type . "web_search_tool_result")
                   (tool_use_id . ,id)
                   (content . ,content)))))))

(defun greger-parser--extract-assistant (node)
  "Extract citations entry from NODE."
  (let ((text-content (greger-parser--extract-text-content node))
        (citation-entries (greger-parser--extract-citation-entries node))
        (content '((type . "text"))))
    (when citation-entries
      (push `(citations . ,citation-entries) content))
    (when text-content
      (push `(text . ,text-content) content))
    `((role . "assistant")
      (content . (,content)))))

(defun greger-parser--extract-citation-entries (node)
  "Extract all citation entries from NODE."
  (let ((citation-entries '()))
   (dolist (child (treesit-node-children node))
     (let ((child-type (treesit-node-type child)))
       (when (string= child-type "citation_entry")
         (push (greger-parser--extract-citation-entry child) citation-entries))))
   citation-entries))

(defun greger-parser--extract-text-content (node)
  "Extract text content from NODE, handling nested structures."
  (let ((result (greger-parser--collect-text-blocks node "")))
    (greger-parser--remove-two-trailing-newlines result)))

(defun greger-parser--remove-two-trailing-newlines (str)
  "Remove exactly two newlines from the end of STR if they exist."
  (replace-regexp-in-string "\n\n\\'" "" str))

(defun greger-parser--remove-single-leading-and-trailing-newline (str)
  "Remove one leading and trailing newline from STR."
  (replace-regexp-in-string "\\`\n\\|\n\\'" "" str))

(defun greger-parser--collect-text-blocks (node result)
  "Recursively collect text from text nodes in NODE and append to RESULT."
  (let ((node-type (treesit-node-type node)))
    (cond
     ((string= node-type "text")
      (concat result (treesit-node-text node t)))
     ;; For code blocks, include the entire text content
     ((string= node-type "code_block")
      (concat result (treesit-node-text node t)))
     ((string= node-type "inline_code")
      (concat result (treesit-node-text node t)))
     ((string= node-type "html_comment")
      ;; Skip HTML comments outside of code blocks
      result)
     (t
      (let ((text-result result))
        (dolist (child (treesit-node-children node))
          (setq text-result (greger-parser--collect-text-blocks child text-result)))
        text-result)))))

(defun greger-parser--parse-json-or-plain-content (content)
  "Parse CONTENT as json if it appears to be json, otherwise return as plain text."
  (if (and (string-match-p "^\\s-*\\[\\|^\\s-*{" content)
           (condition-case nil
               (json-parse-string content :object-type 'alist :array-type 'list)
             (json-parse-error nil)))
      (json-parse-string content :object-type 'alist :array-type 'list)
    content))

(defun greger-parser--extract-citation-entry (node)
  "Extract a citation entry from NODE."
  (let ((url nil)
        (title nil)
        (cited-text nil)
        (encrypted-index nil))
    (dolist (child (treesit-node-children node))
      (let ((child-type (treesit-node-type child)))
        (cond
         ((string= child-type "url")
          (setq url (string-trim (substring (treesit-node-text child t) 3))))
         ((string= child-type "title")
          (setq title (or (greger-parser--extract-value child) "(no value)")))
         ((string= child-type "cited_text")
          (setq cited-text (or (greger-parser--extract-value child) "(no value)")))
         ((string= child-type "encrypted_index")
          (setq encrypted-index (greger-parser--extract-value child))))))
    `((type . "web_search_result_location")
      (url . ,url)
      (title . ,title)
      (cited_text . ,cited-text)
      (encrypted_index . ,encrypted-index))))

;; Dialog to markdown

(defun greger-parser--message-to-markdown (message)
  "Convert MESSAGE to markdown."
  (let ((role (alist-get 'role message))
        (content (alist-get 'content message)))
    (cond
     ((string= role "user")
      (greger-parser--user-to-markdown content))
     ((string= role "assistant")
      (greger-parser--assistant-to-markdown content))
     ((string= role "system")
      (greger-parser--system-to-markdown content))
     (t ""))))

(defun greger-parser--user-to-markdown (content)
  "Convert user CONTENT to markdown."
  (if (stringp content)
      (concat greger-parser-user-tag "\n\n" content)
    ;; Check if this is a pure tool result message (only contains tool_result blocks)
    (if (and (= (length content) 1)
             (string= (alist-get 'type (car content)) "tool_result"))
        ;; Pure tool result - just output it directly
        (greger-parser--tool-result-to-markdown (car content))
      ;; Mixed content - add USER header
      (concat greger-parser-user-tag "\n\n"
              (greger-parser--user-content-blocks-to-markdown content)))))

(defun greger-parser--assistant-to-markdown (content)
  "Convert assistant CONTENT to markdown."
  (if (stringp content)
      (concat greger-parser-assistant-tag "\n\n" content)
    (greger-parser--assistant-content-blocks-to-markdown content)))

(defun greger-parser--system-to-markdown (content)
  "Convert system CONTENT to markdown."
  (concat greger-parser-system-tag "\n\n" content))

(defun greger-parser--user-content-blocks-to-markdown (content-blocks)
  "Convert user CONTENT-BLOCKS to markdown format."
  (if (null content-blocks)
      ""
    (mapconcat (lambda (block)
                 (let ((block-type (alist-get 'type block)))
                   (cond
                    ((string= block-type "text")
                     (alist-get 'text block))
                    (t (greger-parser--block-to-markdown block t)))))
               content-blocks "\n\n")))

(defun greger-parser--assistant-content-blocks-to-markdown (content-blocks)
  "Convert assistant CONTENT-BLOCKS to markdown format."
  (if (null content-blocks)
      ""
    (let ((result "")
          (first-block t))
      (dolist (block content-blocks)
        (let* ((block-type (alist-get 'type block))
               (has-citations (alist-get 'citations block))
               (is-text-block (and (string= block-type "text") (not has-citations)))
               (block-markdown (cond
                               ((and is-text-block first-block)
                                ;; First text block gets assistant header
                                (concat greger-parser-assistant-tag "\n\n" (alist-get 'text block)))
                               (is-text-block
                                ;; Subsequent text blocks need assistant header if previous wasn't text
                                (concat greger-parser-assistant-tag "\n\n" (alist-get 'text block)))
                               (t
                                ;; Non-text blocks handle their own headers
                                (greger-parser--block-to-markdown block t)))))
          (when (not (string= result ""))
            (setq result (concat result "\n\n")))
          (setq result (concat result block-markdown))
          (setq first-block nil)))
      result)))

(defun greger-parser--citations-list-to-markdown (citations)
  "Convert CITATIONS list to markdown citations section."
  (mapconcat #'greger-parser--citation-to-markdown citations "\n\n"))

(defun greger-parser--citation-to-markdown (citation)
  "Convert single CITATION to markdown format."
  (let ((url (alist-get 'url citation))
        (title (alist-get 'title citation))
        (cited-text (alist-get 'cited_text citation))
        (encrypted-index (alist-get 'encrypted_index citation)))
    ;; url includes the leading `## `
    (concat "## " url "\n\n"
            "Title: " title "\n"
            "Cited text: " cited-text "\n"
            "Encrypted index: " encrypted-index)))

(defun greger-parser--block-to-markdown (block &optional skip-header)
  "Convert a content BLOCK to markdown.
If SKIP-HEADER is true, don't add section headers for text blocks."
  (let ((type (alist-get 'type block)))
    (cond
     ((string= type "text")
      (greger-parser--assistant-text-to-markdown block skip-header))
     ((string= type "thinking")
      (greger-parser--thinking-to-markdown block))
     ((string= type "tool_use")
      (greger-parser--tool-use-to-markdown block))
     ((string= type "server_tool_use")
      (greger-parser--server-tool-use-to-markdown block))
     ((string= type "tool_result")
      (greger-parser--tool-result-to-markdown block))
     ((string= type "web_search_tool_result")
      (greger-parser--web-search-tool-result-to-markdown block))
     (t ""))))

(defun greger-parser--assistant-text-to-markdown (block &optional skip-header)
  "Convert assistant text BLOCK to markdown, including citations.
If SKIP-HEADER is nil, don't include the assistant header tag,
assuming it's already been sent in streaming."
  (let ((text (alist-get 'text block))
        (citations (alist-get 'citations block)))
    (cond
     (citations
      (greger-parser--citations-to-markdown block))
     (skip-header
      text)
     (t
      (concat greger-parser-assistant-tag
              "\n\n"
              text)))))

(defun greger-parser--thinking-to-markdown (block)
  "Convert thinking BLOCK to markdown (signature and text)."
  (let ((contents (alist-get 'thinking block))
        (signature (alist-get 'signature block)))
    (concat greger-parser-thinking-tag
            (if signature
                (concat "\n\n" "Signature: " signature)
              "")
            "\n\n"
            contents)))

(defun greger-parser--citations-to-markdown (block)
  "Convert citation BLOCK to markdown with embedded citations."
  (let* ((text (alist-get 'text block))
         (citations (alist-get 'citations block))
         (citations-markdown (greger-parser--citations-list-to-markdown citations)))

    ;; little hack for parsing to still work if there are citations without text
    (when (not text)
      (setq text " "))

    (concat greger-parser-assistant-tag
            "\n\n"
            text
            "\n\n"
            citations-markdown)))

(defun greger-parser--tool-use-to-markdown (tool-use)
  "Convert TOOL-USE to markdown."
  (let ((name (alist-get 'name tool-use))
        (id (alist-get 'id tool-use))
        (input (alist-get 'input tool-use)))

    (concat greger-parser-tool-use-tag "\n\n"
            "Name: " name "\n"
            "ID: " id "\n\n"
            (greger-parser--tool-params-to-markdown id input))))

(defun greger-parser--server-tool-use-to-markdown (tool-use)
  "Convert TOOL-USE to markdown."
  (let ((name (alist-get 'name tool-use))
        (id (alist-get 'id tool-use))
        (input (alist-get 'input tool-use)))

    (concat greger-parser-server-tool-use-tag "\n\n"
            "Name: " name "\n"
            "ID: " id "\n\n"
            (greger-parser--tool-params-to-markdown id input))))

(defun greger-parser--tool-result-to-markdown (tool-result)
  "Convert TOOL-RESULT to markdown."
  (let ((id (alist-get 'tool_use_id tool-result))
        (content (greger-parser--tool-content-to-markdown tool-result)))
    (greger-parser--wrapped-tool-content greger-parser-tool-result-tag id content)))

(defun greger-parser--wrapped-tool-content (tag id content)
  "Wrap CONTENT with TAG and ID for tool display formatting."
  (concat tag
          "\n\n"
          "ID: " id "\n\n"
          "<tool." id ">\n"
          content
          "\n"
          "</tool." id ">"))

(defun greger-parser--web-search-tool-result-to-markdown (tool-result)
  "Convert TOOL-RESULT to markdown."
  (let ((id (alist-get 'tool_use_id tool-result))
        (content (greger-parser--tool-content-to-markdown tool-result)))
    (greger-parser--wrapped-tool-content greger-parser-web-search-tool-result-tag id content)))

(defun greger-parser--tool-content-to-markdown (block)
  "Convert tool content BLOCK to markdown format."
  (let ((content (alist-get 'content block)))
    (if (stringp content)
        content
      (greger-parser--value-to-string content))))

(defun greger-parser--tool-params-to-markdown (id input)
  "Convert tool parameters with ID and INPUT to markdown."
  (if (null input)
      ""
    (mapconcat (lambda (param)
                 (let ((name (if (symbolp (car param))
                                 (symbol-name (car param))
                               (format "%s" (car param))))
                       (value (cdr param)))
                   (concat "## " name "\n\n"
                           "<tool." id ">\n"
                           (greger-parser--value-to-string value) "\n"
                           "</tool." id ">")))
               input "\n\n")))

(defun greger-parser--value-to-string (value)
  "Convert VALUE to string representation."
  (let ((json-encoding-pretty-print t))
    (cond
    ((stringp value)
     ;; Try to parse as JSON and pretty print if valid
     (condition-case nil
         (let ((parsed (json-read-from-string value)))
           ;; If parsing succeeded, encode back with pretty print
           (json-encode parsed))
       (error
        ;; If parsing failed, return original string
        value)))
    ((numberp value) (number-to-string value))
    ((eq value t) "true")
    ((null value) "false")
    ((vectorp value) (json-encode value))
    ((listp value) (json-encode value))
    (t (format "%s" value)))))

(provide 'greger-parser)

;;; greger-parser.el ends here
