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
;; Parses markdown-style dialog format with sections like ## USER:, ## ASSISTANT:, etc.
;; Handles tool use, thinking blocks, and complex content structures.

;;; Code:

(require 'treesit)
(require 'json)
(require 'cl-lib)
(require 'greger-web)

;; Section tag constants
(defconst greger-parser-system-tag "## SYSTEM:")
(defconst greger-parser-user-tag "## USER:")
(defconst greger-parser-assistant-tag "## ASSISTANT:")
(defconst greger-parser-thinking-tag "## THINKING:")
(defconst greger-parser-citations-tag "## CITATIONS:")
(defconst greger-parser-tool-use-tag "## TOOL USE:")
(defconst greger-parser-tool-result-tag "## TOOL RESULT:")
(defconst greger-parser-server-tool-use-tag "## SERVER TOOL USE:")
(defconst greger-parser-server-tool-result-tag "## SERVER TOOL RESULT:")

(add-to-list 'treesit-extra-load-path "/Users/andreas/projects/greger.el/greger-grammar")

;; Entrypoints

(defun greger-parser-markdown-to-dialog (text)
  "Parse greger conversation TEXT using tree-sitter and return structured dialog."
  (unless (treesit-ready-p 'greger)
    (error "Tree-sitter greger parser not available"))

  (with-temp-buffer
    (insert text)
    (let* ((parser (treesit-parser-create 'greger))
           (root-node (treesit-parser-root-node parser)))
      (greger-parser--extract-dialog-from-node root-node))))

(defun greger-parser-dialog-to-markdown (dialog)
  "Convert DIALOG to markdown format."
  (if (null dialog)
      ""
    (mapconcat #'greger-parser--message-to-markdown dialog "\n\n")))

;; Tree-sitter-based markdown-to-dialog parsing

(defun greger-parser--extract-dialog-from-node (node)
  "Extract dialog entries from a tree-sitter NODE."
  (let ((raw-entries '()))
    ;; First extract all entries
    (dolist (child (treesit-node-children node))
      (let ((entry (greger-parser--extract-entry-from-node child)))
        (when entry
          (push entry raw-entries))))
    ;; Then merge assistant-related entries
    (greger-parser--merge-assistant-entries (nreverse raw-entries))))

(defun greger-parser--merge-assistant-entries (entries)
  "Merge consecutive assistant-related entries into single messages."
  (let ((result '())
        (current-assistant-content '())
        )
    (dolist (entry entries)
      (let ((role (cdr (assoc 'role entry))))
        (cond
         ;; If this is an assistant-type message, accumulate content
         ((string= role "assistant")
          (let ((content (cdr (assoc 'content entry))))
            (cond
             ;; Content is already a list of content blocks
             ((and (listp content) (listp (car content)) (assoc 'type (car content)))
              (setq current-assistant-content
                    (append current-assistant-content content)))
             ;; Content is plain text, convert to text block
             ((stringp content)
              (setq current-assistant-content
                    (append current-assistant-content
                            `(((type . "text") (text . ,content))))))
             ;; Content is some other list format
             (t
              (setq current-assistant-content
                    (append current-assistant-content content))))))
         ;; For non-assistant messages, flush any accumulated assistant content first
         (t
          (when current-assistant-content
            (setq result (greger-parser--flush-assistant-content current-assistant-content result))
            (setq current-assistant-content '()))
          (push entry result)))))
    ;; Don't forget any remaining assistant content
    (when current-assistant-content
      (setq result (greger-parser--flush-assistant-content current-assistant-content result)))
    ;; No need to fix types anymore since everything is web_search_tool_result
    (nreverse result)))



(defun greger-parser--flush-assistant-content (content result)
  "Flush accumulated assistant CONTENT to RESULT list, returning updated result."
  (if (and (= (length content) 1)
           (string= (cdr (assoc 'type (car content))) "text")
           (not (assoc 'citations (car content))))
      ;; Single text block without citations - use plain text format
      (cons `((role . "assistant")
              (content . ,(cdr (assoc 'text (car content)))))
            result)
    ;; Multiple blocks or special blocks - use content blocks format
    (cons `((role . "assistant")
            (content . ,content))
          result)))

(defun greger-parser--extract-entry-from-node (node)
  "Extract a single dialog entry from NODE."
  (let ((node-type (treesit-node-type node)))
    (cond
     ((string= node-type "user")
      (greger-parser--extract-user-entry node))
     ((string= node-type "assistant")
      (greger-parser--extract-assistant-entry node))
     ((string= node-type "system")
      (greger-parser--extract-system-entry node))
     ((string= node-type "thinking")
      (greger-parser--extract-thinking-entry node))
     ((string= node-type "tool_use")
      (greger-parser--extract-tool-use-entry node))
     ((string= node-type "tool_result")
      (greger-parser--extract-tool-result-entry node))
     ((string= node-type "server_tool_use")
      (greger-parser--extract-server-tool-use-entry node))
     ((string= node-type "web_search_tool_result")
      (greger-parser--extract-web-search-tool-result-entry node))
     ((string= node-type "citations")
      (greger-parser--extract-citations-entry node))
     (t nil))))

(defun greger-parser--extract-user-entry (node)
  "Extract user entry from NODE."
  (let ((content (greger-parser--extract-text-content node)))
    `((role . "user")
      (content . ,content))))

(defun greger-parser--extract-system-entry (node)
  "Extract system entry from NODE."
  (let ((content (greger-parser--extract-text-content node)))
    `((role . "system")
      (content . ,content))))

(defun greger-parser--extract-thinking-entry (node)
  "Extract thinking entry from NODE."
  (let ((content (greger-parser--extract-text-content node)))
    `((role . "assistant")
      (content . (((type . "thinking")
                   (thinking . ,content)))))))

(defun greger-parser--extract-assistant-entry (node)
  "Extract assistant entry from NODE."
  (let ((content (greger-parser--extract-text-content node)))
    `((role . "assistant")
      (content . ,content))))

(defun greger-parser--extract-tool-use-entry (node)
  "Extract tool use entry from NODE."
  (let ((name nil)
        (id nil)
        (params '()))
    (dolist (child (treesit-node-children node))
      (let ((child-type (treesit-node-type child)))
        (cond
         ((string= child-type "name")
          (setq name (greger-parser--extract-value child)))
         ((string= child-type "id")
          (setq id (greger-parser--extract-value child)))
         ((string= child-type "tool_param")
          (push (greger-parser--extract-tool-param child) params)))))
    (setq params (nreverse params))
    `((role . "assistant")
      (content . (((type . "tool_use")
                   (id . ,id)
                   (name . ,name)
                   (input . ,params)))))))

(defun greger-parser--extract-value (node)
  (let ((child (treesit-node-child-by-field-name node "value")))
    (when child
      (string-trim (treesit-node-text child t)))))

(defun greger-parser--extract-tool-param (node)
  (let ((name nil)
        (value nil))
    (dolist (child (treesit-node-children node))
      (let ((child-type (treesit-node-type child)))
        (cond
         ((string= child-type "name")
          (setq name (intern (string-trim (treesit-node-text child t)))))
         ((string= child-type "value")
          (setq value (greger-parser--extract-tool-content child))))))
    `(,name . ,value)))

(defun greger-parser--extract-tool-content (node)
  (let* ((value-node (treesit-node-child-by-field-name node "value"))
         (value (treesit-node-text value-node)))
    (greger-parser--convert-value
     (greger-parser--strip-single-newlines value))))

(defun greger-parser--strip-single-newlines (str)
  "Strip a single newline from the front and back of STR."
  (string-trim str "\\`\n?" "\n?\\'"))

(defun greger-parser--convert-param-value (value)
  "Convert VALUE to appropriate type (number if it looks like a number, otherwise string)."
  (if (string-match "^[0-9]+$" value)
      (string-to-number value)
    value))

(defun greger-parser--convert-value (str)
  "Convert STR to appropriate Elisp value."
  (let ((trimmed (string-trim str)))
    (cond
     ((string= trimmed "true") t)
     ((string= trimmed "false") nil)
     ((string-match-p "^-?[0-9]+$" trimmed)
      (string-to-number trimmed))
     ((string-match-p "^-?[0-9]*\\.[0-9]+$" trimmed)
      (string-to-number trimmed))
     ((and (string-prefix-p "[" trimmed) (string-suffix-p "]" trimmed))
      (greger-parser--parse-json-array trimmed))
     ((and (string-prefix-p "{" trimmed) (string-suffix-p "}" trimmed))
      (greger-parser--parse-json-object trimmed))
     (t str))))

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

(defun greger-parser--extract-tool-result-entry (node)
  "Extract tool result entry from NODE."
  (let ((id nil)
        (content nil))
    (dolist (child (treesit-node-children node))
      (let ((child-type (treesit-node-type child)))
        (cond
         ((string= child-type "id")
          (setq id (greger-parser--extract-value child
                    )))
         ((string= child-type "content")
          (setq content (greger-parser--extract-tool-content child))))))
    `((role . "user")
      (content . (((type . "tool_result")
                   (tool_use_id . ,id)
                   (content . ,content)))))))

(defun greger-parser--extract-server-tool-use-entry (node)
  "Extract server tool use entry from NODE."
  (let ((name nil)
        (id nil)
        (params '()))
    (dolist (child (treesit-node-children node))
      (let ((child-type (treesit-node-type child)))
        (cond
         ((string= child-type "name")
          (setq name (greger-parser--extract-value child
                      )))
         ((string= child-type "id")
          (setq id (greger-parser--extract-value child
                    )))
         ((string= child-type "tool_param")
          (push (greger-parser--extract-tool-param child) params)))))
    (setq params (nreverse params))
    `((role . "assistant")
      (content . (((type . "server_tool_use")
                   (id . ,id)
                   (name . ,name)
                   (input . ,params)))))))

(defun greger-parser--extract-web-search-tool-result-entry (node)
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

(defun greger-parser--extract-citations-entry (node)
  "Extract citations entry from NODE."
  (let ((text-content nil)
        (citation-entries '()))
    (dolist (child (treesit-node-children node))
      (let ((child-type (treesit-node-type child)))
        (cond
         ((string= child-type "text")
          (setq text-content (greger-parser--extract-text-content child)))
         ((string= child-type "citation_entry")
          (push (greger-parser--extract-citation-entry child) citation-entries)))))
    (setq citation-entries (nreverse citation-entries))
    ;; Return a result that can be merged with other content
    `((role . "assistant")
      (content . (((type . "text")
                   (text . ,text-content)
                   (citations . ,citation-entries)))))))

(defun greger-parser--extract-text-content (node)
  "Extract text content from NODE, handling nested structures."
  (let ((result (greger-parser--collect-text-blocks node "")))
    (string-trim result)))

(defun greger-parser--collect-text-blocks (node result)
  "Recursively collect text from text nodes in NODE and append to RESULT."
  (let ((node-type (treesit-node-type node)))
    (cond
     ((string= node-type "text")
      (concat result (treesit-node-text node t)))
     ((string= node-type "code_block")
      ;; For code blocks, include the entire text content
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
  "Parse CONTENT as JSON if it looks like JSON, otherwise return as plain text."
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
          (setq url (string-trim (treesit-node-text child t))))
         ((string= child-type "title")
          (setq title (greger-parser--extract-value child)))
         ((string= child-type "cited_text")
          (setq cited-text (greger-parser--extract-value child)))
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
  "Convert user content blocks to markdown format."
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
  "Convert assistant content blocks to markdown format."
  (if (null content-blocks)
      ""
    (let ((result "")
          (first-block t))
      (dolist (block content-blocks)
        (let* ((block-type (alist-get 'type block))
               (is-text-block (string= block-type "text"))
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
  (when citations
    (concat greger-parser-citations-tag "\n\n"
            (mapconcat #'greger-parser--citation-to-markdown citations "\n\n"))))

(defun greger-parser--citation-to-markdown (citation)
  "Convert single CITATION to markdown format."
  (let ((url (alist-get 'url citation))
        (title (alist-get 'title citation))
        (cited-text (alist-get 'cited_text citation))
        (encrypted-index (alist-get 'encrypted_index citation)))
    (concat "### " url "\n\n"
            "Title: " title "\n"
            "Cited text: " cited-text "\n"
            "Encrypted index: " encrypted-index)))

(defun greger-parser--block-to-markdown (block &optional skip-header)
  "Convert a content block to markdown.
If SKIP-HEADER is true, don't add section headers for text blocks."
  (let ((type (alist-get 'type block)))
    (cond
     ((string= type "text")
      (if (alist-get 'citations block)
          (greger-parser--citations-to-markdown block)
        (if skip-header
            (alist-get 'text block)
          (concat greger-parser-assistant-tag "\n\n" (alist-get 'text block)))))
     ((string= type "thinking")
      (concat greger-parser-thinking-tag "\n\n" (alist-get 'thinking block)))
     ((string= type "tool_use")
      (greger-parser--tool-use-to-markdown block))
     ((string= type "tool_result")
      (greger-parser--tool-result-to-markdown block))
     ((string= type "server_tool_use")
      (greger-parser--server-tool-use-to-markdown block))
     ((string= type "server_tool_result")
      (greger-parser--server-tool-result-to-markdown block))
     ((string= type "web_search_tool_result")
      (greger-parser--web-search-tool-result-to-markdown block))
     (t ""))))

(defun greger-parser--citations-to-markdown (block)
  (let* ((text (alist-get 'text block))
         (citations (alist-get 'citations block)))
    (concat "<cite>" text "</cite>"
            "\n\n"
            (greger-parser--citations-list-to-markdown citations))))

(defun greger-parser--tool-use-to-markdown (tool-use)
  "Convert TOOL-USE to markdown."
  (let ((name (alist-get 'name tool-use))
        (id (alist-get 'id tool-use))
        (input (alist-get 'input tool-use)))
    (concat greger-parser-tool-use-tag "\n\n"
            "Name: " name "\n"
            "ID: " id "\n\n"
            (greger-parser--tool-params-to-markdown id input))))

(defun greger-parser--tool-result-to-markdown (tool-result)
  "Convert TOOL-RESULT to markdown."
  (let ((id (alist-get 'tool_use_id tool-result))
        (content (alist-get 'content tool-result)))
    (concat greger-parser-tool-result-tag "\n\n"
            "ID: " id "\n\n"
            "<tool." id ">\n"
            (if (stringp content)
                content
              (greger-parser--value-to-string content)) "\n"
            "</tool." id ">")))

(defun greger-parser--server-tool-use-to-markdown (server-tool-use)
  "Convert SERVER-TOOL-USE to markdown."
  (let ((name (alist-get 'name server-tool-use))
        (id (alist-get 'id server-tool-use))
        (input (alist-get 'input server-tool-use)))
    (concat greger-parser-server-tool-use-tag "\n\n"
            "Name: " name "\n"
            "ID: " id "\n\n"
            (greger-parser--tool-params-to-markdown id input))))

(defun greger-parser--server-tool-result-to-markdown (server-tool-result)
  "Convert SERVER-TOOL-RESULT to markdown."
  (let ((id (alist-get 'tool_use_id server-tool-result))
        (content (alist-get 'content server-tool-result)))
    (concat greger-parser-server-tool-result-tag "\n\n"
            "ID: " id "\n\n"
            "<tool." id ">\n"
            (if (stringp content)
                content
              (greger-parser--value-to-string content)) "\n"
            "</tool." id ">")))

(defun greger-parser--web-search-tool-result-to-markdown (web-search-result)
  "Convert WEB-SEARCH-RESULT to markdown."
  (let ((id (alist-get 'tool_use_id web-search-result))
        (content (alist-get 'content web-search-result)))
    (concat greger-parser-server-tool-result-tag "\n\n"
            "ID: " id "\n\n"
            "<tool." id ">\n"
            (if (stringp content)
                content
              (greger-parser--value-to-string content)) "\n"
            "</tool." id ">")))

(defun greger-parser--tool-params-to-markdown (id input)
  "Convert tool parameters with ID and INPUT to markdown."
  (if (null input)
      ""
    (mapconcat (lambda (param)
                 (let ((name (if (symbolp (car param))
                                 (symbol-name (car param))
                               (format "%s" (car param))))
                       (value (cdr param)))
                   (concat "### " name "\n\n"
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

(defun greger-parser--content-block-has-citations (content-block)
  (not (null (assq 'citations content-block))))

(defun greger-parser--needs-block-separator (previous-type current-type)
  "Determine if a separator is needed between block types."
  ;; Always need separator between different block types except specific cases
  (not (or (and (string= previous-type "text") (string= current-type "text"))
           (and (string= previous-type "text") (greger-parser--content-block-has-citations current-type)))))

(provide 'greger-parser)

;;; greger-parser.el ends here
