;;; greger-parser.el --- Dialog parsing for greger -*- lexical-binding: t -*-

;;; Commentary:
;; Parses greger dialog format into structured data using PEG parsing

;;; Code:

(require 'json)

(defvar greger-parser-debug nil
  "Enable debug logging for parser.")

(defun greger-parser--debug (format-string &rest args)
  "Debug logging function."
  (when greger-parser-debug
    (message "[PARSER DEBUG] %s" (apply #'format format-string args))))

;; Constants for section headers
(defconst greger-parser-user-tag "## USER:")
(defconst greger-parser-assistant-tag "## ASSISTANT:")
(defconst greger-parser-system-tag "## SYSTEM:")
(defconst greger-parser-thinking-tag "## THINKING:")
(defconst greger-parser-tool-use-tag "## TOOL USE:")
(defconst greger-parser-tool-result-tag "## TOOL RESULT:")

(defun greger-parser--parse-sections ()
  "Parse buffer into sections using simple regex approach."
  (let ((sections '())
        (section-pattern (format "\\(?:%s\\|%s\\|%s\\|%s\\|%s\\|%s\\)"
                                greger-parser-user-tag
                                greger-parser-assistant-tag
                                greger-parser-system-tag
                                greger-parser-thinking-tag
                                greger-parser-tool-use-tag
                                greger-parser-tool-result-tag))
        (current-pos (point-min))
        (text-length (point-max)))

    ;; Handle untagged content at the beginning
    (when (not (looking-at section-pattern))
      (if (re-search-forward section-pattern nil t)
          (let ((first-section-pos (match-beginning 0)))
            (when (> first-section-pos current-pos)
              (let ((untagged-content (buffer-substring-no-properties current-pos first-section-pos)))
                (unless (string-empty-p (string-trim untagged-content))
                  (push (list 'untagged untagged-content) sections)))
              (setq current-pos first-section-pos)))
        ;; No sections found, treat entire text as untagged if not empty
        (unless (string-empty-p (string-trim (buffer-substring-no-properties current-pos text-length)))
          (push (list 'untagged (buffer-substring-no-properties current-pos text-length)) sections))
        (setq current-pos text-length)))

    ;; Find all section boundaries
    (goto-char current-pos)
    (while (< (point) text-length)
      (when (re-search-forward section-pattern nil t)
        (let* ((section-start (match-beginning 0))
               (section-header (match-string 0))
               (content-start (match-end 0))
               (next-section-pos (if (re-search-forward section-pattern nil t)
                                   (progn (goto-char (match-beginning 0)) (point))
                                 text-length))
               (content (buffer-substring-no-properties content-start next-section-pos)))

          (push (list (greger-parser--header-to-symbol section-header) content) sections)
          (goto-char next-section-pos))))

    (reverse sections)))

(defun greger-parser-parse-dialog (s)
  "Parse the string S into a dialog structure."
  (when (and s (not (string-empty-p (string-trim s))))
    (greger-parser--debug "Starting to parse dialog")
    (with-temp-buffer
      (insert s)
      (goto-char (point-min))
      (let ((sections (greger-parser--parse-sections)))
        (greger-parser--debug "Found %d sections" (length sections))
        (greger-parser--merge-content-blocks
         (delq nil (mapcar #'greger-parser--parse-section sections)))))))



(defun greger-parser--header-to-symbol (header)
  "Convert HEADER string to symbol."
  (cond
   ((string= header greger-parser-user-tag) 'user)
   ((string= header greger-parser-assistant-tag) 'assistant)
   ((string= header greger-parser-system-tag) 'system)
   ((string= header greger-parser-thinking-tag) 'thinking)
   ((string= header greger-parser-tool-use-tag) 'tool-use)
   ((string= header greger-parser-tool-result-tag) 'tool-result)
   (t 'unknown)))

(defun greger-parser--parse-section (section)
  "Parse a SECTION (type . content) into a message."
  (let ((type (car section))
        (content (string-trim (cadr section))))

    (when (not (string-empty-p content))
      (greger-parser--debug "Parsing section type: %s" type)
      (cond
       ((eq type 'untagged)
        (cons 'user (greger-parser--expand-context content)))

       ((eq type 'user)
        (cons 'user (greger-parser--expand-context content)))

       ((eq type 'assistant)
        (cons 'assistant content))

       ((eq type 'system)
        (cons 'system content))

       ((eq type 'thinking)
        (cons 'thinking content))

       ((eq type 'tool-use)
        (cons 'tool-use (greger-parser--parse-tool-use-content content)))

       ((eq type 'tool-result)
        (cons 'tool-result (greger-parser--parse-tool-result-content content)))))))

(defun greger-parser--expand-context (content)
  "Expand any <ai-context> tags in CONTENT, but not those inside code blocks."
  (let ((result "")
        (pos 0)
        (in-code-block nil)
        (in-inline-code nil))

    (while (< pos (length content))
      (cond
       ;; Check for triple backtick code blocks
       ((and (not in-inline-code)
             (string-match "^```" content pos))
        (setq in-code-block (not in-code-block))
        (let ((match-end (match-end 0)))
          (setq result (concat result (substring content pos match-end))
                pos match-end)))

       ;; Check for inline code (single backticks)
       ((and (not in-code-block)
             (string-match "`" content pos))
        (setq in-inline-code (not in-inline-code))
        (let ((match-end (match-end 0)))
          (setq result (concat result (substring content pos match-end))
                pos match-end)))

       ;; Check for ai-context tags (only when not in any code)
       ((and (not in-code-block)
             (not in-inline-code)
             (string-match "<ai-context>\\([^<]+\\)</ai-context>" content pos))
        (let* ((match-start (match-beginning 0))
               (match-end (match-end 0))
               (path (match-string 1 content))
               (expanded (greger-parser--expand-context-path path))
               (replacement (or expanded (format "[Could not load: %s]" path))))
          ;; Add text before match
          (setq result (concat result (substring content pos match-start) replacement)
                pos match-end)))

       ;; Default: add character and advance
       (t
        (setq result (concat result (substring content pos (1+ pos)))
              pos (1+ pos)))))

    result))

(defun greger-parser--expand-context-path (path)
  "Expand the context PATH into its content."
  (cond
   ((string-prefix-p "http" path)
    (greger-parser--text-from-url path))
   ((file-exists-p path)
    (condition-case nil
        (with-temp-buffer
          (insert-file-contents path)
          (buffer-string))
      (error nil)))
   (t nil)))

(defun greger-parser--parse-tool-use-content (content)
  "Parse tool use CONTENT into structured format."
  (let ((lines (split-string content "\n"))  ; Don't omit nulls
        (name nil)
        (id nil)
        (input '())
        (current-param nil)
        (current-value-lines '()))

    ;; Helper to finish current parameter
    (cl-flet ((finish-param ()
                (when current-param
                  ;; TODO: remove debug
                  (message (format "current-value-lines: %s" current-value-lines))
                  (let ((value (string-trim (mapconcat #'identity (reverse current-value-lines) "\n"))))
                    ;; Store as (param-name . value) for JSON object format
                    (push (cons current-param value) input))
                  (setq current-param nil
                        current-value-lines '()))))

      (dolist (line lines)
        (let ((trimmed-line (if line (string-trim line) "")))  ; Handle nil lines
          (cond
           ;; Name line
           ((string-match "^Name:\\s-*\\(.*\\)" trimmed-line)
            (setq name (string-trim (match-string 1 trimmed-line))))

           ;; ID line
           ((string-match "^ID:\\s-*\\(.*\\)" trimmed-line)
            (setq id (string-trim (match-string 1 trimmed-line))))

           ;; Parameter header
           ((string-match "^###\\s-*\\(.*\\)" trimmed-line)
            (finish-param)
            ;; need to match again since finish-param also calls string-match
            (string-match "^###\\s-*\\(.*\\)" trimmed-line)
            (setq current-param (string-trim (match-string 1 trimmed-line))))

           ;; Parameter value line (preserve original line including empty ones)
           (current-param
            (push (or line "") current-value-lines))  ; Handle nil lines

           ;; Skip empty lines when not in parameter
           ((not (string-empty-p trimmed-line))
            (greger-parser--debug "Unexpected line in tool use: %s" trimmed-line)))))

      ;; Finish last parameter
      (finish-param))

    `((type . "tool_use")
      (id . ,id)
      (name . ,name)
      (input . ,(reverse input)))))

(defun greger-parser--parse-tool-result-content (content)
  "Parse tool result CONTENT into structured format."
  (let ((lines (split-string content "\n" t))
        (id nil)
        (result-lines '())
        (in-result nil))

    (dolist (line lines)
      (let ((trimmed-line (string-trim line)))
        (cond
         ;; ID line
         ((and (not in-result) (string-match "^ID:\\s-*\\(.*\\)" trimmed-line))
          (setq id (string-trim (match-string 1 trimmed-line))
                in-result t))

         ;; Result content
         (in-result
          (push line result-lines))

         ;; Skip empty lines before ID
         ((not (string-empty-p trimmed-line))
          (greger-parser--debug "Unexpected line in tool result: %s" trimmed-line)))))

    (let ((result-content (string-trim (mapconcat #'identity (reverse result-lines) "\n"))))
      `((type . "tool_result")
        (tool_use_id . ,id)
        (content . ,result-content)))))

(defun greger-parser--merge-content-blocks (sections)
  "Merge consecutive assistant content blocks into single messages."
  (greger-parser--debug "Merging content blocks: %s" sections)
  (let ((result '())
        (current-assistant-content '()))

    (cl-flet ((flush-assistant ()
                (when current-assistant-content
                  (if (and (= (length current-assistant-content) 1)
                           (let ((block (car current-assistant-content)))
                             (and (alist-get 'type block)
                                  (string= (alist-get 'type block) "text"))))
                      ;; Single text block - use text directly
                      (push `(assistant . ,(alist-get 'text (car current-assistant-content))) result)
                    ;; Multiple blocks or non-text - use JSON
                    (push `(assistant . ,(json-encode (reverse current-assistant-content))) result))
                  (setq current-assistant-content '()))))

      (dolist (section sections)
        (let ((role (car section))
              (content (cdr section)))
          (greger-parser--debug "Processing section with role: %s" role)
          (cond
           ;; Assistant text content
           ((eq role 'assistant)
            (push `((type . "text") (text . ,content)) current-assistant-content))

           ;; Thinking
           ((eq role 'thinking)
            (push `((type . "thinking") (thinking . ,content)) current-assistant-content))

           ;; Tool use
           ((eq role 'tool-use)
            (push content current-assistant-content))

           ;; Tool result - flush assistant and add as user message
           ((eq role 'tool-result)
            (flush-assistant)
            (push `(user . ,(json-encode (list content))) result))

           ;; Other roles - flush assistant and add the message
           (t
            (flush-assistant)
            (push section result)))))

      ;; Flush any remaining assistant content
      (flush-assistant))

    (greger-parser--debug "Final merged result: %s" (reverse result))
    (reverse result)))

;; Dialog to markdown conversion
(defun greger-parser-dialog-to-markdown (dialog)
  "Convert DIALOG structure to markdown format."
  (if (null dialog)
      ""
    (mapconcat #'greger-parser--message-to-markdown dialog "\n\n")))

(defun greger-parser--message-to-markdown (message)
  "Convert a single MESSAGE to markdown format."
  (let ((role (car message))
        (content (cdr message)))
    (cond
     ((eq role 'user)
      (if (and (stringp content)
               (not (string-match-p "^\\[" (string-trim content))))
          (concat greger-parser-user-tag "\n\n" content)
        ;; Handle array content (tool results)
        (greger-parser--content-blocks-to-markdown content)))

     ((eq role 'assistant)
      (if (and (stringp content)
               (not (string-match-p "^\\[" (string-trim content))))
          (concat greger-parser-assistant-tag "\n\n" content)
        ;; Handle array content (mixed content blocks)
        (greger-parser--content-blocks-to-markdown content)))

     ((eq role 'system)
      (concat greger-parser-system-tag "\n\n" content)))))

(defun greger-parser--content-blocks-to-markdown (content-json)
  "Convert content blocks JSON to markdown format."
  (let ((blocks (if (stringp content-json)
                    (json-read-from-string content-json)
                  content-json))
        (result ""))

    (dolist (block (append blocks nil))
      (let ((type (alist-get 'type block)))
        (unless (string-empty-p result)
          (setq result (concat result "\n\n")))

        (cond
         ((string= type "text")
          (let ((text (alist-get 'text block)))
            (setq result (concat result greger-parser-assistant-tag "\n\n" text))))

         ((string= type "thinking")
          (let ((thinking (alist-get 'thinking block)))
            (setq result (concat result greger-parser-thinking-tag "\n\n" thinking))))

         ((string= type "tool_use")
          (let ((id (alist-get 'id block))
                (name (alist-get 'name block))
                (input (alist-get 'input block)))
            (setq result (concat result greger-parser-tool-use-tag "\n\n"
                                "Name: " name "\n"
                                "ID: " id "\n"))

            ;; Handle input parameters
            (when input
              (dolist (param input)
                (when (consp param)
                  (let ((param-name (if (symbolp (car param))
                                       (symbol-name (car param))
                                     (car param)))
                        (param-value (cdr param)))
                    (setq result (concat result "\n### " param-name "\n\n" param-value "\n"))))))))

         ((string= type "tool_result")
          (let ((id (alist-get 'tool_use_id block))
                (content (alist-get 'content block)))
            (setq result (concat result greger-parser-tool-result-tag "\n\n"
                                "ID: " id "\n\n" content)))))))

    result))

(defun greger-parser--text-from-url (url &optional use-highest-readability)
  "Retrieve the text content from URL.
If USE-HIGHEST-READABILITY is non-nil, use eww's readability heuristics."
  (condition-case nil
      (with-current-buffer
          (url-retrieve-synchronously url t nil 10.0)
        (let ((dom (libxml-parse-html-region)))
          (when use-highest-readability
            (setq dom (eww-highest-readability dom))
            (eww-score-readability dom))
          (greger-parser--dom-texts-inline-aware dom)))
    (error nil)))

(defun greger-parser--dom-texts-inline-aware (node &optional block-separator inline-separator)
  "Extract text from the DOM NODE, aware of inline and block elements.
BLOCK-SEPARATOR separates block elements.
INLINE-SEPARATOR separates inline elements."
  (let ((block-separator (or block-separator "\n"))
        (inline-separator (or inline-separator " ")))
    (mapconcat
     (lambda (elem)
       (cond
        ((stringp elem)
         (when (> (length (string-trim elem)) 0)
           elem))
        ((memq (dom-tag elem) '(head meta script style details footer)) "")
        ((memq (dom-tag elem) '(p div h1 h2 h3 h4 h5 h6 pre br hr ul ol li))
         (concat (greger-parser--dom-texts-inline-aware elem block-separator inline-separator)
                 block-separator))
        (t
         (greger-parser--dom-texts-inline-aware elem block-separator inline-separator))))
     (dom-children node)
     inline-separator)))

(provide 'greger-parser)

;;; greger-parser.el ends here
