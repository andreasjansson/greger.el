;;; greger-parser.el --- Dialog parsing for greger -*- lexical-binding: t -*-

;;; Commentary:
;; Parses greger dialog format into structured data using PEG parsing

;;; Code:

(require 'cl-lib)
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

(defun greger-parser--find-next-section-outside-code-blocks (section-pattern start-pos)
  "Find next section header that's not inside a code block or HTML comment.
Returns (match-start . match-end) or nil if not found."
  (let ((in-triple-backtick nil)
        (in-double-backtick nil)
        (in-html-comment nil))

    (cl-loop for pos from start-pos below (point-max)
             do (goto-char pos)
             do (cond
                 ;; HTML comment start (only when not in code blocks)
                 ((and (not in-triple-backtick)
                       (not in-double-backtick)
                       (not in-html-comment)
                       (looking-at "<!--"))
                  (setq in-html-comment t
                        pos (+ pos 3))
                  (message "in html comment")) ; Will be incremented by loop

                 ;; HTML comment end (only when in HTML comment)
                 ((and in-html-comment
                       (looking-at "-->"))
                  (setq in-html-comment nil
                        pos (+ pos 2))) ; Will be incremented by loop

                 ;; Triple backticks (start or end of line)
                 ((and (not in-double-backtick)
                       (not in-html-comment)
                       (looking-at "^```"))
                  (setq in-triple-backtick (not in-triple-backtick)
                        pos (line-end-position)))

                 ;; Double backticks
                 ((and (not in-triple-backtick)
                       (not in-html-comment)
                       (looking-at "``"))
                  (setq in-double-backtick (not in-double-backtick)
                        pos (+ pos 1))) ; Will be incremented by loop

                 ;; Check for section header when not in code block or HTML comment
                 ((and (not in-triple-backtick)
                       (not in-double-backtick)
                       (not in-html-comment)
                       (looking-at section-pattern))
                  (cl-return (cons (match-beginning 0) (match-end 0)))))
             finally return nil)))

(defun greger-parser--parse-sections ()
  "Parse buffer into sections using code-block-aware approach."
  (let ((sections '())
        (section-pattern (format "^\\(?:%s\\|%s\\|%s\\|%s\\|%s\\|%s\\)"
                                greger-parser-user-tag
                                greger-parser-assistant-tag
                                greger-parser-system-tag
                                greger-parser-thinking-tag
                                greger-parser-tool-use-tag
                                greger-parser-tool-result-tag))
        (current-pos (point-min)))

    ;; Handle untagged content at the beginning
    (goto-char current-pos)
    (let ((first-section (greger-parser--find-next-section-outside-code-blocks section-pattern current-pos)))
      (when first-section
        (let ((first-section-pos (car first-section)))
          (when (> first-section-pos current-pos)
            (let ((untagged-content (buffer-substring-no-properties current-pos first-section-pos)))
              (unless (string-empty-p (string-trim untagged-content))
                (push (list 'untagged untagged-content) sections)))
            (setq current-pos first-section-pos))))

      ;; If no sections found, treat entire text as untagged if not empty
      (unless first-section
        (unless (string-empty-p (string-trim (buffer-substring-no-properties current-pos (point-max))))
          (push (list 'untagged (buffer-substring-no-properties current-pos (point-max))) sections))
        (setq current-pos (point-max))))

    ;; Find all section boundaries
    (cl-loop while (< current-pos (point-max))
             for section-match = (greger-parser--find-next-section-outside-code-blocks section-pattern current-pos)
             when section-match
             do (let* ((section-start (car section-match))
                       (section-end (cdr section-match))
                       (section-header (buffer-substring-no-properties section-start section-end))
                       (content-start section-end)
                       (next-section (greger-parser--find-next-section-outside-code-blocks
                                     section-pattern (1+ section-end)))
                       (next-section-pos (if next-section
                                           (car next-section)
                                         (point-max)))
                       (content (buffer-substring-no-properties content-start next-section-pos)))

                  (push (list (greger-parser--header-to-symbol section-header) content) sections)
                  (setq current-pos next-section-pos))
             unless section-match
             do (setq current-pos (point-max)))

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
        `((role . "user") (content . ,(greger-parser--expand-context content))))

       ((eq type 'user)
        `((role . "user") (content . ,(greger-parser--expand-context content))))

       ((eq type 'assistant)
        `((role . "assistant") (content . ,content)))

       ((eq type 'system)
        `((role . "system") (content . ,content)))

       ((eq type 'thinking)
        `((role . "thinking") (content . ,content)))

       ((eq type 'tool-use)
        `((role . "tool-use") (content . ,(greger-parser--parse-tool-use-content content))))

       ((eq type 'tool-result)
        `((role . "tool-result") (content . ,(greger-parser--parse-tool-result-content content))))))))

(defun greger-parser--expand-context (content)
  "Expand any <ai-context> tags in CONTENT and remove HTML comments, but not those inside code blocks."
  (let ((result "")
        (pos 0)
        (in-code-block nil)
        (in-inline-code nil))

    (cl-loop while (< pos (length content))
             do (cond
                 ;; Check for triple backtick code blocks (only at start of line)
                 ((and (not in-inline-code)
                       (or (= pos 0) (= (aref content (1- pos)) ?\n))
                       (string-match "^```" (substring content pos)))
                  (setq in-code-block (not in-code-block))
                  (let ((match-end (+ pos (match-end 0))))
                    (setq result (concat result (substring content pos match-end))
                          pos match-end)))

                 ;; Check for inline code (single backticks)
                 ((and (not in-code-block)
                       (= (aref content pos) ?`)
                       (or (= pos 0) (/= (aref content (1- pos)) ?`))) ; Not part of triple backticks
                  (setq in-inline-code (not in-inline-code))
                  (setq result (concat result (substring content pos (1+ pos)))
                        pos (1+ pos)))

                 ;; Check for HTML comments (remove when not in code blocks)
                 ((and (not in-code-block)
                       (not in-inline-code)
                       (string-match "<!--\\(\\(?:.\\|\n\\)*?\\)-->" (substring content pos)))
                  (let ((match-start pos)
                        (match-end (+ pos (match-end 0))))
                    ;; Skip the HTML comment entirely
                    (setq pos match-end)))

                 ;; Check for ai-context tags (only when not in any code)
                 ((and (not in-code-block)
                       (not in-inline-code)
                       (string-match "<ai-context>\\([^<]+\\)</ai-context>" (substring content pos)))
                  (let* ((match-start pos)
                         (match-end (+ pos (match-end 0)))
                         (path (match-string 1 (substring content pos)))
                         (expanded (greger-parser--expand-context-path path))
                         (replacement (or expanded (format "[Could not load: %s]" path))))
                    ;; Add the replacement
                    (setq result (concat result replacement)
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
                  (let* ((value-str (string-trim (mapconcat #'identity (reverse current-value-lines) "\n")))
                         (value (greger-parser--parse-param-value value-str)))
                    (push (cons (intern current-param) value) input))
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

           ;; Parameter delimiter (--{id}) - finish current parameter
           ((and id (string-match (concat "^--" (regexp-quote id) "$") trimmed-line))
            (finish-param))

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

      ;; Finish last parameter if no delimiter was found
      (finish-param))

    `((type . "tool_use")
      (id . ,id)
      (name . ,name)
      (input . ,(reverse input)))))

(defun greger-parser--parse-param-value (value-str)
  "Parse VALUE-STR into appropriate type (string, number, bool, list, or dict)."
  (cond
   ;; Empty string
   ((string-empty-p value-str) "")

   ;; Boolean values
   ((string= value-str "true") t)
   ((string= value-str "false") nil)

   ;; Try to parse as JSON (list or dict)
   ((or (string-prefix-p "[" value-str)
        (string-prefix-p "{" value-str))
    (condition-case nil
        (json-read-from-string value-str)
      (error value-str)))

   ;; Try to parse as number
   ((string-match-p "^-?[0-9]+\\(?:\\.[0-9]+\\)?$" value-str)
    (string-to-number value-str))

   ;; Default to string
   (t value-str)))

(defun greger-parser--parse-tool-result-content (content)
  "Parse tool result CONTENT into structured format."
  (let ((lines (split-string content "\n"))
        (id nil)
        (result-lines '())
        (in-result nil))

    (dolist (line lines)
      (let ((trimmed-line (if line (string-trim line) "")))  ; Handle potential nil lines
        (cond
         ;; ID line
         ((and (not in-result) (string-match "^ID:\\s-*\\(.*\\)" trimmed-line))
          (setq id (string-trim (match-string 1 trimmed-line))
                in-result t))

         ;; Result delimiter (--{id}) - stop processing result content
         ((and id in-result (string-match (concat "^--" (regexp-quote id) "$") trimmed-line))
          ;; Stop processing when we hit the delimiter
          (cl-return))

         ;; Result content
         (in-result
          (push (or line "") result-lines))  ; Preserve original line including empty ones

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
                      (push `((role . "assistant") (content . ,(alist-get 'text (car current-assistant-content)))) result)
                    ;; Multiple blocks or non-text - use array of objects (NOT JSON string)
                    (push `((role . "assistant") (content . ,(reverse current-assistant-content))) result))
                  (setq current-assistant-content '()))))

      (dolist (section sections)
        (let ((role (alist-get 'role section))
              (content (alist-get 'content section)))
          (greger-parser--debug "Processing section with role: %s" role)
          (cond
           ;; Assistant text content
           ((string= role "assistant")
            (push `((type . "text") (text . ,content)) current-assistant-content))

           ;; Thinking
           ((string= role "thinking")
            (push `((type . "thinking") (thinking . ,content)) current-assistant-content))

           ;; Tool use
           ((string= role "tool-use")
            (push content current-assistant-content))

           ;; Tool result - flush assistant and add as user message
           ((string= role "tool-result")
            (flush-assistant)
            (push `((role . "user") (content . ,(list content))) result))

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
  (let ((role (alist-get 'role message))
        (content (alist-get 'content message)))
    (cond
     ((string= role "user")
      (if (and (stringp content)
               (not (string-match-p "^\\[" (string-trim content))))
          (concat greger-parser-user-tag "\n\n" content)
        ;; Handle array content (tool results)
        (greger-parser--content-blocks-to-markdown content)))

     ((string= role "assistant")
      (if (and (stringp content)
               (not (string-match-p "^\\[" (string-trim content))))
          (concat greger-parser-assistant-tag "\n\n" content)
        ;; Handle array content (mixed content blocks)
        (greger-parser--content-blocks-to-markdown content)))

     ((string= role "system")
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
                    (setq result (concat result "\n### " param-name "\n\n"
                                         (greger-parser--format-param-value param-value) "\n\n"
                                         "--" id "\n"))))))))

         ((string= type "tool_result")
          (let ((id (alist-get 'tool_use_id block))
                (content (alist-get 'content block)))
            (setq result (concat result greger-parser-tool-result-tag "\n\n"
                                "ID: " id "\n\n" content "\n\n--" id "\n")))))))

    result))

(defun greger-parser--format-param-value (value)
  "Format VALUE for display in markdown tool use section."
  (cond
   ((stringp value) value)
   ((booleanp value) (if value "true" "false"))
   ((numberp value) (number-to-string value))
   ((or (listp value) (vectorp value)) (json-encode value))
   (t (format "%s" value))))

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
