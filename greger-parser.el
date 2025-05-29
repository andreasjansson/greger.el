;;; greger-parser.el --- Parser for greger dialog format -*- lexical-binding: t -*-

;;; Commentary:
;; Parses markdown-style dialog format with sections like ## USER:, ## ASSISTANT:, etc.
;; Handles tool use, thinking blocks, and complex content structures.

;;; Code:

(require 'json)
(require 'cl-lib)
(require 'url)
(require 'dom)
(require 'eww)

;; Section tag constants
(defconst greger-parser-user-tag "## USER:")
(defconst greger-parser-assistant-tag "## ASSISTANT:")
(defconst greger-parser-system-tag "## SYSTEM:")
(defconst greger-parser-tool-use-tag "## TOOL USE:")
(defconst greger-parser-tool-result-tag "## TOOL RESULT:")
(defconst greger-parser-thinking-tag "## THINKING:")

;;; Parser state structure

(cl-defstruct greger-parser-state
  input
  pos
  length
  debug)

(defun greger-parser--create-state (input &optional debug)
  "Create a parser state for INPUT with optional DEBUG flag."
  (make-greger-parser-state
   :input (or input "")
   :pos 0
   :length (length (or input ""))
   :debug debug))

(defun greger-parser--debug (state format-string &rest args)
  "Debug logging function using STATE."
  (when (greger-parser-state-debug state)
    (message "[PARSER DEBUG] %s" (apply #'format format-string args))))

;; Main parsing entry points

(defun greger-parser-parse-dialog (markdown &optional debug)
  "Parse MARKDOWN into dialog format with optional DEBUG flag."
  (if (or (null markdown) (string-empty-p (string-trim markdown)))
      '()
    (let ((state (greger-parser--create-state markdown debug)))
      (condition-case err
          (greger-parser--parse-document state)
        (error
         (greger-parser--debug state "Parse error: %s" (error-message-string err))
         '())))))

(defun greger-parser-dialog-to-markdown (dialog)
  "Convert DIALOG to markdown format."
  (if (null dialog)
      ""
    (mapconcat #'greger-parser--message-to-markdown dialog "\n\n")))

;; Parser infrastructure

(defun greger-parser--at-end-p (state)
  "True if at end of input in STATE."
  (>= (greger-parser-state-pos state) (greger-parser-state-length state)))

(defun greger-parser--peek (state &optional offset)
  "Peek at character at current position plus OFFSET in STATE."
  (let ((pos (+ (greger-parser-state-pos state) (or offset 0))))
    (if (and (>= pos 0) (< pos (greger-parser-state-length state)))
        (aref (greger-parser-state-input state) pos)
      nil)))

(defun greger-parser--advance (state &optional n)
  "Advance position by N characters (default 1) in STATE."
  (let ((old-pos (greger-parser-state-pos state)))
    (setf (greger-parser-state-pos state)
          (min (greger-parser-state-length state)
               (+ (greger-parser-state-pos state) (or n 1))))
    (greger-parser--debug state "Advanced from %d to %d" old-pos (greger-parser-state-pos state))))

(defun greger-parser--current-pos (state)
  "Get current position from STATE."
  (greger-parser-state-pos state))

(defun greger-parser--substring (state start &optional end)
  "Get substring from START to END (or current position) in STATE."
  (let ((input (greger-parser-state-input state))
        (length (greger-parser-state-length state))
        (current-pos (greger-parser-state-pos state)))
    (if (and (>= start 0)
             (<= start length)
             (or (null end) (<= end length)))
        (substring input start (or end current-pos))
      "")))

(defun greger-parser--looking-at (state string)
  "True if current position matches STRING in STATE."
  (and string
       (<= (+ (greger-parser-state-pos state) (length string)) (greger-parser-state-length state))
       (string= (greger-parser--substring state (greger-parser-state-pos state)
                                         (+ (greger-parser-state-pos state) (length string)))
                string)))

(defun greger-parser--at-triple-backticks (state)
  "True if current position matches ``` at beginning of line in STATE."
  (and (greger-parser--at-line-start-p state)
       (greger-parser--looking-at state "```")))

;; Character tests

(defun greger-parser--whitespace-p (char)
  "True if CHAR is whitespace."
  (and char (memq char '(?\s ?\t ?\n ?\r))))

(defun greger-parser--horizontal-whitespace-p (char)
  "True if CHAR is horizontal whitespace."
  (and char (memq char '(?\s ?\t))))

(defun greger-parser--newline-p (char)
  "True if CHAR is newline."
  (and char (eq char ?\n)))

;; Navigation

(defun greger-parser--skip-whitespace (state)
  "Skip all whitespace in STATE."
  (let ((start-pos (greger-parser-state-pos state)))
    (while (and (not (greger-parser--at-end-p state))
                (greger-parser--whitespace-p (greger-parser--peek state)))
      (greger-parser--advance state))
    (when (> (greger-parser-state-pos state) start-pos)
      (greger-parser--debug state "Skipped whitespace from %d to %d" start-pos (greger-parser-state-pos state)))))

(defun greger-parser--skip-horizontal-whitespace (state)
  "Skip spaces and tabs in STATE."
  (while (and (not (greger-parser--at-end-p state))
              (greger-parser--horizontal-whitespace-p (greger-parser--peek state)))
    (greger-parser--advance state)))

(defun greger-parser--at-line-start-p (state)
  "True if at start of line in STATE."
  (or (= (greger-parser-state-pos state) 0)
      (greger-parser--newline-p (greger-parser--peek state -1))))

(defun greger-parser--skip-to-line-end (state)
  "Skip to end of current line in STATE."
  (while (and (not (greger-parser--at-end-p state))
              (not (greger-parser--newline-p (greger-parser--peek state))))
    (greger-parser--advance state)))

(defun greger-parser--read-line (state)
  "Read rest of current line in STATE."
  (let ((start (greger-parser--current-pos state)))
    (greger-parser--skip-to-line-end state)
    (string-trim (greger-parser--substring state start))))

;; Section tag handling

(defun greger-parser--section-tags ()
  "List of all section tags."
  (list greger-parser-user-tag
        greger-parser-assistant-tag
        greger-parser-system-tag
        greger-parser-tool-use-tag
        greger-parser-tool-result-tag
        greger-parser-thinking-tag))

(defun greger-parser--find-section-tag (state)
  "Find section tag at current position if at line start in STATE."
  (when (greger-parser--at-line-start-p state)
    (let ((tag (cl-find-if (lambda (tag) (greger-parser--looking-at state tag)) (greger-parser--section-tags))))
      (greger-parser--debug state "Found section tag: %s at pos %d" tag (greger-parser-state-pos state))
      tag)))

(defun greger-parser--consume-section-tag (state tag)
  "Consume TAG and return it in STATE."
  (when (greger-parser--looking-at state tag)
    (greger-parser--debug state "Consuming tag: %s" tag)
    (greger-parser--advance state (length tag))
    tag))

;; Code block detection and skipping

(defun greger-parser--skip-code-block (state)
  "Skip triple-backtick code block in STATE."
  (greger-parser--debug state "Skipping code block at pos %d" (greger-parser-state-pos state))
  (greger-parser--advance state 3) ; Skip opening ```
  (greger-parser--skip-to-line-end state) ; Skip language specifier
  (when (greger-parser--newline-p (greger-parser--peek state))
    (greger-parser--advance state))

  ;; Find closing ```
  (while (and (not (greger-parser--at-end-p state))
              (not (greger-parser--at-triple-backticks state)))
    (greger-parser--advance state))

  ;; Skip closing ```
  (when (greger-parser--at-triple-backticks state)
    (greger-parser--advance state 3)))

(defun greger-parser--skip-inline-code (state)
  "Skip inline code with double backticks in STATE."
  (greger-parser--debug state "Skipping inline code at pos %d" (greger-parser-state-pos state))
  (greger-parser--advance state 1) ; Skip opening `
  (while (and (not (greger-parser--at-end-p state))
              (not (greger-parser--looking-at state "`")))
    (greger-parser--advance state))
  (when (greger-parser--looking-at state "`")
    (greger-parser--advance state 1)))

(defun greger-parser--skip-html-comment (state)
  "Skip HTML comment in STATE."
  (greger-parser--debug state "Skipping HTML comment at pos %d" (greger-parser-state-pos state))
  (greger-parser--advance state 4) ; Skip <!--
  (while (and (not (greger-parser--at-end-p state))
              (not (greger-parser--looking-at state "-->")))
    (greger-parser--advance state))
  (when (greger-parser--looking-at state "-->")
    (greger-parser--advance state 3)))

;; Web URL text extraction (inspired by old aichat parser)

(defun greger-parser--text-from-url (url &optional use-highest-readability)
  "Retrieve the text content from URL.
If USE-HIGHEST-READABILITY is non-nil, use eww's readability heuristics."
  (with-current-buffer
      (url-retrieve-synchronously url t nil 10.0)
    ;; Skip HTTP headers - they end with a double newline
    (goto-char (point-min))
    (when (re-search-forward "\r?\n\r?\n" nil t)
      (delete-region (point-min) (point)))

    ;; Parse the HTML content
    (let ((dom (libxml-parse-html-region (point-min) (point-max))))
      (when use-highest-readability
        (setq dom (eww-highest-readability dom))
        (eww-score-readability dom))
      (greger-parser--dom-texts-inline-aware dom))))

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

(defun greger-parser--is-web-url-p (path)
  "Return non-nil if PATH is a web URL (starts with http:// or https://)."
  (or (string-prefix-p "http://" path)
      (string-prefix-p "https://" path)))

;; Include tag processing

(defun greger-parser--process-include-tag (state)
  "Process an include tag and return the included content in STATE."
  (greger-parser--debug state "Processing include tag at pos %d" (greger-parser-state-pos state))
  (let ((tag-start (greger-parser--current-pos state)))
    ;; Parse the opening tag
    (when (greger-parser--looking-at state "<include")
      (greger-parser--advance state 8) ; Skip "<include"
      (let ((has-code-attr nil))
        ;; Check for optional "code" attribute
        (greger-parser--skip-horizontal-whitespace state)
        (when (greger-parser--looking-at state "code")
          (setq has-code-attr t)
          (greger-parser--advance state 4)
          (greger-parser--skip-horizontal-whitespace state))

        ;; Skip to closing bracket of opening tag
        (when (greger-parser--looking-at state ">")
          (greger-parser--advance state 1)

          ;; Extract the file path
          (let ((path-start (greger-parser--current-pos state)))
            (when (greger-parser--find-closing-tag state "</include>")
              (let ((file-path (string-trim (greger-parser--substring state path-start))))
                (greger-parser--advance state 10) ; Skip "</include>"

                ;; Read and process the file
                (greger-parser--include-file file-path has-code-attr)))))))))

(defun greger-parser--include-file (file-path has-code-attr)
  "Include a file's content, optionally formatting as code.
Supports both local files and web URLs (http:// or https://)."
  (greger-parser--debug "Including file: %s (code: %s)" file-path has-code-attr)
  (condition-case err
      (let ((content
             (if (greger-parser--is-web-url-p file-path)
                 ;; Handle web URL
                 (progn
                   (greger-parser--debug "Downloading content from URL: %s" file-path)
                   (greger-parser--text-from-url file-path t)) ; Use readability heuristics
               ;; Handle local file
               (with-temp-buffer
                 (insert-file-contents file-path)
                 (buffer-string)))))

        ;; Remove trailing newline from content if present
        (when (and (> (length content) 0)
                   (eq (aref content (1- (length content))) ?\n))
          (setq content (substring content 0 -1)))

        (if has-code-attr
            (format "%s:\n```\n%s\n```" file-path content)
          content))
    (error
     (greger-parser--debug "Error reading %s %s: %s"
                          (if (greger-parser--is-web-url-p file-path) "URL" "file")
                          file-path
                          (error-message-string err))
     ;; Return error message as content instead of failing silently
     (format "[Error reading %s: %s]"
             (if (greger-parser--is-web-url-p file-path) "URL" "file")
             file-path))))

(defun greger-parser--skip-include-tag (state)
  "Skip include tag without processing it in STATE."
  (greger-parser--debug state "Skipping include tag at pos %d" (greger-parser-state-pos state))
  (greger-parser--advance state 8) ; Skip "<include"

  ;; Skip optional "code" attribute
  (greger-parser--skip-horizontal-whitespace state)
  (when (greger-parser--looking-at state "code")
    (greger-parser--advance state 4)
    (greger-parser--skip-horizontal-whitespace state))

  ;; Skip to closing bracket of opening tag
  (when (greger-parser--looking-at state ">")
    (greger-parser--advance state 1)

    ;; Skip to closing tag
    (when (greger-parser--find-closing-tag state "</include>")
      (greger-parser--advance state 10)))) ; Skip "</include>"

;; Content reading

(defun greger-parser--read-until-section-tag ()
  "Read characters until section tag, handling code blocks and include tags."
  (let ((start-pos greger-parser--pos)
        (iterations 0)
        (max-iterations (* greger-parser--length 2))) ; Safety limit
    (while (and (not (greger-parser--at-end-p))
                (not (and (greger-parser--at-line-start-p)
                          (greger-parser--find-section-tag)))
                (< iterations max-iterations))
      (setq iterations (1+ iterations))
      (let ((old-pos greger-parser--pos))
        (cond
         ((greger-parser--at-triple-backticks)
          (greger-parser--skip-code-block))
         ((greger-parser--looking-at "`")
          (greger-parser--skip-inline-code))
         ((greger-parser--looking-at "<!--")
          (greger-parser--skip-html-comment))
         ((greger-parser--looking-at "<include")
          (greger-parser--skip-include-tag))
         (t
          (greger-parser--advance)))
        ;; Safety check: ensure we're making progress
        (when (= old-pos greger-parser--pos)
          (greger-parser--debug "No progress at pos %d, forcing advance" greger-parser--pos)
          (greger-parser--advance))))
    (when (>= iterations max-iterations)
      (greger-parser--debug "Hit max iterations in read-until-section-tag")
      (setq greger-parser--pos greger-parser--length))))

(defun greger-parser--read-until-section ()
  "Read content until next section."
  (let ((start (greger-parser--current-pos)))
    (greger-parser--read-until-section-tag)
    (greger-parser--substring start)))

(defun greger-parser--read-until-section-with-comment-removal ()
  "Read content until next section, removing HTML comments and processing include tags."
  (let ((result "")
        (start (greger-parser--current-pos))
        (iterations 0)
        (max-iterations (* greger-parser--length 2))) ; Safety limit
    (while (and (not (greger-parser--at-end-p))
                (not (and (greger-parser--at-line-start-p)
                          (greger-parser--find-section-tag)))
                (< iterations max-iterations))
      (setq iterations (1+ iterations))
      (let ((old-pos greger-parser--pos))
        (cond
         ((greger-parser--at-triple-backticks)
          ;; Add content up to code block
          (setq result (concat result (greger-parser--substring start)))
          (setq start (greger-parser--current-pos))
          (greger-parser--skip-code-block)
          ;; Add the code block
          (setq result (concat result (greger-parser--substring start)))
          (setq start (greger-parser--current-pos)))
         ((greger-parser--looking-at "`")
          ;; Add content up to inline code
          (setq result (concat result (greger-parser--substring start)))
          (setq start (greger-parser--current-pos))
          (greger-parser--skip-inline-code)
          ;; Add the inline code
          (setq result (concat result (greger-parser--substring start)))
          (setq start (greger-parser--current-pos)))
         ((greger-parser--looking-at "<!--")
          ;; Add content up to comment, skip comment entirely
          (setq result (concat result (greger-parser--substring start)))
          (greger-parser--skip-html-comment)
          (setq start (greger-parser--current-pos)))
         ((greger-parser--looking-at "<include")
          ;; Add content up to include tag
          (setq result (concat result (greger-parser--substring start)))
          ;; Process the include tag
          (let ((include-content (greger-parser--process-include-tag)))
            (when include-content
              (setq result (concat result include-content))))
          (setq start (greger-parser--current-pos)))
         (t
          (greger-parser--advance)))
        ;; Safety check: ensure we're making progress
        (when (= old-pos greger-parser--pos)
          (greger-parser--debug "No progress at pos %d, forcing advance" greger-parser--pos)
          (greger-parser--advance))))
    (when (>= iterations max-iterations)
      (greger-parser--debug "Hit max iterations in read-until-section-with-comment-removal")
      (setq greger-parser--pos greger-parser--length))
    ;; Add remaining content
    (setq result (concat result (greger-parser--substring start)))
    result))

(defun greger-parser--parse-section-content ()
  "Parse content until next section, skipping HTML comments."
  (greger-parser--skip-whitespace)
  (let ((content (greger-parser--read-until-section-with-comment-removal)))
    (when (and content (not (string-empty-p (string-trim content))))
      (string-trim content))))

;; High-level parsing

(defun greger-parser--parse-document ()
  "Parse entire document."
  (greger-parser--skip-whitespace)
  (if (greger-parser--at-end-p)
      '()
    (let ((sections '())
          (iterations 0)
          (max-iterations 1000)) ; Safety limit
      ;; Handle untagged content at start
      (let ((untagged (greger-parser--parse-untagged-content)))
        (when untagged
          (push untagged sections)))

      ;; Parse tagged sections
      (while (and (not (greger-parser--at-end-p))
                  (< iterations max-iterations))
        (setq iterations (1+ iterations))
        (let ((old-pos greger-parser--pos))
          (greger-parser--skip-whitespace)
          (when (not (greger-parser--at-end-p))
            (let ((section (greger-parser--parse-section)))
              (when section
                (push section sections))))
          ;; Safety check: ensure we're making progress
          (when (= old-pos greger-parser--pos)
            (greger-parser--debug "No progress in document parsing at pos %d, breaking" greger-parser--pos)
            (break))))

      (when (>= iterations max-iterations)
        (greger-parser--debug "Hit max iterations in parse-document"))

      (greger-parser--merge-consecutive-messages (reverse sections)))))

(defun greger-parser--parse-untagged-content ()
  "Parse content before first section tag."
  (let ((content (greger-parser--parse-section-content)))
    (when content
      (greger-parser--create-user-message content))))

(defun greger-parser--parse-section ()
  "Parse a section starting with a tag."
  (let ((tag (greger-parser--find-section-tag)))
    (when tag
      (greger-parser--consume-section-tag tag)
      (cond
       ((string= tag greger-parser-user-tag)
        (greger-parser--parse-user-section))
       ((string= tag greger-parser-assistant-tag)
        (greger-parser--parse-assistant-section))
       ((string= tag greger-parser-system-tag)
        (greger-parser--parse-system-section))
       ((string= tag greger-parser-thinking-tag)
        (greger-parser--parse-thinking-section))
       ((string= tag greger-parser-tool-use-tag)
        (greger-parser--parse-tool-use-section))
       ((string= tag greger-parser-tool-result-tag)
        (greger-parser--parse-tool-result-section))))))

;; Section parsers

(defun greger-parser--parse-user-section ()
  "Parse USER section."
  (let ((content (greger-parser--parse-section-content)))
    (when content
      (greger-parser--create-user-message content))))

(defun greger-parser--parse-assistant-section ()
  "Parse ASSISTANT section."
  (let ((content (greger-parser--parse-section-content)))
    (when content
      (greger-parser--create-assistant-message content))))

(defun greger-parser--parse-system-section ()
  "Parse SYSTEM section."
  (let ((content (greger-parser--parse-section-content)))
    (when content
      (greger-parser--create-system-message content))))

(defun greger-parser--parse-thinking-section ()
  "Parse THINKING section."
  (let ((content (greger-parser--parse-section-content)))
    (when content
      (greger-parser--create-thinking-message content))))

(defun greger-parser--parse-tool-use-section ()
  "Parse TOOL USE section."
  (greger-parser--skip-whitespace)
  (let ((name (greger-parser--parse-name-line))
        (id (greger-parser--parse-id-line))
        (input (greger-parser--parse-tool-input)))
    (when (and name id)
      (greger-parser--create-tool-use-message name id input))))

(defun greger-parser--parse-tool-result-section ()
  "Parse TOOL RESULT section."
  (greger-parser--skip-whitespace)
  (let ((id (greger-parser--parse-id-line))
        (content (greger-parser--parse-tool-result-content)))
    (when id
      (greger-parser--create-tool-result-message id content))))

;; Tool parsing helpers

(defun greger-parser--parse-name-line ()
  "Parse 'Name: value' line."
  (when (greger-parser--looking-at "Name:")
    (greger-parser--advance 5)
    (greger-parser--skip-horizontal-whitespace)
    (greger-parser--read-line)))

(defun greger-parser--parse-id-line ()
  "Parse 'ID: value' line."
  (greger-parser--skip-whitespace)
  (when (greger-parser--looking-at "ID:")
    (greger-parser--advance 3)
    (greger-parser--skip-horizontal-whitespace)
    (greger-parser--read-line)))

(defun greger-parser--parse-tool-input ()
  "Parse tool input parameters."
  (let ((params '())
        (iterations 0)
        (max-iterations 100)) ; Safety limit
    (greger-parser--skip-whitespace)
    (while (and (greger-parser--can-parse-parameter-p)
                (< iterations max-iterations))
      (setq iterations (1+ iterations))
      (let ((old-pos greger-parser--pos)
            (param (greger-parser--parse-tool-parameter)))
        (when param
          (push param params))
        (greger-parser--skip-whitespace)
        ;; Safety check: ensure we're making progress
        (when (= old-pos greger-parser--pos)
          (greger-parser--debug "No progress in tool input parsing at pos %d, breaking" greger-parser--pos)
          (break))))
    (when (>= iterations max-iterations)
      (greger-parser--debug "Hit max iterations in parse-tool-input"))
    (reverse params)))

(defun greger-parser--can-parse-parameter-p ()
  "Check if we can parse a parameter."
  (and (not (greger-parser--at-end-p))
       (not (and (greger-parser--at-line-start-p)
                 (greger-parser--find-section-tag)))
       (greger-parser--at-line-start-p)
       (greger-parser--looking-at "###")))

(defun greger-parser--parse-tool-parameter ()
  "Parse single tool parameter."
  (when (greger-parser--looking-at "###")
    (greger-parser--advance 3)
    (greger-parser--skip-horizontal-whitespace)
    (let ((name (greger-parser--read-line)))
      (greger-parser--skip-whitespace)
      (let ((value (greger-parser--parse-tool-value)))
        (when (and name (not (string-empty-p name)))
          (cons (intern name) (greger-parser--convert-value (or value ""))))))))

(defun greger-parser--parse-tool-value ()
  "Parse tool parameter value in XML-style tags."
  (when (greger-parser--looking-at "<tool.")
    (let ((tag-start (greger-parser--current-pos)))
      ;; Find end of opening tag
      (greger-parser--skip-to-closing-angle)
      (when (eq (greger-parser--peek) ?>)
        (let* ((opening-tag (greger-parser--substring tag-start (+ (greger-parser--current-pos) 1)))
               (closing-tag (greger-parser--make-closing-tag opening-tag)))
          (greger-parser--advance) ; Skip >
          (greger-parser--skip-whitespace)

          (let ((content-start (greger-parser--current-pos)))
            (if (greger-parser--find-closing-tag closing-tag)
                (let ((content (greger-parser--substring content-start)))
                  (greger-parser--advance (length closing-tag))
                  (greger-parser--normalize-tool-content content))
              ;; If no closing tag found, consume to end of section
              (let ((content (greger-parser--read-until-section)))
                (greger-parser--normalize-tool-content content)))))))))

(defun greger-parser--skip-to-closing-angle ()
  "Skip to closing angle bracket."
  (let ((iterations 0)
        (max-iterations 1000)) ; Safety limit
    (while (and (not (greger-parser--at-end-p))
                (not (eq (greger-parser--peek) ?>))
                (< iterations max-iterations))
      (setq iterations (1+ iterations))
      (greger-parser--advance))
    (when (>= iterations max-iterations)
      (greger-parser--debug "Hit max iterations in skip-to-closing-angle"))))

(defun greger-parser--make-closing-tag (opening-tag)
  "Make closing tag from opening tag."
  (concat "</" (substring opening-tag 1)))

(defun greger-parser--find-closing-tag (closing-tag)
  "Find closing tag, treating all content inside as raw text."
  (let ((found nil)
        (iterations 0)
        (max-iterations (* greger-parser--length 2))) ; Safety limit
    (while (and (not found)
                (not (greger-parser--at-end-p))
                (< iterations max-iterations))
      (setq iterations (1+ iterations))
      (if (greger-parser--looking-at closing-tag)
          (setq found t)
        (greger-parser--advance)))
    (when (>= iterations max-iterations)
      (greger-parser--debug "Hit max iterations in find-closing-tag"))
    found))

(defun greger-parser--parse-tool-result-content ()
  "Parse tool result content."
  (greger-parser--skip-whitespace)
  (or (greger-parser--parse-tool-value) ""))

(defun greger-parser--normalize-tool-content (content)
  "Normalize tool content by trimming outer newlines."
  (if (string-empty-p content)
      ""
    (let ((result content))
      ;; Remove leading newline
      (when (and (> (length result) 0)
                 (eq (aref result 0) ?\n))
        (setq result (substring result 1)))
      ;; Remove trailing newline
      (when (and (> (length result) 0)
                 (eq (aref result (1- (length result))) ?\n))
        (setq result (substring result 0 -1)))
      result)))

(defun greger-parser--convert-value (str)
  "Convert string to appropriate Elisp value."
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
     (t trimmed))))

(defun greger-parser--parse-json-array (str)
  "Parse JSON array string."
  (condition-case nil
      (json-read-from-string str)
    (error str)))

(defun greger-parser--parse-json-object (str)
  "Parse JSON object string."
  (condition-case nil
      (let ((parsed (json-read-from-string str)))
        (mapcar (lambda (pair)
                  (cons (intern (symbol-name (car pair))) (cdr pair)))
                parsed))
    (error str)))

;; Message creation

(defun greger-parser--create-user-message (content)
  "Create user message."
  `((role . "user") (content . ,content)))

(defun greger-parser--create-assistant-message (content)
  "Create assistant message."
  `((role . "assistant") (content . ,content)))

(defun greger-parser--create-system-message (content)
  "Create system message."
  `((role . "system") (content . ,content)))

(defun greger-parser--create-thinking-message (content)
  "Create thinking message."
  `((role . "assistant")
    (content . (((type . "thinking") (thinking . ,content))))))

(defun greger-parser--create-tool-use-message (name id input)
  "Create tool use message."
  `((role . "assistant")
    (content . (((type . "tool_use")
                 (id . ,id)
                 (name . ,name)
                 (input . ,input))))))

(defun greger-parser--create-tool-result-message (id content)
  "Create tool result message."
  `((role . "user")
    (content . (((type . "tool_result")
                 (tool_use_id . ,id)
                 (content . ,content))))))

;; Message merging

(defun greger-parser--merge-consecutive-messages (messages)
  "Merge consecutive messages with same role."
  (if (null messages)
      '()
    (let ((result (list (car messages))))
      (dolist (msg (cdr messages))
        (let* ((last (car result))
               (last-role (alist-get 'role last))
               (curr-role (alist-get 'role msg)))
          (if (string= last-role curr-role)
              ;; Merge with previous
              (progn
                (let ((merged (greger-parser--merge-message-contents last msg)))
                  (setcar result merged)))
            ;; Add as new message
            (progn
              (push msg result)))))
      (reverse result))))

(defun greger-parser--merge-message-contents (msg1 msg2)
  "Merge contents of two messages."
  (let ((role (alist-get 'role msg1))
        (content1 (alist-get 'content msg1))
        (content2 (alist-get 'content msg2)))
    (let ((merged-content (greger-parser--merge-contents content1 content2)))
      `((role . ,role)
        (content . ,merged-content)))))

(defun greger-parser--merge-contents (content1 content2)
  "Merge two content values."
  (let ((blocks1 (greger-parser--content-to-blocks content1))
        (blocks2 (greger-parser--content-to-blocks content2)))
    (let ((result (append blocks1 blocks2)))
      result)))

(defun greger-parser--content-to-blocks (content)
  "Convert content to content blocks."
  (let ((result (cond
                 ((stringp content)
                  `(((type . "text") (text . ,content))))
                 ((listp content)
                  content)
                 (t
                  `(((type . "text") (text . ,(format "%s" content))))))))
    result))

;; Markdown generation

(defun greger-parser--message-to-markdown (message)
  "Convert message to markdown."
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
  "Convert user content to markdown."
  (if (stringp content)
      (concat greger-parser-user-tag "\n\n" content)
    (concat greger-parser-user-tag "\n\n"
            (greger-parser--content-blocks-to-markdown content))))

(defun greger-parser--assistant-to-markdown (content)
  "Convert assistant content to markdown."
  (if (stringp content)
      (concat greger-parser-assistant-tag "\n\n" content)
    (greger-parser--content-blocks-to-markdown content)))

(defun greger-parser--system-to-markdown (content)
  "Convert system content to markdown."
  (concat greger-parser-system-tag "\n\n" content))

(defun greger-parser--content-blocks-to-markdown (blocks)
  "Convert content blocks to markdown."
  (mapconcat #'greger-parser--block-to-markdown blocks "\n\n"))

(defun greger-parser--block-to-markdown (block)
  "Convert single block to markdown."
  (let ((type (alist-get 'type block)))
    (cond
     ((string= type "text")
      (concat greger-parser-assistant-tag "\n\n"
              (alist-get 'text block)))
     ((string= type "thinking")
      (concat greger-parser-thinking-tag "\n\n"
              (alist-get 'thinking block)))
     ((string= type "tool_use")
      (greger-parser--tool-use-to-markdown block))
     ((string= type "tool_result")
      (greger-parser--tool-result-to-markdown block))
     (t ""))))

(defun greger-parser--tool-use-to-markdown (tool-use)
  "Convert tool use to markdown."
  (let ((name (alist-get 'name tool-use))
        (id (alist-get 'id tool-use))
        (input (alist-get 'input tool-use)))
    (concat greger-parser-tool-use-tag "\n\n"
            "Name: " name "\n"
            "ID: " id "\n\n"
            (greger-parser--tool-params-to-markdown id input))))

(defun greger-parser--tool-result-to-markdown (tool-result)
  "Convert tool result to markdown."
  (let ((id (alist-get 'tool_use_id tool-result))
        (content (alist-get 'content tool-result)))
    (concat greger-parser-tool-result-tag "\n\n"
            "ID: " id "\n\n"
            "<tool." id ">\n"
            content "\n"
            "</tool." id ">")))

(defun greger-parser--tool-params-to-markdown (id input)
  "Convert tool parameters to markdown."
  (if (null input)
      ""
    (mapconcat (lambda (param)
                 (let ((name (symbol-name (car param)))
                       (value (cdr param)))
                   (concat "### " name "\n\n"
                           "<tool." id ">\n"
                           (greger-parser--value-to-string value) "\n"
                           "</tool." id ">")))
               input "\n\n")))

(defun greger-parser--value-to-string (value)
  "Convert value to string representation."
  (cond
   ((stringp value) value)
   ((numberp value) (number-to-string value))
   ((eq value t) "true")
   ((eq value nil) "false")
   ((vectorp value) (json-encode value))
   ((listp value) (json-encode value))
   (t (format "%s" value))))

;; Debug helper function
(defun greger-parser-enable-debug ()
  "Enable parser debug output."
  (interactive)
  (setq greger-parser--debug t)
  (message "Parser debug enabled"))

(defun greger-parser-disable-debug ()
  "Disable parser debug output."
  (interactive)
  (setq greger-parser--debug nil)
  (message "Parser debug disabled"))

(provide 'greger-parser)

;;; greger-parser.el ends here
