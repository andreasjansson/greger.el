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

(require 'json)
(require 'cl-lib)
(require 'greger-web)

;; Section tag constants
(defconst greger-parser-user-tag "## USER:")
(defconst greger-parser-assistant-tag "## ASSISTANT:")
(defconst greger-parser-system-tag "## SYSTEM:")
(defconst greger-parser-tool-use-tag "## TOOL USE:")
(defconst greger-parser-tool-result-tag "## TOOL RESULT:")
(defconst greger-parser-server-tool-use-tag "## SERVER TOOL USE:")
(defconst greger-parser-server-tool-result-tag "## SERVER TOOL RESULT:")
(defconst greger-parser-thinking-tag "## THINKING:")

;;; Parser state structure

(cl-defstruct greger-parser-state
  input
  pos
  length
  debug
  metadata)

(defun greger-parser--create-state (input &optional debug)
  "Create a parser state for INPUT with optional DEBUG flag."
  (make-greger-parser-state
   :input (or input "")
   :pos 0
   :length (length (or input ""))
   :debug debug
   :metadata '()))

(defun greger-parser--debug (state format-string &rest args)
  "Debug logging function using STATE.
FORMAT-STRING is the format template and ARGS are the format arguments."
  (when (greger-parser-state-debug state)
    (message "[PARSER DEBUG] %s" (apply #'format format-string args))))

;; Main parsing entry points

(defun greger-parser-parse-dialog (markdown &optional debug)
  "Parse MARKDOWN into dialog format with optional DEBUG flag.
Returns a plist with :messages and :metadata keys."
  (if (or (null markdown) (string-empty-p (string-trim markdown)))
      '(:messages () :metadata ())
    (let ((state (greger-parser--create-state markdown debug)))
      (condition-case err
          (greger-parser--parse-document state)
        (error
         (greger-parser--debug state "Parse error: %s" (error-message-string err))
         '(:messages () :metadata ()))))))

(defun greger-parser-dialog-to-markdown (dialog)
  "Convert DIALOG to markdown format."
  (if (null dialog)
      ""
    (mapconcat #'greger-parser--message-to-markdown dialog "\n\n")))

;; Compatibility function for tests and existing code
(defun greger-parser-parse-dialog-messages-only (markdown &optional debug)
  "Parse MARKDOWN into dialog format, returning only the messages (old format).
This is for backward compatibility with existing tests and code.
DEBUG enables debug logging."
  (let ((result (greger-parser-parse-dialog markdown debug)))
    (plist-get result :messages)))

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

(defun greger-parser--insert-content-at-pos (state content)
  "Insert CONTENT into STATE at the current position and update length."
  (let* ((pos (greger-parser-state-pos state))
         (input (greger-parser-state-input state))
         (before (substring input 0 pos))
         (after (substring input pos))
         (new-input (concat before content after)))
    (setf (greger-parser-state-input state) new-input)
    (setf (greger-parser-state-length state) (length new-input))))

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
        greger-parser-server-tool-use-tag
        greger-parser-server-tool-result-tag
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

;; Web URL text extraction (moved to greger-web.el)

;; Include tag processing

(defun greger-parser--process-include-tag (state)
  "Process an include tag and return the included content in STATE."
  (greger-parser--debug state "Processing include tag at pos %d" (greger-parser-state-pos state))
  (progn
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
                (greger-parser--include-file state file-path has-code-attr)))))))))

(defun greger-parser--include-file (state file-path has-code-attr)
  "Include a file's content, optionally formatting as code using STATE.
Supports both local files and web URLs (http:// or https://).
For local files without code attribute, inserts content into state for
recursive parsing.  Returns nil when content is inserted, or the content
string when it should be appended.
FILE-PATH is the path to include and HAS-CODE-ATTR indicates code formatting."
  (greger-parser--debug state "Including file: %s (code: %s)" file-path has-code-attr)
  (condition-case err
      (let ((content
             (if (greger-web-is-web-url-p file-path)
                 ;; Handle web URL
                 (progn
                   (greger-parser--debug state "Downloading content from URL: %s" file-path)
                   (greger-web-text-from-url file-path t)) ; Use readability heuristics
               ;; Handle local file
               (with-temp-buffer
                 (insert-file-contents file-path)
                 (buffer-string)))))

        ;; Remove trailing newline from content if present
        (when (and (> (length content) 0)
                   (eq (aref content (1- (length content))) ?\n))
          (setq content (substring content 0 -1)))

        (cond
         ;; For files with code attribute or web URLs, return formatted content
         ((or has-code-attr (greger-web-is-web-url-p file-path))
          (if has-code-attr
              (format "%s:\n```\n%s\n```" file-path content)
            content))
         ;; For local files without code attribute, insert content into state for recursive parsing
         (t
          (greger-parser--insert-content-at-pos state content)
          nil))) ; Return nil to indicate content was inserted
    (error
     (greger-parser--debug state "Error reading %s %s: %s"
                          (if (greger-web-is-web-url-p file-path) "URL" "file")
                          file-path
                          (error-message-string err))
     ;; Return error message as content instead of failing silently
     (format "[Error reading %s: %s]"
             (if (greger-web-is-web-url-p file-path) "URL" "file")
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

(defun greger-parser--process-safe-shell-commands-tag (state)
  "Process a safe-shell-commands tag and return the list of commands in STATE."
  (greger-parser--debug state "Processing safe-shell-commands tag at pos %d" (greger-parser-state-pos state))
  (when (greger-parser--looking-at state "<safe-shell-commands>")
    (greger-parser--advance state (length "<safe-shell-commands>")) ; Skip "<safe-shell-commands>"

    ;; Extract the commands content
    (let ((content-start (greger-parser--current-pos state)))
      (when (greger-parser--find-closing-tag state "</safe-shell-commands>")
        (let ((commands-content (greger-parser--substring state content-start)))
          (greger-parser--advance state (length "</safe-shell-commands>")) ; Skip "</safe-shell-commands>"

          ;; Parse commands - split by lines and filter empty ones
          (let ((commands (delq nil
                               (mapcar (lambda (line)
                                        (let ((trimmed (string-trim line)))
                                          (when (not (string-empty-p trimmed))
                                            trimmed)))
                                      (split-string commands-content "\n")))))
            (greger-parser--debug state "Extracted safe shell commands: %s" commands)
            commands))))))

;; Content reading

(defun greger-parser--read-until-section-tag (state)
  "Read characters until section tag, handling code blocks and include tags.
STATE contains the parser state."
  (let ((iterations 0)
        (max-iterations (* (greger-parser-state-length state) 2))) ; Safety limit
    (while (and (not (greger-parser--at-end-p state))
                (not (and (greger-parser--at-line-start-p state)
                          (greger-parser--find-section-tag state)))
                (< iterations max-iterations))
      (setq iterations (1+ iterations))
      (let ((old-pos (greger-parser-state-pos state)))
        (cond
         ((greger-parser--at-triple-backticks state)
          (greger-parser--skip-code-block state))
         ((greger-parser--looking-at state "`")
          (greger-parser--skip-inline-code state))
         ((greger-parser--looking-at state "<!--")
          (greger-parser--skip-html-comment state))
         ((greger-parser--looking-at state "<include")
          (greger-parser--skip-include-tag state))
         (t
          (greger-parser--advance state)))
        ;; Safety check: ensure we're making progress
        (when (= old-pos (greger-parser-state-pos state))
          (greger-parser--debug state "No progress at pos %d, forcing advance" (greger-parser-state-pos state))
          (greger-parser--advance state))))
    (when (>= iterations max-iterations)
      (greger-parser--debug state "Hit max iterations in read-until-section-tag")
      (setf (greger-parser-state-pos state) (greger-parser-state-length state)))))

(defun greger-parser--read-until-section (state)
  "Read content until next section in STATE."
  (let ((start (greger-parser--current-pos state)))
    (greger-parser--read-until-section-tag state)
    (greger-parser--substring state start)))

(defun greger-parser--read-until-section-with-comment-removal (state)
  "Read content until next section, removing HTML comments and processing tags.
STATE contains the parser state."
  (let ((result "")
        (start (greger-parser--current-pos state))
        (iterations 0)
        (max-iterations (* (greger-parser-state-length state) 2))) ; Safety limit
    (while (and (not (greger-parser--at-end-p state))
                (not (and (greger-parser--at-line-start-p state)
                          (greger-parser--find-section-tag state)))
                (< iterations max-iterations))
      (setq iterations (1+ iterations))
      (let ((old-pos (greger-parser-state-pos state)))
        (cond
         ((greger-parser--at-triple-backticks state)
          ;; Add content up to code block
          (setq result (concat result (greger-parser--substring state start)))
          (setq start (greger-parser--current-pos state))
          (greger-parser--skip-code-block state)
          ;; Add the code block
          (setq result (concat result (greger-parser--substring state start)))
          (setq start (greger-parser--current-pos state)))
         ((greger-parser--looking-at state "`")
          ;; Add content up to inline code
          (setq result (concat result (greger-parser--substring state start)))
          (setq start (greger-parser--current-pos state))
          (greger-parser--skip-inline-code state)
          ;; Add the inline code
          (setq result (concat result (greger-parser--substring state start)))
          (setq start (greger-parser--current-pos state)))
         ((greger-parser--looking-at state "<!--")
          ;; Add content up to comment, skip comment entirely
          (setq result (concat result (greger-parser--substring state start)))
          (greger-parser--skip-html-comment state)
          (setq start (greger-parser--current-pos state)))
         ((greger-parser--looking-at state "<include")
          ;; Add content up to include tag
          (setq result (concat result (greger-parser--substring state start)))
          ;; Process the include tag
          (let ((include-content (greger-parser--process-include-tag state)))
            (if include-content
                ;; Content was returned (web URL or code), append it
                (setq result (concat result include-content))
              ;; Content was inserted into state (local file), continue parsing from current position
              nil))
          (setq start (greger-parser--current-pos state)))
         (t
          (greger-parser--advance state)))
        ;; Safety check: ensure we're making progress
        (when (= old-pos (greger-parser-state-pos state))
          (greger-parser--debug state "No progress at pos %d, forcing advance" (greger-parser-state-pos state))
          (greger-parser--advance state))))
    (when (>= iterations max-iterations)
      (greger-parser--debug state "Hit max iterations in read-until-section-with-comment-removal")
      (setf (greger-parser-state-pos state) (greger-parser-state-length state)))
    ;; Add remaining content
    (setq result (concat result (greger-parser--substring state start)))
    result))

(defun greger-parser--read-until-section-with-metadata-extraction (state)
  "Read content until next section, extracting metadata like safe-shell-commands.
Returns a plist with :content and metadata keys.
STATE contains the parser state."
  (let ((result "")
        (safe-shell-commands nil)
        (start (greger-parser--current-pos state))
        (iterations 0)
        (max-iterations (* (greger-parser-state-length state) 2))) ; Safety limit
    (while (and (not (greger-parser--at-end-p state))
                (not (and (greger-parser--at-line-start-p state)
                          (greger-parser--find-section-tag state)))
                (< iterations max-iterations))
      (setq iterations (1+ iterations))
      (let ((old-pos (greger-parser-state-pos state)))
        (cond
         ((greger-parser--at-triple-backticks state)
          ;; Add content up to code block
          (setq result (concat result (greger-parser--substring state start)))
          (setq start (greger-parser--current-pos state))
          (greger-parser--skip-code-block state)
          ;; Add the code block
          (setq result (concat result (greger-parser--substring state start)))
          (setq start (greger-parser--current-pos state)))
         ((greger-parser--looking-at state "`")
          ;; Add content up to inline code
          (setq result (concat result (greger-parser--substring state start)))
          (setq start (greger-parser--current-pos state))
          (greger-parser--skip-inline-code state)
          ;; Add the inline code
          (setq result (concat result (greger-parser--substring state start)))
          (setq start (greger-parser--current-pos state)))
         ((greger-parser--looking-at state "<!--")
          ;; Add content up to comment, skip comment entirely
          (setq result (concat result (greger-parser--substring state start)))
          (greger-parser--skip-html-comment state)
          (setq start (greger-parser--current-pos state)))
         ((greger-parser--looking-at state "<include")
          ;; Add content up to include tag
          (setq result (concat result (greger-parser--substring state start)))
          ;; Process the include tag
          (let ((include-content (greger-parser--process-include-tag state)))
            (if include-content
                ;; Content was returned (web URL or code), append it
                (setq result (concat result include-content))
              ;; Content was inserted into state (local file), continue parsing from current position
              nil))
          (setq start (greger-parser--current-pos state)))
         ((greger-parser--looking-at state "<safe-shell-commands>")
          ;; Add content up to safe-shell-commands tag (but don't include the tag itself)
          (setq result (concat result (greger-parser--substring state start)))
          ;; Process the safe-shell-commands tag
          (let ((commands (greger-parser--process-safe-shell-commands-tag state)))
            (when commands
              (if safe-shell-commands
                  (greger-parser--debug state "Warning: multiple <safe-shell-commands> tags found")
                (setq safe-shell-commands commands))))
          ;; Reset start position for next content
          (setq start (greger-parser--current-pos state)))
         (t
          (greger-parser--advance state)))
        ;; Safety check: ensure we're making progress
        (when (= old-pos (greger-parser-state-pos state))
          (greger-parser--debug state "No progress at pos %d, forcing advance" (greger-parser-state-pos state))
          (greger-parser--advance state))))
    (when (>= iterations max-iterations)
      (greger-parser--debug state "Hit max iterations in read-until-section-with-metadata-extraction")
      (setf (greger-parser-state-pos state) (greger-parser-state-length state)))
    ;; Add remaining content
    (setq result (concat result (greger-parser--substring state start)))

    ;; Return result with metadata
    (let ((trimmed-content (when (and result (not (string-empty-p (string-trim result))))
                            (string-trim result))))
      (list :content trimmed-content
            :safe-shell-commands safe-shell-commands))))

(defun greger-parser--parse-section-content (state)
  "Parse content until next section, skipping HTML comments.
STATE contains the parser state."
  (greger-parser--skip-whitespace state)
  (let ((content (greger-parser--read-until-section-with-comment-removal state)))
    (when (and content (not (string-empty-p (string-trim content))))
      (string-trim content))))

(defun greger-parser--parse-section-content-with-metadata (state)
  "Parse content until next section, extracting metadata like safe-shell-commands.
Returns a plist with :content and metadata keys like :safe-shell-commands.
STATE contains the parser state."
  (greger-parser--skip-whitespace state)
  (let ((result (greger-parser--read-until-section-with-metadata-extraction state)))
    result))

;; High-level parsing

(defun greger-parser--parse-document (state)
  "Parse entire document using STATE.
Returns a plist with :messages and :metadata keys."
  (greger-parser--skip-whitespace state)
  (if (greger-parser--at-end-p state)
      '(:messages () :metadata ())
    (let ((sections '())
          (metadata '())
          (iterations 0)
          (max-iterations 1000)) ; Safety limit
      ;; Handle untagged content at start
      (let ((untagged (greger-parser--parse-untagged-content state)))
        (when untagged
          (push untagged sections)))

      ;; Parse tagged sections
      (while (and (not (greger-parser--at-end-p state))
                  (< iterations max-iterations))
        (setq iterations (1+ iterations))
        (let ((old-pos (greger-parser-state-pos state)))
          (greger-parser--skip-whitespace state)
          (when (not (greger-parser--at-end-p state))
            (let ((section-result (greger-parser--parse-section state)))
              (when section-result
                (if (and (listp section-result) (eq (car section-result) :metadata))
                    ;; This is metadata, not a message - merge the metadata plist
                    (setq metadata (append metadata (cdr section-result)))
                  ;; This is a regular message
                  (push section-result sections)))))
          ;; Safety check: ensure we're making progress
          (when (= old-pos (greger-parser-state-pos state))
            (greger-parser--debug state "No progress in document parsing at pos %d, forcing end" (greger-parser-state-pos state))
            (setf (greger-parser-state-pos state) (greger-parser-state-length state)))))

      (when (>= iterations max-iterations)
        (greger-parser--debug state "Hit max iterations in parse-document"))

      ;; Combine metadata from section returns and parser state
      (let ((combined-metadata (append metadata (greger-parser-state-metadata state))))
        (list :messages (greger-parser--merge-consecutive-messages (reverse sections))
              :metadata combined-metadata)))))

(defun greger-parser--parse-untagged-content (state)
  "Parse content before first section tag using STATE."
  (let ((content (greger-parser--parse-section-content state)))
    (when content
      (greger-parser--create-user-message content))))

(defun greger-parser--parse-section (state)
  "Parse a section starting with a tag using STATE."
  (let ((tag (greger-parser--find-section-tag state)))
    (when tag
      (greger-parser--consume-section-tag state tag)
      (cond
       ((string= tag greger-parser-user-tag)
        (greger-parser--parse-user-section state))
       ((string= tag greger-parser-assistant-tag)
        (greger-parser--parse-assistant-section state))
       ((string= tag greger-parser-system-tag)
        (greger-parser--parse-system-section state))
       ((string= tag greger-parser-thinking-tag)
        (greger-parser--parse-thinking-section state))
       ((string= tag greger-parser-tool-use-tag)
        (greger-parser--parse-tool-use-section state))
       ((string= tag greger-parser-tool-result-tag)
        (greger-parser--parse-tool-result-section state))
       ((string= tag greger-parser-server-tool-use-tag)
        (greger-parser--parse-server-tool-use-section state))
       ((string= tag greger-parser-server-tool-result-tag)
        (greger-parser--parse-server-tool-result-section state))))))

;; Section parsers

(defun greger-parser--parse-user-section (state)
  "Parse USER section using STATE."
  (let ((content (greger-parser--parse-section-content state)))
    (when content
      (greger-parser--create-user-message content))))

(defun greger-parser--parse-assistant-section (state)
  "Parse ASSISTANT section using STATE."
  (let ((content (greger-parser--parse-section-content state)))
    (when content
      (greger-parser--create-assistant-message content))))

(defun greger-parser--parse-system-section (state)
  "Parse SYSTEM section using STATE.
Returns either a system message, metadata, or both."
  (let ((content (greger-parser--parse-section-content-with-metadata state)))
    (cond
     ;; If we extracted safe-shell-commands and no meaningful content, generate system message with safe commands text
     ((and (plist-get content :safe-shell-commands)
           (not (plist-get content :content)))
      (let ((safe-commands-text (greger-parser--generate-safe-shell-commands-text
                                (plist-get content :safe-shell-commands))))
        ;; Store metadata for later extraction and return system message with generated text
        (setf (greger-parser-state-metadata state)
              (append (or (greger-parser-state-metadata state) '())
                      (list :safe-shell-commands (plist-get content :safe-shell-commands))))
        (greger-parser--create-system-message safe-commands-text)))

     ;; If we have both content and safe-shell-commands, combine them
     ((and (plist-get content :safe-shell-commands)
           (plist-get content :content))
      (greger-parser--debug state "Found safe-shell-commands with system content - both will be processed")
      ;; Store metadata for later extraction and return system message with combined content
      (setf (greger-parser-state-metadata state)
            (append (or (greger-parser-state-metadata state) '())
                    (list :safe-shell-commands (plist-get content :safe-shell-commands))))
      (let ((safe-commands-text (greger-parser--generate-safe-shell-commands-text
                                (plist-get content :safe-shell-commands)))
            (original-content (plist-get content :content)))
        (greger-parser--create-system-message
         (if safe-commands-text
             (concat original-content "\n\n" safe-commands-text)
           original-content))))

     ;; Just regular content
     ((plist-get content :content)
      (greger-parser--create-system-message (plist-get content :content)))

     ;; No content
     (t nil))))

(defun greger-parser--parse-thinking-section (state)
  "Parse THINKING section using STATE."
  (let ((content (greger-parser--parse-section-content state)))
    (when content
      (greger-parser--create-thinking-message content))))

(defun greger-parser--parse-tool-use-section (state)
  "Parse TOOL USE section using STATE."
  (greger-parser--skip-whitespace state)
  (let ((name (greger-parser--parse-name-line state))
        (id (greger-parser--parse-id-line state))
        (input (greger-parser--parse-tool-input state)))
    (when (and name id)
      (greger-parser--create-tool-use-message name id input))))

(defun greger-parser--parse-tool-result-section (state)
  "Parse TOOL RESULT section using STATE."
  (greger-parser--skip-whitespace state)
  (let ((id (greger-parser--parse-id-line state))
        (content (greger-parser--parse-tool-result-content state)))
    (when id
      (greger-parser--create-tool-result-message id content))))

(defun greger-parser--parse-server-tool-use-section (state)
  "Parse SERVER TOOL USE section using STATE."
  (greger-parser--skip-whitespace state)
  (let ((name (greger-parser--parse-name-line state))
        (id (greger-parser--parse-id-line state))
        (input (greger-parser--parse-server-tool-input state)))
    (when (and name id)
      (greger-parser--create-server-tool-use-message name id input))))

(defun greger-parser--parse-server-tool-result-section (state)
  "Parse SERVER TOOL RESULT section using STATE."
  (greger-parser--skip-whitespace state)
  (let ((id (greger-parser--parse-id-line state))
        (content (greger-parser--parse-server-tool-result-content state)))
    (when id
      (greger-parser--create-server-tool-result-message id content))))

;; Tool parsing helpers

(defun greger-parser--parse-name-line (state)
  "Parse \='Name: value\=' line using STATE."
  (when (greger-parser--looking-at state "Name:")
    (greger-parser--advance state 5)
    (greger-parser--skip-horizontal-whitespace state)
    (greger-parser--read-line state)))

(defun greger-parser--parse-id-line (state)
  "Parse \='ID: value\=' line using STATE."
  (greger-parser--skip-whitespace state)
  (when (greger-parser--looking-at state "ID:")
    (greger-parser--advance state 3)
    (greger-parser--skip-horizontal-whitespace state)
    (greger-parser--read-line state)))

(defun greger-parser--parse-tool-input (state)
  "Parse tool input parameters using STATE."
  (let ((params '())
        (iterations 0)
        (max-iterations 100)) ; Safety limit
    (greger-parser--skip-whitespace state)
    (while (and (greger-parser--can-parse-parameter-p state)
                (< iterations max-iterations))
      (setq iterations (1+ iterations))
      (let ((old-pos (greger-parser-state-pos state))
            (param (greger-parser--parse-tool-parameter state)))
        (when param
          (push param params))
        (greger-parser--skip-whitespace state)
        ;; Safety check: ensure we're making progress
        (when (= old-pos (greger-parser-state-pos state))
          (greger-parser--debug state "No progress in tool input parsing at pos %d, forcing end" (greger-parser-state-pos state))
          (setf (greger-parser-state-pos state) (greger-parser-state-length state)))))
    (when (>= iterations max-iterations)
      (greger-parser--debug state "Hit max iterations in parse-tool-input"))
    (reverse params)))

(defun greger-parser--can-parse-parameter-p (state)
  "Check if we can parse a parameter using STATE."
  (and (not (greger-parser--at-end-p state))
       (not (and (greger-parser--at-line-start-p state)
                 (greger-parser--find-section-tag state)))
       (greger-parser--at-line-start-p state)
       (greger-parser--looking-at state "###")))

(defun greger-parser--parse-tool-parameter (state)
  "Parse single tool parameter using STATE."
  (when (greger-parser--looking-at state "###")
    (greger-parser--advance state 3)
    (greger-parser--skip-horizontal-whitespace state)
    (let ((name (greger-parser--read-line state)))
      (greger-parser--skip-whitespace state)
      (let ((value (greger-parser--parse-tool-value state)))
        (when (and name (not (string-empty-p name)))
          (cons (intern name) (greger-parser--convert-value (or value ""))))))))

(defun greger-parser--parse-tool-value (state)
  "Parse tool parameter value in XML-style tags using STATE."
  (when (greger-parser--looking-at state "<tool.")
    (let ((tag-start (greger-parser--current-pos state)))
      ;; Find end of opening tag
      (greger-parser--skip-to-closing-angle state)
      (when (eq (greger-parser--peek state) ?>)
        (let* ((opening-tag (greger-parser--substring state tag-start (+ (greger-parser--current-pos state) 1)))
               (closing-tag (greger-parser--make-closing-tag opening-tag)))
          (greger-parser--advance state) ; Skip >
          (greger-parser--skip-whitespace state)

          (let ((content-start (greger-parser--current-pos state)))
            (if (greger-parser--find-closing-tag state closing-tag)
                (let ((content (greger-parser--substring state content-start)))
                  (greger-parser--advance state (length closing-tag))
                  (greger-parser--normalize-tool-content content))
              ;; If no closing tag found, consume to end of section
              (let ((content (greger-parser--read-until-section state)))
                (greger-parser--normalize-tool-content content)))))))))

(defun greger-parser--skip-to-closing-angle (state)
  "Skip to closing angle bracket using STATE."
  (let ((iterations 0)
        (max-iterations 1000)) ; Safety limit
    (while (and (not (greger-parser--at-end-p state))
                (not (eq (greger-parser--peek state) ?>))
                (< iterations max-iterations))
      (setq iterations (1+ iterations))
      (greger-parser--advance state))
    (when (>= iterations max-iterations)
      (greger-parser--debug state "Hit max iterations in skip-to-closing-angle"))))

(defun greger-parser--make-closing-tag (opening-tag)
  "Make closing tag from OPENING-TAG."
  (concat "</" (substring opening-tag 1)))

(defun greger-parser--find-closing-tag (state closing-tag)
  "Find CLOSING-TAG, treating all content inside as raw text using STATE."
  (let ((found nil)
        (iterations 0)
        (max-iterations (* (greger-parser-state-length state) 2))) ; Safety limit
    (while (and (not found)
                (not (greger-parser--at-end-p state))
                (< iterations max-iterations))
      (setq iterations (1+ iterations))
      (if (greger-parser--looking-at state closing-tag)
          (setq found t)
        (greger-parser--advance state)))
    (when (>= iterations max-iterations)
      (greger-parser--debug state "Hit max iterations in find-closing-tag"))
    found))

(defun greger-parser--parse-tool-result-content (state)
  "Parse tool result content using STATE."
  (greger-parser--skip-whitespace state)
  (or (greger-parser--parse-tool-value state) ""))

(defun greger-parser--parse-server-tool-input (state)
  "Parse server tool input parameters using STATE."
  ;; Server tools use the same parameter format as regular tools
  (greger-parser--parse-tool-input state))

(defun greger-parser--parse-server-tool-result-content (state)
  "Parse server tool result content using STATE."
  ;; Server tool results use the same format as regular tool results
  (greger-parser--parse-tool-result-content state))

(defun greger-parser--normalize-tool-content (content)
  "Normalize tool CONTENT by trimming outer newlines."
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
     (t trimmed))))

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

;; Message creation

(defun greger-parser--create-user-message (content)
  "Create user message with CONTENT."
  `((role . "user") (content . ,content)))

(defun greger-parser--create-assistant-message (content)
  "Create assistant message with CONTENT."
  `((role . "assistant") (content . ,content)))

(defun greger-parser--create-system-message (content)
  "Create system message with CONTENT."
  `((role . "system") (content . ,content)))

(defun greger-parser--generate-safe-shell-commands-text (commands)
  "Generate descriptive text for safe shell COMMANDS list."
  (when commands
    (concat "You can run arbitrary shell commands with the shell-command tool, but the following are safe shell commands that will run without requiring user confirmation:\n\n"
            (mapconcat (lambda (cmd) (format "* `%s`" cmd)) commands "\n"))))

(defun greger-parser--create-thinking-message (content)
  "Create thinking message with CONTENT."
  `((role . "assistant")
    (content . (((type . "thinking") (thinking . ,content))))))

(defun greger-parser--create-tool-use-message (name id input)
  "Create tool use message with NAME, ID and INPUT."
  `((role . "assistant")
    (content . (((type . "tool_use")
                 (id . ,id)
                 (name . ,name)
                 (input . ,input))))))

(defun greger-parser--create-tool-result-message (id content)
  "Create tool result message with ID and CONTENT."
  `((role . "user")
    (content . (((type . "tool_result")
                 (tool_use_id . ,id)
                 (content . ,content))))))

(defun greger-parser--create-server-tool-use-message (name id input)
  "Create server tool use message with NAME, ID and INPUT."
  `((role . "assistant")
    (content . (((type . "server_tool_use")
                 (id . ,id)
                 (name . ,name)
                 (input . ,input))))))

(defun greger-parser--create-server-tool-result-message (id content)
  "Create server tool result message with ID and CONTENT."
  `((role . "assistant")
    (content . (((type . "server_tool_result")
                 (tool_use_id . ,id)
                 (content . ,content))))))

;; Message merging

(defun greger-parser--merge-consecutive-messages (messages)
  "Merge consecutive MESSAGES with same role."
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
  "Merge contents of MSG1 and MSG2."
  (let ((role (alist-get 'role msg1))
        (content1 (alist-get 'content msg1))
        (content2 (alist-get 'content msg2)))
    (let ((merged-content (greger-parser--merge-contents content1 content2)))
      `((role . ,role)
        (content . ,merged-content)))))

(defun greger-parser--merge-contents (content1 content2)
  "Merge CONTENT1 and CONTENT2 values."
  (let ((blocks1 (greger-parser--content-to-blocks content1))
        (blocks2 (greger-parser--content-to-blocks content2)))
    (let ((result (append blocks1 blocks2)))
      result)))

(defun greger-parser--content-to-blocks (content)
  "Convert CONTENT to content blocks."
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
    (concat greger-parser-user-tag "\n\n"
            (greger-parser--content-blocks-to-markdown content))))

(defun greger-parser--assistant-to-markdown (content)
  "Convert assistant CONTENT to markdown."
  (if (stringp content)
      (concat greger-parser-assistant-tag "\n\n" content)
    (greger-parser--content-blocks-to-markdown content)))

(defun greger-parser--system-to-markdown (content)
  "Convert system CONTENT to markdown."
  (concat greger-parser-system-tag "\n\n" content))

(defun greger-parser--content-blocks-to-markdown (blocks)
  "Convert content BLOCKS to markdown."
  (mapconcat #'greger-parser--block-to-markdown blocks "\n\n"))

(defun greger-parser--block-to-markdown (block)
  "Convert single BLOCK to markdown."
  (let ((type (alist-get 'type block)))

    ;; TODO: remove debug
    (message (format "type: %s" type))

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
     ((string= type "server_tool_use")
      (greger-parser--server-tool-use-to-markdown block))
     ((string= type "server_tool_result")
      (greger-parser--server-tool-result-to-markdown block))
     ((string= type "web_search_tool_result")
      (greger-parser--web-search-tool-result-to-markdown block))
     (t ""))))

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
            content "\n"
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
                 (let ((name (symbol-name (car param)))
                       (value (cdr param)))
                   (concat "### " name "\n\n"
                           "<tool." id ">\n"
                           (greger-parser--value-to-string value) "\n"
                           "</tool." id ">")))
               input "\n\n")))

(defun greger-parser--value-to-string (value)
  "Convert VALUE to string representation."
  (cond
   ((stringp value) value)
   ((numberp value) (number-to-string value))
   ((eq value t) "true")
   ((null value) "false")
   ((vectorp value) (json-encode value))
   ((listp value) (json-encode value))
   (t (format "%s" value))))

;; Global debug flag for interactive debugging
(defvar greger-parser--global-debug nil
  "Global debug flag for interactive debugging.")

;; Debug helper functions
(defun greger-parser-enable-debug ()
  "Enable parser debug output."
  (interactive)
  (setq greger-parser--global-debug t)
  (message "Parser debug enabled"))

(defun greger-parser-disable-debug ()
  "Disable parser debug output."
  (interactive)
  (setq greger-parser--global-debug nil)
  (message "Parser debug disabled"))

(defun greger-parser-parse-dialog-debug (markdown)
  "Parse MARKDOWN into dialog format with debug enabled."
  (greger-parser-parse-dialog markdown (or greger-parser--global-debug t)))

(provide 'greger-parser)

;;; greger-parser.el ends here
