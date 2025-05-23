;;; aichat-parser.el --- Dialog parsing for aichat -*- lexical-binding: t -*-

;;; Commentary:
;; Parses aichat dialog format into structured data

;;; Code:

(require 'parsec)
(require 'dom)
(require 'eww)

(defvar aichat-parser-user-tag "## USER:")
(defvar aichat-parser-assistant-tag "## ASSISTANT:")
(defvar aichat-parser-system-tag "## SYSTEM:")

(defun aichat-parser-parse-dialog (s)
  "Parse the string S into a dialog structure."
  (parsec-with-input s
    (aichat-parser--parse-spaces)
    (let ((untagged-section (parsec-optional (aichat-parser--parse-untagged-section)))
          (sections (parsec-many (parsec-or
                                  (aichat-parser--parse-user-section)
                                  (aichat-parser--parse-assistant-section)
                                  (aichat-parser--parse-system-section)))))
      (if untagged-section
          (cons untagged-section sections)
        sections))))

(defsubst aichat-parser--parse-spaces ()
  "Parse any number of whitespace characters."
  (parsec-many-as-string
   (parsec-re "[[:space:]\r\n]")))

(defun aichat-parser--parse-comment ()
  "Parse an HTML comment."
  (parsec-and
   (parsec-str "<!--")
   (parsec-until-as-string
    (parsec-try
     (parsec-str "-->")))
   (aichat-parser--parse-spaces)))

(defun aichat-parser--parse-codeblock ()
  "Parse a Markdown code block."
  (parsec-collect-as-string
   (parsec-str "```")
   (let ((out (parsec-until-as-string
               (parsec-str "```")
               :both)))
     (concat (car out) (cdr out)))))

(defun aichat-parser--parse-followed-by-tag (tag)
  "Check if the input is followed by TAG."
  (parsec-lookahead (parsec-str tag)))

(defun aichat-parser--parse-followed-by-section ()
  "Check if the input is followed by a section tag."
  (parsec-or
   (aichat-parser--parse-followed-by-tag aichat-parser-user-tag)
   (aichat-parser--parse-followed-by-tag aichat-parser-assistant-tag)
   (aichat-parser--parse-followed-by-tag aichat-parser-system-tag)
   (parsec-eof)))

(defun aichat-parser--parse-context ()
  "Parse the <ai-context> from the input."
  (parsec-str "<ai-context>")
  (aichat-parser--parse-spaces)
  (let ((context-path (parsec-until-s
                       (parsec-and
                        (aichat-parser--parse-spaces)
                        (parsec-str "</ai-context>")))))
    (aichat-parser--expand-context context-path)))

(defun aichat-parser--expand-context (path)
  "Expand the context PATH into its content."
  (cond
   ((string-prefix-p "http" path)
    (aichat-parser--text-from-url path))
   ((file-exists-p path)
    (with-temp-buffer
      (insert-file-contents path)
      (buffer-string)))
   (t
    (message "Invalid path or URL: %s" path)
    nil)))

(defun aichat-parser--parse-user-content ()
  "Parse the content of a user section."
  (parsec-or
   (parsec-many-till-as-string (parsec-or
                                (parsec-optional* (aichat-parser--parse-comment))
                                (aichat-parser--parse-codeblock)
                                (aichat-parser--parse-context)
                                (parsec-any-ch))
                               (aichat-parser--parse-followed-by-section))))

(defun aichat-parser--parse-assistant-content ()
  "Parse the content of an assistant section."
  (parsec-or
   (parsec-many-till-as-string (parsec-or
                                (aichat-parser--parse-codeblock)
                                (parsec-any-ch))
                               (aichat-parser--parse-followed-by-section))))

(defun aichat-parser--parse-user-section ()
  "Parse a user section."
  (parsec-and
   (parsec-str aichat-parser-user-tag)
   (aichat-parser--parse-spaces)
   (cons 'user (string-trim (aichat-parser--parse-user-content)))))

(defun aichat-parser--parse-untagged-section ()
  "Parse an untagged section."
  (let ((content (string-trim (aichat-parser--parse-user-content))))
    (unless (string= "" content)
      (cons 'user content))))

(defun aichat-parser--parse-assistant-section ()
  "Parse an assistant section."
  (parsec-and
   (parsec-str aichat-parser-assistant-tag)
   (aichat-parser--parse-spaces)
   (cons 'assistant (string-trim (aichat-parser--parse-assistant-content)))))

(defun aichat-parser--parse-system-section ()
  "Parse a system section."
  (parsec-and
   (parsec-str aichat-parser-system-tag)
   (aichat-parser--parse-spaces)
   (cons 'system (string-trim (aichat-parser--parse-user-content)))))

(defun aichat-parser--text-from-url (url &optional use-highest-readability)
  "Retrieve the text content from URL.
If USE-HIGHEST-READABILITY is non-nil, use eww's readability heuristics."
  (with-current-buffer
      (url-retrieve-synchronously url t nil 10.0)
    (let ((dom (libxml-parse-html-region)))
      (when use-highest-readability
        (setq dom (eww-highest-readability dom))
        (eww-score-readability dom))
      (aichat-parser--dom-texts-inline-aware dom))))

(defun aichat-parser--dom-texts-inline-aware (node &optional block-separator inline-separator)
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
         (concat (aichat-parser--dom-texts-inline-aware elem block-separator inline-separator)
                 block-separator))
        (t
         (aichat-parser--dom-texts-inline-aware elem block-separator inline-separator))))
     (dom-children node)
     inline-separator)))

(provide 'aichat-parser)

;;; aichat-parser.el ends here
