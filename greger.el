;;; greger.el --- Chat with language models -*- lexical-binding: t -*-

;; Copyright (C) 2023 Andreas Jansson

;; Author: Andreas Jansson <andreas@jansson.me.uk>
;; Version: 0.1.0
;; URL: https://github.com/andreasjansson/greger.el
;; Package-Requires: ((emacs "28.0") (parsec "0.1.3"))

;;; Commentary:
;; This package provides an interface for interacting with AI language models

;;; Code:

(require 'greger-stream)
(require 'greger-parser)
(require 'cl-lib)

(defconst greger-available-models
  '(replicate/meta/meta-llama-3.1-405b-instruct
    replicate/meta/meta-llama-3-70b-instruct
    replicate/meta/meta-llama-3-70b
    replicate/meta/meta-llama-3-8b-instruct
    replicate/meta/meta-llama-3-8b
    replicate/snowflake/snowflake-arctic-instruct
    claude/claude-3-haiku-20240307
    claude/claude-3-opus-20240229
    claude/claude-3-5-sonnet-20240620
    claude/claude-3-7-sonnet-latest
    claude/claude-sonnet-4-20250514
    claude/claude-opus-4-20250514
    openai/o1-preview
    openai/gpt-4-turbo
    openai/gpt-4o
    openai/gpt-4o-2024-08-06
    openai/gpt-4o-mini
    google/gemini-pro
    groq/llama3-8b-8192
    groq/llama3-70b-8192
    groq/mixtral-8x7b-32768
    ollama/llama3:8b
    ollama/llama3:text
    ollama/mistral:7b
    ollama/phi3:3.8b
    ollama/wizardlm2:7b
    ollama/gemma:2b)
  "List of available models.")

(defcustom greger-model 'claude/claude-sonnet-4-20250514
  "The currently used model."
  :type `(choice ,@(mapcar (lambda (model) `(const ,model)) greger-available-models))
  :group 'greger)

(defcustom greger-default-system-prompt "You are a helpful assistant."
  "Default system prompt used for AI interactions."
  :type 'string
  :group 'greger)

(defcustom greger-temperature 0.8
  "Sampling temperature between 0 and 1."
  :type 'float
  :group 'greger)

(defvar greger-user-tag "## USER:")
(defvar greger-assistant-tag "## ASSISTANT:")
(defvar greger-system-tag "## SYSTEM:")

;; Tool section collapsing variables
(defvar greger-tool-section-max-lines 4
  "Maximum number of lines to show in collapsed tool sections.")

(defvar greger-tool-overlays nil
  "List of overlays used for collapsible tool sections.")

;; Face definitions for tool tags
(defface greger-tool-tag-face
  '((t :height 0.7 :foreground "gray50"))
  "Face for tool opening and closing tags."
  :group 'greger)

(defface greger-tool-content-face
  '((t :inherit default))
  "Face for tool content."
  :group 'greger)

(defvar greger-mode-map
  (let ((map (make-sparse-keymap)))
                                        ;(define-key map (kbd "M-<return>") #'greger-buffer)
    (define-key map (kbd "M-<return>") #'greger-agent-buffer)
                                        ;(define-key map (kbd "M-<return>") #'greger-buffer)
    (define-key map (kbd "C-; a") #'greger-insert-assistant-tag)
    (define-key map (kbd "C-; u") #'greger-insert-user-tag)
    (define-key map (kbd "C-; s") #'greger-insert-system-tag)
    (define-key map (kbd "C-; f") #'greger-context-file)
    (define-key map (kbd "C-; b") #'greger-context-buffer)
    (define-key map (kbd "C-; h") #'greger-context-http-url)
    (define-key map (kbd "C-; m") #'greger-set-model)
    (define-key map (kbd "C-; c") #'greger-copy-code)
    map)
  "Keymap for `greger-mode'.")

(define-derived-mode greger-mode gfm-mode "AI"
  "Major mode for interacting with AI."
  (use-local-map greger-mode-map)
  (setq-local markdown-fontify-code-blocks-natively t)
  (setq-local mode-line-misc-info '(:eval (symbol-name greger-model))))

;;;###autoload
(defun greger ()
  "Create a new buffer and switch to `greger-mode`."
  (interactive)
  (let ((buffer (generate-new-buffer "*greger*")))
    (switch-to-buffer buffer)
    (greger-mode)
    (insert greger-system-tag
            "\n\n" greger-default-system-prompt "\n\n"
            greger-user-tag
            "\n\n")
    (message (format "Using model %s" greger-model))))

(defun greger-insert-assistant-tag ()
  "Insert the assistant tag into the buffer."
  (interactive)
  (insert greger-assistant-tag "\n\n"))

(defun greger-insert-user-tag ()
  "Insert the user tag into the buffer."
  (interactive)
  (insert greger-user-tag "\n\n"))

(defun greger-insert-system-tag ()
  "Insert the system tag into the buffer."
  (interactive)
  (insert greger-system-tag "\n\n"))

(defun greger-buffer ()
  "Send the buffer content to AI as a dialog."
  (interactive)
  (let* ((buffer-content (buffer-substring-no-properties (point-min) (point-max)))
         (dialog (greger-parser-parse-dialog buffer-content)))
    (unless dialog
      (error "Failed to parse dialog.  Did you forget to close a html tag?"))
    (goto-char (point-max))
    (greger--maybe-insert-assistant-tag)
    (let ((complete-callback (lambda ()
                               (insert "\n\n" greger-user-tag "\n\n"))))
      (greger-stream-dialog dialog complete-callback))))

(defun greger-context-file ()
  "Prompt the user to select a file and insert an <ai-context> at point."
  (interactive)
  (let ((file (expand-file-name (read-file-name "Select file: " nil nil t))))
    (if (file-exists-p file)
        (insert (format "<ai-context>%s</ai-context>\n\n" file))
      (message "File does not exist!"))))

(defun greger-context-buffer ()
  "Prompt the user to select a buffer and insert an <ai-context> at point."
  (interactive)
  (let ((buffer-name (read-buffer "Select buffer: " nil t)))
    (when buffer-name
      (let ((buffer (get-buffer buffer-name)))
        (when buffer
          (let ((path (buffer-file-name buffer)))
            (insert (format "<ai-context>%s</ai-context>\n\n" path))))))))

(defun greger-context-http-url (url)
  "Insert an <ai-context> to the provided URL at point."
  (interactive "sURL: ")
  (insert (format "<ai-context>%s</ai-context>\n\n" url)))

(defun greger-copy-code ()
  "Copy the current code block under point."
  (interactive)
  (let ((code-block (greger--get-current-code-block)))
    (if code-block
        (progn
          (kill-new code-block)
          (message (format "Copied code: %s" (greger--truncate-with-ellipsis code-block 40))))
      (error "Point is not inside a code block"))))

(defun greger-set-model ()
  "Set the current AI model."
  (interactive)
  (let ((model (completing-read "Choose AI model: " greger-available-models nil t)))
    (customize-set-variable 'greger-model (intern model))
    (message "Model set to %s" model)))

(defun greger-paragraph ()
  "Generate a response for the current paragraph."
  (interactive)
  (let (para-start para-end para-text)
    (backward-paragraph)
    (setq para-start (point))
    (forward-paragraph)
    (setq para-end (point))
    (setq para-text (string-trim (buffer-substring-no-properties para-start para-end)))
    (goto-char para-end)
    (insert "\n\n")
    (greger-stream greger-default-system-prompt para-text)))

(defun greger-prompt (prompt)
  "Ask AI a PROMPT and insert the response at the current point."
  (interactive "sPrompt: ")
  (greger-stream greger-default-system-prompt prompt))

(defun greger-stream (system-prompt prompt &optional complete-callback cancel-callback)
  "Stream the AI response for SYSTEM-PROMPT and user PROMPT.
COMPLETE-CALLBACK is called when done.
CANCEL-CALLBACK is called if cancelled."
  (let ((dialog `(((role . "system") (content . ,system-prompt))
                  ((role . "user") (content . ,prompt)))))
    (greger-stream-dialog dialog complete-callback cancel-callback)))

(defun greger-stream-dialog (dialog &optional complete-callback cancel-callback)
  "Stream the AI response for the multi-turn DIALOG.
COMPLETE-CALLBACK is called when done.
CANCEL-CALLBACK is called if cancelled."
  (greger-stream-to-buffer greger-model dialog complete-callback cancel-callback))

;; Private helper functions

(defun greger--maybe-insert-assistant-tag ()
  "Insert the assistant tag if it's not already present."
  (let ((found-tag nil))
    (save-excursion
      (re-search-backward (concat greger-parser-user-tag "\\|" greger-parser-assistant-tag "\\|" greger-parser-system-tag "\\|" greger-parser-tool-use-tag "\\|" greger-parser-tool-result-tag "\\|" greger-parser-thinking-tag) nil t)
      (setq found-tag (match-string 0)))
    (unless (string= found-tag greger-parser-assistant-tag)
      (insert "\n\n" greger-assistant-tag "\n\n"))))

(defun greger--get-current-code-block ()
  "Return the current code block under point, or nil if not found."
  (save-excursion
    (when (re-search-backward "^```" nil t)
      (forward-line)
      (let ((start (point)))
        (when (re-search-forward "^```" nil t)
          (backward-char 4)
          (buffer-substring-no-properties start (point)))))))

(defun greger--truncate-with-ellipsis (str max-width)
  "Truncate STR to MAX-WIDTH characters, adding an ellipsis if necessary."
  (let ((len (length str)))
    (if (<= len max-width)
        str
      (concat (substring str 0 (- max-width 3)) "..."))))

(provide 'greger)

;;; greger.el ends here
