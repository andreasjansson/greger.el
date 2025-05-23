;;; aichat.el --- Chat with language models -*- lexical-binding: t -*-

;; Copyright (C) 2023 Andreas Jansson

;; Author: Andreas Jansson <andreas@jansson.me.uk>
;; Version: 0.1.0
;; URL: https://github.com/andreasjansson/aichat.el
;; Package-Requires: ((emacs "28.0") (parsec "0.1.3"))

;;; Commentary:
;; This package provides an interface for interacting with AI language models

;;; Code:

(require 'aichat-stream)
(require 'aichat-parser)

(defconst aichat-available-models
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

(defcustom aichat-model 'claude/claude-3-haiku-20240307
  "The currently used model."
  :type `(choice ,@(mapcar (lambda (model) `(const ,model)) aichat-available-models))
  :group 'aichat)

(defcustom aichat-default-system-prompt "You are a helpful assistant."
  "Default system prompt used for AI interactions."
  :type 'string
  :group 'aichat)

(defcustom aichat-temperature 0.8
  "Sampling temperature between 0 and 1."
  :type 'float
  :group 'aichat)

(defvar aichat-user-tag "## USER:")
(defvar aichat-assistant-tag "## ASSISTANT:")
(defvar aichat-system-tag "## SYSTEM:")

(defvar aichat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-<return>") #'aichat-buffer)
    (define-key map (kbd "C-; a") #'aichat-insert-assistant-tag)
    (define-key map (kbd "C-; u") #'aichat-insert-user-tag)
    (define-key map (kbd "C-; s") #'aichat-insert-system-tag)
    (define-key map (kbd "C-; f") #'aichat-context-file)
    (define-key map (kbd "C-; b") #'aichat-context-buffer)
    (define-key map (kbd "C-; h") #'aichat-context-http-url)
    (define-key map (kbd "C-; m") #'aichat-set-model)
    (define-key map (kbd "C-; c") #'aichat-copy-code)
    map)
  "Keymap for `aichat-mode'.")

(define-derived-mode aichat-mode gfm-mode "AI"
  "Major mode for interacting with AI."
  (use-local-map aichat-mode-map)
  (setq-local markdown-fontify-code-blocks-natively t)
  (setq-local mode-line-misc-info '(:eval (symbol-name aichat-model))))

;;;###autoload
(defun aichat ()
  "Create a new buffer and switch to `aichat-mode`."
  (interactive)
  (let ((buffer (generate-new-buffer "*aichat*")))
    (switch-to-buffer buffer)
    (aichat-mode)
    (insert aichat-system-tag
            "\n\n" aichat-default-system-prompt "\n\n"
            aichat-user-tag
            "\n\n")
    (message (format "Using model %s" aichat-model))))

(defun aichat-insert-assistant-tag ()
  "Insert the assistant tag into the buffer."
  (interactive)
  (insert aichat-assistant-tag "\n\n"))

(defun aichat-insert-user-tag ()
  "Insert the user tag into the buffer."
  (interactive)
  (insert aichat-user-tag "\n\n"))

(defun aichat-insert-system-tag ()
  "Insert the system tag into the buffer."
  (interactive)
  (insert aichat-system-tag "\n\n"))

(defun aichat-buffer ()
  "Send the buffer content to AI as a dialog."
  (interactive)
  (let* ((buffer-content (buffer-substring-no-properties (point-min) (point-max)))
         (dialog (aichat-parser-parse-dialog buffer-content)))
    (unless dialog
      (error "Failed to parse dialog.  Did you forget to close a html tag?"))
    (goto-char (point-max))
    (aichat--maybe-insert-assistant-tag)
    (let ((complete-callback (lambda ()
                               (insert "\n\n" aichat-user-tag "\n\n"))))
      (aichat-stream-dialog dialog complete-callback))))

(defun aichat-context-file ()
  "Prompt the user to select a file and insert an <ai-context> at point."
  (interactive)
  (let ((file (expand-file-name (read-file-name "Select file: " nil nil t))))
    (if (file-exists-p file)
        (insert (format "<ai-context>%s</ai-context>\n\n" file))
      (message "File does not exist!"))))

(defun aichat-context-buffer ()
  "Prompt the user to select a buffer and insert an <ai-context> at point."
  (interactive)
  (let ((buffer-name (read-buffer "Select buffer: " nil t)))
    (when buffer-name
      (let ((buffer (get-buffer buffer-name)))
        (when buffer
          (let ((path (buffer-file-name buffer)))
            (insert (format "<ai-context>%s</ai-context>\n\n" path))))))))

(defun aichat-context-http-url (url)
  "Insert an <ai-context> to the provided URL at point."
  (interactive "sURL: ")
  (insert (format "<ai-context>%s</ai-context>\n\n" url)))

(defun aichat-copy-code ()
  "Copy the current code block under point."
  (interactive)
  (let ((code-block (aichat--get-current-code-block)))
    (if code-block
        (progn
          (kill-new code-block)
          (message (format "Copied code: %s" (aichat--truncate-with-ellipsis code-block 40))))
      (error "Point is not inside a code block"))))

(defun aichat-set-model ()
  "Set the current AI model."
  (interactive)
  (let ((model (completing-read "Choose AI model: " aichat-available-models nil t)))
    (customize-set-variable 'aichat-model (intern model))
    (message "Model set to %s" model)))

(defun aichat-paragraph ()
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
    (aichat-stream aichat-default-system-prompt para-text)))

(defun aichat-prompt (prompt)
  "Ask AI a PROMPT and insert the response at the current point."
  (interactive "sPrompt: ")
  (aichat-stream aichat-default-system-prompt prompt))

(defun aichat-stream (system-prompt prompt &optional complete-callback cancel-callback)
  "Stream the AI response for SYSTEM-PROMPT and user PROMPT.
COMPLETE-CALLBACK is called when done.
CANCEL-CALLBACK is called if cancelled."
  (let ((dialog `((system . ,system-prompt)
                  (user . ,prompt))))
    (aichat-stream-dialog dialog complete-callback cancel-callback)))

(defun aichat-stream-dialog (dialog &optional complete-callback cancel-callback)
  "Stream the AI response for the multi-turn DIALOG.
COMPLETE-CALLBACK is called when done.
CANCEL-CALLBACK is called if cancelled."
  (aichat-stream-to-buffer aichat-model dialog complete-callback cancel-callback))

;; Private helper functions

(defun aichat--maybe-insert-assistant-tag ()
  "Insert the assistant tag if it's not already present."
  (let ((found-tag nil))
    (save-excursion
      (while (and (not found-tag)
                  (re-search-backward (concat aichat-user-tag "\\|" aichat-assistant-tag "\\|" aichat-system-tag) nil t))
        (setq found-tag (match-string 0))))
    (unless (string= found-tag aichat-assistant-tag)
      (insert "\n\n" aichat-assistant-tag "\n\n"))))

(defun aichat--get-current-code-block ()
  "Return the current code block under point, or nil if not found."
  (save-excursion
    (when (re-search-backward "^```" nil t)
      (forward-line)
      (let ((start (point)))
        (when (re-search-forward "^```" nil t)
          (backward-char 4)
          (buffer-substring-no-properties start (point)))))))

(defun aichat--truncate-with-ellipsis (str max-width)
  "Truncate STR to MAX-WIDTH characters, adding an ellipsis if necessary."
  (let ((len (length str)))
    (if (<= len max-width)
        str
      (concat (substring str 0 (- max-width 3)) "..."))))

(provide 'aichat)

;;; aichat.el ends here
