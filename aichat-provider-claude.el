;;; aichat-provider-claude.el --- Claude provider support -*- lexical-binding: t -*-

;;; Commentary:
;; Support for Anthropic's Claude models

;;; Code:

(require 'json)

(defun aichat-provider-claude-config (provider-name model-name)
  "Create configuration for Claude PROVIDER-NAME with MODEL-NAME."
  (list :provider provider-name
        :model model-name
        :url "https://api.anthropic.com/v1/messages"
        :request-builder #'aichat-provider-claude--build-request
        :text-extractor #'aichat-provider-claude--extract-text))

(defun aichat-provider-claude--build-request (config dialog)
  "Build Claude request using CONFIG for DIALOG."
  (let* ((provider-name (plist-get config :provider))
         (model-name (plist-get config :model))
         (url (plist-get config :url))
         (api-key (aichat-providers--get-api-key provider-name))
         (headers (aichat-provider-claude--build-headers api-key))
         (data (aichat-provider-claude--build-data model-name dialog)))
    (list :url url
          :method "POST"
          :headers headers
          :data data)))

(defun aichat-provider-claude--build-headers (api-key)
  "Build headers for Claude with API-KEY."
  `(("Content-Type" . "application/json")
    ("x-api-key" . ,api-key)
    ("anthropic-version" . "2023-06-01")
    ("anthropic-beta" . "messages-2023-12-15")))

(defun aichat-provider-claude--build-data (model-name dialog)
  "Build request data for Claude MODEL-NAME with DIALOG."
  (let ((system-message nil)
        (user-messages ()))

    ;; Separate system messages from user/assistant messages
    (dolist (message dialog)
      (let ((role (car message))
            (content (cdr message)))
        (if (eq role 'system)
            (unless system-message
              (setq system-message content))
          (push `((role . ,(symbol-name role))
                  (content . ,content))
                user-messages))))

    (json-encode `(("model" . ,model-name)
                   ("messages" . ,(nreverse user-messages))
                   ("system" . ,system-message)
                   ("max_tokens" . 8192)
                   ("stream" . t)))))

(defun aichat-provider-claude--extract-text (event)
  "Extract text from Claude EVENT."
  (let ((event-parts (split-string event "\n" t)))
    (when (>= (length event-parts) 2)
      (let ((event-type (car event-parts))
            (event-data (substring (cadr event-parts) 6)))
        (if (string-prefix-p "event: content_block_delta" event-type)
            (aichat-provider-claude--extract-delta-text event-data)
          "")))))

(defun aichat-provider-claude--extract-delta-text (data)
  "Extract text from Claude content_block_delta DATA."
  (condition-case nil
      (let* ((block-data (json-read-from-string data))
             (delta (assoc-default 'delta block-data))
             (text (assoc-default 'text delta)))
        (or text ""))
    (error "")))

(provide 'aichat-provider-claude)

;;; aichat-provider-claude.el ends here
