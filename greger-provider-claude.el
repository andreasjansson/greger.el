;;; greger-provider-claude.el --- Claude provider support -*- lexical-binding: t -*-

;;; Commentary:
;; Support for Anthropic's Claude models

;;; Code:

(require 'json)

(defun greger-provider-claude-config (provider-name model-name)
  "Create configuration for Claude PROVIDER-NAME with MODEL-NAME."
  (list :provider provider-name
        :model model-name
        :url "https://api.anthropic.com/v1/messages"
        :request-builder #'greger-provider-claude--build-request
        :text-extractor #'greger-provider-claude--extract-text))

(defun greger-provider-claude--build-request (config dialog &optional tools)
  "Build Claude request using CONFIG for DIALOG with optional TOOLS."
  (let* ((provider-name (plist-get config :provider))
         (model-name (plist-get config :model))
         (url (plist-get config :url))
         (api-key (greger-providers--get-api-key provider-name))
         (headers (greger-provider-claude--build-headers api-key))
         (data (greger-provider-claude--build-data model-name dialog tools)))
    (list :url url
          :method "POST"
          :headers headers
          :data data)))

(defun greger-provider-claude--build-headers (api-key)
  "Build headers for Claude with API-KEY."
  `(("Content-Type" . "application/json")
    ("x-api-key" . ,api-key)
    ("anthropic-version" . "2023-06-01")
    ("anthropic-beta" . "token-efficient-tools-2025-02-19")))

(defun greger-provider-claude--build-data (model-name dialog &optional tools)
  "Build request data for Claude MODEL-NAME with DIALOG and optional TOOLS."
  (let ((system-message nil)
        (user-messages ())
        (request-data nil))

    ;; Separate system messages from user/assistant messages
    (dolist (message dialog)
      (let ((role (alist-get 'role message))
            (content (alist-get 'content message)))
        (if (string= role "system")
            (unless system-message
              (setq system-message content))
          (push `((role . ,role)
                  (content . ,content))
                user-messages))))

    ;; Reverse to get correct order
    (setq user-messages (nreverse user-messages))

    ;; Find the last message with dict content and add ephemeral cache control
    (let ((last-dict-message nil))
      (dolist (message user-messages)
        (let ((content (alist-get 'content message)))
          (when (and (listp content) (not (stringp content)))
            (setq last-dict-message message))))

      (when last-dict-message
        (let ((content-list (alist-get 'content last-dict-message)))
          ;; Modify the first content item in place
          (when (and content-list (listp content-list))
            (let ((first-content-item (car content-list)))
              (when (and first-content-item (listp first-content-item))
                ;; Modify the car of the content-list directly
                (setcar content-list
                        (cons '(cache_control . ((type . "ephemeral")))
                              first-content-item))))))))

    ;; Build base request
    (setq request-data `(("model" . ,model-name)
                        ("messages" . ,user-messages)
                        ("max_tokens" . 64000)
                        ("stream" . t)))

    ;; Add system message if present
    (when system-message
      (push `("system" . ,system-message) request-data))

    ;; Add tools if present
    (when tools
      (push `("tools" . ,tools) request-data)
      (push `("tool_choice" . (("type" . "auto"))) request-data))

    (json-encode request-data)))

(defun greger-provider-claude--extract-text (event)
  "Extract text from Claude EVENT."
  (let ((event-parts (split-string event "\n" t)))
    (when (>= (length event-parts) 2)
      (let ((event-type (car event-parts))
            (event-data (substring (cadr event-parts) 6)))
        (if (string-prefix-p "event: content_block_delta" event-type)
            (greger-provider-claude--extract-delta-text event-data)
          "")))))

(defun greger-provider-claude--extract-delta-text (data)
  "Extract text from Claude content_block_delta DATA."
  (condition-case nil
      (let* ((block-data (json-read-from-string data))
             (delta (assoc-default 'delta block-data))
             (text (assoc-default 'text delta)))
        (or text ""))
    (error "")))

(provide 'greger-provider-claude)

;;; greger-provider-claude.el ends hereb
