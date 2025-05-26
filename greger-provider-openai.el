;;; greger-provider-openai.el --- OpenAI-compatible provider support -*- lexical-binding: t -*-

;;; Commentary:
;; Support for OpenAI and OpenAI-compatible providers (Replicate, Groq, Ollama)

;;; Code:

(require 'json)

(defun greger-provider-openai-config (provider-name model-name)
  "Create configuration for OpenAI-compatible PROVIDER-NAME with MODEL-NAME."
  (list :provider provider-name
        :model model-name
        :url (greger-provider-openai--get-url provider-name)
        :request-builder #'greger-provider-openai--build-request
        :text-extractor (if (greger-provider-openai--is-o1-model-p model-name)
                            #'greger-provider-openai--extract-o1-text
                          #'greger-provider-openai--extract-streaming-text)))

(defun greger-provider-openai--get-url (provider-name)
  "Get API URL for PROVIDER-NAME."
  (cond
   ((string= provider-name "replicate")
    "https://openai-proxy.replicate.com/v1/chat/completions")
   ((string= provider-name "openai")
    "https://api.openai.com/v1/chat/completions")
   ((string= provider-name "groq")
    "https://api.groq.com/openai/v1/chat/completions")
   ((string= provider-name "ollama")
    "http://localhost:11434/v1/chat/completions")))

(defun greger-provider-openai--build-request (config dialog)
  "Build OpenAI request using CONFIG for DIALOG."
  (let* ((provider-name (plist-get config :provider))
         (model-name (plist-get config :model))
         (url (plist-get config :url))
         (api-key (greger-providers--get-api-key provider-name))
         (headers (greger-provider-openai--build-headers provider-name api-key))
         (data (greger-provider-openai--build-data model-name dialog)))
    (list :url url
          :method "POST"
          :headers headers
          :data data)))

(defun greger-provider-openai--build-headers (provider-name api-key)
  "Build headers for PROVIDER-NAME with API-KEY."
  (let ((headers '(("Content-Type" . "application/json"))))
    (when api-key
      (push `("Authorization" . ,(format "Bearer %s" api-key)) headers))
    headers))

(defun greger-provider-openai--build-data (model-name dialog)
  "Build request data for MODEL-NAME with DIALOG."
  (let* ((is-o1-model (greger-provider-openai--is-o1-model-p model-name))
         (filtered-dialog (if is-o1-model
                              (seq-filter (lambda (message)
                                            (not (eq (car message) 'system)))
                                          dialog)
                            dialog))
         (messages (mapcar (lambda (message)
                             `((role . ,(symbol-name (car message)))
                               (content . ,(cdr message))))
                           filtered-dialog))
         (token-param (if is-o1-model 'max_completion_tokens 'max_tokens))
         (base-request `(("messages" . ,messages)
                         ("model" . ,model-name)
                         (,token-param . 8192))))

    ;; Add continuation message if last message is from assistant
    (let ((last-message (car (last filtered-dialog))))
      (when (eq (car last-message) 'assistant)
        (setq messages (append messages
                               '(((role . "user")
                                  (content . "Seamlessly continue generating from the point it cut off.")))))))

    (if is-o1-model
        (json-encode base-request)
      (json-encode (append base-request '(("stream" . t)))))))

(defun greger-provider-openai--is-o1-model-p (model-name)
  "Check if MODEL-NAME is an o1 model."
  (member model-name '("o1-preview" "o1-mini")))

(defun greger-provider-openai--extract-streaming-text (event)
  "Extract text from streaming EVENT."
  (if (string-prefix-p "data: " event)
      (let ((data-string (substring event 6)))
        (if (string-prefix-p "[DONE]" data-string)
            ""
          (condition-case nil
              (let* ((data (json-read-from-string data-string))
                     (choices (assoc-default 'choices data))
                     (first-choice (elt choices 0))
                     (delta (assoc-default 'delta first-choice))
                     (content (assoc-default 'content delta)))
                (or content ""))
            (error ""))))
    ""))

(defun greger-provider-openai--extract-o1-text (event)
  "Extract text from o1 model EVENT."
  (condition-case nil
      (let* ((data (json-read-from-string event))
             (choices (assoc-default 'choices data))
             (first-choice (elt choices 0))
             (message (assoc-default 'message first-choice))
             (content (assoc-default 'content message)))
        (or content ""))
    (error "")))

(provide 'greger-provider-openai)

;;; greger-provider-openai.el ends here
