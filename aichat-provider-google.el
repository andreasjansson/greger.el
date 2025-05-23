;;; aichat-provider-google.el --- Google provider support -*- lexical-binding: t -*-

;;; Commentary:
;; Support for Google's Gemini models

;;; Code:

(require 'json)

(defun aichat-provider-google-config (provider-name model-name)
  "Create configuration for Google PROVIDER-NAME with MODEL-NAME."
  (list :provider provider-name
        :model model-name
        :url "https://generativelanguage.googleapis.com/v1beta/models/gemini-pro:streamGenerateContent"
        :request-builder #'aichat-provider-google--build-request
        :text-extractor #'aichat-provider-google--extract-text))

(defun aichat-provider-google--build-request (config dialog)
  "Build Google request using CONFIG for DIALOG."
  (let* ((provider-name (plist-get config :provider))
         (model-name (plist-get config :model))
         (url (plist-get config :url))
         (api-key (aichat-providers--get-api-key provider-name))
         (headers (aichat-provider-google--build-headers api-key))
         (data (aichat-provider-google--build-data model-name dialog)))
    (list :url url
          :method "POST"
          :headers headers
          :data data)))

(defun aichat-provider-google--build-headers (api-key)
  "Build headers for Google with API-KEY."
  `(("Content-Type" . "application/json")
    ("Authorization" . ,(format "Bearer %s" api-key))))

(defun aichat-provider-google--build-data (model-name dialog)
  "Build request data for Google MODEL-NAME with DIALOG."
  (let ((parts (mapcar (lambda (message)
                         `((role . ,(aichat-provider-google--transform-role (car message)))
                           (text . ,(cdr message))))
                       dialog)))
    (json-encode `(("contents" . (("parts" . ,parts)))))))

(defun aichat-provider-google--transform-role (role)
  "Transform ROLE for Google API."
  (cond
   ((eq role 'system) "user")  ; Google handles system prompts differently
   ((eq role 'user) "user")
   ((eq role 'assistant) "model")))

(defun aichat-provider-google--extract-text (event)
  "Extract text from Google EVENT."
  (if (string-prefix-p "data: " event)
      (let ((data-string (substring event 6)))
        (if (string-prefix-p "[DONE]" data-string)
            ""
          (condition-case nil
              (let* ((data (json-read-from-string data-string))
                     (candidates (assoc-default 'candidates data))
                     (first-candidate (elt candidates 0))
                     (content (assoc-default 'content first-candidate))
                     (parts (assoc-default 'parts content))
                     (first-part (elt parts 0))
                     (text (assoc-default 'text first-part)))
                (or text ""))
            (error ""))))
    ""))

(provide 'aichat-provider-google)

;;; aichat-provider-google.el ends here
