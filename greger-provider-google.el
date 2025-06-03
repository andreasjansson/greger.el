;;; greger-provider-google.el --- Google provider support -*- lexical-binding: t -*-

;; Author: Andreas Jansson <andreas@jansson.me.uk>
;; Version: 0.1.0
;; URL: https://github.com/andreasjansson/greger.el

;;; Commentary:
;; Support for Google's Gemini models

;;; Code:

(require 'json)

(defun greger-provider-google-config (provider-name model-name)
  "Create configuration for Google PROVIDER-NAME with MODEL-NAME."
  (list :provider provider-name
        :model model-name
        :url "https://generativelanguage.googleapis.com/v1beta/models/gemini-pro:streamGenerateContent"
        :request-builder #'greger-provider-google--build-request
        :text-extractor #'greger-provider-google--extract-text))

(defun greger-provider-google--build-request (config dialog)
  "Build Google request using CONFIG for DIALOG."
  (let* ((provider-name (plist-get config :provider))
         (model-name (plist-get config :model))
         (url (plist-get config :url))
         (api-key (greger-providers--get-api-key provider-name))
         (headers (greger-provider-google--build-headers api-key))
         (data (greger-provider-google--build-data model-name dialog)))
    (list :url url
          :method "POST"
          :headers headers
          :data data)))

(defun greger-provider-google--build-headers (api-key)
  "Build headers for Google with API-KEY."
  `(("Content-Type" . "application/json")
    ("Authorization" . ,(format "Bearer %s" api-key))))

(defun greger-provider-google--build-data (model-name dialog)
  "Build request data for Google MODEL-NAME with DIALOG."
  (let ((parts (mapcar (lambda (message)
                         `((role . ,(greger-provider-google--transform-role (car message)))
                           (text . ,(cdr message))))
                       dialog)))
    (json-encode `(("contents" . (("parts" . ,parts)))))))

(defun greger-provider-google--transform-role (role)
  "Transform ROLE for Google API."
  (cond
   ((eq role 'system) "user")  ; Google handles system prompts differently
   ((eq role 'user) "user")
   ((eq role 'assistant) "model")))

(defun greger-provider-google--extract-text (event)
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

(provide 'greger-provider-google)

;;; greger-provider-google.el ends here
