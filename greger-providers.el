;;; greger-providers.el --- AI provider configurations -*- lexical-binding: t -*-

;; Author: Andreas Jansson <andreas@jansson.me.uk>
;; Version: 0.1.0
;; URL: https://github.com/andreasjansson/greger.el
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:
;; Manages different AI provider configurations and request building

;;; Code:

(require 'json)
(require 'greger-provider-openai)
(require 'greger-provider-claude)
(require 'greger-provider-google)

(defun greger-providers-get-config (model)
  "Get provider configuration for MODEL."
  (let* ((model-string (symbol-name model))
         (parts (split-string model-string "/"))
         (provider-name (car parts))
         (model-name (mapconcat #'identity (cdr parts) "/")))
    (cond
     ((member provider-name '("replicate" "openai" "groq" "ollama"))
      (greger-provider-openai-config provider-name model-name))
     ((string= provider-name "claude")
      (greger-provider-claude-config provider-name model-name))
     ((string= provider-name "google")
      (greger-provider-google-config provider-name model-name))
     (t
      (error "Unknown provider: %s" provider-name)))))

(defun greger-providers-build-request (provider-config dialog &optional tools)
  "Build request using PROVIDER-CONFIG for DIALOG with optional TOOLS."
  (let ((builder (plist-get provider-config :request-builder)))
    (funcall builder provider-config dialog tools)))

(defun greger-providers-extract-text (provider-config chunk)
  "Extract text from CHUNK using PROVIDER-CONFIG."
  (let ((extractor (plist-get provider-config :text-extractor)))
    (funcall extractor chunk)))

(defun greger-providers--get-api-key (provider-name)
  "Get API key for PROVIDER-NAME."
  (let ((env-var (cond
                  ((string= provider-name "replicate") "REPLICATE_API_KEY")
                  ((string= provider-name "claude") "ANTHROPIC_API_KEY")
                  ((string= provider-name "google") "GEMINI_API_KEY")
                  ((string= provider-name "openai") "OPENAI_API_KEY")
                  ((string= provider-name "groq") "GROQ_API_KEY")
                  ((string= provider-name "ollama") nil))))
    (when env-var
      (let ((value (getenv env-var)))
        (unless value
          (error "Please set the %s environment variable" env-var))
        value))))



(provide 'greger-providers)

;;; greger-providers.el ends here
