;;; aichat-providers.el --- AI provider configurations -*- lexical-binding: t -*-

;;; Commentary:
;; Manages different AI provider configurations and request building

;;; Code:

(require 'json)
(require 'aichat-provider-openai)
(require 'aichat-provider-claude)
(require 'aichat-provider-google)

(defun aichat-providers-get-config (model)
  "Get provider configuration for MODEL."
  (let* ((model-string (symbol-name model))
         (parts (split-string model-string "/"))
         (provider-name (car parts))
         (model-name (mapconcat #'identity (cdr parts) "/")))
    (cond
     ((member provider-name '("replicate" "openai" "groq" "ollama"))
      (aichat-provider-openai-config provider-name model-name))
     ((string= provider-name "claude")
      (aichat-provider-claude-config provider-name model-name))
     ((string= provider-name "google")
      (aichat-provider-google-config provider-name model-name))
     (t
      (error "Unknown provider: %s" provider-name)))))

(defun aichat-providers-build-request (provider-config dialog)
  "Build request using PROVIDER-CONFIG for DIALOG."
  (let ((builder (plist-get provider-config :request-builder)))
    (funcall builder provider-config dialog)))

(defun aichat-providers-extract-text (provider-config chunk)
  "Extract text from CHUNK using PROVIDER-CONFIG."
  (let ((extractor (plist-get provider-config :text-extractor)))
    (funcall extractor chunk)))

(defun aichat-providers--get-api-key (provider-name)
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

(provide 'aichat-providers)

;;; aichat-providers.el ends here
