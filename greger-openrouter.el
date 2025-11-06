;;; greger-openrouter.el --- OpenRouter client for greger -*- lexical-binding: t -*-

;; Copyright (C) 2025 Andreas Jansson

;; Author: Andreas Jansson <andreas@jansson.me.uk>
;; Version: 0.1.0
;; URL: https://github.com/andreasjansson/greger.el
;; SPDX-License-Identifier: MIT

;;; Commentary:
;; OpenRouter API client for greger - provides access to 400+ models including
;; GPT-5, GPT-5 Codex, and others through OpenRouter's unified API.

;;; Code:

(require 'json)
(require 'cl-lib)

(defconst greger-openrouter-api-url "https://openrouter.ai/api/v1/chat/completions"
  "OpenRouter API endpoint.")

(cl-defstruct greger-openrouter-state
  accumulated-output
  current-tool-calls
  current-text
  thinking-started
  text-started
  annotations
  reasoning-started
  current-reasoning-text
  current-reasoning-details
  process
  output-buffer
  undo-handle
  block-start-callback
  text-delta-callback
  block-stop-callback
  complete-callback
  restore-callback
  error-callback
  error-message)

(cl-defun greger-openrouter-stream (&key model dialog tools buffer enable-web-search block-start-callback text-delta-callback block-stop-callback complete-callback thinking-budget max-tokens auth-key error-callback)
  "Stream request to OpenRouter API.
MODEL is the OpenRouter model identifier.
DIALOG is the conversation history.
TOOLS are tool definitions.
BUFFER is the output buffer.
ENABLE-WEB-SEARCH determines if we append :online to the model.
BLOCK-START-CALLBACK is called when content blocks begin.
TEXT-DELTA-CALLBACK for incremental text.
BLOCK-STOP-CALLBACK when blocks complete.
COMPLETE-CALLBACK when the entire response finishes.
THINKING-BUDGET is the number of thinking tokens.
MAX-TOKENS is the maximum number of tokens to generate.
AUTH-KEY is the OpenRouter API key.
ERROR-CALLBACK is called when errors occur."
  (let* ((output-buffer (or buffer (current-buffer)))
         (undo-handle (prepare-change-group output-buffer))
         (request-spec (greger-openrouter--build-request model dialog tools thinking-budget max-tokens auth-key enable-web-search))
         (restore-callback (lambda (state)
                             (let ((buffer (greger-openrouter-state-output-buffer state)))
                               (when (buffer-live-p buffer)
                                 (with-current-buffer buffer
                                   (undo-amalgamate-change-group (greger-openrouter-state-undo-handle state))
                                   (accept-change-group (greger-openrouter-state-undo-handle state)))))))
         (process (greger-openrouter--start-curl-process request-spec))
         (state (make-greger-openrouter-state
                 :accumulated-output ""
                 :current-tool-calls (make-hash-table :test 'equal)
                 :current-text ""
                 :process process
                 :block-start-callback block-start-callback
                 :text-delta-callback text-delta-callback
                 :block-stop-callback block-stop-callback
                 :complete-callback complete-callback
                 :restore-callback restore-callback
                 :output-buffer output-buffer
                 :undo-handle undo-handle
                 :error-callback error-callback)))
    
    (activate-change-group undo-handle)
    
    (set-process-filter process
                        (lambda (_proc output)
                          (greger-openrouter--process-output-chunk output state)))
    
    (set-process-sentinel process
                          (lambda (proc _event)
                            (greger-openrouter--handle-completion proc state)))
    
    (set-process-query-on-exit-flag process nil)
    
    state))

(defun greger-openrouter--build-request (model dialog tools thinking-budget max-tokens auth-key enable-web-search)
  "Build OpenRouter API request."
  (let* ((headers (greger-openrouter--build-headers auth-key))
         (data (greger-openrouter--build-data model dialog tools thinking-budget max-tokens enable-web-search)))
    (message "url: %s; request data: %s" greger-openrouter-api-url data)
    (list :url greger-openrouter-api-url
          :method "POST"
          :headers headers
          :data data)))

(defun greger-openrouter--build-headers (api-key)
  "Build OpenRouter headers - Bearer auth."
  `(("Content-Type" . "application/json")
    ("Authorization" . ,(concat "Bearer " api-key))
    ("HTTP-Referer" . "https://github.com/andreasjansson/greger.el")
    ("X-Title" . "Greger.el")))

(defun greger-openrouter--build-data (model dialog tools thinking-budget max-tokens enable-web-search)
  "Build OpenRouter request data in OpenAI format."
  (let* ((messages (greger-openrouter--convert-dialog-to-messages dialog))
         (actual-model (if enable-web-search
                           (concat model ":online")
                         model))
         (request-data `(("model" . ,actual-model)
                         ("max_tokens" . ,max-tokens)
                         ("stream" . t))))
    
    (push `("messages" . ,messages) request-data)
    
    (when tools
      (let ((converted-tools (greger-openrouter--convert-tools tools)))
        (push `("tools" . ,converted-tools) request-data)
        (push `("tool_choice" . "auto") request-data)))
    
    (when (and thinking-budget (> thinking-budget 0))
      (push `("reasoning" . (("max_tokens" . ,thinking-budget))) request-data)
      (push `("include_reasoning" . t) request-data))
    
    (json-encode request-data)))

(defun greger-openrouter--convert-dialog-to-messages (dialog)
  "Convert Greger dialog format to OpenAI message format."
  (let (messages
        system-message)
    (dolist (msg dialog)
      (let ((role (alist-get 'role msg))
            (content (alist-get 'content msg)))
        
        (cond
         ((string= role "system")
          (setq system-message content))
         
         ((stringp content)
          (push `((role . ,role) (content . ,content)) messages))
         
         ((listp content)
          (let ((converted (greger-openrouter--convert-content-blocks role content)))
            (when converted
              (setq messages (append converted messages))))))))
    
    (let ((result (nreverse messages)))
      (when system-message
        (setq result (cons `((role . "system") (content . ,system-message)) result)))
      result)))

(defun greger-openrouter--convert-content-blocks (role content-blocks)
  "Convert Anthropic content blocks to OpenAI format."
  (let (result-messages
        current-text
        tool-calls)
    
    (dolist (block content-blocks)
      (let ((type (alist-get 'type block)))
        (cond
         ((string= type "text")
          (setq current-text (alist-get 'text block)))
         
         ((string= type "thinking")
          (setq current-text (alist-get 'thinking block)))
         
         ((string= type "tool_use")
          (let ((tool-call `((id . ,(alist-get 'id block))
                             (type . "function")
                             (function . ((name . ,(alist-get 'name block))
                                          (arguments . ,(json-encode (alist-get 'input block))))))))
            (push tool-call tool-calls)))
         
         ((string= type "tool_result")
          (let ((tool-msg `((role . "tool")
                            (tool_call_id . ,(alist-get 'tool_use_id block))
                            (content . ,(alist-get 'content block)))))
            (push tool-msg result-messages))))))
    
    (when (or current-text tool-calls)
      (let ((msg `((role . ,role))))
        (when current-text
          (push `(content . ,current-text) msg))
        (when tool-calls
          (push `(tool_calls . ,(vconcat (nreverse tool-calls))) msg))
        (push msg result-messages)))
    
    (nreverse result-messages)))

(defun greger-openrouter--convert-tools (tools)
  "Convert Anthropic tool format to OpenAI function calling format.
OpenAI doesn't support 'default' values in parameters, so we strip them.
Also ensures 'required' is always a vector (array in JSON) not null."
  (mapcar
   (lambda (tool)
     (let* ((input-schema (alist-get 'input_schema tool))
            (cleaned-schema (greger-openrouter--remove-defaults-from-schema input-schema))
            (fixed-schema (greger-openrouter--fix-required-field cleaned-schema)))
       `((type . "function")
         (function . ((name . ,(alist-get 'name tool))
                      (description . ,(alist-get 'description tool))
                      (parameters . ,fixed-schema))))))
   tools))

(defun greger-openrouter--fix-required-field (schema)
  "Ensure the 'required' field is always a vector, not nil.
OpenAI expects 'required' to be an array, even if empty."
  (if (not (listp schema))
      schema
    (mapcar
     (lambda (entry)
       (if (and (consp entry) (eq (car entry) 'required))
           ;; Convert nil or empty list to empty vector
           (cons 'required (if (cdr entry)
                               (if (vectorp (cdr entry))
                                   (cdr entry)
                                 (vconcat (cdr entry)))
                             []))
         (if (consp entry)
             (cons (car entry)
                   (greger-openrouter--fix-required-field (cdr entry)))
           entry)))
     schema)))

(defun greger-openrouter--remove-defaults-from-schema (schema)
  "Remove 'default' keys from SCHEMA recursively.
OpenAI function calling doesn't support default values."
  (cond
   ((not (consp schema)) schema)
   
   ;; Check if this is an alist (list of cons cells)
   ((and (listp schema) (consp (car schema)))
    ;; This is an alist - filter out 'default' keys
    (let ((filtered (seq-filter
                     (lambda (pair)
                       (not (eq (car pair) 'default)))
                     schema)))
      ;; Recursively process values
      (mapcar (lambda (pair)
                (cons (car pair)
                      (greger-openrouter--remove-defaults-from-schema (cdr pair))))
              filtered)))
   
   ;; Regular list - process each element
   ((listp schema)
    (mapcar #'greger-openrouter--remove-defaults-from-schema schema))
   
   ;; Cons cell - process both parts
   ((consp schema)
    (cons (greger-openrouter--remove-defaults-from-schema (car schema))
          (greger-openrouter--remove-defaults-from-schema (cdr schema))))
   
   ;; Atom - return as-is
   (t schema)))

(defun greger-openrouter--process-output-chunk (output state)
  "Process streaming output chunk."
  (setf (greger-openrouter-state-accumulated-output state)
        (concat (greger-openrouter-state-accumulated-output state) output))
  
  (greger-openrouter--process-events state))

(defun greger-openrouter--process-events (state)
  "Process OpenAI-style SSE events."
  (let ((accumulated (greger-openrouter-state-accumulated-output state)))
    
    (while (string-match "\n" accumulated)
      (let* ((line-end (match-end 0))
             (line (substring accumulated 0 (1- line-end))))
        
        (when (string-prefix-p "data: " line)
          (let ((data-json (substring line 6)))
            (unless (string= data-json "[DONE]")
              (greger-openrouter--handle-event data-json state))))
        
        (setq accumulated (substring accumulated line-end))))
    
    (setf (greger-openrouter-state-accumulated-output state) accumulated)))

(defun greger-openrouter--handle-event (data-json state)
  "Handle OpenAI-style streaming event."

  (message "data-json: %s" data-json)

  (condition-case err
      (let* ((data (json-read-from-string data-json))
             (error-data (alist-get 'error data)))
        
        ;; Check for API errors first
        (when error-data
          (let ((error-message (format "OpenRouter API error: %s" 
                                       (or (alist-get 'message error-data)
                                           (json-encode error-data)))))
            (setf (greger-openrouter-state-error-message state) error-message)
            (message "OpenRouter error: %s" error-message)
            (error error-message)))
        
        (let* ((choices (alist-get 'choices data))
               (choice (when choices (aref choices 0)))
               (delta (alist-get 'delta choice))
               (message (alist-get 'message choice))
               (finish-reason (alist-get 'finish_reason choice)))
          
          (when delta
            (cond
             ((alist-get 'content delta)
              (let ((text (alist-get 'content delta))
                    (block-start-callback (greger-openrouter-state-block-start-callback state))
                    (text-delta-callback (greger-openrouter-state-text-delta-callback state)))
                
                (unless (greger-openrouter-state-text-started state)
                  (setf (greger-openrouter-state-text-started state) t)
                  (when block-start-callback
                    (funcall block-start-callback
                             `((type . "text")
                               (text . "")))))
                
                (setf (greger-openrouter-state-current-text state)
                      (concat (greger-openrouter-state-current-text state) text))
                (when text-delta-callback
                  (funcall text-delta-callback text))))
             
             ((alist-get 'reasoning delta)
              (greger-openrouter--handle-reasoning-delta delta state))
             
             ((alist-get 'tool_calls delta)
              (greger-openrouter--accumulate-tool-calls delta state))))
          
          (when (and message (alist-get 'annotations message))
            (setf (greger-openrouter-state-annotations state)
                  (alist-get 'annotations message)))
          
          (when finish-reason
            (greger-openrouter--handle-finish state finish-reason))))
    (error
     (let ((error-message (format "Failed to parse event: %s" (error-message-string err))))
       (setf (greger-openrouter-state-error-message state) error-message)
       (message "OpenRouter parse error: %s" error-message)))))

(defun greger-openrouter--handle-reasoning-delta (delta state)
  "Handle reasoning/thinking delta and convert to Greger thinking format."
  (let ((reasoning-text (alist-get 'reasoning delta))
        (block-start-callback (greger-openrouter-state-block-start-callback state))
        (text-delta-callback (greger-openrouter-state-text-delta-callback state)))
    
    (unless (greger-openrouter-state-thinking-started state)
      (setf (greger-openrouter-state-thinking-started state) t)
      (when block-start-callback
        (funcall block-start-callback
                 `((type . "thinking")
                   (thinking . "")
                   (signature . "")))))
    
    (when (and reasoning-text text-delta-callback)
      (funcall text-delta-callback reasoning-text))))

(defun greger-openrouter--accumulate-tool-calls (delta state)
  "Accumulate tool call deltas."
  (let ((tool-calls (alist-get 'tool_calls delta))
        (accumulated (greger-openrouter-state-current-tool-calls state)))
    
    (when tool-calls
      (seq-doseq (call tool-calls)
        (let* ((index (alist-get 'index call))
               (id (alist-get 'id call))
               (function-delta (alist-get 'function call))
               (name (alist-get 'name function-delta))
               (arguments (alist-get 'arguments function-delta))
               (existing (gethash index accumulated)))
          
          (if existing
              (let ((existing-args (alist-get 'arguments existing)))
                (setf (alist-get 'arguments existing)
                      (concat existing-args arguments)))
            (puthash index
                     `((id . ,id)
                       (name . ,name)
                       (arguments . ,arguments))
                     accumulated)))))))

(defun greger-openrouter--handle-finish (state finish-reason)
  "Handle completion based on finish reason."
  (when-let ((callback (greger-openrouter-state-complete-callback state)))
    (funcall callback (greger-openrouter--build-content-blocks state))))

(defun greger-openrouter--build-content-blocks (state)
  "Build Anthropic-style content blocks from accumulated state."
  (let ((tool-calls (greger-openrouter-state-current-tool-calls state))
        (annotations (greger-openrouter-state-annotations state))
        blocks)
    
    (when (> (hash-table-count tool-calls) 0)
      (maphash
       (lambda (_index call)
         (let* ((id (alist-get 'id call))
                (name (alist-get 'name call))
                (arguments (alist-get 'arguments call))
                (parsed-args (condition-case nil
                                 (json-read-from-string arguments)
                               (error '()))))
           (push `((type . "tool_use")
                   (id . ,id)
                   (name . ,name)
                   (input . ,parsed-args))
                 blocks)))
       tool-calls))
    
    (when annotations
      (let ((citations (seq-filter
                        (lambda (annotation)
                          (string= (alist-get 'type annotation) "url_citation"))
                        annotations)))
        (when citations
          (push `((type . "text")
                  (text . "")
                  (citations . ,(mapcar #'greger-openrouter--convert-annotation-to-citation citations)))
                blocks))))
    
    (nreverse blocks)))

(defun greger-openrouter--convert-annotation-to-citation (annotation)
  "Convert OpenRouter annotation to Greger citation format."
  `((type . "web_search_result_location")
    (url . ,(alist-get 'url annotation))
    (title . ,(or (alist-get 'title annotation) ""))
    (cited_text . ,(or (alist-get 'text annotation) ""))
    (encrypted_index . "")))

(defun greger-openrouter--handle-completion (proc state)
  "Handle process completion."
  (when (memq (process-status proc) '(exit signal))
    (funcall (greger-openrouter-state-restore-callback state) state)
    
    (let ((exit-code (process-exit-status proc))
          (stored-error (greger-openrouter-state-error-message state)))
      (cond
       ((and (= exit-code 0) (not stored-error))
        (when-let ((callback (greger-openrouter-state-complete-callback state)))
          (funcall callback (greger-openrouter--build-content-blocks state))))
       
       ((= exit-code 2)
        (message "Process interrupted"))
       
       (t
        (let ((error-message (or stored-error
                                 (format "Process exited with status code %d" exit-code))))
          (when-let ((callback (greger-openrouter-state-error-callback state)))
            (funcall callback error-message))))))))

(defun greger-openrouter--start-curl-process (request-spec)
  "Start curl process for OpenRouter."
  (start-process-shell-command
   "greger-openrouter-curl" nil
   (greger-openrouter--build-curl-command request-spec)))

(defun greger-openrouter--build-curl-command (request-spec)
  "Build curl command for OpenRouter."
  (let ((url (plist-get request-spec :url))
        (method (plist-get request-spec :method))
        (headers (plist-get request-spec :headers))
        (data (plist-get request-spec :data)))
    (format "curl -s -X %s %s %s %s"
            method
            (mapconcat (lambda (header)
                         (format "-H \"%s: %s\"" (car header) (cdr header)))
                       headers " ")
            (if data
                (format "--data-raw %s" (shell-quote-argument data))
              "")
            url)))

(defun greger-openrouter--cancel-request (state)
  "Cancel streaming request."
  (let ((process (greger-openrouter-state-process state)))
    (when (process-live-p process)
      (message "Interrupting generation")
      (interrupt-process process)
      (sit-for 0.1)
      (delete-process process))
    (funcall (greger-openrouter-state-restore-callback state) state)))

(provide 'greger-openrouter)

;;; greger-openrouter.el ends here
