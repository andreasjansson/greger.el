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

(defconst greger-openrouter-api-url "https://openrouter.ai/api/v1/responses"
  "OpenRouter Responses API endpoint.")

(cl-defstruct greger-openrouter-state
  accumulated-output
  current-text
  current-reasoning-text
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

(cl-defun greger-openrouter-stream (&key model dialog tools server-tools buffer block-start-callback text-delta-callback block-stop-callback complete-callback thinking-budget max-tokens auth-key error-callback)
  "Stream request to OpenRouter API.
MODEL is the OpenRouter model identifier.
DIALOG is the conversation history.
TOOLS are tool definitions.
SERVER-TOOLS are server tool names like web_search.
BUFFER is the output buffer.
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
         (request-spec (greger-openrouter--build-request model dialog tools server-tools thinking-budget max-tokens auth-key))
         (restore-callback (lambda (state)
                             (let ((buffer (greger-openrouter-state-output-buffer state)))
                               (when (buffer-live-p buffer)
                                 (with-current-buffer buffer
                                   (undo-amalgamate-change-group (greger-openrouter-state-undo-handle state))
                                   (accept-change-group (greger-openrouter-state-undo-handle state)))))))
         (process (greger-openrouter--start-curl-process request-spec))
         (state (make-greger-openrouter-state
                 :accumulated-output ""
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

(defun greger-openrouter--build-request (model dialog tools server-tools thinking-budget max-tokens auth-key)
  "Build OpenRouter Responses API request."
  (let* ((headers (greger-openrouter--build-headers auth-key))
         (data (greger-openrouter--build-data model dialog tools server-tools thinking-budget max-tokens)))
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

(defun greger-openrouter--build-data (model dialog tools server-tools thinking-budget max-tokens)
  "Build OpenRouter request data in Responses API format.
SERVER-TOOLS is a list of server tool names like (\"web_search\") that will be
converted to OpenAI's Responses API format."
  (let* ((messages (greger-openrouter--convert-dialog-to-messages dialog))
         (request-data `(("model" . ,model)
                         ("max_output_tokens" . ,max-tokens)
                         ("stream" . t))))
    
    (push `("input" . ,messages) request-data)
    
    (when (or tools server-tools)
      (let ((all-tools (greger-openrouter--convert-all-tools tools server-tools)))
        (push `("tools" . ,all-tools) request-data)))
    
    (when (and thinking-budget (> thinking-budget 0))
      (push `("reasoning" . (("effort" . "medium"))) request-data))
    
    (json-encode request-data)))

(defun greger-openrouter--convert-all-tools (tools server-tools)
  "Convert both user TOOLS and SERVER-TOOLS to Responses API format.
USER tools are converted using the standard OpenAI function format.
SERVER-TOOLS like web_search are converted to OpenAI's server tool format."
  (let ((converted-tools '()))
    
    (when tools
      (setq converted-tools (greger-openrouter--convert-tools tools)))
    
    (when (member "web_search" server-tools)
      (push `((type . "web_search")
              (search_context_size . "medium"))
            converted-tools))
    
    (vconcat (nreverse converted-tools))))

(defun greger-openrouter--convert-dialog-to-messages (dialog)
  "Convert Greger dialog format to Responses API input format."
  (let (messages
        system-content)
    (dolist (msg dialog)
      (let ((role (alist-get 'role msg))
            (content (alist-get 'content msg)))
        
        (cond
         ((string= role "system")
          (setq system-content content))
         
         ((stringp content)
          (push `((type . "message")
                  (role . ,role)
                  (content . [((type . "input_text")
                               (text . ,content))]))
                messages))
         
         ((listp content)
          (let ((converted (greger-openrouter--convert-content-blocks role content)))
            (when converted
              (setq messages (append converted messages))))))))
    
    (let ((result (nreverse messages)))
      (when system-content
        (setq result (cons `((type . "message")
                             (role . "system")
                             (content . [((type . "input_text")
                                          (text . ,system-content))]))
                           result)))
      result)))

(defun greger-openrouter--convert-content-blocks (role content-blocks)
  "Convert Greger content blocks to Responses API format."
  (let (result-messages
        current-content-items)
    
    (dolist (block content-blocks)
      (let ((type (alist-get 'type block)))
        (cond
         ((string= type "text")
          (push `((type . "input_text")
                  (text . ,(alist-get 'text block)))
                current-content-items))
         
         ((string= type "thinking")
          (push `((type . "input_text")
                  (text . ,(alist-get 'thinking block)))
                current-content-items))
         
         ((string= type "tool_use")
          (when current-content-items
            (push `((type . "message")
                    (role . ,role)
                    (content . ,(vconcat (nreverse current-content-items))))
                  result-messages)
            (setq current-content-items nil)))
         
         ((string= type "tool_result")
          (push `((type . "message")
                  (role . "user")
                  (content . [((type . "input_text")
                               (text . ,(alist-get 'content block)))]))
                result-messages)))))
    
    (when current-content-items
      (push `((type . "message")
              (role . ,role)
              (content . ,(vconcat (nreverse current-content-items))))
            result-messages))
    
    (nreverse result-messages)))

(defun greger-openrouter--convert-tools (tools)
  "Convert Anthropic tool format to Responses API format.
The Responses API uses a flat structure with type, name, description, parameters
at the top level (not nested under 'function' like Chat Completions).
Also strips 'default' values and ensures 'required' is always a vector."
  (mapcar
   (lambda (tool)
     (let* ((input-schema (alist-get 'input_schema tool))
            (cleaned-schema (greger-openrouter--remove-defaults-from-schema input-schema))
            (fixed-schema (greger-openrouter--fix-required-field cleaned-schema)))
       `((type . "function")
         (name . ,(alist-get 'name tool))
         (description . ,(alist-get 'description tool))
         (strict . nil)
         (parameters . ,fixed-schema))))
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
  "Handle Responses API streaming event."

  (message "data-json: %s" data-json)

  (condition-case err
      (let* ((data (json-read-from-string data-json))
             (error-data (alist-get 'error data))
             (event-type (alist-get 'type data)))
        
        ;; Check for API errors first
        (when error-data
          (let ((error-message (format "OpenRouter API error: %s" 
                                       (or (alist-get 'message error-data)
                                           (json-encode error-data)))))
            (setf (greger-openrouter-state-error-message state) error-message)
            (message "OpenRouter error: %s" error-message)
            (error error-message)))
        
        (let* ((item (alist-get 'item data))
               (content-part (alist-get 'content_part data))
               (delta (alist-get 'delta data))
               (response (alist-get 'response data)))
          
          (cond
           ;; Handle reasoning summary part being added (starts thinking block)
           ((string= event-type "response.reasoning_summary_part.added")
            (when-let ((block-start-callback (greger-openrouter-state-block-start-callback state)))
              (funcall block-start-callback
                       `((type . "thinking")
                         (thinking . "")
                         (signature . "")))))
           
           ;; Handle reasoning summary text updates
           ((string= event-type "response.reasoning_summary_text.delta")
            (when-let ((text (alist-get 'delta data)))
              (setf (greger-openrouter-state-current-reasoning-text state)
                    (concat (or (greger-openrouter-state-current-reasoning-text state) "") text))
              (when-let ((text-delta-callback (greger-openrouter-state-text-delta-callback state)))
                (funcall text-delta-callback text))))
           
           ;; Handle content part being added (starts the text block)
           ((string= event-type "response.content_part.added")
            (when-let* ((part (alist-get 'part data))
                        ((string= (alist-get 'type part) "output_text")))
              (when-let ((block-start-callback (greger-openrouter-state-block-start-callback state)))
                (funcall block-start-callback
                         `((type . "text")
                           (text . ""))))))
           
           ;; Handle incremental text updates
           ((string= event-type "response.output_text.delta")
            (when-let ((text (alist-get 'delta data)))
              (let ((text-delta-callback (greger-openrouter-state-text-delta-callback state)))
                (setf (greger-openrouter-state-current-text state)
                      (concat (greger-openrouter-state-current-text state) text))
                (when text-delta-callback
                  (funcall text-delta-callback text)))))
           
           ;; Handle completion
           ((string= event-type "response.completed")
            (when response
              (let* ((output (alist-get 'output response))
                     (message-item (when output
                                     (seq-find (lambda (item)
                                                 (string= (alist-get 'type item) "message"))
                                               output)))
                     (content (when message-item (alist-get 'content message-item)))
                     (text-content (when content
                                     (seq-find (lambda (item)
                                                 (string= (alist-get 'type item) "output_text"))
                                               content)))
                     (text (when text-content (alist-get 'text text-content))))
                
                (when text
                  ;; If we never got streaming events, start the block now
                  (unless (greger-openrouter-state-text-started state)
                    (setf (greger-openrouter-state-text-started state) t)
                    (when-let ((block-start-callback (greger-openrouter-state-block-start-callback state)))
                      (funcall block-start-callback
                               `((type . "text")
                                 (text . "")))))
                  
                  ;; Set the full text
                  (setf (greger-openrouter-state-current-text state) text)
                  
                  ;; If we have a text-delta callback, send all the text at once
                  (when-let ((text-delta-callback (greger-openrouter-state-text-delta-callback state)))
                    (funcall text-delta-callback text)))
                
                (greger-openrouter--handle-finish state "stop")))))))
    (error
     (let ((error-message (format "Failed to parse event: %s" (error-message-string err))))
       (setf (greger-openrouter-state-error-message state) error-message)
       (message "OpenRouter parse error: %s" error-message)))))



(defun greger-openrouter--handle-finish (state _finish-reason)
  "Handle completion based on finish reason."
  (let ((content-blocks (greger-openrouter--build-content-blocks state))
        (block-stop-callback (greger-openrouter-state-block-stop-callback state)))
    
    (when block-stop-callback
      (dolist (block content-blocks)
        (let ((type (alist-get 'type block)))
          (funcall block-stop-callback type block))))
    
    (when-let ((callback (greger-openrouter-state-complete-callback state)))
      (funcall callback content-blocks))))

(defun greger-openrouter--build-content-blocks (state)
  "Build Anthropic-style content blocks from accumulated state.
Note: We don't include citations from web search because the text already
contains inline markdown links to sources."
  (let ((reasoning-text (greger-openrouter-state-current-reasoning-text state))
        (current-text (greger-openrouter-state-current-text state))
        blocks)
    
    (when reasoning-text
      (push `((type . "thinking")
              (thinking . ,reasoning-text)
              (signature . ""))
            blocks))
    
    (when current-text
      (push `((type . "text")
              (text . ,current-text))
            blocks))
    
    (nreverse blocks)))



(defun greger-openrouter--convert-citations-to-annotations (citations)
  "Convert Greger citation list back to OpenRouter annotations format."
  (vconcat
   (mapcar
    (lambda (citation)
      (let ((url (alist-get 'url citation))
            (title (alist-get 'title citation))
            (cited-text (alist-get 'cited_text citation)))
        `((type . "url_citation")
          (url_citation . ((url . ,url)
                           (title . ,title)
                           (content . ,cited-text)
                           (start_index . 0)
                           (end_index . 0))))))
    citations)))

(defun greger-openrouter--convert-annotations-to-citations (annotations)
  "Convert OpenRouter annotations from web search to Greger citations format."
  (when (vectorp annotations)
    (let (citations)
      (seq-doseq (annotation annotations)
        (when (string= (alist-get 'type annotation) "url_citation")
          (let ((url (alist-get 'url annotation))
                (start-idx (alist-get 'start_index annotation))
                (end-idx (alist-get 'end_index annotation)))
            (push `((type . "web_search_result_location")
                    (url . ,url)
                    (title . "")
                    (cited_text . ,(format "chars %d-%d" start-idx end-idx))
                    (encrypted_index . ""))
                  citations))))
      (nreverse citations))))

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
