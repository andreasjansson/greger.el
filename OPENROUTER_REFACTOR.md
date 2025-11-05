# OpenRouter Integration Proposal

## Overview

Add OpenRouter support to Greger to access GPT-5, GPT-5 Codex, and 400+ other models while maintaining 100% backward compatibility with the current Claude-based implementation.

## Key Requirements Met

1. ✅ **Current code continues working unchanged** - NO modifications to existing functions
2. ✅ Separate code path via configuration
3. ✅ Beta feature that can be enabled/disabled
4. ✅ Uses messages API (not responses API) for full chat history control
5. ✅ Access to GPT-5, GPT-5 Codex, and other models

## Core Principle: PARALLEL Implementation

**CRITICAL**: This is NOT a refactor. This is adding a completely parallel code path that:
- Leaves ALL existing code untouched (except for ONE dispatch point)
- Duplicates functionality in new files rather than abstracting
- Only activates when explicitly configured

## Architecture Overview

### Current State (UNTOUCHED)
- `greger-client.el` - Stays exactly as-is, zero changes
- `greger.el` - Current implementation stays, just adds ONE dispatch at entry point
- All current functions remain unchanged

### What Actually Changes

**ONLY ONE FUNCTION MODIFIED** in `greger.el`:

```elisp
;; BEFORE (current code):
(defun greger--run-agent-loop (state)
  "Run the main agent loop with STATE."
  (let* ((tools (greger-tools-get-schemas greger-tools))
         (server-tools ...)
         ...)
    (greger-client-stream ...)))

;; AFTER (with dispatch):
(defun greger--run-agent-loop (state)
  "Run the main agent loop with STATE."
  (if (eq greger-provider 'openrouter)
      (greger-openrouter--run-agent-loop state)  ; NEW: OpenRouter path
    (greger--run-agent-loop-claude state)))      ; OLD: Renamed but unchanged

;; NEW: Exact copy of old implementation
(defun greger--run-agent-loop-claude (state)
  "Run the main agent loop with STATE using Claude/Anthropic."
  ;; EXACT COPY of current greger--run-agent-loop implementation
  ;; No changes to any logic
  (let* ((tools (greger-tools-get-schemas greger-tools))
         (server-tools (when greger-server-tools
                        (greger-server-tools-get-schemas greger-server-tools)))
         (chat-buffer (greger-state-chat-buffer state))
         (dialog (greger-parser-markdown-buffer-to-dialog chat-buffer))
         (safe-shell-commands (greger-parser-find-safe-shell-commands-in-buffer chat-buffer))
         (tool-use-metadata (greger-state-tool-use-metadata state))
         (current-iteration (greger-state-current-iteration state))
         (auth-key (or (and greger-anthropic-key-fn (funcall greger-anthropic-key-fn))
                       (getenv "ANTHROPIC_API_KEY"))))
    
    (setf (plist-get tool-use-metadata :safe-shell-commands) safe-shell-commands)
    
    (when (>= current-iteration greger-max-iterations)
      (error "Maximum iterations (%d) reached, stopping agent execution" greger-max-iterations))
    
    (unless auth-key
      (error "No API key found.  Set ANTHROPIC_API_KEY environment variable or configure greger-anthropic-key-fn"))
    
    (with-current-buffer chat-buffer
      (let ((client-state (greger-client-stream
                           :model greger-model
                           :dialog dialog
                           :tools tools
                           :server-tools server-tools
                           :buffer chat-buffer
                           :thinking-budget greger-current-thinking-budget
                           :auth-key auth-key
                           :block-start-callback (lambda (content-block)
                                                   (greger--append-streaming-content-header state content-block))
                           :text-delta-callback (lambda (text)
                                                  (greger--append-text state (greger--clean-excessive-newlines text)))
                           :block-stop-callback (lambda (type content-block)
                                                  (greger--append-handle-content-block-stop state type content-block))
                           :complete-callback (lambda (content-blocks) (greger--handle-stream-completion state content-blocks))
                           :error-callback (lambda (error-message)
                                             (greger--handle-client-error state error-message))
                           :max-tokens greger-max-tokens)))
        
        (setf (greger-state-client-state state) client-state)
        (setq greger--current-state state)
        (greger--update-buffer-state)))))
```

That's it. That's the ONLY change to existing files.

### New Files Created

#### 1. Configuration (NEW FILE: `greger-config.el`)

Just adds new config variables, doesn't touch existing ones:

```elisp
(defcustom greger-provider 'anthropic
  "Provider to use for API calls. Options: 'anthropic or 'openrouter.
Default: 'anthropic (existing behavior)"
  :type '(choice (const :tag "Anthropic (Claude)" anthropic)
                 (const :tag "OpenRouter (Beta)" openrouter))
  :group 'greger)

(defcustom greger-openrouter-api-key-fn nil
  "Function to call to get the OpenRouter API key.
If nil, uses OPENROUTER_API_KEY environment variable."
  :type '(choice (const nil) function)
  :group 'greger)

(defcustom greger-openrouter-model "openai/gpt-5-codex"
  "Model to use when provider is 'openrouter"
  :type 'string
  :group 'greger)
```

#### 2. OpenRouter Implementation (NEW FILE: `greger-openrouter.el`)

This is a COMPLETE parallel implementation - it duplicates functionality rather than sharing code with Claude:

```elisp
;;; greger-openrouter.el --- OpenRouter client for greger -*- lexical-binding: t -*-

(require 'json)
(require 'cl-lib)

(defconst greger-openrouter-api-url "https://openrouter.ai/api/v1/chat/completions"
  "OpenRouter API endpoint.")

;; State structure - parallel to greger-client-state but for OpenRouter
(cl-defstruct greger-openrouter-state
  accumulated-output
  current-tool-calls  ; OpenAI accumulates tool calls differently
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

;;; Main streaming function - parallel to greger-client-stream
(cl-defun greger-openrouter-stream (&key model dialog tools buffer block-start-callback text-delta-callback block-stop-callback complete-callback thinking-budget max-tokens auth-key error-callback)
  "Stream request to OpenRouter API.
Similar to greger-client-stream but for OpenRouter/OpenAI format."
  (let* ((output-buffer (or buffer (current-buffer)))
         (undo-handle (prepare-change-group output-buffer))
         (request-spec (greger-openrouter--build-request model dialog tools thinking-budget max-tokens auth-key))
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

(defun greger-openrouter--build-request (model dialog tools thinking-budget max-tokens auth-key)
  "Build OpenRouter API request."
  (let* ((headers (greger-openrouter--build-headers auth-key))
         (data (greger-openrouter--build-data model dialog tools thinking-budget max-tokens)))
    (list :url greger-openrouter-api-url
          :method "POST"
          :headers headers
          :data data)))

(defun greger-openrouter--build-headers (api-key)
  "Build OpenRouter headers - Bearer auth, not x-api-key like Anthropic."
  `(("Content-Type" . "application/json")
    ("Authorization" . ,(concat "Bearer " api-key))
    ("HTTP-Referer" . "https://github.com/andreasjansson/greger.el")
    ("X-Title" . "Greger.el")))

(defun greger-openrouter--build-data (model dialog tools thinking-budget max-tokens)
  "Build OpenRouter request data in OpenAI format."
  (let ((messages (greger-openrouter--convert-dialog-to-messages dialog))
        (request-data `(("model" . ,model)
                        ("max_tokens" . ,max-tokens)
                        ("stream" . t))))
    
    (push `("messages" . ,messages) request-data)
    
    ;; Add tools in OpenAI format
    (when tools
      (push `("tools" . ,(greger-openrouter--convert-tools tools)) request-data)
      (push `("tool_choice" . "auto") request-data))
    
    ;; Add reasoning for thinking (if model supports it)
    (when (and thinking-budget (> thinking-budget 0))
      (push `("reasoning" . (("max_tokens" . ,thinking-budget))) request-data)
      (push `("include_reasoning" . t) request-data))
    
    (json-encode request-data)))

(defun greger-openrouter--convert-dialog-to-messages (dialog)
  "Convert Greger dialog format to OpenAI/OpenRouter message format.
This handles the differences between Anthropic's content blocks and OpenAI's simpler format."
  (let (messages)
    (dolist (msg dialog)
      (let ((role (alist-get 'role msg))
            (content (alist-get 'content msg)))
        
        ;; Skip system messages for now (handle separately)
        (unless (string= role "system")
          (cond
           ;; Simple string content
           ((stringp content)
            (push `((role . ,role) (content . ,content)) messages))
           
           ;; Complex content - need to convert
           ((listp content)
            (let ((converted (greger-openrouter--convert-content-blocks role content)))
              (when converted
                (setq messages (append converted messages)))))))))
    
    (nreverse messages)))

(defun greger-openrouter--convert-content-blocks (role content-blocks)
  "Convert Anthropic content blocks to OpenAI format.
Returns list of messages (may be multiple for tool calls)."
  (let (result-messages
        current-text
        tool-calls)
    
    (dolist (block content-blocks)
      (let ((type (alist-get 'type block)))
        (cond
         ;; Text content
         ((string= type "text")
          (setq current-text (alist-get 'text block)))
         
         ;; Thinking - treat as text for now
         ((string= type "thinking")
          (setq current-text (alist-get 'thinking block)))
         
         ;; Tool use - convert to OpenAI tool call format
         ((string= type "tool_use")
          (let ((tool-call `((id . ,(alist-get 'id block))
                             (type . "function")
                             (function . ((name . ,(alist-get 'name block))
                                          (arguments . ,(json-encode (alist-get 'input block))))))))
            (push tool-call tool-calls)))
         
         ;; Tool result - convert to OpenAI tool message format
         ((string= type "tool_result")
          (let ((tool-msg `((role . "tool")
                            (tool_call_id . ,(alist-get 'tool_use_id block))
                            (content . ,(alist-get 'content block)))))
            (push tool-msg result-messages))))))
    
    ;; Build assistant message if we have content or tool calls
    (when (or current-text tool-calls)
      (let ((msg `((role . ,role))))
        (when current-text
          (push `(content . ,current-text) msg))
        (when tool-calls
          (push `(tool_calls . ,(vconcat (nreverse tool-calls))) msg))
        (push msg result-messages)))
    
    (nreverse result-messages)))

(defun greger-openrouter--convert-tools (tools)
  "Convert Anthropic tool format to OpenAI function calling format."
  (mapcar
   (lambda (tool)
     `((type . "function")
       (function . ((name . ,(alist-get 'name tool))
                    (description . ,(alist-get 'description tool))
                    (parameters . ,(alist-get 'input_schema tool))))))
   tools))

(defun greger-openrouter--process-output-chunk (output state)
  "Process streaming output chunk - similar to greger-client but for OpenAI format."
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
  "Handle OpenAI-style streaming event with delta."
  (let* ((data (json-read-from-string data-json))
         (choices (alist-get 'choices data))
         (choice (when choices (aref choices 0)))
         (delta (alist-get 'delta choice))
         (finish-reason (alist-get 'finish_reason choice)))
    
    (when delta
      (cond
       ;; Text delta
       ((alist-get 'content delta)
        (when-let ((callback (greger-openrouter-state-text-delta-callback state)))
          (funcall callback (alist-get 'content delta))))
       
       ;; Reasoning/thinking delta
       ((alist-get 'reasoning delta)
        (when-let ((callback (greger-openrouter-state-text-delta-callback state)))
          (funcall callback (alist-get 'reasoning delta))))
       
       ;; Tool call delta - accumulate
       ((alist-get 'tool_calls delta)
        (greger-openrouter--accumulate-tool-calls delta state))))
    
    ;; Handle completion
    (when finish-reason
      (greger-openrouter--handle-finish state finish-reason))))

(defun greger-openrouter--accumulate-tool-calls (delta state)
  "Accumulate tool call deltas - OpenAI sends them incrementally."
  ;; OpenAI streams tool calls in chunks, need to accumulate them
  ;; This is more complex than Anthropic's approach
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
              ;; Append to existing
              (let ((existing-args (alist-get 'arguments existing)))
                (setf (alist-get 'arguments existing)
                      (concat existing-args arguments)))
            ;; Create new entry
            (puthash index
                     `((id . ,id)
                       (name . ,name)
                       (arguments . ,arguments))
                     accumulated)))))))

(defun greger-openrouter--handle-finish (state finish-reason)
  "Handle completion based on finish reason."
  (cond
   ((string= finish-reason "stop")
    ;; Normal completion - call complete callback
    (when-let ((callback (greger-openrouter-state-complete-callback state)))
      (funcall callback (greger-openrouter--build-content-blocks state))))
   
   ((string= finish-reason "tool_calls")
    ;; Tool calls completed - convert and call complete callback
    (when-let ((callback (greger-openrouter-state-complete-callback state)))
      (funcall callback (greger-openrouter--build-content-blocks state))))))

(defun greger-openrouter--build-content-blocks (state)
  "Build Anthropic-style content blocks from accumulated state.
This converts back to Greger's internal format."
  (let ((tool-calls (greger-openrouter-state-current-tool-calls state))
        blocks)
    
    (when (> (hash-table-count tool-calls) 0)
      (maphash
       (lambda (_index call)
         (let* ((id (alist-get 'id call))
                (name (alist-get 'name call))
                (arguments (alist-get 'arguments call))
                (parsed-args (json-read-from-string arguments)))
           (push `((type . "tool_use")
                   (id . ,id)
                   (name . ,name)
                   (input . ,parsed-args))
                 blocks)))
       tool-calls))
    
    (nreverse blocks)))

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
```

#### 3. Agent Loop Entry Point (NEW FUNCTION in `greger.el`)

Add the OpenRouter agent loop as a completely separate implementation:

```elisp
(require 'greger-config)
(require 'greger-provider)
(require 'greger-provider-anthropic)
(require 'greger-provider-openrouter)

(defun greger--run-agent-loop (state)
  "Run the main agent loop with STATE using configured provider."
  (let* ((provider greger-provider)
         (model (if (eq provider 'openrouter)
                    greger-openrouter-model
                  greger-model))
         (auth-key (greger--get-auth-key provider))
         (tools (greger-tools-get-schemas greger-tools))
         (server-tools (greger--get-server-tools-for-provider provider))
         (chat-buffer (greger-state-chat-buffer state))
         (dialog (greger-parser-markdown-buffer-to-dialog chat-buffer)))
    
    ;; Use generic provider interface
    (let ((client-state 
           (greger-provider-stream
            provider
            model
            dialog
            tools
            server-tools
            chat-buffer
            ;; Callbacks remain the same
            :block-start-callback (lambda (block) ...)
            :text-delta-callback (lambda (text) ...)
            :block-stop-callback (lambda (type block) ...)
            :complete-callback (lambda (blocks) ...)
            :error-callback (lambda (error) ...)
            auth-key
            `(:thinking-budget ,greger-current-thinking-budget
              :max-tokens ,greger-max-tokens))))
      
      (setf (greger-state-client-state state) client-state)
      (setq greger--current-state state)
      (greger--update-buffer-state))))

(defun greger--get-auth-key (provider)
  "Get API key for PROVIDER."
  (pcase provider
    ('anthropic
     (or (and greger-anthropic-key-fn (funcall greger-anthropic-key-fn))
         (getenv "ANTHROPIC_API_KEY")))
    ('openrouter
     (or (and greger-openrouter-api-key-fn (funcall greger-openrouter-api-key-fn))
         (getenv "OPENROUTER_API_KEY")))))

(defun greger--get-server-tools-for-provider (provider)
  "Get server tools appropriate for PROVIDER.
OpenRouter doesn't support Anthropic server tools, return nil."
  (pcase provider
    ('anthropic
     (when greger-server-tools
       (greger-server-tools-get-schemas greger-server-tools)))
    ('openrouter nil)))  ;; OpenRouter doesn't support Anthropic's server tools
```

#### 6. Provider Switching UI

Add interactive commands to switch providers:

```elisp
(defun greger-set-provider ()
  "Interactively set the API provider."
  (interactive)
  (let ((choice (completing-read
                 "Select provider: "
                 '(("Anthropic (Claude)" . anthropic)
                   ("OpenRouter (Beta)" . openrouter))
                 nil t)))
    (setq greger-provider (cdr (assoc choice 
                                      '(("Anthropic (Claude)" . anthropic)
                                        ("OpenRouter (Beta)" . openrouter)))))
    (message "Provider set to: %s" choice)))

(defun greger-set-openrouter-model ()
  "Set the OpenRouter model."
  (interactive)
  (let ((model (completing-read "OpenRouter model: " 
                                greger-openrouter-models nil nil)))
    (setq greger-openrouter-model model)
    (message "OpenRouter model set to: %s" model)))

;; Add to greger-mode-map
(define-key greger-mode-map (kbd "C-; p") #'greger-set-provider)
(define-key greger-mode-map (kbd "C-; o") #'greger-set-openrouter-model)
```

## OpenRouter API Details

### Key Differences from Anthropic

1. **API Endpoint**: `https://openrouter.ai/api/v1/chat/completions` (OpenAI-compatible)

2. **Authentication**: Bearer token in Authorization header (not x-api-key)

3. **Message Format**: OpenAI-style messages (simpler than Anthropic's content blocks)
   ```json
   {
     "role": "user",
     "content": "Hello"
   }
   ```

4. **Tool Calling**: OpenAI function calling format
   ```json
   {
     "tools": [{
       "type": "function",
       "function": {
         "name": "read-file",
         "description": "...",
         "parameters": {...}
       }
     }]
   }
   ```

5. **Streaming**: Server-Sent Events with `delta` fields
   ```json
   {
     "choices": [{
       "delta": {"content": "text chunk"},
       "finish_reason": null
     }]
   }
   ```

6. **Thinking/Reasoning**: 
   - Uses `reasoning` parameter with `effort` levels or `max_tokens`
   - Reasoning tokens appear in `delta.reasoning` field
   - Some models (GPT-5, Claude via OpenRouter) support this
   - Not all models expose reasoning tokens (varies by model)

7. **No Server Tools**: OpenRouter doesn't support Anthropic's server-side tools like `web_search`

### Model Identifiers

- OpenAI models: `"openai/gpt-5-codex"`, `"openai/gpt-5"`, `"openai/gpt-5-mini"`
- Claude via OpenRouter: `"anthropic/claude-sonnet-4"`, `"anthropic/claude-opus-4"`
- Other providers: `"google/gemini-2.5-pro"`, etc.

### Reasoning Tokens Handling

For models that support reasoning (thinking):

```json
{
  "reasoning": {
    "effort": "high",  // or "low", "medium"
    "max_tokens": 4096  // explicit budget (alternative to effort)
  }
}
```

Response includes reasoning in separate field:
```json
{
  "choices": [{
    "delta": {
      "reasoning": "thinking text...",
      "content": "response text..."
    }
  }]
}
```

## Migration Path

### Phase 1: Foundation (No Breaking Changes)
1. Create `greger-config.el` with provider configuration
2. Create `greger-provider.el` with generic protocol
3. Keep `greger-client.el` as-is, add backward-compatible wrapper

### Phase 2: Anthropic Refactor (No Breaking Changes)
1. Copy current implementation to `greger-provider-anthropic.el`
2. Implement protocol methods
3. Update `greger-client.el` to dispatch to provider implementations
4. All existing code continues to work via `'anthropic` default

### Phase 3: OpenRouter Implementation (Beta)
1. Implement `greger-provider-openrouter.el`
2. Add configuration UI
3. Document beta status and limitations
4. Users opt-in by setting `greger-provider` to `'openrouter`

## Testing Strategy

1. **Regression Testing**: Run full test suite with `greger-provider` = `'anthropic`
2. **Dual Testing**: Same tests run with both providers where applicable
3. **Provider-Specific Tests**: OpenRouter-specific features (model switching, etc.)
4. **Manual Testing**: Real-world usage with both providers

## Limitations (Beta)

1. **No Server Tools**: OpenRouter doesn't support Anthropic's server tools (web_search)
2. **Reasoning Varies**: Not all models expose reasoning tokens
3. **Different Capabilities**: Each model has different context lengths, capabilities
4. **Cost Tracking**: Different pricing models (OpenRouter adds small markup)

## Documentation Updates

1. Update README with provider selection instructions
2. Add OpenRouter setup guide (API key, model selection)
3. Document known limitations
4. Add provider comparison table

## Benefits

1. ✅ **Zero Risk**: Current code unchanged, fully backward compatible
2. ✅ **Opt-In Beta**: Users explicitly enable OpenRouter
3. ✅ **Model Flexibility**: Access 400+ models including GPT-5, GPT-5 Codex
4. ✅ **Cost Options**: Compare pricing across providers
5. ✅ **Future Ready**: Easy to add more providers (Gemini direct, etc.)
6. ✅ **Clean Abstraction**: Provider interface isolates implementation details

## Example Usage

```elisp
;; In your config:

;; Use OpenRouter with GPT-5 Codex
(setq greger-provider 'openrouter)
(setq greger-openrouter-model "openai/gpt-5-codex")
(setq greger-openrouter-api-key-fn
      (lambda () (auth-source-pick-first-password :host "openrouter.ai")))

;; Or stick with Claude (default)
(setq greger-provider 'anthropic)
(setq greger-model 'claude-sonnet-4-20250514)

;; Switch providers on the fly
M-x greger-set-provider RET OpenRouter (Beta) RET
M-x greger-set-openrouter-model RET openai/gpt-5 RET
```

## Implementation Priority

1. **High Priority**
   - `greger-config.el` - Configuration layer
   - `greger-provider.el` - Protocol definition
   - `greger-provider-openrouter.el` - Basic OpenRouter support
   - Message format conversion
   - Tool calling format conversion

2. **Medium Priority**
   - Reasoning/thinking token handling
   - Error handling improvements
   - Model switching UI
   - Documentation

3. **Low Priority**
   - Cost tracking per provider
   - Provider-specific optimizations
   - Advanced model features

## Open Questions

1. **Thinking Format**: Should we normalize thinking tokens across providers or preserve native format?
   - **Recommendation**: Normalize to Anthropic's thinking block format for consistency in parser

2. **Model Configuration**: Per-provider model lists or unified?
   - **Recommendation**: Separate lists since model identifiers differ completely

3. **Tool Schema**: Convert on-the-fly or maintain separate schemas?
   - **Recommendation**: Convert on-the-fly, keep single tool definition format (Anthropic-style)

4. **Error Messages**: Provider-specific or normalized?
   - **Recommendation**: Normalize error categories, preserve original in details

## Conclusion

This design provides a clean path to OpenRouter support while maintaining 100% backward compatibility. The provider abstraction makes it easy to add more providers in the future, and the opt-in nature means existing users see zero disruption.
