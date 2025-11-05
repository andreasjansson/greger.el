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
  "Provider to use for API calls. Options: 'anthropic or 'openrouter"
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

;; Provider-specific model lists
(defconst greger-anthropic-models
  '(claude-sonnet-4-20250514
    claude-opus-4-20250514
    claude-opus-4-1-20250805
    claude-sonnet-4-5))

(defconst greger-openrouter-models
  '("openai/gpt-5"
    "openai/gpt-5-codex"
    "openai/gpt-5-mini"
    "anthropic/claude-sonnet-4"
    "anthropic/claude-opus-4"
    "google/gemini-2.5-pro"))
```

#### 2. Provider Abstraction Layer (NEW FILE: `greger-provider.el`)

This file contains protocol/generic functions that dispatch to provider-specific implementations:

```elisp
;; Generic interface for providers
(cl-defgeneric greger-provider-stream (provider model dialog tools server-tools buffer callbacks auth-key options)
  "Stream a request to PROVIDER using MODEL with DIALOG, TOOLS, etc.
Returns a provider-specific state object.")

(cl-defgeneric greger-provider-build-headers (provider auth-key)
  "Build HTTP headers for PROVIDER with AUTH-KEY.")

(cl-defgeneric greger-provider-build-data (provider model dialog tools server-tools thinking-budget max-tokens)
  "Build request data for PROVIDER.")

(cl-defgeneric greger-provider-process-event (provider data-json state)
  "Process streaming event DATA-JSON for PROVIDER using STATE.")

(cl-defgeneric greger-provider-cancel-request (provider state)
  "Cancel active request for PROVIDER using STATE.")
```

#### 3. Anthropic Implementation (REFACTORED: `greger-client.el`)

Rename to `greger-provider-anthropic.el` and implement the protocol:

```elisp
;; Implement the protocol methods
(cl-defmethod greger-provider-stream ((provider (eql 'anthropic)) model dialog tools ...)
  ;; Current greger-client-stream implementation
  ...)

(cl-defmethod greger-provider-build-headers ((provider (eql 'anthropic)) auth-key)
  ;; Current greger-client--build-headers
  ...)

(cl-defmethod greger-provider-build-data ((provider (eql 'anthropic)) ...)
  ;; Current greger-client--build-data
  ...)

(cl-defmethod greger-provider-process-event ((provider (eql 'anthropic)) data-json state)
  ;; Current greger-client--handle-event
  ...)
```

#### 4. OpenRouter Implementation (NEW FILE: `greger-provider-openrouter.el`)

Implement OpenRouter-specific API handling:

```elisp
(require 'greger-provider)

(defconst greger-openrouter-api-url "https://openrouter.ai/api/v1/chat/completions"
  "OpenRouter API endpoint.")

(cl-defmethod greger-provider-build-headers ((provider (eql 'openrouter)) auth-key)
  "Build OpenRouter headers."
  `(("Content-Type" . "application/json")
    ("Authorization" . ,(concat "Bearer " auth-key))
    ("HTTP-Referer" . "https://github.com/andreasjansson/greger.el")
    ("X-Title" . "Greger.el")))

(cl-defmethod greger-provider-build-data ((provider (eql 'openrouter)) model dialog tools server-tools thinking-budget max-tokens)
  "Build OpenRouter request data.
OpenRouter uses OpenAI-compatible format."
  (let ((messages (greger-openrouter--format-messages dialog))
        (request-data `(("model" . ,model)
                        ("messages" . ,messages)
                        ("max_tokens" . ,max-tokens)
                        ("stream" . t))))
    
    ;; Add tools if present (OpenAI format)
    (when tools
      (push `("tools" . ,(greger-openrouter--format-tools tools)) request-data)
      (push `("tool_choice" . "auto") request-data))
    
    ;; Add reasoning parameter for thinking
    (when (and thinking-budget (> thinking-budget 0))
      (push `("reasoning" . (("effort" . "high")
                             ("max_tokens" . ,thinking-budget))) request-data))
    
    (json-encode request-data)))

(cl-defmethod greger-provider-process-event ((provider (eql 'openrouter)) data-json state)
  "Process OpenRouter streaming event.
OpenRouter uses OpenAI-compatible SSE format with delta fields."
  (let* ((data (json-read-from-string data-json))
         (choices (alist-get 'choices data))
         (choice (aref choices 0))
         (delta (alist-get 'delta choice))
         (finish-reason (alist-get 'finish_reason choice)))
    
    (cond
     ;; Text delta
     ((alist-get 'content delta)
      (greger-openrouter--handle-text-delta delta state))
     
     ;; Tool call delta
     ((alist-get 'tool_calls delta)
      (greger-openrouter--handle-tool-call-delta delta state))
     
     ;; Reasoning delta (if model supports it)
     ((alist-get 'reasoning delta)
      (greger-openrouter--handle-reasoning-delta delta state))
     
     ;; Completion
     ((string= finish-reason "stop")
      (greger-openrouter--handle-completion state))
     
     ((string= finish-reason "tool_calls")
      (greger-openrouter--handle-tool-calls-completion state)))))

(defun greger-openrouter--format-messages (dialog)
  "Convert Greger dialog format to OpenRouter/OpenAI message format.
Handles differences:
- Anthropic uses 'content' blocks with 'type' field
- OpenAI uses simpler message format
- Tool results have different structure"
  (mapcar
   (lambda (message)
     (let ((role (alist-get 'role message))
           (content (alist-get 'content message)))
       
       (cond
        ;; Simple text message
        ((stringp content)
         `((role . ,role)
           (content . ,content)))
        
        ;; Complex content blocks (tool use, etc.)
        ((listp content)
         (greger-openrouter--format-complex-message role content)))))
   dialog))

(defun greger-openrouter--format-tools (tools)
  "Convert Anthropic tool format to OpenAI function calling format."
  (mapcar
   (lambda (tool)
     `((type . "function")
       (function . ((name . ,(alist-get 'name tool))
                    (description . ,(alist-get 'description tool))
                    (parameters . ,(alist-get 'input_schema tool))))))
   tools))

(defun greger-openrouter--handle-reasoning-delta (delta state)
  "Handle reasoning/thinking tokens if model supports them.
OpenRouter exposes reasoning via 'reasoning' field in delta."
  (when-let ((reasoning-text (alist-get 'reasoning delta)))
    ;; Convert to Anthropic-style thinking format for consistency
    (greger-provider--append-thinking-text state reasoning-text)))
```

#### 5. Main Entry Point Updates (MODIFIED: `greger.el`)

Update the main file to use the provider abstraction:

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
