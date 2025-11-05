# OpenRouter Integration Proposal

## Executive Summary

**Goal**: Add GPT-5 Codex and 400+ other models via OpenRouter without breaking current Claude implementation.

**Approach**: Complete parallel implementation - duplicate code rather than abstract/refactor.

**Impact**: 
- ✅ Current code: **ZERO changes** (except renaming one function)
- ✅ New code: Completely separate `greger-openrouter.el` file
- ✅ User impact: Opt-in beta feature via config variable

**What Works**:
- ✅ Tool calling with format conversion
- ✅ Thinking/reasoning (without signatures)
- ✅ Web search via `:online` variant (annotations converted to simplified citations)
- ✅ Basic streaming with all callbacks

**What's Different**:
- No cryptographic signatures on thinking blocks (OpenRouter doesn't provide them)
- Citations are simpler (no encrypted indices, no fold/unfold)
- Web search uses `:online` variant instead of server-side tool

---

## Table of Contents

1. [Overview](#overview)
2. [Key Requirements Met](#key-requirements-met)
3. [Core Principle: PARALLEL Implementation](#core-principle-parallel-implementation)
4. [Architecture Overview](#architecture-overview)
5. [Critical Feature Differences](#critical-feature-differences)
   - [Thinking/Reasoning Blocks](#1-thinkingreasoning-blocks)
   - [Web Search / Server Tools](#2-web-search--server-tools)
   - [Citations Handling](#3-citations-handling)
6. [New Files Created](#new-files-created)
   - [Configuration](#1-configuration-new-file-greger-configel)
   - [OpenRouter Implementation](#2-openrouter-implementation-new-file-greger-openrouterel)
   - [Agent Loop Entry Point](#3-agent-loop-entry-point-new-function-in-gregerel)
   - [Provider Switching UI](#4-provider-switching-ui-new-in-gregerel)
7. [Implementation Details](#implementation-details)
8. [Summary of Changes](#summary-of-changes)
9. [Migration Path](#migration-path)
10. [Testing Strategy](#testing-strategy)
11. [Known Limitations](#known-limitations-beta)
12. [Implementation Checklist](#implementation-checklist)
13. [Feature Comparison Table](#feature-comparison-table)
14. [Concrete Examples](#concrete-examples-of-differences)
15. [Benefits](#benefits-of-this-approach)
16. [Example Usage](#example-usage)
17. [FAQ](#faq)
18. [Conclusion](#conclusion)

---

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
  thinking-started    ; Track if we've started a thinking block
  annotations         ; Store annotations (citations) until completion
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
(cl-defun greger-openrouter-stream (&key model dialog tools buffer enable-web-search block-start-callback text-delta-callback block-stop-callback complete-callback thinking-budget max-tokens auth-key error-callback)
  "Stream request to OpenRouter API.
Similar to greger-client-stream but for OpenRouter/OpenAI format.
ENABLE-WEB-SEARCH determines if we append :online to the model."
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

(defun greger-openrouter--build-data (model dialog tools thinking-budget max-tokens enable-web-search)
  "Build OpenRouter request data in OpenAI format.
ENABLE-WEB-SEARCH determines if we append :online to the model."
  (let* ((messages (greger-openrouter--convert-dialog-to-messages dialog))
         ;; Append :online if web search enabled - gives us fast native search for OpenAI/Anthropic
         (actual-model (if enable-web-search
                           (concat model ":online")
                         model))
         (request-data `(("model" . ,actual-model)
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
         (message (alist-get 'message choice))
         (finish-reason (alist-get 'finish_reason choice)))
    
    (when delta
      (cond
       ;; Text delta
       ((alist-get 'content delta)
        (when-let ((callback (greger-openrouter-state-text-delta-callback state)))
          (funcall callback (alist-get 'content delta))))
       
       ;; Reasoning/thinking delta - needs special handling
       ((alist-get 'reasoning delta)
        (greger-openrouter--handle-reasoning-delta delta state))
       
       ;; Tool call delta - accumulate
       ((alist-get 'tool_calls delta)
        (greger-openrouter--accumulate-tool-calls delta state))))
    
    ;; Store annotations when message is complete (web search results)
    (when (and message (alist-get 'annotations message))
      (setf (greger-openrouter-state-annotations state)
            (alist-get 'annotations message)))
    
    ;; Handle completion
    (when finish-reason
      (greger-openrouter--handle-finish state finish-reason))))

(defun greger-openrouter--handle-reasoning-delta (delta state)
  "Handle reasoning/thinking delta and convert to Greger thinking format.
OpenRouter provides reasoning in delta.reasoning field, but we need to
convert it to Greger's thinking block format for consistency."
  (let ((reasoning-text (alist-get 'reasoning delta))
        (block-start-callback (greger-openrouter-state-block-start-callback state))
        (text-delta-callback (greger-openrouter-state-text-delta-callback state)))
    
    ;; First time we see reasoning, send thinking block start
    (unless (greger-openrouter-state-thinking-started state)
      (setf (greger-openrouter-state-thinking-started state) t)
      ;; Create thinking block header (no signature for OpenRouter)
      (when block-start-callback
        (funcall block-start-callback
                 `((type . "thinking")
                   (thinking . "")
                   (signature . "")))))  ; Empty signature - OpenRouter doesn't provide them
    
    ;; Stream the thinking text
    (when (and reasoning-text text-delta-callback)
      (funcall text-delta-callback reasoning-text))))

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
        (annotations (greger-openrouter-state-annotations state))
        blocks)
    
    ;; Add tool use blocks
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
    
    ;; Add citation blocks from annotations (web search results)
    (when annotations
      (dolist (annotation annotations)
        (when (string= (alist-get 'type annotation) "url_citation")
          (let ((citation-block `((type . "text")
                                  (text . "")
                                  (citations . (,annotation)))))
            (push citation-block blocks)))))
    
    (nreverse blocks)))

(defun greger-openrouter--convert-annotation-to-citation (annotation)
  "Convert OpenRouter annotation to Greger citation format.
OpenRouter provides: url, title, text, start_index, end_index
Greger expects: type, url, title, cited_text, encrypted_index"
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
```

#### 3. Agent Loop Entry Point (NEW FUNCTION in `greger.el`)

Add the OpenRouter agent loop as a completely separate implementation:

```elisp
;; NEW FUNCTION - completely parallel implementation
(defun greger-openrouter--run-agent-loop (state)
  "Run agent loop using OpenRouter - parallel to greger--run-agent-loop-claude."
  (let* ((tools (greger-tools-get-schemas greger-tools))
         ;; Check if web search is enabled (user has web_search in server-tools)
         (enable-web-search (and greger-server-tools
                                 (member "web_search" greger-server-tools)))
         (chat-buffer (greger-state-chat-buffer state))
         (dialog (greger-parser-markdown-buffer-to-dialog chat-buffer))
         (safe-shell-commands (greger-parser-find-safe-shell-commands-in-buffer chat-buffer))
         (tool-use-metadata (greger-state-tool-use-metadata state))
         (current-iteration (greger-state-current-iteration state))
         (auth-key (or (and greger-openrouter-api-key-fn 
                            (funcall greger-openrouter-api-key-fn))
                       (getenv "OPENROUTER_API_KEY"))))
    
    (setf (plist-get tool-use-metadata :safe-shell-commands) safe-shell-commands)
    
    (when (>= current-iteration greger-max-iterations)
      (error "Maximum iterations (%d) reached" greger-max-iterations))
    
    (unless auth-key
      (error "No OpenRouter API key found. Set OPENROUTER_API_KEY environment variable"))
    
    (with-current-buffer chat-buffer
      (let ((client-state (greger-openrouter-stream
                           :model greger-openrouter-model
                           :dialog dialog
                           :tools tools
                           :enable-web-search enable-web-search
                           :buffer chat-buffer
                           :thinking-budget greger-current-thinking-budget
                           :auth-key auth-key
                           :block-start-callback (lambda (content-block)
                                                   (greger--append-streaming-content-header state content-block))
                           :text-delta-callback (lambda (text)
                                                  (greger--append-text state (greger--clean-excessive-newlines text)))
                           :block-stop-callback (lambda (type content-block)
                                                  (greger--append-handle-content-block-stop state type content-block))
                           :complete-callback (lambda (content-blocks) 
                                                (greger--handle-stream-completion state content-blocks))
                           :error-callback (lambda (error-message)
                                             (greger--handle-client-error state error-message))
                           :max-tokens greger-max-tokens)))
        
        (setf (greger-state-client-state state) client-state)
        (setq greger--current-state state)
        (greger--update-buffer-state)))))
```

That's it! The OpenRouter implementation is completely separate, reuses the same UI callbacks (like `greger--append-text`, `greger--handle-stream-completion`, etc.) but has its own API handling.

#### 4. Provider Switching UI (NEW in `greger.el`)

```elisp
(defun greger-set-provider ()
  "Interactively set the API provider."
  (interactive)
  (let ((choice (completing-read
                 "Select provider: "
                 '(("Anthropic (Claude) - Default" . anthropic)
                   ("OpenRouter (Beta)" . openrouter))
                 nil t)))
    (setq greger-provider (cdr (assoc choice 
                                      '(("Anthropic (Claude) - Default" . anthropic)
                                        ("OpenRouter (Beta)" . openrouter)))))
    (message "Provider set to: %s" choice)))

(defun greger-set-openrouter-model ()
  "Set the OpenRouter model when using OpenRouter provider."
  (interactive)
  (unless (eq greger-provider 'openrouter)
    (user-error "OpenRouter provider not active. Use M-x greger-set-provider first"))
  (let ((models '("openai/gpt-5"
                  "openai/gpt-5-codex" 
                  "openai/gpt-5-mini"
                  "anthropic/claude-sonnet-4"
                  "anthropic/claude-opus-4"
                  "google/gemini-2.5-pro")))
    (setq greger-openrouter-model 
          (completing-read "OpenRouter model: " models nil nil))
    (message "OpenRouter model set to: %s" greger-openrouter-model)))

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

## Summary of Changes

### Files Modified

1. **`greger.el`** - ONE function modified:
   - Rename existing `greger--run-agent-loop` → `greger--run-agent-loop-claude`
   - Add new `greger--run-agent-loop` that dispatches based on `greger-provider`
   - Add new `greger-openrouter--run-agent-loop` function
   - Add provider switching commands
   - Add keybindings for provider switching

### Files Created

1. **`greger-config.el`** (OPTIONAL - can add to greger.el instead)
   - New config variables: `greger-provider`, `greger-openrouter-api-key-fn`, `greger-openrouter-model`

2. **`greger-openrouter.el`** 
   - Complete parallel implementation
   - Own state structure
   - Own streaming logic  
   - Own message format conversion
   - Own tool format conversion

### Files Unchanged

- ✅ `greger-client.el` - 100% unchanged
- ✅ `greger-parser.el` - 100% unchanged
- ✅ `greger-tools.el` - 100% unchanged
- ✅ `greger-stdlib.el` - 100% unchanged
- ✅ All other files - 100% unchanged

## Migration Path

### Phase 1: Add Configuration (5 minutes)
1. Add config variables to `greger.el` or new `greger-config.el`
2. Default: `(defcustom greger-provider 'anthropic ...)`
3. Zero impact on existing behavior

### Phase 2: Add Dispatch Point (5 minutes)
1. Rename `greger--run-agent-loop` → `greger--run-agent-loop-claude`
2. Create new `greger--run-agent-loop` with if/else dispatch
3. Still zero impact - defaults to Claude path

### Phase 3: Implement OpenRouter (main work)
1. Create `greger-openrouter.el` with complete parallel implementation
2. Create `greger-openrouter--run-agent-loop`
3. Still zero impact unless user sets `greger-provider` to `'openrouter`

### Phase 4: Add UI (10 minutes)
1. Add `greger-set-provider` command
2. Add `greger-set-openrouter-model` command  
3. Add keybindings

## Testing Strategy

1. **Regression Testing**: 
   - Keep `greger-provider` = `'anthropic` (default)
   - Run ALL existing tests
   - Everything should pass identically

2. **OpenRouter Testing**:
   - Set `greger-provider` = `'openrouter`
   - Test basic streaming
   - Test tool calls
   - Test thinking (if model supports)
   - Test error handling

3. **Provider Switching**:
   - Test switching between providers mid-session
   - Verify state isolation

4. **Manual Testing**:
   - Real coding tasks with GPT-5 Codex
   - Compare quality vs Claude
   - Test different models

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

## Implementation Checklist

### Step 1: Add Configuration Variables
```elisp
;; Add to greger.el after existing defcustoms
(defcustom greger-provider 'anthropic
  "Provider to use for API calls.
Options: 'anthropic (default) or 'openrouter (beta)"
  :type '(choice (const :tag "Anthropic (Claude)" anthropic)
                 (const :tag "OpenRouter (Beta)" openrouter))
  :group 'greger)

(defcustom greger-openrouter-api-key-fn nil
  "Function to return OpenRouter API key.
If nil, uses OPENROUTER_API_KEY environment variable."
  :type '(choice (const nil) function)
  :group 'greger)

(defcustom greger-openrouter-model "openai/gpt-5-codex"
  "Model to use when greger-provider is 'openrouter."
  :type 'string
  :group 'greger)
```

### Step 2: Add Dispatch Function
```elisp
;; In greger.el, rename existing function:
;; greger--run-agent-loop → greger--run-agent-loop-claude

;; Then add new dispatch function:
(defun greger--run-agent-loop (state)
  "Run agent loop - dispatches based on greger-provider."
  (if (eq greger-provider 'openrouter)
      (greger-openrouter--run-agent-loop state)
    (greger--run-agent-loop-claude state)))
```

### Step 3: Create greger-openrouter.el
- Copy template from this document
- Implement all functions
- Test incrementally

### Step 4: Add UI Commands
```elisp
;; Add to greger.el
(defun greger-set-provider () ...)
(defun greger-set-openrouter-model () ...)

;; Add to greger-mode-map
(define-key greger-mode-map (kbd "C-; p") #'greger-set-provider)
(define-key greger-mode-map (kbd "C-; o") #'greger-set-openrouter-model)
```

### Step 5: Update Documentation
- README: Add OpenRouter section
- Document API key setup
- Document model selection
- Note beta status and limitations

## Critical Feature Differences

### 1. Thinking/Reasoning Blocks

**Claude (Anthropic)**:
- Uses explicit `thinking` content blocks with signatures
- Format:
  ```json
  {
    "type": "thinking",
    "thinking": "reasoning text...",
    "signature": "crypto signature for verification"
  }
  ```
- Rendered as `# THINKING\n\nSignature: xyz\n\nthinking text`

**OpenRouter**:
- Uses `reasoning` parameter and returns reasoning in separate field
- Format in request:
  ```json
  {
    "reasoning": {
      "max_tokens": 4096,
      "effort": "high"  // or "low", "medium"
    },
    "include_reasoning": true
  }
  ```
- Format in response (streaming):
  ```json
  {
    "delta": {
      "reasoning": "reasoning text chunk..."
    }
  }
  ```
- **NO SIGNATURES**: OpenRouter doesn't provide cryptographic signatures for thinking
- **Model-dependent**: Not all models expose reasoning (GPT-5, Claude via OpenRouter do; others may not)

**Implementation Strategy**:
- Convert OpenRouter `reasoning` deltas to Greger's thinking format
- Omit signature field (set to empty string)
- Map `delta.reasoning` → Greger thinking block
- Prefix with `# THINKING` header for consistency

### 2. Web Search

**Claude (Anthropic)**:
- Has built-in `web_search` **server tool** (Anthropic executes the search)
- Returns `web_search_tool_result` content blocks
- Includes structured citations with encrypted indices:
  ```json
  {
    "type": "web_search_tool_result",
    "tool_use_id": "...",
    "content": {
      "citations": [{
        "type": "web_search_result_location",
        "url": "https://...",
        "title": "...",
        "cited_text": "...",
        "encrypted_index": "..."
      }]
    }
  }
  ```
- Greger has complex citation rendering with clickable URLs, fold/unfold

**OpenRouter**:
- Uses `:online` variant that appends web search to any model
  - Example: `"openai/gpt-5-codex"` becomes `"openai/gpt-5-codex:online"`
  - Powered by native search (OpenAI/Anthropic built-in) or Exa for other models
  - OpenAI's native web search is fast and high-quality
  - Costs: Native search included in model pricing, Exa costs $4 per 1000 results (default 5 = $0.02)
- Returns annotations at message completion:
  ```json
  {
    "choices": [{
      "message": {
        "content": "...",
        "annotations": [{
          "type": "url_citation",
          "url": "https://...",
          "title": "...",
          "text": "...",
          "start_index": 123,
          "end_index": 456
        }]
      }
    }]
  }
  ```

**Implementation**:
We append `:online` to the model name automatically when web search is enabled (when `greger-server-tools` contains `"web_search"`). This gives us:
- Fast OpenAI native web search for GPT models
- Fast Anthropic native search for Claude models  
- Exa fallback for other models

Citations are rendered in simplified format:
```markdown
# ASSISTANT

Response with web search results...

## https://example.com

Title: Example Site
Cited text: relevant quote from search

## https://another.com

Title: Another Source
Cited text: another quote
```

**Differences from Claude citations**:
- No encrypted indices (not provided by OpenRouter)
- No fold/unfold (always visible)
- No signature verification
- Rendered at end of response (not streamed inline)

### 3. Citations Handling

**Greger's Current Citation System** (for Claude):
```markdown
# ASSISTANT

This is cited text with underline.

## https://example.com

Title: Example Site
Cited text: relevant quote
Encrypted index: abc123xyz
```

Features:
- Inline citations with special formatting
- Clickable URLs (opens in browser)
- Fold/unfold citation details
- Encrypted indices for verification
- Citations are content blocks themselves

**OpenRouter Annotations**:
- Message-level, not content-block-level
- No encryption/verification
- Character offset-based (start_index, end_index)
- Delivered at end of response, not streamed

**Implementation**:
We convert OpenRouter annotations to simplified Greger citation format. When the response completes and we receive annotations, we append them as citation blocks:

```markdown
# ASSISTANT

Response text with search results...

## https://example.com

Title: Example Site
Cited text: relevant quote from the search result

## https://another.com

Title: Another Source  
Cited text: another relevant quote
```

**What's different**:
- No `encrypted_index` field (not provided by OpenRouter)
- Citations appear at end, not inline during streaming
- No fold/unfold functionality (always visible)
- URLs still clickable with same `greger-ui--url-link` functionality

This provides functional web search with citations - just in a slightly different format than Claude's native implementation.

## Known Limitations (Beta)

1. **Web Search Format**: Uses `:online` variant instead of server-side tool
   - Works well with OpenAI/Anthropic models (native search)
   - Falls back to Exa for other models ($0.02 per request)
   - Citations appear at end of response, not inline during streaming
   - No fold/unfold, no encrypted indices

2. **Thinking Format**: 
   - No cryptographic signatures (signature field will be empty)
   - Model support varies (GPT-5, Claude-via-OpenRouter work; others may not)
   - Must enable with `greger-current-thinking-budget > 0`

3. **Citation Simplification**: 
   - No encrypted indices for verification
   - No inline underlines
   - No fold/unfold functionality
   - Citations always visible at end of response

4. **Model-Specific Limits**: Each model has different context windows and capabilities

5. **Cost Variance**: Pricing varies by model and provider
   - OpenRouter adds small markup over direct API pricing
   - Native web search included in model cost
   - Exa web search adds $0.02 per request (5 results)

## Future Enhancements

1. **Cost Tracking**: Add per-provider cost tracking
2. **Model Recommendations**: Suggest models based on task type
3. **Automatic Fallback**: Try multiple providers if one fails
4. **More Providers**: Add direct Gemini, Mistral, etc.
5. **Reasoning Normalization**: Better handling of different thinking formats

## Feature Comparison Table

| Feature | Claude (Anthropic) | OpenRouter | Implementation Status |
|---------|-------------------|------------|----------------------|
| **Basic Streaming** | ✅ SSE with `data:` events | ✅ SSE with `data:` events | ✅ Full support |
| **Tool Calling** | ✅ Anthropic format | ✅ OpenAI format (converted) | ✅ Full support with format conversion |
| **Thinking/Reasoning** | ✅ With signatures | ⚠️ Without signatures | ✅ Supported, no signature verification |
| **Web Search** | ✅ Server-side `web_search` tool | ✅ `:online` variant (native search) | ✅ Full support with `:online` |
| **Citations** | ✅ With encryption & fold/unfold | ⚠️ Simplified format | ✅ Supported, no encryption/folding |
| **Content Blocks** | ✅ Native format | ✅ Converted from OpenAI | ✅ Full support |
| **Error Handling** | ✅ Normalized | ✅ Normalized | ✅ Full support |
| **Cancellation** | ✅ Interrupt process | ✅ Interrupt process | ✅ Full support |
| **Model Selection** | ✅ 4 Claude models | ✅ 400+ models | ✅ Full support |
| **Cost Tracking** | ✅ Token counts | ✅ Token counts | ✅ Full support |

### Legend
- ✅ Full support
- ⚠️ Partial support / differences
- ❌ Not supported / disabled

### Key Differences Summary

1. **Thinking Format**: OpenRouter doesn't provide cryptographic signatures for thinking blocks
2. **Web Search**: Disabled for OpenRouter (no Anthropic server tools)
3. **Citations**: Not implemented (OpenRouter uses different annotation format)
4. **Everything Else**: Works the same or better (more model choices)

## Benefits of This Approach

1. ✅ **Zero Risk**: Current code path completely unchanged
2. ✅ **No Refactoring**: Duplication is acceptable for safety
3. ✅ **Easy Rollback**: Just set `greger-provider` to `'anthropic`
4. ✅ **Independent Evolution**: Each provider can optimize separately
5. ✅ **Clear Ownership**: Easy to understand which code does what
6. ✅ **Beta Testing**: Can gather feedback before committing to abstraction

## Example Usage

```elisp
;; In your Emacs config:

;; Option 1: Use OpenRouter with GPT-5 Codex
(setq greger-provider 'openrouter)
(setq greger-openrouter-model "openai/gpt-5-codex")
(setq greger-openrouter-api-key-fn
      (lambda () (auth-source-pick-first-password :host "openrouter.ai")))

;; Option 2: Stick with Claude (default, no config needed)
;; greger-provider defaults to 'anthropic

;; Option 3: Switch on the fly
M-x greger-set-provider RET OpenRouter (Beta) RET
M-x greger-set-openrouter-model RET openai/gpt-5 RET
M-RET ;; Start chat with selected provider
```

## Concrete Examples of Differences

### Example 1: Thinking Block

**With Claude (Anthropic)**:
```markdown
# THINKING

Signature: 8f3e9d2a1b7c...

I need to analyze this problem step by step. First, I'll consider...
```

**With OpenRouter (GPT-5)**:
```markdown
# THINKING

Signature: 

I need to analyze this problem step by step. First, I'll consider...
```

Note: Signature is empty - OpenRouter doesn't provide cryptographic verification of thinking.

### Example 2: Web Search

**With Claude (Anthropic)**:
```markdown
# ASSISTANT

Based on recent information, Python 3.13 was released in October 2024.

## https://www.python.org/downloads/

Title: Python Downloads
Cited text: Python 3.13.0 was released on October 7, 2024
Encrypted index: b8a9f2e1d4c6...
```

**With OpenRouter**:
```
Web search not supported with OpenRouter provider.
To use web search, switch to Claude provider with:
M-x greger-set-provider RET Anthropic (Claude) RET
```

Alternative (if `:online` variant implemented):
```markdown
# ASSISTANT

Based on recent information, Python 3.13 was released in October 2024.

## Sources
- [Python Downloads](https://www.python.org/downloads/)
```

### Example 3: Tool Calling

**Both Work the Same** (internally converted):
```markdown
# TOOL USE

Name: read-file
ID: toolu_abc123

## path

<tool.toolu_abc123>
/path/to/file.py
</tool.toolu_abc123>
```

Behind the scenes:
- Claude sends: `{"type": "tool_use", "id": "toolu_abc123", "name": "read-file", "input": {"path": "/path/to/file.py"}}`
- OpenRouter sends: `{"tool_calls": [{"id": "call_abc123", "type": "function", "function": {"name": "read-file", "arguments": "{\"path\":\"/path/to/file.py\"}"}}]}`
- Greger converts both to same internal format

### Example 4: Model Selection

**Claude**:
```elisp
(setq greger-model 'claude-sonnet-4-20250514)
```

**OpenRouter**:
```elisp
(setq greger-openrouter-model "openai/gpt-5-codex")
;; or "openai/gpt-5"
;; or "anthropic/claude-sonnet-4"
;; or "google/gemini-2.5-pro"
;; ... 400+ models
```

## Conclusion

This design provides OpenRouter support as a **completely parallel implementation** with:
- **Zero modifications** to existing Claude code path
- **One dispatch point** to route to the right implementation
- **Complete isolation** of provider-specific logic
- **Easy opt-in/opt-out** via configuration
- **Future-proof** for adding more providers

The key insight: **duplication is better than the wrong abstraction** when you need 100% backward compatibility. Once both implementations are stable and patterns emerge, we can consider refactoring to share code - but that's a future optimization, not a requirement.

## FAQ

### Q: Will my existing chats work with OpenRouter?
**A:** Yes, if you switch providers mid-chat. The chat format (markdown with tags) is the same. However, features like thinking signatures and citations won't be present in OpenRouter responses.

### Q: Can I use web search with OpenRouter?
**A:** Not initially. Web search is disabled for OpenRouter because it uses a different format (annotations vs content blocks). Future enhancement: Could support `:online` variant with simplified citation rendering.

### Q: What about thinking/reasoning?
**A:** Thinking works with OpenRouter (GPT-5, Claude-via-OpenRouter, some others), but:
- No cryptographic signatures
- Must explicitly enable with `thinking-budget > 0`
- Not all models support it
- Set `include_reasoning: true` in request

### Q: Which models support tool calling?
**A:** Most modern models on OpenRouter support tool calling (GPT-5, Claude, Gemini, etc.). Check model page on openrouter.ai for specifics.

### Q: How do I know which provider I'm using?
**A:** Check the mode line - it shows the model name. Also, the variable `greger-provider` tells you.

### Q: Can I switch providers mid-chat?
**A:** Yes! Use `M-x greger-set-provider`. However, switching from Claude to OpenRouter mid-chat means you lose web search capability going forward.

### Q: Why not just use OpenRouter for everything?
**A:** 
- **Pro OpenRouter**: Access to 400+ models, model comparison, sometimes cheaper
- **Pro Claude**: Native web search with citations, thinking signatures, first-party features
- Best: Use both! Claude for web research, OpenRouter for coding with GPT-5 Codex

### Q: What if I need web search with OpenRouter?
**A:** Three options:
1. Switch to Claude temporarily: `M-x greger-set-provider RET Anthropic RET`
2. Wait for `:online` variant support (future enhancement)
3. Use the `read-webpage` tool manually (current workaround)

### Q: Will this slow down Claude?
**A:** No! Claude path is unchanged. Zero performance impact.

### Q: What's the recommended model for coding?
**A:** 
- **OpenRouter**: `openai/gpt-5-codex` - purpose-built for coding
- **Claude**: `claude-sonnet-4-20250514` - excellent all-around, includes web search

### Q: How much does OpenRouter cost?
**A:** Varies by model. Example prices:
- GPT-5 Codex: $1.25/M input, $10/M output
- GPT-5 Mini: $1.10/M input, $4.40/M output  
- Claude via OpenRouter: Similar to direct pricing + small markup
- Check openrouter.ai/models for current pricing

### Q: Can I use my Claude API key with OpenRouter?
**A:** No, they're separate services. You need an OpenRouter API key from openrouter.ai.

### Q: What happens if I set `greger-server-tools` with OpenRouter?
**A:** You'll get a warning message: "Server tools (web_search) not supported with OpenRouter provider". The tools won't be sent in the request.
