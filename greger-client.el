;;; greger-client.el --- Claude client for greger -*- lexical-binding: t -*-

;; Copyright (C) 2023 Andreas Jansson

;; Author: Andreas Jansson <andreas@jansson.me.uk>
;; Version: 0.1.0
;; URL: https://github.com/andreasjansson/greger.el
;; SPDX-License-Identifier: MIT

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:
;; Simplified Claude client supporting only claude-sonnet-4 and claude-opus-4

;;; Code:

(require 'json)
(require 'cl-lib)

;;; Constants and configuration

(defconst greger-client-supported-models
  '(claude-sonnet-4-20250514 claude-opus-4-20250514)
  "List of supported Claude models.")

(defconst greger-client-api-url "https://api.anthropic.com/v1/messages"
  "Claude API endpoint URL.")

;;; Data structures

(cl-defstruct greger-client-state
  accumulated-output
  content-blocks
  process
  output-buffer ;; used for undo handle
  undo-handle
  block-start-callback
  text-delta-callback
  block-stop-callback
  complete-callback
  restore-callback)

;;; Public API

(cl-defun greger-client-stream (&key model dialog tools server-tools buffer block-start-callback text-delta-callback block-stop-callback complete-callback)
  "Stream AI responses with callbacks for handling content types and updates.
MODEL specifies which AI model to use, DIALOG contains the conversation,
TOOLS and SERVER-TOOLS enable function calling, BUFFER is the output target.
BLOCK-START-CALLBACK is called when content blocks begin, TEXT-DELTA-CALLBACK
for incremental text, BLOCK-STOP-CALLBACK when blocks complete, and
COMPLETE-CALLBACK when the entire response finishes."
  (unless (memq model greger-client-supported-models)
    (error "Unsupported model: %s. Supported models: %s"
           model greger-client-supported-models))

  (let* ((output-buffer (or buffer (current-buffer)))
         (undo-handle (prepare-change-group output-buffer))
         (request-spec (greger-client--build-request model dialog tools server-tools))
         (restore-callback (lambda (state)
                             (let ((buffer (greger-client-state-output-buffer state)))
                               (when (buffer-live-p buffer)
                                 (with-current-buffer buffer
                                   (undo-amalgamate-change-group (greger-client-state-undo-handle state))
                                   (accept-change-group (greger-client-state-undo-handle state)))))))

         (process (greger-client--start-curl-process request-spec))
         (state (make-greger-client-state
                 :accumulated-output ""
                 :content-blocks '()
                 :process process
                 :block-start-callback block-start-callback
                 :text-delta-callback text-delta-callback
                 :block-stop-callback block-stop-callback
                 :complete-callback complete-callback
                 :restore-callback restore-callback
                 :output-buffer output-buffer
                 :undo-handle undo-handle)))

    (activate-change-group undo-handle)

    (set-process-filter process
                       (lambda (_proc output)
                         (greger-client--process-output-chunk output state)))

    (set-process-sentinel process
                         (lambda (proc _event)
                           (greger-client--handle-completion proc state)))

    (set-process-query-on-exit-flag process nil)

    state))

;;; Request building

(defun greger-client--build-request (model dialog &optional tools server-tools)
  "Build Claude request for MODEL with DIALOG and optional TOOLS and SERVER-TOOLS."
  (let* ((api-key (greger-client--get-api-key))
         (headers (greger-client--build-headers api-key))
         (data (greger-client--build-data model dialog tools server-tools)))
    (list :url greger-client-api-url
          :method "POST"
          :headers headers
          :data data)))

"Stream AI responses with real-time callbacks for DIALOG using MODEL.
Calls BLOCK-START-CALLBACK when new content blocks begin, TEXT-DELTA-CALLBACK
for incremental text updates, BLOCK-STOP-CALLBACK when blocks complete, and
COMPLETE-CALLBACK when the entire response finishes. TOOLS and SERVER-TOOLS
enable function calling capabilities."
  (defun greger-client--get-api-key ()
  "Get Claude API key from environment."
  (let ((api-key (getenv "ANTHROPIC_API_KEY")))
    (unless api-key
      (error "Please set the ANTHROPIC_API_KEY environment variable"))
    api-key))

(defun greger-client--build-headers (api-key)
  "Build headers for Claude with API-KEY."
  `(("Content-Type" . "application/json")
    ("x-api-key" . ,api-key)
    ("anthropic-version" . "2023-06-01")
    ("anthropic-beta" . "token-efficient-tools-2025-02-19")))

(defun greger-client--build-data (model dialog &optional tools server-tools)
  "Build request data for Claude MODEL with DIALOG and optional tools.
TOOLS and SERVER-TOOLS add function calling capabilities to the request."
  (let ((system-message nil)
        (user-messages ())
        (request-data nil))

    ;; Separate system messages from user/assistant messages
    (dolist (message dialog)
      (let ((role (alist-get 'role message))
            (content (alist-get 'content message)))
        (if (string= role "system")
            (unless system-message
              (setq system-message content))
          (push `((role . ,role)
                  (content . ,content))
                user-messages))))

    ;; Reverse to get correct order
    (setq user-messages (nreverse user-messages))

    ;; Find the last message with dict content and add ephemeral cache control
    (let ((last-dict-message nil))
      (dolist (message user-messages)
        (let ((content (alist-get 'content message)))
          (when (and (listp content) (not (stringp content)))
            (setq last-dict-message message))))

      (when last-dict-message
        (let ((content-list (alist-get 'content last-dict-message)))
          ;; Modify the first content item in place
          (when (and content-list (listp content-list))
            (let ((first-content-item (car content-list)))
              (when (and first-content-item (listp first-content-item))
                ;; Modify the car of the content-list directly
                (setcar content-list
                        (cons '(cache_control . ((type . "ephemeral")))
                              first-content-item))))))))

    ;; Build base request
    (setq request-data `(("model" . ,(symbol-name model))
                        ("messages" . ,user-messages)
                        ;("max_tokens" . 32000) ;; TODO: make this configurable
                        ("max_tokens" . 8000)
                        ("stream" . t)))

    ;; Add system message if present
    (when system-message
      (push `("system" . ,system-message) request-data))

    ;; Add tools if present
    (when (or tools server-tools)
      ;; TODO: why are we parsing server tools here?
      (let* ((parsed-server-tools (when server-tools
                                    (mapcar (lambda (json-string)
                                              (json-parse-string json-string :object-type 'alist))
                                            server-tools)))
             (all-tools (append (or tools '()) (or parsed-server-tools '()))))
        (push `("tools" . ,all-tools) request-data)
        (push `("tool_choice" . (("type" . "auto"))) request-data)))

    (json-encode request-data)))

;;; Stream processing

(defun greger-client--check-for-error (output)
  "Check OUTPUT for error responses and raise an error if found.
Returns nil if no error found or if OUTPUT is not valid JSON."
  (condition-case nil
      (let ((data (json-read-from-string output)))
        (when (and (listp data)
                   (string= (alist-get 'type data) "error"))
          (let* ((error-info (alist-get 'error data))
                 (error-message (alist-get 'message error-info))
                 (error-type (alist-get 'type error-info)))
            (error "API Error (%s): %s" error-type error-message))))
    (json-error nil)
    (json-readtable-error nil)))

(defun greger-client--process-output-chunk (output state)
  "Process a chunk of OUTPUT using STATE."

  ;; TODO: remove debug
  ;(message "output: %s" output)

  ;; Check for error responses and raise an error if found
  (greger-client--check-for-error output)

  ;; Update working buffer for chunk processing
  (setf (greger-client-state-accumulated-output state)
        (concat (greger-client-state-accumulated-output state) output))

  (greger-client--process-events state))

(defun greger-client--process-events (state)
  "Process Claude streaming events from accumulated output in STATE."
  (let ((accumulated (greger-client-state-accumulated-output state)))

    ;; Process complete lines (events)
    (while (string-match "\n" accumulated)
      (let* ((line-end (match-end 0))
             (line (substring accumulated 0 (1- line-end))))

        ;; Process the line if it's a data event
        (when (string-prefix-p "data: " line)
          (let ((data-json (substring line 6)))
            (unless (string= data-json "[DONE]")
              (greger-client--handle-event data-json state))))

        ;; Remove processed line
        (setq accumulated (substring accumulated line-end))))

    ;; Store remaining incomplete data
    (setf (greger-client-state-accumulated-output state) accumulated)))

(defun greger-client--handle-event (data-json state)
  "Handle a Claude streaming event with DATA-JSON using STATE."
  (let* ((data (json-read-from-string data-json))
         (type (alist-get 'type data)))
    (cond
     ;; Content block start - create new content block
     ((string= type "content_block_start")
      (greger-client--handle-content-block-start data state))

     ;; Content block delta - update existing content block
     ((string= type "content_block_delta")
      (greger-client--handle-content-block-delta data state))

     ;; Content block stop - finalize tool input if needed
     ((string= type "content_block_stop")
      (greger-client--handle-content-block-stop data state)))))

(defun greger-client--handle-content-block-start (data state)
  "Initialize new streaming content block from DATA in STATE."
  (let* ((index (alist-get 'index data))
         (content-block (copy-alist (alist-get 'content_block data)))
         (blocks (greger-client-state-content-blocks state))
         (type (alist-get 'type content-block))
         (citations (alist-get 'citations content-block)))

    ;; Initialize content for accumulation.
    ;; For tool_use and server_tool_use we make the input object a
    ;; string while we accumulate the output, and turn it back into
    ;; an object again in greger-client--handle-content-stop
    (cond
     ((string= type "tool_use")
      (setf (alist-get 'input content-block) ""))

     ;; {"type":"content_block_start","index":1,"content_block":{"type":"server_tool_use","id":"srvtoolu_01AZ1324bmQ29XW4fSECQJWH","name":"web_search","input":{}}}
     ((string= type "server_tool_use")
      (setf (alist-get 'input content-block) ""))

     ;; {"type":"content_block_start","index":0,"content_block":{"type":"text","text":""}}
     ;; {"type":"content_block_start","index":3,"content_block":{"citations":[],"type":"text","text":""}}
     ((string= type "text")
      (setf (alist-get 'text content-block) "")
      ;; For text blocks with citations, initialize citations as empty list
      (when citations
        (setf (alist-get 'citations content-block) '())))

     ;; {"type":"content_block_start","index":2,"content_block":{"type":"web_search_tool_result","tool_use_id":"srvtoolu_01AZ1324bmQ29XW4fSECQJWH","content":[{"type":"web_search_result","title":"Sweden Population (2025) - Worldometer","url":"https://www.worldometers.info/world-population/sweden-population/","encrypted_content":"Ev0P...YMYAw==","page_age":null}, [...] ]}}
     ((string= type "web_search_tool_result")
      ;; No initialization needed - content is already present
      nil))

    (when-let ((callback (greger-client-state-block-start-callback state)))
      (funcall callback content-block))

    ;; Add block at the right index
    (greger-client--ensure-block-at-index blocks index content-block state)))

(defun greger-client--handle-content-block-delta (data state)
  "Process incremental content updates from streaming DATA in STATE."
  (let* ((index (alist-get 'index data))
         (delta (alist-get 'delta data))
         (delta-type (alist-get 'type delta))
         (blocks (greger-client-state-content-blocks state))
         (block (nth index blocks)))

    ;; TODO: do we need to handle content block stop out-of-order,
    ;; before content-block start has created the block in the state's content-blocks?

    (cond

     ;; assistant text and thinking
     ;; {"type":"content_block_delta","index":0,"delta":{"type":"text_delta","text":"I'll search for the"}}
     ((string= delta-type "text_delta")
      (let ((text (alist-get 'text delta))
            (has-citations (alist-get 'citations block)))
        (setf (alist-get 'text block)
              (concat (alist-get 'text block) text))
        ;; Only call text callback for live display if this block doesn't have citations
        ;; Citation blocks should not stream text - they'll be handled in block-stop
        (unless has-citations
          (when-let ((callback (greger-client-state-text-delta-callback state)))
           (funcall callback text)))))

     ;; tool_use and server_tool_use
     ;; {"type":"content_block_delta","index":1,"delta":{"type":"input_json_delta","partial_json":""}}
     ((string= delta-type "input_json_delta")
      (let ((partial-json (alist-get 'partial_json delta)))
        (setf (alist-get 'input block)
              (concat (alist-get 'input block) partial-json))))

     ;; Citations accumulation
     ;; {"type":"content_block_delta","index":3,"delta":{"type":"citations_delta","citation":{"type":"web_search_result_location",...}}}
     ((string= delta-type "citations_delta")
      (let ((citation (alist-get 'citation delta))
            (current-citations (alist-get 'citations block)))
        ;; Add the new citation to the list
        (setf (alist-get 'citations block)
              (append current-citations (list citation))))))))

"Initialize a new streaming content block at INDEX from DATA.
Sets up accumulation buffers for tool calls and converts structured input
to strings for incremental building during streaming."
  "Finalize content block at INDEX when streaming stops.
Converts accumulated tool call strings back to structured objects and
triggers completion callbacks for the finished block."
  (defun greger-client--handle-content-block-stop (data state)
  (let* ((index (alist-get 'index data))
         (blocks (greger-client-state-content-blocks state))
         (block (nth index blocks))
         (type (alist-get 'type block)))

    ;; TODO: do we need to handle content block stop out-of-order,
    ;; before content-block start has created the block in the state's content-blocks?

    (cond
     ;; Handle tool use blocks - turn accumulated JSON string back into an object
     ((or (string= type "tool_use") (string= type "server_tool_use"))
      (let ((input-str (alist-get 'input block)))
        (if (string-empty-p input-str)
            (setf (alist-get 'input block) '())
          (setf (alist-get 'input block) (json-read-from-string input-str))))))

    (when-let ((callback (greger-client-state-block-stop-callback state)))
      (funcall callback type block))))

(defun greger-client--ensure-block-at-index (_blocks index new-block state)
  "Ensure BLOCKS list has NEW-BLOCK at INDEX, extending if necessary.
STATE is used to update the parsed content blocks."
  (let ((current-blocks (greger-client-state-content-blocks state)))
    ;; Extend list if needed
    (while (<= (length current-blocks) index)
      (setq current-blocks (append current-blocks (list nil))))

    ;; Set the block at index
    (setf (nth index current-blocks) new-block)
    (setf (greger-client-state-content-blocks state) current-blocks)))

(defun greger-client--handle-completion (proc state)
  "Handle process completion for PROC using STATE."
  (when (memq (process-status proc) '(exit signal))
    (funcall (greger-client-state-restore-callback state) state)

    (if (= (process-exit-status proc) 0)
        (when-let ((callback (greger-client-state-complete-callback state)))
          (funcall callback (greger-client-state-content-blocks state)))
      ;; TODO: Callback
      (message "Process exited"))))

(defun greger-client--cancel-request (state)
  "Cancel streaming request using STATE."
  (let ((process (greger-client-state-process state)))
    (when (process-live-p process)
      (message "Interrupting generation")
      (interrupt-process process)
      (sit-for 0.1)
      (delete-process process))
    (funcall (greger-client-state-restore-callback state) state)))

;;; Utility functions

(defun greger-client--start-curl-process (request-spec)
  "Start curl process with REQUEST-SPEC."
  (start-process-shell-command
   "greger-curl" nil
   (greger-client--build-curl-command request-spec)))

(defun greger-client--build-curl-command (request-spec)
  "Build curl command from REQUEST-SPEC."
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

(provide 'greger-client)

;;; greger-client.el ends here
