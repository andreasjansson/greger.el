;;; greger-client.el --- Claude client for greger -*- lexical-binding: t -*-

;; Author: Andreas Jansson <andreas@jansson.me.uk>
;; Version: 0.1.0
;; URL: https://github.com/andreasjansson/greger.el

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
  complete-response
  parsed-content-blocks
  process
  output-buffer
  undo-handle
  original-quit-binding
  text-start-callback
  text-callback
  complete-callback
  cancel-callback
  restore-callback)

;;; Public API

(cl-defun greger-client-stream (&key model dialog tools buffer text-start-callback text-callback complete-callback cancel-callback)
  "Send streaming request for MODEL with DIALOG and TOOLS.
Text is inserted into BUFFER.
TEXT-START-CALLBACK is called when text streaming starts.
TEXT-CALLBACK is called for each text chunk with (text).
COMPLETE-CALLBACK is called when done with the parsed content blocks array.
CANCEL-CALLBACK is called if cancelled.
BUFFER defaults to current buffer if not specified.

MODEL should be one of the supported Claude models:
claude-sonnet-4-20250514 or claude-opus-4-20250514."
  (unless (memq model greger-client-supported-models)
    (error "Unsupported model: %s. Supported models: %s"
           model greger-client-supported-models))

  (let* ((output-buffer (or buffer (current-buffer)))
         (undo-handle (prepare-change-group output-buffer))
         (original-quit-binding (local-key-binding (kbd "C-g")))
         (request-spec (greger-client--build-request model dialog tools))
         (restore-callback (lambda (state)
                             (with-current-buffer (greger-client-state-output-buffer state)
                               (local-set-key (kbd "C-g")
                                              (greger-client-state-original-quit-binding state))
                               (undo-amalgamate-change-group (greger-client-state-undo-handle state))
                               (accept-change-group (greger-client-state-undo-handle state)))))
         (wrapped-complete-callback (lambda (parsed-blocks _state)
                                      (when complete-callback
                                        (funcall complete-callback parsed-blocks))))
         (process (greger-client--start-curl-process request-spec))
         (state (make-greger-client-state
                 :accumulated-output ""
                 :complete-response ""
                 :parsed-content-blocks '()
                 :process process
                 :text-start-callback text-start-callback
                 :text-callback text-callback
                 :complete-callback wrapped-complete-callback
                 :cancel-callback cancel-callback
                 :restore-callback restore-callback
                 :output-buffer output-buffer
                 :undo-handle undo-handle
                 :original-quit-binding original-quit-binding)))

    (activate-change-group undo-handle)

    (set-process-filter process
                       (lambda (_proc output)
                         (greger-client--process-output-chunk output state)))

    (set-process-sentinel process
                         (lambda (proc _event)
                           (greger-client--handle-completion proc state)))

    (set-process-query-on-exit-flag process nil)

    (greger-client--setup-cancel-binding state)

    state))

;;; Request building

(defun greger-client--build-request (model dialog &optional tools)
  "Build Claude request for MODEL with DIALOG and optional TOOLS."
  (let* ((api-key (greger-client--get-api-key))
         (headers (greger-client--build-headers api-key))
         (data (greger-client--build-data model dialog tools)))
    (list :url greger-client-api-url
          :method "POST"
          :headers headers
          :data data)))

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

(defun greger-client--build-data (model dialog &optional tools)
  "Build request data for Claude MODEL with DIALOG and optional TOOLS."
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
                        ("max_tokens" . 64000)
                        ("stream" . t)))

    ;; Add system message if present
    (when system-message
      (push `("system" . ,system-message) request-data))

    ;; Add tools if present
    (when tools
      (push `("tools" . ,tools) request-data)
      (push `("tool_choice" . (("type" . "auto"))) request-data))

    (json-encode request-data)))

;;; Stream processing

(defun greger-client--setup-cancel-binding (state)
  "Setup \\[keyboard-quit] binding for cancellation in the output buffer.
STATE contains the client state information."
  (with-current-buffer (greger-client-state-output-buffer state)
    (local-set-key (kbd "C-g")
                   (lambda ()
                     (interactive)
                     (greger-client--cancel-request state)))))

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
  ;; Check for error responses and raise an error if found
  (greger-client--check-for-error output)

  ;; Always accumulate for complete response
  (setf (greger-client-state-complete-response state)
        (concat (greger-client-state-complete-response state) output))

  ;; Update working buffer for chunk processing
  (setf (greger-client-state-accumulated-output state)
        (concat (greger-client-state-accumulated-output state) output))

  (greger-client--process-claude-events state))

(defun greger-client--process-claude-events (state)
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
              (greger-client--handle-claude-event data-json state))))

        ;; Remove processed line
        (setq accumulated (substring accumulated line-end))))

    ;; Store remaining incomplete data
    (setf (greger-client-state-accumulated-output state) accumulated)))

(defun greger-client--handle-claude-event (data-json state)
  "Handle a Claude streaming event with DATA-JSON using STATE.

Example of incoming data json (one data-json per line):
{\"type\":\"message_start\",\"message\":{\"id\":\"msg_01Qm7bzEMGbdRyAuF5Lrb1Tg\",
\"type\":\"message\",\"role\":\"assistant\",\"model\":\"claude-sonnet-4-20250514\",
\"content\":[],\"stop_reason\":null,\"stop_sequence\":null,
\"usage\":{\"input_tokens\":2626,\"cache_creation_input_tokens\":0,
\"cache_read_input_tokens\":0,\"output_tokens\":1,\"service_tier\":\"standard\"}}}
{\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"text\",\"text\":\"\"}}
{\"type\": \"ping\"}
{\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"text_delta\",\"text\":\"I\"}}
{\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"text_delta\",\"text\":\"'ll first read the existing file to see what's already there,\"}}
{\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"text_delta\",\"text\":\" then add a new function in the same style.\"}}
{\"type\":\"content_block_stop\",\"index\":0}
{\"type\":\"content_block_start\",\"index\":1,\"content_block\":{\"type\":\"tool_use\",\"id\":\"toolu_01NmTNDZcJdGAMsrWQy1Heff\",\"name\":\"read-file\",\"input\":{}}}
{\"type\":\"content_block_delta\",\"index\":1,\"delta\":{\"type\":\"input_json_delta\",\"partial_json\":\"\"}}
{\"type\":\"content_block_delta\",\"index\":1,\"delta\":{\"type\":\"input_json_delta\",\"partial_json\":\"{\\\"\"}}
{\"type\":\"content_block_delta\",\"index\":1,\"delta\":{\"type\":\"input_json_delta\",\"partial_json\":\"path\\\": \\\"~/s\"}}
{\"type\":\"content_block_delta\",\"index\":1,\"delta\":{\"type\":\"input_json_delta\",\"partial_json\":\"cratch/aicha\"}}
{\"type\":\"content_block_delta\",\"index\":1,\"delta\":{\"type\":\"input_json_delta\",\"partial_json\":\"t/hel\"}}
{\"type\":\"content_block_delta\",\"index\":1,\"delta\":{\"type\":\"input_json_delta\",\"partial_json\":\"lo.\"}}
{\"type\":\"content_block_delta\",\"index\":1,\"delta\":{\"type\":\"input_json_delta\",\"partial_json\":\"py\\\"\"}}
{\"type\":\"content_block_delta\",\"index\":1,\"delta\":{\"type\":\"input_json_delta\",\"partial_json\":\", \\\"includ\"}}
{\"type\":\"content_block_delta\",\"index\":1,\"delta\":{\"type\":\"input_json_delta\",\"partial_json\":\"e_li\"}}
{\"type\":\"content_block_delta\",\"index\":1,\"delta\":{\"type\":\"input_json_delta\",\"partial_json\":\"ne_numb\"}}
{\"type\":\"content_block_delta\",\"index\":1,\"delta\":{\"type\":\"input_json_delta\",\"partial_json\":\"ers\\\": true}\"}}
{\"type\":\"content_block_stop\",\"index\":1}
{\"type\":\"message_delta\",\"delta\":{\"stop_reason\":\"tool_use\",\"stop_sequence\":null},\"usage\":{\"output_tokens\":108}}
{\"type\":\"message_stop\"}"
  (let* ((data (json-read-from-string data-json))
         (type (alist-get 'type data)))
    (cond
     ;; Content block start - create new content block
     ((string= type "content_block_start")
      (let* ((index (alist-get 'index data))
             (content-block (copy-alist (alist-get 'content_block data)))
             (blocks (greger-client-state-parsed-content-blocks state)))

        ;; Initialize content for accumulation
        (cond
         ((string= (alist-get 'type content-block) "tool_use")
          (setf (alist-get 'input content-block) ""))
         ((string= (alist-get 'type content-block) "text")
          (setf (alist-get 'text content-block) "")))

        (when (and (string= (alist-get 'type content-block) "text")
                   (greger-client-state-text-start-callback state))
          (funcall (greger-client-state-text-start-callback state)))

        ;; Add block at the right index
        (greger-client--ensure-block-at-index blocks index content-block state)))

     ;; Content block delta - update existing content block
     ((string= type "content_block_delta")
      (let* ((index (alist-get 'index data))
             (delta (alist-get 'delta data))
             (delta-type (alist-get 'type delta))
             (blocks (greger-client-state-parsed-content-blocks state)))

        (when (< index (length blocks))
          (let ((block (nth index blocks)))
            (cond
             ;; Text delta
             ((string= delta-type "text_delta")
              (let ((text (alist-get 'text delta)))
                (setf (alist-get 'text block)
                      (concat (alist-get 'text block) text))
                ;; Call text callback for live display
                (when (greger-client-state-text-callback state)
                  (funcall (greger-client-state-text-callback state) text))))

             ;; Tool input delta
             ((string= delta-type "input_json_delta")
              (let ((partial-json (alist-get 'partial_json delta)))
                (setf (alist-get 'input block)
                      (concat (alist-get 'input block) partial-json)))))))))

     ;; Content block stop - finalize tool input if needed
     ((string= type "content_block_stop")
      (let* ((index (alist-get 'index data))
             (blocks (greger-client-state-parsed-content-blocks state)))

        (when (< index (length blocks))
          (let ((block (nth index blocks)))
            (when (and (string= (alist-get 'type block) "tool_use")
                       (stringp (alist-get 'input block)))
              ;; Parse accumulated JSON input
              (let ((input-str (alist-get 'input block)))
                (condition-case nil
                    (if (string-empty-p input-str)
                        (setf (alist-get 'input block) '())
                      (setf (alist-get 'input block)
                            (json-read-from-string input-str)))
                  (error
                   (setf (alist-get 'input block) '()))))))))))))

(defun greger-client--ensure-block-at-index (_blocks index new-block state)
  "Ensure BLOCKS list has NEW-BLOCK at INDEX, extending if necessary.
STATE is used to update the parsed content blocks."
  (let ((current-blocks (greger-client-state-parsed-content-blocks state)))
    ;; Extend list if needed
    (while (<= (length current-blocks) index)
      (setq current-blocks (append current-blocks (list nil))))

    ;; Set the block at index
    (setf (nth index current-blocks) new-block)
    (setf (greger-client-state-parsed-content-blocks state) current-blocks)))

(defun greger-client--handle-completion (proc state)
  "Handle process completion for PROC using STATE."
  (when (memq (process-status proc) '(exit signal))
    (funcall (greger-client-state-restore-callback state) state)

    (if (= (process-exit-status proc) 0)
        (when (greger-client-state-complete-callback state)
          (let ((parsed-blocks (greger-client-state-parsed-content-blocks state)))
            (funcall (greger-client-state-complete-callback state) parsed-blocks state)))
      (when (greger-client-state-cancel-callback state)
        (funcall (greger-client-state-cancel-callback state))))))

(defun greger-client--cancel-request (state)
  "Cancel streaming request using STATE."
  (let ((process (greger-client-state-process state)))
    (when (process-live-p process)
      (message "Interrupting generation")
      (interrupt-process process)
      (sit-for 0.1)
      (delete-process process)
      (when (greger-client-state-cancel-callback state)
        (funcall (greger-client-state-cancel-callback state))))
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
