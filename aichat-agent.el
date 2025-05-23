;;; aichat-agent.el --- Agent capabilities for aichat -*- lexical-binding: t -*-

;;; Commentary:
;; Provides agent functionality with tool support for aichat

;;; Code:

(require 'json)
(require 'aichat-stream)
(require 'aichat-tools)

(defcustom aichat-agent-tools '(read-file write-file list-directory)
  "List of tools available to the agent."
  :type '(repeat symbol)
  :group 'aichat)

(defcustom aichat-agent-max-iterations 10
  "Maximum number of agent iterations before stopping."
  :type 'integer
  :group 'aichat)

(defcustom aichat-agent-auto-approve t
  "Whether to automatically approve tool calls without user confirmation."
  :type 'boolean
  :group 'aichat)

(defcustom aichat-agent-debug t
  "Whether to show debug information."
  :type 'boolean
  :group 'aichat)

(defvar aichat-agent--current-iteration 0
  "Current iteration count for the active agent session.")

(defvar aichat-agent--current-dialog nil
  "Current dialog being processed.")

(defun aichat-agent-buffer ()
  "Send buffer content to AI as an agent dialog with tool support."
  (interactive)
  (let* ((buffer-content (buffer-substring-no-properties (point-min) (point-max)))
         (dialog (aichat-parser-parse-dialog buffer-content)))
    (unless dialog
      (error "Failed to parse dialog. Did you forget to close a html tag?"))

    (goto-char (point-max))
    (aichat--maybe-insert-assistant-tag)

    (setq aichat-agent--current-iteration 0)
    (setq aichat-agent--current-dialog (aichat-agent--enhance-dialog-with-system-prompt dialog))

    (aichat-agent--debug "=== STARTING AGENT SESSION ===")

    (aichat-agent--run-agent-loop)))

(defun aichat-agent--debug (format-string &rest args)
  "Debug logging function."
  (when aichat-agent-debug
    (message "[AGENT DEBUG] %s" (apply #'format format-string args))))

(defun aichat-agent--run-agent-loop ()
  "Run the main agent loop, similar to toololo's approach."
  (let ((tools (aichat-tools-get-schemas aichat-agent-tools)))

    (aichat-agent--debug "=== ITERATION %d ===" aichat-agent--current-iteration)
    (aichat-agent--debug "Dialog length: %d messages" (length aichat-agent--current-dialog))

    ;; Log the COMPLETE dialog being sent to Claude
    (aichat-agent--debug "===== COMPLETE DIALOG BEING SENT TO CLAUDE =====")
    (dolist (msg aichat-agent--current-dialog)
      (aichat-agent--debug "MESSAGE TYPE: %s" (car msg))
      (aichat-agent--debug "MESSAGE CONTENT: %s" (cdr msg))
      (aichat-agent--debug "---"))
    (aichat-agent--debug "===== END DIALOG =====")

    ;; Check max iterations
    (if (>= aichat-agent--current-iteration aichat-agent-max-iterations)
        (progn
          (aichat-agent--debug "MAX ITERATIONS REACHED - STOPPING")
          (insert (format "\n\nMaximum iterations (%d) reached. Stopping agent execution.\n\n"
                         aichat-agent-max-iterations))
          (aichat-agent--finish-response))

      ;; Get Claude's response
      (aichat-agent--debug "CALLING aichat-stream-to-buffer-with-tools...")
      (aichat-stream-to-buffer-with-tools
       aichat-model aichat-agent--current-dialog tools
       (lambda (complete-response)
         (aichat-agent--debug "RECEIVED COMPLETE RESPONSE")
         (aichat-agent--handle-response complete-response))))))

(defun aichat-agent--enhance-dialog-with-system-prompt (dialog)
  "Replace or add agent system prompt to DIALOG."
  ;; Remove any existing system message and add our agent system prompt
  (let ((non-system-messages (seq-filter (lambda (msg) (not (eq (car msg) 'system))) dialog)))
    (cons `(system . ,(aichat-agent--get-system-prompt)) non-system-messages)))

(defun aichat-agent--get-system-prompt ()
  "Get the system prompt for agent capabilities."
  (format "You are a helpful assistant with access to file system tools. Available tools: %s

IMPORTANT: Use tools only when they would genuinely help answer the user's question. DO NOT call the same tool repeatedly with the same parameters.

When you do use tools:
1. Use them efficiently - if you need multiple files, call multiple tools in one response
2. Always explain what you found and provide a final answer to the user
3. DO NOT repeat tool calls you've already made - check the conversation history first

If you've already gotten the information you need from previous tool calls in this conversation, use that information to answer the user directly WITHOUT calling more tools.

For a simple 'ls' command, call list-directory ONCE and then provide the formatted output."
          (mapconcat #'symbol-name aichat-agent-tools ", ")))

(defun aichat-agent--handle-response (complete-response)
  "Handle the complete streaming response from Claude."
  (aichat-agent--debug "RESPONSE LENGTH: %d" (length complete-response))

  (condition-case err
      (let* ((parsed-response (aichat-agent--parse-streaming-response complete-response))
             (content (alist-get 'content parsed-response))
             (stop-reason (alist-get 'stop_reason parsed-response)))

        (aichat-agent--debug "PARSED SUCCESSFULLY")
        (aichat-agent--debug "STOP REASON: %s" stop-reason)
        (aichat-agent--debug "CONTENT BLOCKS: %d" (if (vectorp content) (length content) 0))

        ;; Log all content blocks
        (when (vectorp content)
          (dotimes (i (length content))
            (let ((block (aref content i)))
              (aichat-agent--debug "CONTENT BLOCK %d: type=%s" i (alist-get 'type block))
              (cond
               ((string= (alist-get 'type block) "text")
                (aichat-agent--debug "  TEXT: %s" (alist-get 'text block)))
               ((string= (alist-get 'type block) "tool_use")
                (aichat-agent--debug "  TOOL: %s INPUT: %s"
                                    (alist-get 'name block)
                                    (json-encode (alist-get 'input block))))))))

        ;; Add assistant message to dialog
        (let ((assistant-content (aichat-agent--format-assistant-content content)))
          (aichat-agent--debug "ADDING ASSISTANT MESSAGE TO DIALOG")
          (setq aichat-agent--current-dialog
                (append aichat-agent--current-dialog `((assistant . ,assistant-content)))))

        ;; Check if we have tool calls
        (if (string= stop-reason "tool_use")
            (let ((tool-calls (aichat-agent--extract-tool-calls content)))
              (aichat-agent--debug "TOOL USE DETECTED! Found %d tool calls" (length tool-calls))
              (if tool-calls
                  (progn
                    (setq aichat-agent--current-iteration (1+ aichat-agent--current-iteration))
                    (aichat-agent--execute-tools tool-calls))
                (progn
                  (aichat-agent--debug "NO TOOL CALLS FOUND DESPITE tool_use STOP REASON!")
                  (aichat-agent--finish-response))))
          (progn
            (aichat-agent--debug "NO TOOL USE - CONVERSATION COMPLETE")
            (aichat-agent--finish-response))))
    (error
     (aichat-agent--debug "ERROR HANDLING RESPONSE: %s" (error-message-string err))
     (insert (format "\n\n**Debug: Parse Error**\n```\n%s\n```\n\n" (error-message-string err)))
     (aichat-agent--finish-response))))

(defun aichat-agent--format-assistant-content (content)
  "Format assistant CONTENT for the dialog."
  (let ((formatted-content (mapcar (lambda (block)
                                   (cond
                                    ((string= (alist-get 'type block) "text")
                                     `((type . "text")
                                       (text . ,(alist-get 'text block))))
                                    ((string= (alist-get 'type block) "tool_use")
                                     `((type . "tool_use")
                                       (id . ,(alist-get 'id block))
                                       (name . ,(alist-get 'name block))
                                       (input . ,(alist-get 'input block))))
                                    (t block)))
                                 content)))
    (json-encode formatted-content)))

(defun aichat-agent--extract-tool-calls (content)
  "Extract tool calls from CONTENT."
  (let ((tool-calls '()))
    (seq-doseq (block content)
      (when (string= (alist-get 'type block) "tool_use")
        (aichat-agent--debug "EXTRACTING TOOL CALL: %s with input: %s"
                            (alist-get 'name block)
                            (json-encode (alist-get 'input block)))
        (push block tool-calls)))
    (reverse tool-calls)))

(defun aichat-agent--parse-streaming-response (streaming-response)
  "Parse streaming response to extract the final message data."
  (let ((content-blocks '())
        (stop-reason nil))

    (aichat-agent--debug "PARSING STREAMING RESPONSE...")

    ;; Split into lines and process each event
    (dolist (line (split-string streaming-response "\n"))
      (when (string-prefix-p "data: " line)
        (let ((data-json (substring line 6)))
          (unless (string= data-json "[DONE]")
            (condition-case nil
                (let* ((data (json-read-from-string data-json))
                       (type (alist-get 'type data)))
                  (cond
                   ;; Content block start - new content block
                   ((string= type "content_block_start")
                    (let ((content-block (alist-get 'content_block data)))
                      (aichat-agent--debug "CONTENT BLOCK START: %s" (alist-get 'type content-block))
                      ;; Initialize input as empty string for tool_use blocks
                      (when (string= (alist-get 'type content-block) "tool_use")
                        (push '(input . "") content-block))
                      ;; Initialize text as empty string for text blocks
                      (when (string= (alist-get 'type content-block) "text")
                        (push '(text . "") content-block))
                      (push content-block content-blocks)))

                   ;; Content block delta - updates to content block
                   ((string= type "content_block_delta")
                    (let* ((index (alist-get 'index data))
                           (delta (alist-get 'delta data))
                           (delta-type (alist-get 'type delta)))
                      (when (< index (length content-blocks))
                        (let ((block (nth (- (length content-blocks) index 1) content-blocks)))
                          (cond
                           ;; Text delta
                           ((string= delta-type "text_delta")
                            (let ((text (alist-get 'text delta)))
                              (setcdr (assoc 'text block)
                                     (concat (alist-get 'text block) text))))
                           ;; Tool input delta
                           ((string= delta-type "input_json_delta")
                            (let ((partial-json (alist-get 'partial_json delta)))
                              (aichat-agent--debug "ACCUMULATING INPUT JSON: '%s'" partial-json)
                              (setcdr (assoc 'input block)
                                     (concat (alist-get 'input block) partial-json)))))))))

                   ;; Message delta contains stop reason
                   ((string= type "message_delta")
                    (let ((delta (alist-get 'delta data)))
                      (setq stop-reason (alist-get 'stop_reason delta))
                      (aichat-agent--debug "MESSAGE DELTA: stop_reason=%s" stop-reason)))))
              (error
               (aichat-agent--debug "ERROR PARSING STREAM EVENT: %s" data-json)))))))

    ;; Parse accumulated input JSON in tool_use blocks
    (setq content-blocks
          (mapcar (lambda (block)
                    (if (and (string= (alist-get 'type block) "tool_use")
                            (alist-get 'input block)
                            (stringp (alist-get 'input block)))
                        (let ((input-str (alist-get 'input block)))
                          (aichat-agent--debug "PARSING TOOL INPUT JSON: '%s'" input-str)
                          (condition-case nil
                              (if (string-empty-p input-str)
                                  ;; Empty input becomes empty object
                                  (progn
                                    (setcdr (assoc 'input block) '())
                                    block)
                                ;; Parse the JSON
                                (let ((parsed-input (json-read-from-string input-str)))
                                  (setcdr (assoc 'input block) parsed-input)
                                  block))
                            (error
                             (aichat-agent--debug "ERROR PARSING TOOL INPUT JSON: %s" input-str)
                             ;; On error, set to empty object
                             (setcdr (assoc 'input block) '())
                             block)))
                      block))
                  (reverse content-blocks)))

    (aichat-agent--debug "FINAL PARSED CONTENT: %d blocks" (length content-blocks))

    ;; Return the parsed message format
    `((content . ,(apply #'vector content-blocks))
      (stop_reason . ,stop-reason))))

(defun aichat-agent--execute-tools (tool-calls)
  "Execute TOOL-CALLS and add results to dialog."
  (aichat-agent--debug "=== EXECUTING TOOLS ===")
  (aichat-agent--debug "NUMBER OF TOOL CALLS: %d" (length tool-calls))

  (let ((tool-results '())
        (approved-calls '()))

    ;; Process each tool call
    (dolist (tool-call tool-calls)
      (let* ((tool-name (alist-get 'name tool-call))
             (tool-id (alist-get 'id tool-call))
             (tool-input (alist-get 'input tool-call))
             (approval (aichat-agent--request-approval tool-name tool-input)))

        (aichat-agent--debug "PROCESSING TOOL: %s (id: %s) input: %s approved: %s"
                            tool-name tool-id (json-encode tool-input) approval)

        (if approval
            (progn
              (push tool-call approved-calls)
              (condition-case err
                  (let ((result (aichat-tools-execute tool-name tool-input)))
                    (aichat-agent--debug "TOOL EXECUTION SUCCESS: result length=%d" (length result))
                    (push `((type . "tool_result")
                           (tool_use_id . ,tool-id)
                           (content . ,result)) tool-results))
                (error
                 (aichat-agent--debug "TOOL EXECUTION ERROR: %s" (error-message-string err))
                 (push `((type . "tool_result")
                        (tool_use_id . ,tool-id)
                        (content . ,(format "Error: %s" (error-message-string err))))
                       tool-results))))
          (progn
            (aichat-agent--debug "TOOL CALL NOT APPROVED")
            (push `((type . "tool_result")
                   (tool_use_id . ,tool-id)
                   (content . "Tool call was not approved by user")) tool-results)))))

    ;; Display tool execution
    (aichat-agent--display-tool-execution approved-calls tool-results)

    ;; Add tool results to dialog
    (let ((user-content (json-encode (reverse tool-results))))
      (aichat-agent--debug "ADDING TOOL RESULTS TO DIALOG")
      (aichat-agent--debug "USER CONTENT: %s" user-content)
      (setq aichat-agent--current-dialog
            (append aichat-agent--current-dialog `((user . ,user-content)))))

    (aichat-agent--debug "DIALOG NOW HAS %d MESSAGES" (length aichat-agent--current-dialog))

    ;; Continue the agent loop
    (insert "\n\n")
    (aichat-agent--debug "CONTINUING AGENT LOOP...")
    (aichat-agent--run-agent-loop)))

(defun aichat-agent--request-approval (tool-name tool-input)
  "Request user approval for TOOL-NAME with TOOL-INPUT."
  (if aichat-agent-auto-approve
      t
    (let ((args-str (if tool-input (json-encode tool-input) "{}")))
      (y-or-n-p (format "Execute %s with args %s? " tool-name args-str)))))

(defun aichat-agent--display-tool-execution (tool-calls results)
  "Display the execution of TOOL-CALLS and their RESULTS."
  (insert "\n\n🔧 **Tool Execution:**\n\n")
  (dolist (tool-call tool-calls)
    (let* ((tool-name (alist-get 'name tool-call))
           (tool-input (alist-get 'input tool-call))
           (tool-id (alist-get 'id tool-call))
           (result (seq-find (lambda (r)
                              (string= (alist-get 'tool_use_id r) tool-id))
                            results)))
      (insert (format "- **%s**" tool-name))
      (when tool-input
        (insert (format " `%s`" (json-encode tool-input))))
      (insert "\n")
      (when result
        (let ((content (alist-get 'content result)))
          (insert (format "  Result: %s\n"
                         (if (> (length content) 200)
                             (concat (substring content 0 200) "...")
                           content)))))))
  (insert "\n"))

(defun aichat-agent--finish-response ()
  "Finish the agent response."
  (aichat-agent--debug "=== FINISHING RESPONSE - CONVERSATION COMPLETE ===")
  (goto-char (point-max))
  (unless (looking-back (concat aichat-user-tag "\n\n") nil)
    (insert "\n\n" aichat-user-tag "\n\n"))
  (setq aichat-agent--current-iteration 0)
  (setq aichat-agent--current-dialog nil))

(defun aichat-agent-set-auto-approve (enable)
  "Set auto-approval of tool calls to ENABLE."
  (interactive "P")
  (setq aichat-agent-auto-approve (if enable t nil))
  (message "Auto-approval %s"
           (if aichat-agent-auto-approve "enabled" "disabled")))

(defun aichat-agent-toggle-debug ()
  "Toggle debug output."
  (interactive)
  (setq aichat-agent-debug (not aichat-agent-debug))
  (message "Agent debug %s" (if aichat-agent-debug "enabled" "disabled")))

(provide 'aichat-agent)

;;; aichat-agent.el ends here
