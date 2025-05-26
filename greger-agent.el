;;; greger-agent.el --- Agent capabilities for greger -*- lexical-binding: t -*-

;;; Commentary:
;; Provides agent functionality with tool support for greger

;;; Code:

(require 'json)
(require 'greger-stream)
(require 'greger-tools)

(defcustom greger-agent-tools '(read-file list-directory file-replace-region file-insert-text file-delete-region file-prepend-text file-append-text write-new-file make-directory rename-file ripgrep)
  "List of tools available to the agent."
  :type '(repeat symbol)
  :group 'greger)

(defcustom greger-agent-max-iterations 10
  "Maximum number of agent iterations before stopping."
  :type 'integer
  :group 'greger)

(defcustom greger-agent-auto-approve t
  "Whether to automatically approve tool calls without user confirmation."
  :type 'boolean
  :group 'greger)

(defcustom greger-agent-debug t
  "Whether to show debug information."
  :type 'boolean
  :group 'greger)

(defvar greger-agent--current-iteration 0
  "Current iteration count for the active agent session.")

(defvar greger-agent--current-dialog nil
  "Current dialog being processed.")

(defun greger-agent-buffer ()
  "Send buffer content to AI as an agent dialog with tool support."
  (interactive)
  (let* ((buffer-content (buffer-substring-no-properties (point-min) (point-max)))
         (dialog (greger-parser-parse-dialog buffer-content)))
    (unless dialog
      (error "Failed to parse dialog. Did you forget to close a html tag?"))

    (goto-char (point-max))
    (greger--maybe-insert-assistant-tag)

    (setq greger-agent--current-iteration 0)
    (setq greger-agent--current-dialog (greger-agent--enhance-dialog-with-system-prompt dialog))
    (setq greger-agent--chat-buffer (current-buffer))  ; Store the chat buffer

    (greger-agent--debug "--- DIALOG --- %s" greger-agent--current-dialog)

    (greger-agent--debug "=== STARTING AGENT SESSION ===")

    (greger-agent--run-agent-loop)))

(defun greger-agent--debug (format-string &rest args)
  "Debug logging function."
  (when greger-agent-debug
    (message "[AGENT DEBUG] %s" (apply #'format format-string args))))

(defun greger-agent--run-agent-loop ()
  "Run the main agent loop."
  (let ((tools (greger-tools-get-schemas greger-agent-tools)))

    (greger-agent--debug "=== ITERATION %d ===" greger-agent--current-iteration)
    (greger-agent--debug "Dialog length: %d messages" (length greger-agent--current-dialog))

    ;; Check max iterations
    (if (>= greger-agent--current-iteration greger-agent-max-iterations)
        (progn
          (greger-agent--debug "MAX ITERATIONS REACHED - STOPPING")
          (insert (format "\n\nMaximum iterations (%d) reached. Stopping agent execution.\n\n"
                         greger-agent-max-iterations))
          (greger-agent--finish-response))

      ;; Get Claude's response
      (greger-agent--debug "CALLING greger-stream-to-buffer-with-tools...")
      (greger-stream-to-buffer-with-tools
       greger-model greger-agent--current-dialog tools
       (lambda (content-blocks)
         (greger-agent--debug "RECEIVED PARSED CONTENT BLOCKS")
         (greger-agent--handle-parsed-response content-blocks))))))

(defun greger-agent--enhance-dialog-with-system-prompt (dialog)
  "Enhance DIALOG with agent capabilities while preserving original system prompt."
  (let* ((system-messages (seq-filter (lambda (msg) (eq (car msg) 'system)) dialog))
         (non-system-messages (seq-filter (lambda (msg) (not (eq (car msg) 'system))) dialog))
         (original-system (when system-messages (cdar system-messages)))
         (agent-prompt (greger-agent--get-system-prompt))
         (combined-system (if original-system
                             (format "%s\n\n%s" original-system agent-prompt)
                           agent-prompt)))
    (cons `(system . ,combined-system) non-system-messages)))

(defun greger-agent--get-system-prompt ()
  "Get the system prompt for agent capabilities."
  (format "You have access to tools: %s

IMPORTANT: Use tools only when they would genuinely help safisfy the users prompt. Prefer to output text directly over writing files, unless the user expliclty asks for files to be written.

When you do use tools:
1. Use them efficiently - if you need multiple files, call multiple tools in one response
2. Always explain what you found and provide a final answer to the user
3. DO NOT repeat tool calls you've already made - check the conversation history first

If you've already gotten the information you need from previous tool calls in this conversation, use that information to answer the user directly WITHOUT calling more tools."
          (mapconcat #'symbol-name greger-agent-tools ", ")))

(defun greger-agent--handle-parsed-response (content-blocks)
  "Handle the parsed CONTENT-BLOCKS from Claude."
  (greger-agent--debug "CONTENT BLOCKS: %s" content-blocks)

  ;; Add assistant message to dialog
  (let ((assistant-content (json-encode content-blocks)))
    (greger-agent--debug "ADDING ASSISTANT MESSAGE TO DIALOG")
    (setq greger-agent--current-dialog
          (append greger-agent--current-dialog `((assistant . ,assistant-content)))))

  ;; Check if we have tool calls
  (let ((tool-calls (greger-agent--extract-tool-calls content-blocks)))
    (if tool-calls
        (progn
          (greger-agent--debug "TOOL USE DETECTED! Found %d tool calls" (length tool-calls))
          (setq greger-agent--current-iteration (1+ greger-agent--current-iteration))
          (greger-agent--execute-tools tool-calls))
      (progn
        (greger-agent--debug "NO TOOL USE - CONVERSATION COMPLETE")
        (greger-agent--finish-response)))))

(defun greger-agent--extract-tool-calls (content-blocks)
  "Extract tool calls from CONTENT-BLOCKS."
  (let ((tool-calls '()))
    (dolist (block content-blocks)
      (when (string= (alist-get 'type block) "tool_use")
        (greger-agent--debug "EXTRACTING TOOL CALL: %s with input: %s"
                            (alist-get 'name block)
                            (json-encode (alist-get 'input block)))
        (push block tool-calls)))
    (reverse tool-calls)))

(defun greger-agent--execute-tools (tool-calls)
  "Execute TOOL-CALLS and continue the conversation."
  (let ((results '()))
    (dolist (tool-call tool-calls)
      (let* ((tool-name (alist-get 'name tool-call))
             (tool-input (alist-get 'input tool-call))
             (tool-id (alist-get 'id tool-call)))

        (if (greger-agent--request-approval tool-name tool-input)
            (condition-case err
                (let ((result (greger-tools-execute tool-name tool-input)))
                  (push `((type . "tool_result")
                         (tool_use_id . ,tool-id)
                         (content . ,result))
                        results))
              (error
               (push `((type . "tool_result")
                      (tool_use_id . ,tool-id)
                      (content . ,(format "Error executing tool: %s" (error-message-string err)))
                      (is_error . t))
                     results)))
          (push `((type . "tool_result")
                 (tool_use_id . ,tool-id)
                 (content . "Tool execution declined by user")
                 (is_error . t))
                results))))

    ;; Display tool execution
    (greger-agent--display-tool-execution tool-calls (reverse results))

    ;; Add tool results to dialog
    (let ((user-content (json-encode (reverse results))))
      (setq greger-agent--current-dialog
            (append greger-agent--current-dialog `((user . ,user-content)))))

    ;; Continue the loop
    (greger-agent--run-agent-loop)))

;; In greger-agent.el, update the display function:

(defun greger-agent--display-tool-execution (tool-calls results)
  "Display the execution of TOOL-CALLS and their RESULTS in the new markdown format."
  (with-current-buffer greger-agent--chat-buffer
    (goto-char (point-max))

    ;; Display each tool call and its result
    (dolist (tool-call tool-calls)
      (let* ((tool-name (alist-get 'name tool-call))
             (tool-input (alist-get 'input tool-call))
             (tool-id (alist-get 'id tool-call))
             (result (seq-find (lambda (r)
                                (string= (alist-get 'tool_use_id r) tool-id))
                              results)))

        ;; Insert tool use
        (insert (format "\n\n## TOOL USE:\n\nName: %s\nID: %s\n" tool-name tool-id))
        (when tool-input
          (dolist (param tool-input)
            (insert (format "\n### %s\n\n%s\n" (car param) (cdr param)))))

        ;; Insert tool result
        (when result
          (let ((content (alist-get 'content result)))
            (insert (format "\n\n## TOOL RESULT:\n\nID: %s\n\n%s\n" tool-id content))))))))

(defun greger-agent--finish-response ()
  "Finish the agent response."
  (greger-agent--debug "=== FINISHING RESPONSE - CONVERSATION COMPLETE ===")
  (with-current-buffer greger-agent--chat-buffer  ; Ensure we're in the chat buffer
    (goto-char (point-max))
    (unless (looking-back (concat greger-user-tag "\n\n") nil)
      (insert "\n\n" greger-user-tag "\n\n")))
  (setq greger-agent--current-iteration 0)
  (setq greger-agent--current-dialog nil)
  (setq greger-agent--chat-buffer nil))

(defun greger-agent--request-approval (tool-name tool-input)
  "Request approval for TOOL-NAME with TOOL-INPUT."
  (if greger-agent-auto-approve
      t
    (y-or-n-p (format "Execute %s with %s? " tool-name (json-encode tool-input)))))

(defun greger-agent-set-auto-approve (enable)
  "Set auto-approval of tool calls to ENABLE."
  (interactive "P")
  (setq greger-agent-auto-approve (if enable t nil))
  (message "Auto-approval %s"
           (if greger-agent-auto-approve "enabled" "disabled")))

(defun greger-agent-toggle-debug ()
  "Toggle debug output."
  (interactive)
  (setq greger-agent-debug (not greger-agent-debug))
  (message "Agent debug %s" (if greger-agent-debug "enabled" "disabled")))

(provide 'greger-agent)

;;; greger-agent.el ends here
