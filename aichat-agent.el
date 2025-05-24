;;; aichat-agent.el --- Agent capabilities for aichat -*- lexical-binding: t -*-

;;; Commentary:
;; Provides agent functionality with tool support for aichat

;;; Code:

(require 'json)
(require 'aichat-stream)
(require 'aichat-tools)

(defcustom aichat-agent-tools '(read-file list-directory replace-function file-replace-region file-insert-text file-delete-region file-prepend-text file-append-text write-new-file make-directory rename-file)
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
    (setq aichat-agent--chat-buffer (current-buffer))  ; Store the chat buffer

    (aichat-agent--debug "--- DIALOG --- %s" aichat-agent--current-dialog)

    (aichat-agent--debug "=== STARTING AGENT SESSION ===")

    (aichat-agent--run-agent-loop)))

(defun aichat-agent--debug (format-string &rest args)
  "Debug logging function."
  (when aichat-agent-debug
    (message "[AGENT DEBUG] %s" (apply #'format format-string args))))

(defun aichat-agent--run-agent-loop ()
  "Run the main agent loop."
  (let ((tools (aichat-tools-get-schemas aichat-agent-tools)))

    (aichat-agent--debug "=== ITERATION %d ===" aichat-agent--current-iteration)
    (aichat-agent--debug "Dialog length: %d messages" (length aichat-agent--current-dialog))

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
       (lambda (content-blocks)
         (aichat-agent--debug "RECEIVED PARSED CONTENT BLOCKS")
         (aichat-agent--handle-parsed-response content-blocks))))))

(defun aichat-agent--enhance-dialog-with-system-prompt (dialog)
  "Enhance DIALOG with agent capabilities while preserving original system prompt."
  (let* ((system-messages (seq-filter (lambda (msg) (eq (car msg) 'system)) dialog))
         (non-system-messages (seq-filter (lambda (msg) (not (eq (car msg) 'system))) dialog))
         (original-system (when system-messages (cdar system-messages)))
         (agent-prompt (aichat-agent--get-system-prompt))
         (combined-system (if original-system
                             (format "%s\n\n%s" original-system agent-prompt)
                           agent-prompt)))
    (cons `(system . ,combined-system) non-system-messages)))

(defun aichat-agent--get-system-prompt ()
  "Get the system prompt for agent capabilities."
  (format "You have access to tools: %s

IMPORTANT: Use tools only when they would genuinely help safisfy the users prompt. Prefer to output text directly over writing files, unless the user expliclty asks for files to be written.

When you do use tools:
1. Use them efficiently - if you need multiple files, call multiple tools in one response
2. Always explain what you found and provide a final answer to the user
3. DO NOT repeat tool calls you've already made - check the conversation history first

If you've already gotten the information you need from previous tool calls in this conversation, use that information to answer the user directly WITHOUT calling more tools."
          (mapconcat #'symbol-name aichat-agent-tools ", ")))

(defun aichat-agent--handle-parsed-response (content-blocks)
  "Handle the parsed CONTENT-BLOCKS from Claude."
  (aichat-agent--debug "CONTENT BLOCKS: %s" content-blocks)

  ;; Add assistant message to dialog
  (let ((assistant-content (json-encode content-blocks)))
    (aichat-agent--debug "ADDING ASSISTANT MESSAGE TO DIALOG")
    (setq aichat-agent--current-dialog
          (append aichat-agent--current-dialog `((assistant . ,assistant-content)))))

  ;; Check if we have tool calls
  (let ((tool-calls (aichat-agent--extract-tool-calls content-blocks)))
    (if tool-calls
        (progn
          (aichat-agent--debug "TOOL USE DETECTED! Found %d tool calls" (length tool-calls))
          (setq aichat-agent--current-iteration (1+ aichat-agent--current-iteration))
          (aichat-agent--execute-tools tool-calls))
      (progn
        (aichat-agent--debug "NO TOOL USE - CONVERSATION COMPLETE")
        (aichat-agent--finish-response)))))

(defun aichat-agent--extract-tool-calls (content-blocks)
  "Extract tool calls from CONTENT-BLOCKS."
  (let ((tool-calls '()))
    (dolist (block content-blocks)
      (when (string= (alist-get 'type block) "tool_use")
        (aichat-agent--debug "EXTRACTING TOOL CALL: %s with input: %s"
                            (alist-get 'name block)
                            (json-encode (alist-get 'input block)))
        (push block tool-calls)))
    (reverse tool-calls)))

(defun aichat-agent--execute-tools (tool-calls)
  "Execute TOOL-CALLS and continue the conversation."
  (let ((results '()))
    (dolist (tool-call tool-calls)
      (let* ((tool-name (alist-get 'name tool-call))
             (tool-input (alist-get 'input tool-call))
             (tool-id (alist-get 'id tool-call)))

        (if (aichat-agent--request-approval tool-name tool-input)
            (condition-case err
                (let ((result (aichat-tools-execute tool-name tool-input)))
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
    (aichat-agent--display-tool-execution tool-calls (reverse results))

    ;; Add tool results to dialog
    (let ((user-content (json-encode (reverse results))))
      (setq aichat-agent--current-dialog
            (append aichat-agent--current-dialog `((user . ,user-content)))))

    ;; Continue the loop
    (aichat-agent--run-agent-loop)))

(defun aichat-agent--display-tool-execution (tool-calls results)
  "Display the execution of TOOL-CALLS and their RESULTS."
  (with-current-buffer aichat-agent--chat-buffer  ; Ensure we're in the chat buffer
    (goto-char (point-max))  ; Go to the end of the chat buffer
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
    (insert "\n")))

(defun aichat-agent--finish-response ()
  "Finish the agent response."
  (aichat-agent--debug "=== FINISHING RESPONSE - CONVERSATION COMPLETE ===")
  (with-current-buffer aichat-agent--chat-buffer  ; Ensure we're in the chat buffer
    (goto-char (point-max))
    (unless (looking-back (concat aichat-user-tag "\n\n") nil)
      (insert "\n\n" aichat-user-tag "\n\n")))
  (setq aichat-agent--current-iteration 0)
  (setq aichat-agent--current-dialog nil)
  (setq aichat-agent--chat-buffer nil))

(defun aichat-agent--request-approval (tool-name tool-input)
  "Request approval for TOOL-NAME with TOOL-INPUT."
  (if aichat-agent-auto-approve
      t
    (y-or-n-p (format "Execute %s with %s? " tool-name (json-encode tool-input)))))

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
