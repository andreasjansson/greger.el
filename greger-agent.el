;;; greger-agent.el --- Agent capabilities for greger -*- lexical-binding: t -*-

;;; Commentary:
;; Provides agent functionality with tool support for greger

;;; Code:

(require 'json)
(require 'cl-lib)
(require 'greger-stream)
(require 'greger-tools)

;; not using patch tool, it's too messy
(defcustom greger-agent-tools '("read-file" "list-directory" "str-replace" "insert" "write-new-file" "replace-file" "replace-function" "make-directory" "rename-file" "ripgrep" "git-log" "git-show-commit" "shell-command")
  "List of tools available to the agent."
  :type '(repeat symbol)
  :group 'greger)

(defcustom greger-agent-max-iterations 100
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

;;; Agent state structure

(cl-defstruct greger-agent-state
  current-iteration
  chat-buffer
  directory)

(defun greger-agent-buffer ()
  "Send buffer content to AI as an agent dialog with tool support."
  (interactive)
  (let* ((buffer-content (buffer-substring-no-properties (point-min) (point-max)))
         (dialog (greger-parser-parse-dialog buffer-content)))
    (unless dialog
      (error "Failed to parse dialog. Did you forget to close a html tag?"))

    (goto-char (point-max))

    (let ((agent-state (make-greger-agent-state
                        :current-iteration 0
                        :chat-buffer (current-buffer)
                        :directory default-directory)))

      (greger-agent--debug "--- DIALOG --- %s" dialog)
      (greger-agent--debug "=== STARTING AGENT SESSION ===")

      (greger-agent--run-agent-loop agent-state))))

(defun greger-agent--debug (format-string &rest args)
  "Debug logging function."
  (when greger-agent-debug
    (message "[AGENT DEBUG] %s" (apply #'format format-string args))))

(defun greger-agent--run-agent-loop (agent-state)
  "Run the main agent loop with AGENT-STATE."
  (let* ((tools (greger-tools-get-schemas greger-agent-tools))
         (chat-buffer (greger-agent-state-chat-buffer agent-state))
         (buffer-content (with-current-buffer chat-buffer
                           (buffer-substring-no-properties (point-min) (point-max))))
         (current-dialog (greger-parser-parse-dialog buffer-content))
         (current-iteration (greger-agent-state-current-iteration agent-state)))

    (greger-agent--debug "=== ITERATION %d ===" current-iteration)
    (greger-agent--debug "Dialog length: %d messages" (length current-dialog))

    ;; Check max iterations
    (if (>= current-iteration greger-agent-max-iterations)
        (progn
          (greger-agent--debug "MAX ITERATIONS REACHED - STOPPING")
          (with-current-buffer chat-buffer
            (goto-char (point-max))
            (insert (format "\n\nMaximum iterations (%d) reached. Stopping agent execution.\n\n"
                           greger-agent-max-iterations)))
          (greger-agent--finish-response agent-state))

      ;; Get Claude's response
      (greger-agent--debug "CALLING greger-stream-to-buffer-with-tools...")
      (greger-stream-to-buffer-with-tools
       :model greger-model
       :dialog current-dialog
       :tools tools
       :buffer chat-buffer
       :complete-callback (lambda (content-blocks)
                            (greger-agent--debug "RECEIVED PARSED CONTENT BLOCKS")
                            (greger-agent--handle-parsed-response content-blocks agent-state))))))

(defun greger-agent--handle-parsed-response (content-blocks agent-state)
  "Handle the parsed CONTENT-BLOCKS from Claude using AGENT-STATE."
  (greger-agent--debug "CONTENT BLOCKS: %s" content-blocks)

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
                (let* ((default-directory greger-agent--directory)
                       (result (greger-tools-execute tool-name tool-input greger-agent--chat-buffer)))
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

    ;; Continue the loop
    (greger-agent--run-agent-loop)))

;; In greger-agent.el, update the display function:

(defun greger-agent--display-tool-execution (tool-calls results)
  "Display the execution of TOOL-CALLS and their RESULTS using parser markdown conversion."
  (with-current-buffer greger-agent--chat-buffer
    (goto-char (point-max))

    ;; The tool calls are already in the right format, just convert them
    (let ((tool-blocks-markdown (greger-parser--content-blocks-to-markdown tool-calls)))
      (unless (string-empty-p tool-blocks-markdown)
        (insert "\n\n" tool-blocks-markdown)))

    ;; Convert tool results to markdown
    (let ((result-blocks-markdown (greger-parser--content-blocks-to-markdown results)))
      (unless (string-empty-p result-blocks-markdown)
        (insert "\n\n" result-blocks-markdown)))))

(defun greger-agent--finish-response ()
  "Finish the agent response."
  (greger-agent--debug "=== FINISHING RESPONSE - CONVERSATION COMPLETE ===")
  (with-current-buffer greger-agent--chat-buffer  ; Ensure we're in the chat buffer
    (goto-char (point-max))
    (unless (looking-back (concat greger-user-tag "\n\n") nil)
      (insert "\n\n" greger-user-tag "\n\n")))
  (setq greger-agent--current-iteration 0)
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
