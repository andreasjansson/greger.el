;;; greger-agent.el --- Agent capabilities for greger -*- lexical-binding: t -*-

;; Author: Andreas Jansson <andreas@jansson.me.uk>
;; Version: 0.1.0
;; URL: https://github.com/andreasjansson/greger.el
;; Package-Requires: ((emacs "25.1"))

;;; Commentary:
;; Provides agent functionality with tool support for greger

;;; Code:

(require 'json)
(require 'cl-lib)
(require 'greger)
(require 'greger-parser)
(require 'greger-stream)
(require 'greger-tools)
(require 'greger-stdlib)
;; Optional LSP integration
(condition-case nil
    (require 'greger-lib-lsp)
  (error nil))

;; not using patch tool, it's too messy
(defun greger-agent--default-tools ()
  "Return default tools list, including LSP tools if available."
  (let ((base-tools '("read-file" "list-directory" "str-replace" "insert" "write-new-file" "replace-file" "replace-function" "make-directory" "rename-file" "ripgrep" "git-log" "git-show-commit" "shell-command" "read-webpage"))
        (lsp-tools '("lsp-rename" "lsp-find-definition" "lsp-find-references" "lsp-format" "lsp-document-symbols")))
    (if (and (boundp 'greger-lib-lsp-available) greger-lib-lsp-available)
        (append base-tools lsp-tools)
      base-tools)))

(defcustom greger-agent-tools (greger-agent--default-tools)
  "List of tools available to the agent."
  :type '(repeat symbol)
  :group 'greger)

(defcustom greger-agent-max-iterations 100
  "Maximum number of agent iterations before stopping."
  :type 'integer
  :group 'greger)



(defcustom greger-agent-debug nil
  "Whether to show debug information."
  :type 'boolean
  :group 'greger)

;;; Agent state structure

(cl-defstruct greger-agent-state
  current-iteration
  chat-buffer
  directory
  metadata)

(defun greger-agent-buffer ()
  "Send buffer content to AI as an agent dialog with tool support."
  (interactive)
  (let* ((buffer-content (buffer-substring-no-properties (point-min) (point-max)))
         (parse-result (greger-parser-parse-dialog buffer-content))
         (dialog (plist-get parse-result :messages))
         (metadata (plist-get parse-result :metadata)))
    (unless dialog
      (error "Failed to parse dialog. Did you forget to close a html tag?"))

    (goto-char (point-max))

    (let ((agent-state (make-greger-agent-state
                        :current-iteration 0
                        :chat-buffer (current-buffer)
                        :directory default-directory
                        :metadata metadata)))

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
         (parse-result (greger-parser-parse-dialog buffer-content))
         (current-dialog (plist-get parse-result :messages))
         (current-iteration (greger-agent-state-current-iteration agent-state)))

    (greger-agent--debug "=== ITERATION %d ===" current-iteration)
    (greger-agent--debug "Dialog length: %d messages" (length current-dialog))

    ;; Check max iterations
    (if (>= current-iteration greger-agent-max-iterations)
        (progn
          (greger-agent--debug "MAX ITERATIONS REACHED - STOPPING")
          (greger-agent--append-text (format "\n\nMaximum iterations (%d) reached. Stopping agent execution.\n\n"
                                             greger-agent-max-iterations)
                                     agent-state)
          (greger-agent--finish-response agent-state))

      ;; Get Claude's response
      (greger-agent--debug "CALLING greger-stream-to-buffer-with-tools...")
      (greger-stream-to-buffer-with-tools
       :model greger-model
       :dialog current-dialog
       :tools tools
       :buffer chat-buffer
       :text-start-callback (lambda ()
                              (greger-agent--append-text (concat "\n\n" greger-assistant-tag "\n\n") agent-state))
       :text-callback (lambda (text)
                        (greger-agent--append-text text agent-state))
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
          (setf (greger-agent-state-current-iteration agent-state)
                (1+ (greger-agent-state-current-iteration agent-state)))
          (greger-agent--execute-tools tool-calls agent-state))
      (progn
        (greger-agent--debug "NO TOOL USE - CONVERSATION COMPLETE")
        (greger-agent--finish-response agent-state)))))

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

(defun greger-agent--tool-placeholder (tool-id)
  "Generate placeholder string for TOOL-ID."
  (format "<!-- TOOL_RESULT_PLACEHOLDER_%s -->" tool-id))

(defun greger-agent--execute-tools (tool-calls agent-state)
  "Execute TOOL-CALLS using AGENT-STATE in parallel with callbacks."
  (let* ((total-tools (length tool-calls))
         (completed-tools 0)
         (search-start-pos nil))

    ;; First, display the tool calls and reserve space for each tool's output
    (with-current-buffer (greger-agent-state-chat-buffer agent-state)
      (goto-char (point-max))

      ;; Remember where to start searching for placeholders
      (setq search-start-pos (point))

      ;; Display each tool call followed by its placeholder
      (dolist (tool-call tool-calls)
        (let ((tool-id (alist-get 'id tool-call))
              (tool-block-markdown (greger-parser--content-blocks-to-markdown (list tool-call))))
          (unless (string-empty-p tool-block-markdown)
            (insert "\n\n" tool-block-markdown))
          (insert "\n\n" (greger-agent--tool-placeholder tool-id)))))

    ;; Execute all tools in parallel
    (dolist (tool-call tool-calls)
      (let* ((tool-name (alist-get 'name tool-call))
             (tool-input (alist-get 'input tool-call))
             (tool-id (alist-get 'id tool-call)))

        (let ((default-directory (greger-agent-state-directory agent-state)))
          (greger-tools-execute
           tool-name
           tool-input
           (lambda (result error)
             (greger-agent--handle-tool-completion
              tool-id result error agent-state search-start-pos
              (lambda ()
                (setq completed-tools (1+ completed-tools))
                (when (= completed-tools total-tools)
                  (greger-agent--run-agent-loop agent-state)))))
           (greger-agent-state-chat-buffer agent-state)
           (greger-agent-state-metadata agent-state)))))))

(defun greger-agent--append-text (text agent-state)
  (with-current-buffer (greger-agent-state-chat-buffer agent-state)
    (goto-char (point-max))
    (insert text)))

(defun greger-agent--handle-tool-completion (tool-id result error agent-state search-start-pos completion-callback)
  "Handle completion of a tool execution by updating the buffer and calling COMPLETION-CALLBACK."
  (let ((tool-result (if error
                        `((type . "tool_result")
                          (tool_use_id . ,tool-id)
                          (content . ,(if (stringp error)
                                         error
                                       (format "Error executing tool: %s" (error-message-string error))))
                          (is_error . t))
                      `((type . "tool_result")
                        (tool_use_id . ,tool-id)
                        (content . ,result)))))

    ;; Update the buffer at the correct position
    (with-current-buffer (greger-agent-state-chat-buffer agent-state)
      (save-excursion
        (goto-char search-start-pos)
        ;; Find and replace the placeholder
        (when (search-forward (greger-agent--tool-placeholder tool-id) nil t)
          (replace-match "")
          (let ((result-markdown (greger-parser--content-blocks-to-markdown (list tool-result))))
            (unless (string-empty-p result-markdown)
              (insert result-markdown))))))

    ;; Call completion callback
    (funcall completion-callback)))

(defun greger-agent--display-tool-execution (tool-calls results agent-state)
  "Display the execution of TOOL-CALLS and their RESULTS using AGENT-STATE.
This function is kept for backward compatibility but is no longer used in the new callback-based approach."
  (with-current-buffer (greger-agent-state-chat-buffer agent-state)
    (goto-char (point-max))

    ;; The tool calls are already in the right format, just convert them
    (let ((tool-blocks-markdown (greger-parser--content-blocks-to-markdown tool-calls)))
      (unless (string-empty-p tool-blocks-markdown)
        (insert "\n\n" tool-blocks-markdown)))

    ;; Convert tool results to markdown
    (let ((result-blocks-markdown (greger-parser--content-blocks-to-markdown results)))
      (unless (string-empty-p result-blocks-markdown)
        (insert "\n\n" result-blocks-markdown)))))

(defun greger-agent--finish-response (agent-state)
  "Finish the agent response using AGENT-STATE."
  (greger-agent--debug "=== FINISHING RESPONSE - CONVERSATION COMPLETE ===")
  (with-current-buffer (greger-agent-state-chat-buffer agent-state)
    (goto-char (point-max))
    (unless (looking-back (concat greger-user-tag "\n\n") nil)
      (insert "\n\n" greger-user-tag "\n\n")))
  ;; Reset the state
  (setf (greger-agent-state-current-iteration agent-state) 0))



(defun greger-agent-toggle-debug ()
  "Toggle debug output."
  (interactive)
  (setq greger-agent-debug (not greger-agent-debug))
  (message "Agent debug %s" (if greger-agent-debug "enabled" "disabled")))

(provide 'greger-agent)

;;; greger-agent.el ends here
