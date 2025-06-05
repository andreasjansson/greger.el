;;; greger.el --- Chat with language models -*- lexical-binding: t -*-

;; Copyright (C) 2023 Andreas Jansson

;; Author: Andreas Jansson <andreas@jansson.me.uk>
;; Version: 0.1.0
;; URL: https://github.com/andreasjansson/greger.el
;; Package-Requires: ((emacs "28.1") (markdown-mode "2.3"))
;; Keywords: ai, chat, language-models, tools
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
;; This package provides an interface for interacting with AI language models

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'markdown-mode)

(require 'greger-client)
(require 'greger-parser)
(require 'greger-tools)
(require 'greger-stdlib)
(require 'greger-ui)



(defconst greger-available-models
  '(claude-sonnet-4-20250514
    claude-opus-4-20250514)
  "List of available models.")

(defcustom greger-model 'claude-sonnet-4-20250514
  "The currently used model."
  :type `(choice ,@(mapcar (lambda (model) `(const ,model)) greger-available-models))
  :group 'greger)

(defcustom greger-default-system-prompt "You are a helpful assistant."
  "Default system prompt used for AI interactions."
  :type 'string
  :group 'greger)

(defcustom greger-temperature 0.8
  "Sampling temperature between 0 and 1."
  :type 'float
  :group 'greger)

;; Tool configuration and agent functionality

(defun greger--default-tools ()
  "Return default tools list, including LSP tools if available."
  (let ((base-tools '("read-file" "list-directory" "str-replace" "insert" "write-new-file" "replace-file" "make-directory" "rename-file" "ripgrep" "git-log" "git-show-commit" "shell-command" "read-webpage" "delete-files"))
        (lsp-tools '("lsp-rename" "lsp-find-definition" "lsp-find-references" "lsp-format" "lsp-document-symbols")))
    (if (and (boundp 'greger--lsp-available) greger--lsp-available)
        (append base-tools lsp-tools)
      base-tools)))

(defcustom greger-tools (greger--default-tools)
  "List of tools available to the agent."
  :type '(repeat symbol)
  :group 'greger)

(defcustom greger-server-tools '("web_search")
  "List of server tools available to the agent (e.g., web_search)."
  :type '(repeat symbol)
  :group 'greger)

(defcustom greger-max-iterations 100
  "Maximum number of agent iterations before stopping."
  :type 'integer
  :group 'greger)

(defcustom greger-debug nil
  "Whether to show debug information."
  :type 'boolean
  :group 'greger)

;;; Agent state structure

(cl-defstruct greger-state
  current-iteration
  chat-buffer
  directory
  metadata
  client-state
  executing-tools)

(defvar greger-user-tag "## USER:")
(defvar greger-assistant-tag "## ASSISTANT:")
(defvar greger-system-tag "## SYSTEM:")



(defvar-local greger--current-state nil
  "Buffer-local variable to track the current state.")

(defvar-local greger--buffer-read-only-by-greger nil
  "Buffer-local variable to track if buffer is read-only due to greger activity.")





(defface greger-tool-param-heading-face
  '((t :foreground "#6699CC" :weight bold :height 1.0))
  "Face for ### tool parameter headings in greger mode."
  :group 'greger)

(defface greger-user-heading-face
  '((t :foreground "#66DD66" :weight bold :height 1.2))
  "Face for ## USER: headings in greger mode."
  :group 'greger)

(defface greger-tool-result-heading-face
  '((t :foreground "#66AA88" :weight bold :height 1.2))
  "Face for ## TOOL RESULT: headings in greger mode."
  :group 'greger)

(defface greger-assistant-heading-face
  '((t :foreground "#AA9922" :weight bold :height 1.2))
  "Face for ## ASSISTANT: headings in greger mode."
  :group 'greger)

(defface greger-thinking-heading-face
  '((t :foreground "#9966CC" :weight bold :height 1.2))
  "Face for ## THINKING: headings in greger mode."
  :group 'greger)

(defface greger-tool-use-heading-face
  '((t :foreground "#8866BB" :weight bold :height 1.2))
  "Face for ## TOOL USE: headings in greger mode."
  :group 'greger)

(defface greger-system-heading-face
  '((t :foreground "#CC6666" :weight bold :height 1.2))
  "Face for ## SYSTEM: headings in greger mode."
  :group 'greger)

(defvar greger-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-<return>") #'greger-buffer)
    (define-key map (kbd "C-M-<return>") #'greger-buffer-no-tools)
    (define-key map (kbd "C-g") #'greger-interrupt)
    (define-key map (kbd "C-; a") #'greger-insert-assistant-tag)
    (define-key map (kbd "C-; u") #'greger-insert-user-tag)
    (define-key map (kbd "C-; s") #'greger-insert-system-tag)
    (define-key map (kbd "C-; i") #'greger-insert-include)
    (define-key map (kbd "C-; I") #'greger-insert-include-code)
    (define-key map (kbd "C-; f") #'greger-insert-include-file)
    (define-key map (kbd "C-; b") #'greger-insert-include-buffer-code)
    (define-key map (kbd "C-; m") #'greger-set-model)
    (define-key map (kbd "C-; c") #'greger-copy-code)
    (define-key map (kbd "C-; d") #'greger-debug-request)
    (define-key map (kbd "TAB") #'greger-ui-toggle-section)
    (define-key map (kbd "<tab>") #'greger-ui-toggle-section)
    map)
  "Keymap for `greger-mode'.")

(define-derived-mode greger-mode gfm-mode "Greger"
  "Major mode for interacting with AI."
  (use-local-map greger-mode-map)
  (setq-local markdown-fontify-code-blocks-natively t)
  (setq-local mode-line-misc-info '(:eval (greger--mode-line-info)))
  ;; Set up UI folding (both tools and citations)
  (greger-ui-setup-folding)
  ;; Set up custom heading font-lock
  (greger--setup-heading-font-lock)
  ;; Add hook to update tool sections when buffer changes
  (add-hook 'after-change-functions #'greger--after-change-function nil t)
  ;; Add font-lock hook for immediate tool tag styling
  (add-hook 'font-lock-extend-region-functions #'greger--extend-font-lock-region nil t))

;;;###autoload
(defun greger ()
  "Create a new buffer and switch to `greger-mode`."
  (interactive)
  (let ((buffer (generate-new-buffer "*greger*")))
    (switch-to-buffer buffer)
    (greger-mode)
    (insert greger-system-tag
            "\n\n" greger-default-system-prompt "\n\n"
            greger-user-tag
            "\n\n")
    (message "Using model %s" greger-model)))

(defun greger-insert-assistant-tag ()
  "Insert the assistant tag into the buffer."
  (interactive)
  (insert greger-assistant-tag "\n\n"))

(defun greger-insert-user-tag ()
  "Insert the user tag into the buffer."
  (interactive)
  (insert greger-user-tag "\n\n"))

(defun greger-insert-system-tag ()
  "Insert the system tag into the buffer."
  (interactive)
  (insert greger-system-tag "\n\n"))

(defun greger-interrupt ()
  "Interrupt ongoing generation if active, otherwise call `keyboard-quit'."
  (interactive)

  (let* ((buffer (current-buffer))
         (state (buffer-local-value 'greger--current-state buffer)))
    (cond
     ;; If there's an active client state, cancel the streaming request
     ((and state (greger-state-client-state state))
      (greger-client--cancel-request (greger-state-client-state state))
      (setf (greger-state-client-state state) nil)
      (greger--update-buffer-state)
      'generating)
     ;; If there are executing tools, cancel them
     ((and state
           (greger-state-executing-tools state)
           (> (hash-table-count (greger-state-executing-tools state)) 0))
      (let ((executing-tools (greger-state-executing-tools state)))
        (maphash (lambda (_tool-id greger-tool)
                   (let ((cancel-fn (greger-tool-cancel-fn greger-tool)))
                     (when (functionp cancel-fn)
                       (funcall cancel-fn))))
                 executing-tools)
        (greger--update-buffer-state))
      'executing)
     ;; Default case: call keyboard-quit
     (t
      (keyboard-quit)
      'idle))))

(defun greger-buffer-no-tools ()
  "Send the buffer content to AI as a dialog without tool use."
  (interactive)
  (let ((greger-tools '()))
    (greger-buffer)))

(defun greger-insert-include ()
  "Prompt the user to select a file and insert an <include> at point."
  (interactive)
  (let ((file (read-string "Filename or URL: ")))
    (insert (format "<include>%s</include>\n\n" file))))

(defun greger-insert-include-file ()
  "Prompt the user to select a file and insert an <include> at point."
  (interactive)
  (let ((file (expand-file-name (read-file-name "Select file: " nil nil t))))
    (if (file-exists-p file)
        (insert (format "<include>%s</include>\n\n" file))
      (message "File does not exist!"))))

(defun greger-insert-include-code ()
  "Prompt the user to select a file and insert an <include> at point."
  (interactive)
  (let ((file (read-string "Filename or URL: ")))
    (insert (format "<include code>%s</include>\n\n" file))))

(defun greger-insert-include-buffer-code ()
  "Prompt the user to select a buffer and insert an <include code> at point."
  (interactive)
  (let ((buffer-name (read-buffer "Select buffer: " nil t)))
    (when buffer-name
      (let ((buffer (get-buffer buffer-name)))
        (when buffer
          (let ((path (buffer-file-name buffer)))
            (insert (format "<include code>%s</include>\n\n" path))))))))

(defun greger-copy-code ()
  "Copy the current code block under point."
  (interactive)
  (let ((code-block (greger--get-current-code-block)))
    (if code-block
        (progn
          (kill-new code-block)
          (message "Copied code: %s" (greger--truncate-with-ellipsis code-block 40)))
      (error "Point is not inside a code block"))))

(defun greger-set-model ()
  "Set the current model."
  (interactive)
  (let ((model (completing-read "Choose model: " greger-available-models nil t)))
    (customize-set-variable 'greger-model (intern model))
    (message "Model set to %s" model)))

(defun greger-debug-request ()
  "Debug the request data by parsing the buffer and saving the request data output."
  (interactive)
  (let* ((filename (read-string "Save to filename (default: request.json): " nil nil "request.json"))
         (buffer-content (buffer-substring-no-properties (point-min) (point-max)))
         (parse-result (greger-parser-parse-dialog buffer-content))
         (dialog (plist-get parse-result :messages))
         (tools (when greger-tools
                  (greger-tools-get-schemas greger-tools)))
         (model greger-model)
         (request-data nil))

    (unless dialog
      (error "Failed to parse dialog. Check your buffer format"))

    ;; Get server tools
    (let ((server-tools (when greger-server-tools
                          (greger-server-tools-get-schemas greger-server-tools))))
      ;; Get the JSON request data using the new client
      (setq request-data (greger-client--build-data model dialog tools server-tools)))

    ;; Parse the JSON and re-encode with proper formatting
    (condition-case err
        (let* ((parsed-json (json-read-from-string request-data)))
          ;; Write to file with proper indentation
          (with-temp-file filename
            (let ((json-encoding-pretty-print t))
              (insert (json-encode parsed-json))))
          (message "Request data saved to %s" filename))
      (error
       ;; Fallback: just save the raw JSON string if parsing fails
       (with-temp-file filename
         (insert request-data))
       (message "Request data saved to %s (raw format due to parsing error: %s)"
                filename (error-message-string err))))))

;; Main buffer function with agent functionality

(defun greger-buffer ()
  "Send buffer content to AI as an agent dialog with tool support."
  (interactive)
  (let* ((buffer-content (buffer-substring-no-properties (point-min) (point-max)))
         (parse-result (greger-parser-parse-dialog buffer-content))
         (dialog (plist-get parse-result :messages))
         (metadata (plist-get parse-result :metadata)))
    (unless dialog
      (error "Failed to parse dialog. Did you forget to close a html tag?"))

    (goto-char (point-max))

    (let ((state (make-greger-state
                        :current-iteration 0
                        :chat-buffer (current-buffer)
                        :directory default-directory
                        :metadata metadata)))

      (greger--debug "--- DIALOG --- %s" dialog)
      (greger--debug "=== STARTING AGENT SESSION ===")

      (greger--run-agent-loop state))))

(defun greger--debug (format-string &rest args)
  "Debug logging function.
FORMAT-STRING is the format string.
ARGS are arguments to format."
  (when greger-debug
    (message "[GREGER DEBUG] %s" (apply #'format format-string args))))

(defun greger--get-current-state ()
  "Get the current greger state: \='idle, \='generating, or \='executing."
  (let ((state (buffer-local-value 'greger--current-state (current-buffer))))
    (cond
     ;; Check if we're generating (client-state is active)
     ((and state (greger-state-client-state state))
      'generating)
     ;; Check if we're executing tools
     ((and state
           (greger-state-executing-tools state)
           (> (hash-table-count (greger-state-executing-tools state)) 0))
      'executing)
     ;; Otherwise we're idle
     (t 'idle))))

(defun greger--mode-line-info ()
  "Generate mode line information showing model and current state."
  (let ((state (greger--get-current-state))
        (model-name (symbol-name greger-model)))
    (concat model-name
            (pcase state
              ('generating " [Generating]")
              ('executing " [Executing]")
              ('idle "")))))

(defun greger--set-buffer-read-only (read-only)
  "Set buffer read-only state for greger operations.
READ-ONLY is t to make read-only, nil to make writable."
  (if read-only
      (unless greger--buffer-read-only-by-greger
        (setq greger--buffer-read-only-by-greger t)
        (setq buffer-read-only t))
    (when greger--buffer-read-only-by-greger
      (setq greger--buffer-read-only-by-greger nil)
      (setq buffer-read-only nil))))

(defun greger--update-buffer-state ()
  "Update buffer read-only state based on current greger state."
  (let ((state (greger--get-current-state)))
    (greger--set-buffer-read-only (not (eq state 'idle)))
    ;; Force mode line update
    (force-mode-line-update)))

(defun greger--run-agent-loop (state)
  "Run the main agent loop with AGENT-STATE."
  (let* ((tools (greger-tools-get-schemas greger-tools))
         (server-tools (when greger-server-tools
                         (greger-server-tools-get-schemas greger-server-tools)))
         (chat-buffer (greger-state-chat-buffer state))
         (buffer-content (with-current-buffer chat-buffer
                           (buffer-substring-no-properties (point-min) (point-max))))
         (parse-result (greger-parser-parse-dialog buffer-content))
         (current-dialog (plist-get parse-result :messages))
         (current-iteration (greger-state-current-iteration state)))

    (greger--debug "=== ITERATION %d ===" current-iteration)
    (greger--debug "Dialog length: %d messages" (length current-dialog))

    ;; Check max iterations
    (when (>= current-iteration greger-max-iterations)
      (error "Maximum iterations (%d) reached, stopping agent execution" greger-max-iterations))

    ;; Get Claude's response
    (greger--debug "CALLING greger-client-stream...")
    (let ((client-state (greger-client-stream
                         :model greger-model
                         :dialog current-dialog
                         :tools tools
                         :server-tools server-tools
                         :buffer chat-buffer
                         :block-start-callback (lambda (content-block) (greger--append-streaming-content-header state content-block))
                         :text-delta-callback (lambda (text) (greger--append-text state text))
                         :block-stop-callback (lambda (type content-block) (greger--append-nonstreaming-content-block state type content-block))
                         :complete-callback (lambda (content-blocks) (greger--handle-stream-completion state content-blocks)))))
      ;; Store the client state for potential cancellation
      (setf (greger-state-client-state state) client-state)
      ;; Set buffer-local variable for greger-interrupt to access
      (with-current-buffer chat-buffer
        (setq greger--current-state state) ;; TODO: why do we set that _here_? Or should it be greger--current-client-state instead?
        (greger--update-buffer-state)))))

(defun greger--append-streaming-content-header (state content-block)
  ;; TODO: remove debug
  (let ((type (alist-get 'type content-block))
        (has-citations (greger-parser--content-block-has-citations content-block)))
   (cond
    ((and (string= type "text") (not has-citations))
     ;; TODO: remove debug

     (greger--append-text state (concat "\n\n" greger-parser-assistant-tag "\n\n")))
    ((string= type "thinking")
     (greger--append-text state (concat "\n\n" greger-parser-thinking-tag "\n\n")))
    (t nil))))

(defun greger--handle-stream-completion (state content-blocks)
  (let ((tool-calls (greger--extract-tool-calls content-blocks))
        (citations (greger--extract-citations content-blocks)))

    ;; Process citations if any
    (when citations
      (greger--debug "CITATIONS DETECTED! Found %d citation blocks" (length citations))
      (greger--append-citations-markdown state citations))

    ;; TODO: remove debug
    (if tool-calls
        (progn
          (greger--debug "TOOL USE DETECTED! Found %d tool calls" (length tool-calls))
          (setf (greger-state-current-iteration state)
                (1+ (greger-state-current-iteration state)))
          ;; TODO: execute tool calls in greger--append-content-block instead
          (greger--execute-tools tool-calls state))
      (progn
        (greger--debug "NO TOOL USE - CONVERSATION COMPLETE")
        (greger--finish-response state))))

  (with-current-buffer (greger-state-chat-buffer state)
    (greger--update-buffer-state)))

(defun greger--content-block-supports-streaming (content-block)
  (let ((type (alist-get 'type content-block))
        (has-citations (alist-get 'citations content-block)))
    (and (or (string= type "text") (string= type "thinking"))
         (not has-citations))))

(defun greger--append-nonstreaming-content-block (state type content-block)
  (greger--debug "CONTENT BLOCK: %s" content-block)

  (unless (greger--content-block-supports-streaming content-block)

   (let ((markdown (greger-parser--block-to-markdown content-block)))
     (greger--append-text
      state (concat (unless (greger-parser--content-block-has-citations content-block) "\n\n")
                    markdown)))

   (when (string= type "tool_use")
     (let ((tool-id (alist-get 'id content-block)))
       (greger--append-text state (concat "\n\n" (greger--tool-placeholder tool-id)))))

   ;; Update buffer state after client completes
   (with-current-buffer (greger-state-chat-buffer state)
     (greger--update-buffer-state))))

(defun greger--extract-tool-calls (content-blocks)
  "Extract tool calls from CONTENT-BLOCKS."
  (let ((tool-calls '()))
    (dolist (block content-blocks)
      (when (string= (alist-get 'type block) "tool_use")
        (greger--debug "EXTRACTING TOOL CALL: %s with input: %s"
                      (alist-get 'name block)
                      (json-encode (alist-get 'input block)))
        (push block tool-calls)))
    (reverse tool-calls)))

(defun greger--extract-citations (content-blocks)
  "Extract all citations from CONTENT-BLOCKS."
  (let ((all-citations '()))
    (dolist (block content-blocks)
      (let ((citations (alist-get 'citations block)))
        (when citations
          (setq all-citations (append all-citations citations)))))
    all-citations))

(defun greger--append-citations-markdown (state citations)
  "Append citations as markdown to the buffer using STATE and CITATIONS list."
  (when citations
    (let ((citations-markdown (greger--format-citations-as-markdown citations)))
      (greger--append-text state (concat "\n\n" citations-markdown)))))

(defun greger--format-citations-as-markdown (citations)
  "Format CITATIONS list as markdown according to the greger citation format."
  (when citations
    (concat greger-parser-citations-tag "\n\n"
            (mapconcat #'greger--format-single-citation-as-markdown citations "\n\n"))))

(defun greger--format-single-citation-as-markdown (citation)
  "Format a single CITATION as markdown."
  (let ((url (alist-get 'url citation))
        (title (alist-get 'title citation))
        (cited-text (alist-get 'cited_text citation))
        (encrypted-index (alist-get 'encrypted_index citation)))
    (concat "### " url "\n\n"
            "Title: " title "\n"
            "Cited text: " cited-text "\n"
            "Encrypted index: " encrypted-index)))

(defun greger--tool-placeholder (tool-id)
  "Generate placeholder string for TOOL-ID."
  (format "<!-- TOOL_RESULT_PLACEHOLDER_%s -->" tool-id))

(defun greger--execute-tools (tool-calls state)
  "Execute TOOL-CALLS using AGENT-STATE in parallel with callbacks."
  (let* ((total-tools (length tool-calls))
         (completed-tools 0)
         (executing-tools-map (make-hash-table :test 'equal)))

    ;; Initialize executing-tools in state if not already set
    (unless (greger-state-executing-tools state)
      (setf (greger-state-executing-tools state) executing-tools-map))

    ;; Update buffer state to show we're executing tools
    (with-current-buffer (greger-state-chat-buffer state)
      (greger--update-buffer-state))

    ;; First, display the tool calls and reserve space for each tool's output
    (with-current-buffer (greger-state-chat-buffer state)
      (let ((inhibit-read-only t))
        (goto-char (point-max))

        ;; Display each tool call followed by its placeholder
        ;; (dolist (tool-call tool-calls)
        ;;   (let ((tool-id (alist-get 'id tool-call))
        ;;         (tool-block-markdown (greger-parser--content-blocks-to-markdown (list tool-call))))
        ;;     (unless (string-empty-p tool-block-markdown)
        ;;       (insert "\n\n" tool-block-markdown))
        ;;     (insert "\n\n" (greger--tool-placeholder tool-id))))
        ))

    ;; Execute all tools in parallel
    (dolist (tool-call tool-calls)
      (let* ((tool-name (alist-get 'name tool-call))
             (tool-input (alist-get 'input tool-call))
             (tool-id (alist-get 'id tool-call))
             (default-directory (greger-state-directory state))

             ;; TODO: This is ugly, we really should be separating the creation and execution of tools
             ;; We're only doing this because for synchronous tools we can't set the tool in the
             ;; hashmap after execution, because sync tools have already removed the key then.
             (placeholder-tool (make-greger-tool :cancel-fn nil)))

        (puthash tool-id placeholder-tool (greger-state-executing-tools state))

        (greger--update-buffer-state)
        (sit-for 0.001) ;; update the buffer state

        (let ((greger-tool (greger-tools-execute
                            :tool-name tool-name
                            :args tool-input
                            :callback (lambda (result error)
                                        ;; Remove tool from executing-tools when complete
                                        (remhash tool-id (greger-state-executing-tools state))

                                        (greger--handle-tool-completion
                                         :tool-id tool-id
                                         :result result
                                         :error error
                                         :state state
                                         :completion-callback (lambda ()
                                                                (setq completed-tools (1+ completed-tools))
                                                                (when (= completed-tools total-tools)
                                                                  (greger--run-agent-loop state)))))
                            :buffer (greger-state-chat-buffer state)
                            :metadata (greger-state-metadata state))))

          ;; TODO: here again, it's ugly
          (when (greger-tool-cancel-fn greger-tool)
            (puthash tool-id greger-tool (greger-state-executing-tools state))))))))

(defun greger--append-text (state text)
  "Append TEXT to the chat buffer in AGENT-STATE."
  (with-current-buffer (greger-state-chat-buffer state)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert text))))

(cl-defun greger--handle-tool-completion (&key tool-id result error state completion-callback)
  "Handle completion of a tool execution by updating buffer and calling callback.
TOOL-ID is the tool identifier.
RESULT is the tool execution result.
ERROR is any error that occurred.
AGENT-STATE contains the current agent state.
COMPLETION-CALLBACK is called when complete."
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
    (with-current-buffer (greger-state-chat-buffer state)
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char (point-max))
          ;; Find and replace the placeholder
          (when (search-backward (greger--tool-placeholder tool-id) nil t)
            (replace-match "")
            (let ((result-markdown (greger-parser--content-blocks-to-markdown (list tool-result))))
              (unless (string-empty-p result-markdown)
                (insert result-markdown)))))))

    ;; Update buffer state after tool completion
    (with-current-buffer (greger-state-chat-buffer state)
      (greger--update-buffer-state))

    ;; Call completion callback
    (funcall completion-callback)))

(defun greger--finish-response (state)
  "Finish the agent response using AGENT-STATE."
  (greger--debug "=== FINISHING RESPONSE - CONVERSATION COMPLETE ===")
  (with-current-buffer (greger-state-chat-buffer state)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (unless (looking-back (concat greger-user-tag "\n\n") nil)
        (insert "\n\n" greger-user-tag "\n\n")))
    ;; Clear the buffer-local agent state
    (setq greger--current-state nil)
    ;; Update buffer state to idle
    (greger--update-buffer-state))
  ;; Reset the state
  (setf (greger-state-current-iteration state) 0)
  (setf (greger-state-client-state state) nil))

(defun greger-toggle-debug ()
  "Toggle debug output."
  (interactive)
  (setq greger-debug (not greger-debug))
  (message "Greger debug %s" (if greger-debug "enabled" "disabled")))





(defun greger--setup-heading-font-lock ()
  "Set up font-lock for headings to override markdown's larger font sizes."
  ;; Remove existing markdown heading font-lock rules for level 2 and 3 headings
  (setq-local font-lock-keywords
              (cl-remove-if
               (lambda (rule)
                 (and (listp rule)
                      (stringp (car rule))
                      (or (string-match-p "^\\^##" (car rule))
                          (string-match-p "^\\^###" (car rule))
                          (string-match-p "markdown-header-face-[23]" (format "%s" rule)))))
               font-lock-keywords))

  ;; Add our custom font-lock rules with highest priority
  (font-lock-add-keywords
   nil
   '(;; Level 2 headings (conversation roles)
     ("^## USER:.*$" 0 'greger-user-heading-face t)
     ("^## ASSISTANT:.*$" 0 'greger-assistant-heading-face t)
     ("^## SYSTEM:.*$" 0 'greger-system-heading-face t)
     ("^## THINKING:.*$" 0 'greger-thinking-heading-face t)
     ("^## TOOL USE:.*$" 0 'greger-tool-use-heading-face t)
     ("^## TOOL RESULT:.*$" 0 'greger-tool-result-heading-face t)
     ;; Level 3 headings (tool parameters)
     ("^###\\s-+.*$" 0 'greger-tool-param-heading-face t))
   'prepend)

  ;; Also remap the markdown faces
  ;(face-remap-add-relative 'markdown-header-face-2 'greger-assistant-heading-face)
  ;(face-remap-add-relative 'markdown-header-face-3 'greger-tool-param-heading-face)
  (font-lock-flush))

(defun greger--extend-font-lock-region ()
  "Extend font-lock region for greger mode.
Returns nil to indicate no region extension is needed."
  nil)

(defun greger--after-change-function (beg end _len)
  "Update tool sections and citations after buffer changes.
BEG is the beginning of the changed region.
END is the end of the changed region.
_LEN is the length of the pre-change text (unused)."
  ;; Only run timer-based cleanup for complex changes or when not actively streaming
  (when (and (> (- end beg) 0)  ; Only if there was an actual change
             (not (greger--is-actively-streaming)))
    (run-with-idle-timer 0.1 nil #'greger-ui-refresh-folding)))

(defun greger--is-actively-streaming ()
  "Check if we're currently streaming content from the AI."
  (and greger--current-state
       (greger-state-client-state greger--current-state)))

;; Private helper functions

(defun greger--get-current-code-block ()
  "Return the current code block under point, or nil if not found."
  (save-excursion
    (when (re-search-backward "^```" nil t)
      (forward-line)
      (let ((start (point)))
        (when (re-search-forward "^```" nil t)
          (backward-char 4)
          (buffer-substring-no-properties start (point)))))))

(defun greger--truncate-with-ellipsis (str max-width)
  "Truncate STR to MAX-WIDTH characters, adding an ellipsis if necessary."
  (let ((len (length str)))
    (if (<= len max-width)
        str
      (concat (substring str 0 (- max-width 3)) "..."))))



(provide 'greger)

;;; greger.el ends here
