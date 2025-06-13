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
(require 'treesit)

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

(defcustom greger-allow-all-shell-commands nil
  "Allow all shell commands to run without asking for permission. May order 4,000 pounds of meat."
  :type 'boolean
  :group 'greger)

;; Tool configuration and agent functionality

(defcustom greger-tools '("read-file" "list-directory" "str-replace" "insert" "write-new-file" "replace-file" "make-directory" "rename-file" "ripgrep" "git-log" "git-show-commit" "shell-command" "read-webpage" "delete-files")
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

;;; Agent state structure

(cl-defstruct greger-state
  current-iteration
  chat-buffer
  directory
  tool-use-metadata
  client-state
  executing-tools)

(defvar-local greger--current-state nil
  "Buffer-local variable to track the current state.")

(defvar-local greger--buffer-read-only-by-greger nil
  "Buffer-local variable to track if buffer is read-only due to greger activity.")

(defcustom greger-citation-summary-face 'underline
  "Face to use for citation text when folded."
  :type 'face
  :group 'greger)

(defface greger-user-header-face
  '((t (:foreground "cyan" :weight bold :height 1.1)))
  "Face for USER headers."
  :group 'greger)

(defface greger-assistant-header-face
  '((t (:foreground "green" :weight bold :height 1.1)))
  "Face for ASSISTANT headers."
  :group 'greger)

(defface greger-system-header-face
  '((t (:foreground "orange" :weight bold :height 1.1)))
  "Face for SYSTEM headers."
  :group 'greger)

(defface greger-thinking-header-face
  '((t (:foreground "magenta" :weight bold :height 1.1)))
  "Face for THINKING headers."
  :group 'greger)

(defface greger-tool-header-face
  '((t (:foreground "yellow" :weight bold :height 1.1)))
  "Face for tool-related headers (TOOL USE, TOOL RESULT, etc.)."
  :group 'greger)

(defface greger-subheading-face
  '((t (:foreground "coral" :weight semi-bold)))
  "Face for subheadings like tool parameters and citation entries."
  :group 'greger)

(defface greger-field-name-face
  '((t (:foreground "lightyellow")))
  "Face for field names like 'Name:', 'ID:', etc."
  :group 'greger)

(defface greger-tool-param-name-face
  '((t (:foreground "lightgreen")))
  "Face for tool parameter names like 'path', 'content', etc."
  :group 'greger)

(defface greger-key-face
  '((t (:foreground "lightblue")))
  "Face for tool parameter names like 'path', 'content', etc."
  :group 'greger)

(defface greger-tool-tag-face
  '((t (:foreground "gray" :height 0.6)))
  "Face for tool start and end tags."
  :group 'greger)

(defface greger-citation-face
  '((t (:underline "#555588")))
  "Face for links."
  :group 'greger)

(defface greger-link-face
  '((t (:foreground "aqua" :weight semi-bold)))
  "Face for links."
  :group 'greger)

(defface greger-error-face
  '((t (:background "red" :foreground "white")))
  "Face for parse errors in greger-mode."
  :group 'greger)

(defvar greger-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-<return>") #'greger-buffer)
    (define-key map (kbd "C-M-<return>") #'greger-buffer-no-tools)
    (define-key map (kbd "C-g") #'greger-interrupt)
    (define-key map (kbd "C-; a") #'greger-insert-assistant-tag)
    (define-key map (kbd "C-; u") #'greger-insert-user-tag)
    (define-key map (kbd "C-; s") #'greger-insert-system-tag)
    (define-key map (kbd "C-; m") #'greger-set-model)
    (define-key map (kbd "C-; c") #'greger-ui--copy-code)
    (define-key map (kbd "C-; D") #'greger-debug-request)
    map)
  "Keymap for `greger-mode'.")

(defvar greger--treesit-font-lock-settings
  (treesit-font-lock-rules
   :language 'greger
   :feature 'headers
   :override t
   '((user_header) @greger-user-header-face
     (assistant_header) @greger-assistant-header-face
     (system_header) @greger-system-header-face
     (thinking_header) @greger-thinking-header-face
     (tool_use_header) @greger-tool-header-face
     (tool_result_header) @greger-tool-header-face
     (server_tool_use_header) @greger-tool-header-face
     (web_search_tool_result_header) @greger-tool-header-face)

   :language 'greger
   :feature 'folding
   :override t
   '((assistant (citation_entry) @greger-ui--citation-entry-folding-fn)
     (tool_content_tail) @greger-ui--tool-content-tail-folding-fn
     (tool_content_head) @greger-ui--tool-content-head-folding-fn)

   :language 'greger
   :feature 'subheadings
   :override t
   '((citation_entry) @greger-subheading-face)

   :language 'greger
   :feature 'fields
   :override t
   '((tool_param_header) @greger-tool-param-name-face
     (key) @greger-key-face
     (url) @greger-ui--url-link-fn
     )

   :language 'greger
   :feature 'tool-tags
   :override t
   '((tool_start_tag) @greger-tool-tag-face
     (tool_end_tag) @greger-tool-tag-face)

   :language 'greger
   :feature 'comments
   :override t
   '((html_comment) @font-lock-comment-face)

   :language 'greger
   :feature 'error
   :override t
   '((ERROR) @greger-error-face))
  "Tree-sitter font-lock settings for `greger-mode'.")

(defvar greger--treesit-indent-rules
  `((greger
     ((node-is "user") column-0 0)
     ((node-is "assistant") column-0 0)
     ((node-is "system") column-0 0)
     ((node-is "thinking") column-0 0)
     ((node-is "tool_use") column-0 0)
     ((node-is "tool_result") column-0 0)
     ((node-is "server_tool_use") column-0 0)
     ((node-is "web_search_tool_result") column-0 0)))
  "Tree-sitter indentation rules for `greger-mode'.")

;;;###autoload
(define-derived-mode greger-mode prog-mode "Greger"
  "Major mode for editing Greger files with tree-sitter support."
  ;; Try to use tree-sitter if available
  (greger-parser-activate-tree-sitter)
  (treesit-ready-p 'greger)
  (treesit-parser-create 'greger)
  (setq-local treesit-font-lock-settings greger--treesit-font-lock-settings)
  (setq-local treesit-font-lock-feature-list
              '((error)
                (headers folding tool-folding fields)
                (tool-tags comments)
                (subheadings)))
  (setq-local treesit-simple-indent-rules greger--treesit-indent-rules)
  
  ;; Disabled because this crashes Emacs.
  ;; Reproduce: At beginning of buffer, run (treesit-search-forward-goto (treesit-node-at (point)) "" t t t)
  ;; (setq-local treesit-defun-type-regexp (rx line-start (or "user" "assistant") line-end))
  
  (treesit-major-mode-setup)

  (setq-local mode-line-misc-info '(:eval (greger--mode-line-info)))
  (use-local-map greger-mode-map))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.greger\\'" . greger-mode))

;;;###autoload
(defun greger ()
  "Create a new buffer and switch to `greger-mode`."
  (interactive)
  (let ((buffer (generate-new-buffer "*greger*")))
    (switch-to-buffer buffer)
    (greger-mode)
    (insert greger-parser-system-tag
            "\n\n" greger-default-system-prompt "\n\n"
            greger-parser-user-tag
            "\n\n")))

(defun greger-insert-assistant-tag ()
  "Insert the assistant tag into the buffer."
  (interactive)
  (insert greger-parser-assistant-tag "\n\n"))

(defun greger-insert-user-tag ()
  "Insert the user tag into the buffer."
  (interactive)
  (insert greger-parser-user-tag "\n\n"))

(defun greger-insert-system-tag ()
  "Insert the system tag into the buffer."
  (interactive)
  (insert greger-parser-system-tag "\n\n"))

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
      'idle)))

  ;; to not get stuck in read only
  (greger--set-buffer-read-only nil))

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
         (dialog (greger-parser-markdown-buffer-to-dialog (current-buffer)))
         (tools (when greger-tools
                  (greger-tools-get-schemas greger-tools)))
         (server-tools (when greger-server-tools
                          (greger-server-tools-get-schemas greger-server-tools)))
         (model greger-model)
         (greger-client--build-data model dialog tools server-tools))

    (let* ((parsed-json (json-read-from-string request-data)))
      (with-temp-file filename
        (let ((json-encoding-pretty-print t))
          (insert (json-encode parsed-json))))
      (message "Request data saved to %s" filename))))

(defun greger-buffer ()
  "Send buffer content to AI as an agent dialog with tool support."
  (interactive)
  ;; (goto-char (point-max)) ; TODO: is this needed?
  (greger--run-agent-loop (make-greger-state
                           :current-iteration 0
                           :chat-buffer (current-buffer)
                           :directory default-directory
                           :tool-use-metadata `(:safe-shell-commands () :allow-all-shell-commands ,greger-allow-all-shell-commands))))

(defun greger-buffer-no-tools ()
  "Send the buffer content to AI as a dialog without tool use."
  (interactive)
  (let ((greger-tools '())
        (greger-server-tools '()))
    (greger-buffer)))

(defun greger--get-current-state ()
  "Get the current greger state: \='idle, \='generating, or \='executing."
  (let ((state (buffer-local-value 'greger--current-state (current-buffer))))
    (cond
     ;; Check if we're executing tools
     ((and state
           (greger-state-executing-tools state)
           (> (hash-table-count (greger-state-executing-tools state)) 0))
      'executing)
     ;; Check if we're generating (client-state is active)
     ((and state (greger-state-client-state state))
      'generating)
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
  "Run the main agent loop with STATE."
  (let* ((tools (greger-tools-get-schemas greger-tools))
         (server-tools (when greger-server-tools
                         (greger-server-tools-get-schemas greger-server-tools)))
         (chat-buffer (greger-state-chat-buffer state))
         (dialog (greger-parser-markdown-buffer-to-dialog chat-buffer))
         (safe-shell-commands (greger-parser-find-safe-shell-commands-in-buffer chat-buffer))
         (tool-use-metadata (greger-state-tool-use-metadata state))
         (current-iteration (greger-state-current-iteration state)))

    (setf (plist-get tool-use-metadata :safe-shell-commands) safe-shell-commands)

    (when (>= current-iteration greger-max-iterations)
      (error "Maximum iterations (%d) reached, stopping agent execution" greger-max-iterations))

    (let ((client-state (greger-client-stream
                         :model greger-model
                         :dialog dialog
                         :tools tools
                         :server-tools server-tools
                         :buffer chat-buffer
                         :block-start-callback (lambda (content-block)
                                                 (greger--append-streaming-content-header state content-block))
                         :text-delta-callback (lambda (text)
                                                (greger--append-text state (greger--clean-excessive-newlines text)))
                         :block-stop-callback (lambda (type content-block)
                                                (greger--append-nonstreaming-content-block state type content-block))
                         :complete-callback (lambda (content-blocks) (greger--handle-stream-completion state content-blocks)))))

      ;; Store the client state for potential cancellation
      (setf (greger-state-client-state state) client-state)
      ;; Set buffer-local variable for greger-interrupt to access
      (with-current-buffer chat-buffer
        (setq greger--current-state state) ;; TODO: why do we set that _here_? Or should it be greger--current-client-state instead?
        (greger--update-buffer-state)))))

(defun greger--clean-excessive-newlines (text)
  "Remove excessive newlines from the end of TEXT, keeping at most two.
If TEXT ends with more than two consecutive newlines, remove all but the first two."
  (replace-regexp-in-string "\n\n\n+\\'" "\n\n" text))

(defun greger--append-streaming-content-header (state content-block)
  (let ((type (alist-get 'type content-block))
        (has-citations (not (null (assq 'citations content-block)))))
   (cond
    ((and (string= type "text") (not has-citations))
     (greger--append-text state (concat "\n\n" greger-parser-assistant-tag "\n\n")))
    ((string= type "thinking")
     (greger--append-text state (concat "\n\n" greger-parser-thinking-tag "\n\n")))
    (t nil))))

(defun greger--handle-stream-completion (state content-blocks)
  (let ((tool-calls (greger--extract-tool-calls content-blocks)))

    (if tool-calls
        (progn
          (setf (greger-state-current-iteration state)
                (1+ (greger-state-current-iteration state)))
          ;; TODO: execute tool calls in greger--append-content-block instead
          (greger--execute-tools tool-calls state))
      (progn
        (greger--finish-response state))))

  (with-current-buffer (greger-state-chat-buffer state)
    (greger--update-buffer-state)))

(defun greger--content-block-supports-streaming (content-block)
  (let ((type (alist-get 'type content-block))
        (citations (alist-get 'citations content-block)))
    (and (or (string= type "text") (string= type "thinking"))
         (not citations))))

(defun greger--append-nonstreaming-content-block (state type content-block)
  (unless (greger--content-block-supports-streaming content-block)
   (let ((markdown (greger-parser--block-to-markdown content-block)))
     (greger--append-text
      state (concat "\n\n" markdown)))

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
        (push block tool-calls)))
    (reverse tool-calls)))

(defun greger--tool-placeholder (tool-id)
  "Generate placeholder string for TOOL-ID."
  (greger-parser--wrapped-tool-content greger-parser-tool-result-tag tool-id "Loading..."))

(defun greger--execute-tools (tool-calls state)
  "Execute TOOL-CALLS using STATE in parallel with callbacks."
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
        (goto-char (point-max))))

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
                            :metadata (greger-state-tool-use-metadata state))))

          ;; TODO: here again, it's ugly
          (when (greger-tool-cancel-fn greger-tool)
            (puthash tool-id greger-tool (greger-state-executing-tools state))))))))

(defun greger--append-text (state text)
  "Append TEXT to the chat buffer in STATE."
  (let ((buffer (greger-state-chat-buffer state)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert text))))))

(cl-defun greger--handle-tool-completion (&key tool-id result error state completion-callback)
  "Handle completion of a tool execution by updating buffer and calling callback.
TOOL-ID is the tool identifier.
RESULT is the tool execution result.
ERROR is any error that occurred.
STATE contains the current agent state.
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
            (let ((result-markdown (greger-parser--tool-result-to-markdown tool-result)))
              (unless (string-empty-p result-markdown)
                (insert result-markdown)))))))

    ;; Update buffer state after tool completion
    (with-current-buffer (greger-state-chat-buffer state)
      (greger--update-buffer-state))

    ;; Call completion callback
    (funcall completion-callback)))

(defun greger--finish-response (state)
  "Finish the agent response using STATE."
  (with-current-buffer (greger-state-chat-buffer state)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (unless (looking-back (concat greger-parser-user-tag "\n\n") nil)
        (insert "\n\n" greger-parser-user-tag "\n\n")))
    ;; Clear the buffer-local agent state
    (setq greger--current-state nil)
    ;; Update buffer state to idle
    (greger--update-buffer-state))
  ;; Reset the state
  (setf (greger-state-current-iteration state) 0)
  (setf (greger-state-client-state state) nil))

(provide 'greger)

;;; greger.el ends here
