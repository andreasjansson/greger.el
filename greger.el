;;; greger.el --- Agentic coding environment with tool use, using Claude -*- lexical-binding: t -*-

;; Copyright (C) 2025 Andreas Jansson

;; Author: Andreas Jansson <andreas@jansson.me.uk>
;; Version: 0.1.0
;; URL: https://github.com/andreasjansson/greger.el
;; Package-Requires: ((emacs "29.1"))
;; Keywords: agent, agentic, ai, chat, language-models, tools
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
;; Agentic coding environment using Claude and a suite of built-in tools.
;; Capable of viewing and editing files, searching code, listing files,
;; searching the web and downloading web pages.  Extensible with custom
;; tools defined in Emacs Lisp.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'treesit)

(require 'greger-client)
(require 'greger-parser)
(require 'greger-tools)
(require 'greger-stdlib)
(require 'greger-ui)
(require 'greger-ui)

(defconst greger-available-models
  '(claude-sonnet-4-20250514
    claude-opus-4-20250514)
  "List of available models.")

(defcustom greger-model 'claude-sonnet-4-20250514
  "The currently used model."
  :type `(choice ,@(mapcar (lambda (model) `(const ,model)) greger-available-models))
  :group 'greger)

(defcustom greger-default-system-prompt "You are an expert coding agent."
  "Default system prompt used for AI interactions."
  :type 'string
  :group 'greger)

(defcustom greger-max-tokens 32000
  "Maximum number of tokens to generate."
  :type 'integer
  :group 'greger)

(defcustom greger-thinking-budget 4096
  "Default budget for thinking (internal reasoning) content, in tokens.
Set to 0 to disable thinking entirely."
  :type 'integer
  :group 'greger)

(defcustom greger-allow-all-shell-commands nil
  "Allow all shell commands to run without permission.
May order 4,000 pounds of meat."
  :type 'boolean
  :group 'greger)

(defcustom greger-anthropic-key-fn nil
  "Function to call to get the Anthropic API key.
This function should return a string containing the API key.
If nil, the ANTHROPIC_API_KEY environment variable will be used."
  :type '(choice (const nil) function)
  :group 'greger)

;; Tool configuration and agent functionality

(defcustom greger-tools '("read-file" "write-new-file" "replace-file" "str-replace" "make-directory" "rename-file" "delete-files" "list-directory" "ripgrep" "shell-command" "read-webpage")
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

(defvar-local greger-current-thinking-budget greger-thinking-budget
  "Thinking budget for the current Greger chat.")

(defvar-local greger-follow-mode t
  "When non-nil, keep point at the bottom of the chat during updates.
When nil, preserve point position using `save-excursion'.")

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

(defface greger-field-name-face
  '((t (:foreground "lightyellow")))
  "Face for field names like \='Name:\=', \='ID:\=', etc."
  :group 'greger)

(defface greger-tool-param-name-face
  '((t (:foreground "lightgreen")))
  "Face for tool parameter names like \='path\=', \='content\=', etc."
  :group 'greger)

(defface greger-key-face
  '((t (:foreground "lightblue")))
  "Face for tool parameter names like \='path\=', \='content\=', etc."
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
    (define-key map (kbd "M-RET") #'greger-buffer)  ; Alternative for Linux compatibility
    (define-key map (kbd "C-M-<return>") #'greger-buffer-no-tools)
    (define-key map (kbd "C-M-RET") #'greger-buffer-no-tools)  ; Alternative for Linux compatibility
    (define-key map (kbd "C-g") #'greger-interrupt)
    (define-key map (kbd "C-; a") #'greger-insert-assistant-tag)
    (define-key map (kbd "C-; u") #'greger-insert-user-tag)
    (define-key map (kbd "C-; s") #'greger-insert-system-tag)
    (define-key map (kbd "C-; m") #'greger-set-model)
    (define-key map (kbd "C-; c") #'greger-ui-copy-code)
    (define-key map (kbd "C-; t") #'greger-toggle-thinking)
    (define-key map (kbd "C-; f") #'greger-toggle-follow-mode)
    (define-key map (kbd "C-; C-f") #'greger-ui-toggle-folding)
    (define-key map (kbd "C-; b") #'greger-mention-buffer-file)
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
   :feature 'tool-syntax-highlighting
   :override t
   '((tool_use) @greger-ui--tool-use-syntax-highlighting
     (tool_result) @greger-ui--tool-result-syntax-highlighting)

   :language 'greger
   :feature 'folding
   :override t
   '((assistant (citation_entry) @greger-ui--citation-entry-folding)
     (tool_content_tail) @greger-ui--tool-content-tail-folding
     (tool_content_head) @greger-ui--tool-content-head-folding
     (thinking_signature) @greger-ui--thinking-signature-hiding)

   :language 'greger
   :feature 'tool-tags
   :override t
   '((tool_start_tag) @greger-ui--make-tool-tag-invisible
     (tool_end_tag) @greger-ui--make-tool-tag-invisible
     (tool_use (id) @greger-ui--make-tool-use-id-invisible)
     (tool_result (id) @greger-ui--make-tool-result-id-invisible)
     (server_tool_use (id) @greger-ui--make-tool-use-id-invisible)
     (web_search_tool_result (id) @greger-ui--make-tool-result-id-invisible)
     (tool_param_header) @greger-tool-param-name-face
     (key) @greger-key-face
     (url) @greger-ui--url-link)

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
(add-to-list 'auto-mode-alist '("\\.greger\\'" . greger-mode))

;;;###autoload
(defun greger-install-grammar ()
  "Install greger tree-sitter grammar."
  (interactive)
  (add-to-list 'treesit-language-source-alist '(greger "https://github.com/andreasjansson/greger-grammar" "main"))
  (treesit-install-language-grammar 'greger)
  (unless (treesit-ready-p 'greger)
    (error "Tree-sitter for Greger isn't available")))

;;;###autoload
(define-derived-mode greger-mode prog-mode "Greger"
  "Major mode for editing Greger files with tree-sitter support."
  ;; Try to use tree-sitter if available
  (unless (treesit-language-available-p 'greger)
    (greger-install-grammar))
  (unless (treesit-ready-p 'greger)
    (error "Greger grammar is not installed"))

  (treesit-parser-create 'greger)
  (setq-local treesit-font-lock-settings greger--treesit-font-lock-settings)
  (setq-local treesit-font-lock-feature-list
              '((tool-tags tool-syntax-highlighting)
                (headers folding comments tool-result-syntax)
                (error)))
  (setq-local treesit-simple-indent-rules greger--treesit-indent-rules)

  ;; This crashes Emacs 29.0.91 but not Emacs 30.1. TODO: test if it crashes Emacs 29.1
  ;; Reproduce: At beginning of buffer, run (treesit-search-forward-goto (treesit-node-at (point)) "" t t t)
  (setq-local treesit-defun-type-regexp (rx line-start
                                            (or "system"
                                                "user"
                                                "assistant"
                                                "tool_use"
                                                "server_tool_user"
                                                "tool_result"
                                                "web_search_tool_result")
                                            line-end))

  (treesit-major-mode-setup)

  (setq-local mode-line-misc-info '(:eval (greger--mode-line-info)))
  (use-local-map greger-mode-map)

  (setq-local greger-current-thinking-budget greger-thinking-budget))

;;;###autoload
(defun greger (&optional with-context)
  "Create a new buffer and switch to `greger-mode`.
With WITH-CONTEXT (e.g. \[universal-argument] \[greger]), save
the current buffer, split the window if not already split, and
insert location information at the beginning of the user section."
  (interactive "P")
  (let ((buffer (generate-new-buffer "*greger*"))
        (source-info (when with-context
                       (save-buffer)
                       (list (buffer-file-name)
                             (line-number-at-pos)
                             (current-column)))))
    (when with-context
      ;; Split horizontally if not already split
      (when (= (length (window-list)) 1)
        (split-window-right))
      ;; Move to the next window
      (other-window 1))
    (switch-to-buffer buffer)
    (greger-mode)
    (insert greger-parser-system-tag
            "\n\n" greger-default-system-prompt "\n\n"
            greger-parser-user-tag
            "\n\n")
    (when source-info
      (let ((file-name (nth 0 source-info))
            (line-num (nth 1 source-info))
            (column (nth 2 source-info)))
        (insert (format "In %s, at line %d%s, implement the following:\n\n"
                        file-name
                        line-num
                        (if (> column 0)
                            (format " and column %d" column)
                          "")))))
    buffer))

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

  ;; to not get stuck in read only
  (greger--set-buffer-read-only nil)

  (let* ((buffer (current-buffer))
         (state (buffer-local-value 'greger--current-state buffer)))
    (cond

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

     ;; If there's an active client state, cancel the streaming request
     ((and state (greger-state-client-state state))
      (greger-client--cancel-request (greger-state-client-state state))
      (greger--finish-response state)
      'generating)

     ;; Default case: call keyboard-quit
     (t
      (keyboard-quit)
      'idle))))

(defun greger-set-model ()
  "Set the current model."
  (interactive)
  (let ((model (completing-read "Choose model: " greger-available-models nil t)))
    (customize-set-variable 'greger-model (intern model))
    (message "Model set to %s" model)))

(defun greger-toggle-thinking ()
  "Toggle thinking on/off."
  (interactive)
  (if (> greger-current-thinking-budget 0)
      (progn
        (setq-local greger-current-thinking-budget 0)
        (message "Thinking disabled"))
    (progn
      (setq-local greger-current-thinking-budget greger-thinking-budget)
      (message "Thinking enabled (budget: %d tokens)" greger-current-thinking-budget)))
  (force-mode-line-update))

(defun greger-toggle-follow-mode ()
  "Toggle follow mode on/off.
When follow mode is enabled, point moves to the bottom during chat updates.
When disabled, point position is preserved using `save-excursion'."
  (interactive)
  (setq-local greger-follow-mode (not greger-follow-mode))
  (when greger-follow-mode
    (goto-char (point-max)))
  (message "Follow mode %s" (if greger-follow-mode "enabled" "disabled"))
  (force-mode-line-update))

(defun greger-mention-buffer-file (buffer)
  "Insert the file path of BUFFER at point.
When called interactively, prompts for buffer selection.
When called programmatically, BUFFER can be a buffer object or buffer name.
Uses relative path if file is in current working directory or subdirectory.
Uses ~/path notation if file is in home directory or subdirectory."
  (interactive "bBuffer: ")

  (if-let* ((buf (if (bufferp buffer) buffer (get-buffer buffer)))
            (file-path (when buf (buffer-file-name buf))))
      (let* ((file-path-abs (expand-file-name file-path))
             (home-dir (expand-file-name "~/"))
             (cwd (file-name-as-directory (expand-file-name default-directory)))
             (formatted-path (cond
                              ;; If file is in cwd or subdirectory, use relative path
                              ((string-prefix-p cwd file-path-abs)
                               (file-relative-name file-path))
                              ;; If file is in home directory or subdirectory, use ~/path
                              ((string-prefix-p home-dir file-path-abs)
                               (concat "~/" (file-relative-name file-path home-dir)))
                              ;; Otherwise, use absolute path
                              (t file-path))))
        (insert formatted-path))
    (user-error "Buffer %s has no associated file" (or (buffer-name buf) buffer))))

(defun greger-debug-request ()
  "Debug the request data by parsing the buffer and saving the request data output.
After saving, opens the JSON file in a new buffer for inspection."
  (interactive)
  (let* ((filename (read-string "Save to filename (default: request.json): " nil nil "request.json"))
         (dialog (greger-parser-markdown-buffer-to-dialog (current-buffer)))
         (tools (when greger-tools
                  (greger-tools-get-schemas greger-tools)))
         (server-tools (when greger-server-tools
                         (greger-server-tools-get-schemas greger-server-tools)))
         (model greger-model)
         (request-data (greger-client--build-data model dialog tools server-tools greger-current-thinking-budget greger-max-tokens))
         (parsed-json (json-read-from-string request-data)))

    (with-temp-file filename
      (let ((json-encoding-pretty-print t))
        (insert (json-encode parsed-json))))
    (message "Request data saved to %s" filename)
    ;; Open the file in a new buffer for inspection
    (find-file filename)))

(defun greger-buffer (&optional no-tools)
  "Send buffer content to AI as an agent dialog with tool support.
When NO-TOOLS is set, disable tools and thinking."
  (interactive "P")
  (greger--ensure-buffer-can-be-submitted)

  (let ((greger-tools (if no-tools '() greger-tools))
        (greger-server-tools (if no-tools '() greger-server-tools))
        (greger-current-thinking-budget (if no-tools 0 greger-current-thinking-budget)))
    (greger--run-agent-loop (make-greger-state
                             :current-iteration 0
                             :chat-buffer (current-buffer)
                             :directory default-directory
                             :tool-use-metadata `(:safe-shell-commands () :allow-all-shell-commands ,greger-allow-all-shell-commands)))))

(defun greger-buffer-no-tools ()
  "Send the buffer content to AI as a dialog without tool use or thinking."
  (interactive)
  (greger-buffer t))

(defmacro greger--maybe-save-excursion (&rest body)
  "Execute BODY, optionally preserving point position.
If `greger-follow-mode' is enabled, point follows output
at the end of the buffer.
If `greger-follow-mode' is disabled, use `save-excursion'
to preserve point position."
  `(if greger-follow-mode
       (progn ,@body)
     (save-excursion ,@body)))

(defun greger--live-chat-buffer (state)
  "Return the chat buffer from STATE if it's still live."
  (let ((buffer (greger-state-chat-buffer state)))
    (when (buffer-live-p buffer)
      buffer)))

(defun greger--get-current-status ()
  "Get the current greger status: \='idle, \='generating, or \='executing."
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
  (let ((status (greger--get-current-status))
        (model-name (symbol-name greger-model)))
    (concat model-name
            (if (> greger-current-thinking-budget 0)
                (format " [T:%d]" greger-current-thinking-budget)
              "")
            (pcase status
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
  (let ((status (greger--get-current-status)))
    (greger--set-buffer-read-only (not (eq status 'idle)))
    ;; Force mode line update
    (force-mode-line-update)))

(defun greger--ensure-buffer-can-be-submitted ()
  "Ensure buffer can be submitted by fixing common issues with last message.
Uses tree-sitter to find the last node and applies heuristics:
- If last assistant text is empty or whitespace-only, insert '.'
- If last assistant text ends with whitespace, trim it
- If last message is thinking, insert '# ASSISTANT\\n\\n.' before submitting"
  (let* ((root-node (treesit-buffer-root-node))
         (last-node (greger--find-last-message-node root-node)))

    (when last-node
      (let ((node-type (treesit-node-type last-node)))
        (cond
         ((string= node-type "user")
          (greger--insert-if-empty-content last-node "Continue"))

         ((string= node-type "assistant")
          (greger--insert-if-empty-content last-node "."))

         ((string= node-type "thinking")
          (greger--insert-assistant-after-thinking last-node)))))))

(defun greger--find-last-message-node (root-node)
  "Find the last message node in ROOT-NODE."
  (let ((children (treesit-node-children root-node))
        (last-message-node nil))
    (dolist (child children)
      (let ((node-type (treesit-node-type child)))
        (when (member node-type '("assistant" "thinking" "user" "system"
                                  "tool_use" "server_tool_use" "tool_result"
                                  "web_search_tool_result" "untagged_text"))
          (setq last-message-node child))))
    last-message-node))

(defun greger--insert-if-empty-content (node text)
  "Add TEXT to NODE content if content is empty."
  (let* ((header (treesit-node-child node 0 t))
         (start-pos (treesit-node-end header))
         (end-pos (treesit-node-end node))
         (content (buffer-substring-no-properties start-pos end-pos)))

    ;; Insert a dot if content is empty
    (when (string= (string-trim content) "")
      (greger--maybe-save-excursion
       (goto-char end-pos)
       (insert text)))))

(defun greger--insert-assistant-after-thinking (thinking-node)
  "Insert assistant response after THINKING-NODE."
  (let ((end-pos (treesit-node-end thinking-node)))
    (greger--maybe-save-excursion
     (goto-char end-pos)
     (insert "\n\n# ASSISTANT\n\n."))))

(defun greger--run-agent-loop (state)
  "Run the main agent loop with STATE."
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
        
        ;; Store the client state for potential cancellation
        (setf (greger-state-client-state state) client-state)
        ;; Set buffer-local variable for greger-interrupt to access
        (setq greger--current-state state) ;; TODO: why do we set that _here_? Or should it be greger--current-client-state instead?
        (greger--update-buffer-state)))))

(defun greger--clean-excessive-newlines (text)
  "Remove excessive newlines from the end of TEXT, keeping at most two.
If TEXT ends with more than two consecutive newlines, remove all but the
first two."
  (replace-regexp-in-string "\n\n\n+\\'" "\n\n" text))

(defun greger--append-streaming-content-header (state content-block)
  "Append appropriate header for streaming CONTENT-BLOCK to STATE."
  (let ((type (alist-get 'type content-block))
        (has-citations (assq 'citations content-block)))
    (cond
     ((and (string= type "text") (not has-citations))
      (greger--append-text state (concat "\n\n" greger-parser-assistant-tag "\n\n")))
     ((string= type "thinking")
      (greger--append-text state (concat "\n\n" greger-parser-thinking-tag "\n\n")))
     (t nil))))

(defun greger--handle-stream-completion (state content-blocks)
  "Handle completion of stream with STATE and CONTENT-BLOCKS."
  (if-let ((tool-calls (greger--extract-tool-calls content-blocks)))
      (progn
        (setf (greger-state-current-iteration state)
              (1+ (greger-state-current-iteration state)))
        ;; TODO: execute tool calls in greger--append-content-block instead
        (greger--execute-tools tool-calls state))
    (greger--finish-response state))

  (when-let ((buffer (greger--live-chat-buffer state)))
    (with-current-buffer buffer
      (greger--update-buffer-state))))

(defun greger--content-block-supports-streaming (content-block)
  "Check if CONTENT-BLOCK can be streamed incrementally.
Returns non-nil for text and thinking blocks without citations, which can
be displayed as they arrive rather than waiting for completion."
  (let ((type (alist-get 'type content-block))
        (citations (alist-get 'citations content-block)))
    (and (or (string= type "text") (string= type "thinking"))
         (not citations))))

(defun greger--append-handle-content-block-stop (state type content-block)
  "Append non-streaming CONTENT-BLOCK of TYPE to STATE."
  ;; Only append markdown if it hasn't done that already during streaming
  (unless (greger--content-block-supports-streaming content-block)
    (let ((markdown (greger-parser--block-to-markdown content-block)))
      (greger--append-text state (concat "\n\n" markdown))))

  ;; Special cases for tool use and thinking
  (cond
   ((string= type "tool_use")
    (let ((tool-id (alist-get 'id content-block)))
      (greger--append-text state (concat "\n\n" (greger--tool-result-placeholder tool-id)))))
   ((string= type "thinking")
    (let ((signature (alist-get 'signature content-block)))
      (greger--insert-thinking-signature state signature))))

  ;; Update buffer state after client completes
  (when-let ((buffer (greger--live-chat-buffer state)))
    (with-current-buffer buffer
      (greger--update-buffer-state))))

(defun greger--insert-thinking-signature (state signature)
  "Insert thinking SIGNATURE after the last thinking tag, using STATE.
Assumes the last inserted thing is a thinking tag."
  (with-current-buffer (greger-state-chat-buffer state)
    (save-excursion
      (goto-char (point-max))
      (re-search-backward "^# THINKING")
      (forward-line 1)
      (let ((inhibit-read-only t))
        (insert "\n"
                "Signature: " signature
                "\n")))))

(defun greger--extract-tool-calls (content-blocks)
  "Extract tool call from CONTENT-BLOCKS."
  (let ((tool-calls '()))
    (dolist (block content-blocks)
      (when (string= (alist-get 'type block) "tool_use")
        (push block tool-calls)))
    (reverse tool-calls)))

(defun greger--tool-result-placeholder (tool-id)
  "Generate placeholder string for TOOL-ID."
  ;; It's ugly that we need to insert the trailing \n
  ;; But we need it because otherwise the closing tool tag is not recognized.
  ;; TODO: update greger-grammar
  (let ((placeholder (concat (greger-parser--wrapped-tool-content greger-parser-tool-result-tag tool-id "") "\n")))
    ;; Add a text property to the first character to indicate this is a generating tool result
    (put-text-property 0 1 'greger-tool-result-generating t placeholder)
    placeholder))

(defun greger--execute-tools (tool-calls state)
  "Execute TOOL-CALLS using STATE in parallel with callbacks."
  (let* ((total-tools (length tool-calls))
         (completed-tools 0)
         (executing-tools-map (make-hash-table :test 'equal)))

    ;; Initialize executing-tools in state if not already set
    (unless (greger-state-executing-tools state)
      (setf (greger-state-executing-tools state) executing-tools-map))

    ;; Update buffer state to show we're executing tools
    (when-let ((buffer (greger--live-chat-buffer state)))
      (with-current-buffer buffer
        (greger--update-buffer-state)

        (greger--maybe-save-excursion
         (goto-char (point-max)))))

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
                                                                ;; TODO: add completed-tools to state instead of this nested callback
                                                                (setq completed-tools (1+ completed-tools))
                                                                (when (and (= completed-tools total-tools)
                                                                           (greger-state-chat-buffer state))
                                                                  (greger--run-agent-loop state)))))
                            :streaming-callback (lambda (text)
                                                  (greger--append-tool-result-text state tool-id text))
                            :buffer (greger-state-chat-buffer state)
                            :metadata (greger-state-tool-use-metadata state))))

          ;; TODO: here again, it's ugly
          (when (greger-tool-cancel-fn greger-tool)
            (puthash tool-id greger-tool (greger-state-executing-tools state))))))))

(defun greger--append-text (state text)
  "Append TEXT to the chat buffer in STATE."
  (let ((buffer (greger-state-chat-buffer state)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (greger--maybe-save-excursion
         (goto-char (point-max))
         (insert text))))))

(cl-defun greger--handle-tool-completion (&key tool-id result error state completion-callback)
  "Handle completion of a tool execution.
Updates buffer and calls callback when tool execution finishes.
TOOL-ID is the tool identifier.
RESULT is the tool execution result.
ERROR is any error that occurred.
STATE contains the current agent state.
COMPLETION-CALLBACK is called when complete."
  (let ((tool-result-text (if error
                              (if (listp error)
                                  (error-message-string error)
                                error)
                            result)))

    (greger--append-tool-result-text state tool-id tool-result-text t)

    ;; Call completion callback
    (funcall completion-callback)))

(defun greger--append-tool-result-text (state tool-id text &optional is-completed)
  "Append TEXT to the tool result content node for TOOL-ID in STATE's buffer.
When IS-COMPLETED is non-nil, also trim trailing newline after the tool's
end tag and update the buffer state."
  (when-let ((buffer (greger--live-chat-buffer state)))
    (with-current-buffer buffer
      (when-let ((inhibit-read-only t)
                 (tool-result-content-node (greger--find-tool-result-content-node tool-id))
                 (tool-result-content-end (treesit-node-end tool-result-content-node)))
        (greger--maybe-save-excursion
         (goto-char (1- tool-result-content-end))

         ;; Process terminal sequences to handle progress bars and dynamic output
         ;; The function processes text at current point, inserting and moving cursor
         (greger-ui--process-terminal-sequences text)

         (when is-completed
           ;; Trim trailing newline after closing tag
           (when-let* ((tool-result-content-node (greger--find-tool-result-content-node tool-id)) ;; content node might have gotten outdated, so need to get it again
                       (tool-end-tag (treesit-node-next-sibling tool-result-content-node t)))
             (when (string= (treesit-node-type tool-end-tag) "tool_end_tag")
               (goto-char (treesit-node-end tool-end-tag))
               (when (looking-at-p "\n")
                 (delete-char 1))))

           ;; Remove the generating property from the tool result
           (when-let ((tool-result-node (greger--find-tool-result-node tool-id)))
             (let ((start (treesit-node-start tool-result-node))
                   (end (treesit-node-end tool-result-node)))
               (remove-text-properties start end '(greger-tool-result-generating))))

           ;; Update buffer state after tool completion
           (greger--update-buffer-state)))))))

(defun greger--find-tool-result-content-node (tool-id)
  "Find the tool_content node for the tool_result with TOOL-ID.
Uses treesit to query for a tool_result with matching id and returns
the tool_content node within its content section."
  (when-let* ((query `((tool_result (id (value) @id) (:match ,tool-id @id)
                                    (content) @content)))
              (capture (treesit-query-capture (treesit-buffer-root-node) query))
              (content-node (alist-get 'content capture))
              ;; for some reason, querying directly for tool_content fails, but this works
              (content-node-first-child (car (treesit-node-children content-node))))
    (treesit-search-subtree content-node-first-child "tool_content")))

(defun greger--find-tool-result-node (tool-id)
  "Find the tool_result node with TOOL-ID.
Uses treesit to query for a tool_result with matching id and returns
the tool_result node itself."
  (let* ((query `((tool_result (id (value) @id) (:match ,tool-id @id)) @tool-result))
         (capture (treesit-query-capture (treesit-buffer-root-node) query)))
    (alist-get 'tool-result capture)))

(defun greger--handle-client-error (state error-message)
  "Handle client error with STATE and ERROR-MESSAGE."
  ;; This will be called from the main thread via the completion handler
  (when-let ((buffer (greger--live-chat-buffer state)))
    (with-current-buffer buffer
      ;; Clear the buffer-local agent state
      (setq greger--current-state nil)
      ;; Update buffer state to idle
      (greger--update-buffer-state)))
  ;; Reset the state
  (setf (greger-state-current-iteration state) 0)
  (setf (greger-state-client-state state) nil)
  ;; Raise the error in the main thread where it can be caught by tests
  (warn "%s" error-message))

(defun greger--finish-response (state)
  "Finish the agent response using STATE."
  (when-let ((buffer (greger--live-chat-buffer state)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (greger--maybe-save-excursion
         (goto-char (point-max))
         (unless (looking-back (concat greger-parser-user-tag "\n\n") nil)
           (insert "\n\n" greger-parser-user-tag "\n\n"))))
      ;; Clear the buffer-local agent state
      (setq greger--current-state nil)
      ;; Update buffer state to idle
      (greger--update-buffer-state)))
  ;; Reset the state
  (setf (greger-state-current-iteration state) 0)
  (setf (greger-state-client-state state) nil))

(provide 'greger)

;;; greger.el ends here
