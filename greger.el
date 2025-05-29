;;; greger.el --- Chat with language models -*- lexical-binding: t -*-

;; Copyright (C) 2023 Andreas Jansson

;; Author: Andreas Jansson <andreas@jansson.me.uk>
;; Version: 0.1.0
;; URL: https://github.com/andreasjansson/greger.el
;; Package-Requires: ((emacs "28.0") (parsec "0.1.3"))

;;; Commentary:
;; This package provides an interface for interacting with AI language models

;;; Code:

(require 'greger-stream)
(require 'greger-parser)
(require 'cl-lib)

(defconst greger-available-models
  '(replicate/meta/meta-llama-3.1-405b-instruct
    replicate/meta/meta-llama-3-70b-instruct
    replicate/meta/meta-llama-3-70b
    replicate/meta/meta-llama-3-8b-instruct
    replicate/meta/meta-llama-3-8b
    replicate/snowflake/snowflake-arctic-instruct
    claude/claude-3-haiku-20240307
    claude/claude-3-opus-20240229
    claude/claude-3-5-sonnet-20240620
    claude/claude-3-7-sonnet-latest
    claude/claude-sonnet-4-20250514
    claude/claude-opus-4-20250514
    openai/o1-preview
    openai/gpt-4-turbo
    openai/gpt-4o
    openai/gpt-4o-2024-08-06
    openai/gpt-4o-mini
    google/gemini-pro
    groq/llama3-8b-8192
    groq/llama3-70b-8192
    groq/mixtral-8x7b-32768
    ollama/llama3:8b
    ollama/llama3:text
    ollama/mistral:7b
    ollama/phi3:3.8b
    ollama/wizardlm2:7b
    ollama/gemma:2b)
  "List of available models.")

(defcustom greger-model 'claude/claude-sonnet-4-20250514
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

(defvar greger-user-tag "## USER:")
(defvar greger-assistant-tag "## ASSISTANT:")
(defvar greger-system-tag "## SYSTEM:")

;; Tool section collapsing variables
(defvar greger-tool-section-max-lines 4
  "Maximum number of lines to show in collapsed tool sections.")

(defvar greger-tool-overlays nil
  "List of overlays used for collapsible tool sections.")

;; Face definitions for tool tags
(defface greger-tool-tag-face
  '((t :height 0.6 :foreground "gray70"))
  "Face for tool opening and closing tags."
  :group 'greger)

(defface greger-tool-content-face
  '((t :inherit default))
  "Face for tool content."
  :group 'greger)

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
    (define-key map (kbd "M-<return>") #'greger-agent-buffer)
    (define-key map (kbd "C-M-<return>") #'greger-buffer-no-tools)
                                        ;(define-key map (kbd "M-<return>") #'greger-buffer)
    (define-key map (kbd "C-; a") #'greger-insert-assistant-tag)
    (define-key map (kbd "C-; u") #'greger-insert-user-tag)
    (define-key map (kbd "C-; s") #'greger-insert-system-tag)
    (define-key map (kbd "C-; i") #'greger-insert-include)
    (define-key map (kbd "C-; I") #'greger-insert-include-code)
    (define-key map (kbd "C-; f") #'greger-insert-include-file)
    (define-key map (kbd "C-; b") #'greger-insert-include-buffer-code)
    (define-key map (kbd "C-; m") #'greger-set-model)
    (define-key map (kbd "C-; c") #'greger-copy-code)
    (define-key map (kbd "TAB") #'greger-toggle-tool-section)
    (define-key map (kbd "<tab>") #'greger-toggle-tool-section)
    map)
  "Keymap for `greger-mode'.")

(define-derived-mode greger-mode gfm-mode "AI"
  "Major mode for interacting with AI."
  (use-local-map greger-mode-map)
  (setq-local markdown-fontify-code-blocks-natively t)
  (setq-local mode-line-misc-info '(:eval (symbol-name greger-model)))
  ;; Set up invisible text for tool sections
  (add-to-invisibility-spec 'greger-tool-section)
  ;; Set up tool section highlighting and collapsing
  (greger--setup-tool-sections)
  ;; Set up custom heading font-lock
  (greger--setup-heading-font-lock)
  ;; Add hook to update tool sections when buffer changes
  (add-hook 'after-change-functions #'greger--after-change-function nil t))

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
    (message (format "Using model %s" greger-model))))

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

(defun greger-buffer-no-tools ()
  "Send the buffer content to AI as a dialog without tool use."
  (interactive)
  (let ((greger-agent-tools '()))
    (greger-agent-buffer)))

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
          (message (format "Copied code: %s" (greger--truncate-with-ellipsis code-block 40))))
      (error "Point is not inside a code block"))))

(defun greger-set-model ()
  "Set the current AI model."
  (interactive)
  (let ((model (completing-read "Choose AI model: " greger-available-models nil t)))
    (customize-set-variable 'greger-model (intern model))
    (message "Model set to %s" model)))

;; Tool section collapsing functions

(defun greger--setup-tool-sections ()
  "Set up tool section highlighting and collapsing in the current buffer."
  (greger--clear-tool-overlays)
  (greger--find-and-setup-tool-sections))

(defun greger--clear-tool-overlays ()
  "Clear all tool section overlays in the current buffer."
  (cl-loop for overlay in greger-tool-overlays
           do (delete-overlay overlay))
  (setq greger-tool-overlays nil))

(defun greger--find-and-setup-tool-sections ()
  "Find all tool sections and set them up with appropriate faces and collapsing."
  (save-excursion
    (goto-char (point-min))
    (cl-loop while (re-search-forward "<tool\\.[^>]+>" nil t)
             do (greger--setup-single-tool-section))))

(defun greger--setup-single-tool-section ()
  "Set up a single tool section starting from the current match."
  (let ((start-tag-start (match-beginning 0))
        (start-tag-end (match-end 0))
        (tool-id (greger--extract-tool-id (match-string 0))))
    (when tool-id
      (let ((end-tag-pattern (concat "</tool\\." (regexp-quote tool-id) ">"))
            (content-start start-tag-end)
            content-end
            end-tag-start
            end-tag-end)

        ;; Find the corresponding closing tag
        (when (re-search-forward end-tag-pattern nil t)
          (setq end-tag-start (match-beginning 0)
                end-tag-end (match-end 0)
                content-end end-tag-start)

          ;; Create overlays for styling
          (greger--create-tag-overlay start-tag-start start-tag-end)
          (greger--create-tag-overlay end-tag-start end-tag-end)

          ;; Set up collapsible content
          (greger--setup-collapsible-content content-start content-end tool-id))))))

(defun greger--extract-tool-id (tag-string)
  "Extract tool ID from a tool tag string like '<tool.abc123>'."
  (when (string-match "<tool\\.\\([^>]+\\)>" tag-string)
    (match-string 1 tag-string)))

(defun greger--create-tag-overlay (start end)
  "Create an overlay for a tool tag to make it small and less visible."
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'face 'greger-tool-tag-face)
    (overlay-put overlay 'greger-tool-tag t)
    (push overlay greger-tool-overlays)
    overlay))

(defun greger--setup-collapsible-content (content-start content-end tool-id)
  "Set up collapsible content between CONTENT-START and CONTENT-END for TOOL-ID."
  (let* ((content (buffer-substring-no-properties content-start content-end))
         (lines (split-string content "\n"))
         (line-count (length lines)))

    (when (> line-count greger-tool-section-max-lines)
      ;; Create the collapsible overlay
      (greger--create-collapsible-overlay content-start content-end tool-id lines))))

(defun greger--create-collapsible-overlay (content-start content-end tool-id lines)
  "Create a collapsible overlay for tool content."
  (let* ((visible-lines (cl-subseq lines 0 greger-tool-section-max-lines))
         (hidden-lines (cl-subseq lines greger-tool-section-max-lines))
         (total-lines (length lines))
         (hidden-line-count (length hidden-lines))
         (visible-text (mapconcat #'identity visible-lines "\n"))
         (hidden-text (mapconcat #'identity hidden-lines "\n"))

         ;; Calculate positions for visible and hidden parts
         (visible-end (+ content-start (length visible-text)))
         (hidden-start (+ visible-end 1)) ; +1 for the newline

         ;; Create overlay for the hidden part
         (hidden-overlay (make-overlay hidden-start content-end)))

    (overlay-put hidden-overlay 'invisible 'greger-tool-section)
    (overlay-put hidden-overlay 'greger-tool-section t)
    (overlay-put hidden-overlay 'greger-tool-id tool-id)
    (overlay-put hidden-overlay 'greger-collapsed t)

    ;; Add expansion indicator with line count
    (let ((indicator-overlay (make-overlay visible-end visible-end)))
      (overlay-put indicator-overlay 'after-string
                   (propertize (format "... [+%d lines, TAB to expand]" hidden-line-count)
                              'face 'greger-tool-tag-face))
      (overlay-put indicator-overlay 'greger-tool-indicator t)
      (overlay-put indicator-overlay 'greger-tool-id tool-id)
      (push indicator-overlay greger-tool-overlays))

    (push hidden-overlay greger-tool-overlays)))

(defun greger-toggle-tool-section ()
  "Toggle the tool section at point between collapsed and expanded state."
  (interactive)
  (let ((tool-id (greger--get-tool-id-at-point)))
    (if tool-id
        (greger--toggle-tool-section-by-id tool-id)
      (message "Not inside a tool section"))))

(defun greger--get-tool-id-at-point ()
  "Get the tool ID for the tool section at point, if any."
  ;; First check overlays at point
  (or (cl-loop for overlay in (overlays-at (point))
               for tool-id = (overlay-get overlay 'greger-tool-id)
               when tool-id return tool-id)
      ;; If not found, search backwards and forwards for tool tags
      (greger--find-tool-id-near-point)))

(defun greger--find-tool-id-near-point ()
  "Find tool ID near point by searching for tool tags."
  (save-excursion
    (let ((start-pos (point))
          tool-id)
      ;; Search backwards for opening tag
      (when (re-search-backward "<tool\\.[^>]+>" nil t)
        (let ((open-tag-start (match-beginning 0))
              (open-tag-end (match-end 0))
              (tag-tool-id (greger--extract-tool-id (match-string 0))))
          ;; Check if we're within this tool section
          (when tag-tool-id
            (let ((close-pattern (concat "</tool\\." (regexp-quote tag-tool-id) ">")))
              (when (re-search-forward close-pattern nil t)
                (let ((close-tag-end (match-end 0)))
                  ;; If original point is between open and close tags
                  (when (and (>= start-pos open-tag-start)
                             (<= start-pos close-tag-end))
                    (setq tool-id tag-tool-id))))))))
      tool-id)))

(defun greger--toggle-tool-section-by-id (tool-id)
  "Toggle the tool section with the given TOOL-ID."
  (cl-loop for overlay in greger-tool-overlays
           when (and (overlay-get overlay 'greger-tool-section)
                     (string= (overlay-get overlay 'greger-tool-id) tool-id))
           do (greger--toggle-overlay-visibility overlay tool-id)))

(defun greger--toggle-overlay-visibility (overlay tool-id)
  "Toggle the visibility of OVERLAY for TOOL-ID."
  (let ((is-collapsed (overlay-get overlay 'greger-collapsed)))
    (if is-collapsed
        (greger--expand-tool-section overlay tool-id)
      (greger--collapse-tool-section overlay tool-id))))

(defun greger--expand-tool-section (overlay tool-id)
  "Expand the tool section by making OVERLAY visible."
  (overlay-put overlay 'invisible nil)
  (overlay-put overlay 'greger-collapsed nil)

  ;; Remove the expansion indicator
  (cl-loop for indicator-overlay in greger-tool-overlays
           when (and (overlay-get indicator-overlay 'greger-tool-indicator)
                     (string= (overlay-get indicator-overlay 'greger-tool-id) tool-id))
           do (progn
                (delete-overlay indicator-overlay)
                (setq greger-tool-overlays
                      (remove indicator-overlay greger-tool-overlays)))))

(defun greger--collapse-tool-section (overlay tool-id)
  "Collapse the tool section by making OVERLAY invisible."
  (overlay-put overlay 'invisible 'greger-tool-section)
  (overlay-put overlay 'greger-collapsed t)

  ;; Calculate the number of hidden lines for the indicator
  (let* ((content (buffer-substring-no-properties (overlay-start overlay) (overlay-end overlay)))
         (lines (split-string content "\n"))
         (hidden-line-count (length lines))
         (overlay-start (overlay-start overlay))
         (indicator-pos (max (point-min) (1- overlay-start)))
         (indicator-overlay (make-overlay indicator-pos indicator-pos)))
    (overlay-put indicator-overlay 'after-string
                 (propertize (format "... [+%d lines, TAB to expand]" hidden-line-count)
                            'face 'greger-tool-tag-face))
    (overlay-put indicator-overlay 'greger-tool-indicator t)
    (overlay-put indicator-overlay 'greger-tool-id tool-id)
    (push indicator-overlay greger-tool-overlays)))

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

(defun greger--after-change-function (beg end len)
  "Update tool sections after buffer changes."
  ;; Simple approach: refresh all tool sections
  ;; This could be optimized to only refresh affected sections
  (when (> (- end beg) 0)  ; Only if there was an actual change
    (run-with-idle-timer 0.1 nil #'greger--setup-tool-sections)))

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
