;;; aichat-stream.el --- Streaming support for aichat -*- lexical-binding: t -*-

;;; Commentary:
;; Handles streaming responses from AI providers

;;; Code:

(require 'json)
(require 'cl-lib)
(require 'aichat-providers)

;;; Data structures

(cl-defstruct aichat-stream-state
  accumulated-output
  complete-response
  parsed-content-blocks
  process
  output-buffer
  insert-position
  undo-handle
  original-quit-binding
  text-callback
  complete-callback
  cancel-callback
  restore-callback)

;;; Public API

(defun aichat-stream-to-buffer-with-tools (model dialog tools complete-callback &optional cancel-callback)
  "Send streaming request for MODEL with DIALOG and TOOLS, inserting text into current buffer.
COMPLETE-CALLBACK is called when done with the parsed content blocks array.
CANCEL-CALLBACK is called if cancelled."
  (let* ((output-buffer (current-buffer))
         (insert-position (point))
         (undo-handle (prepare-change-group output-buffer))
         (original-quit-binding (local-key-binding (kbd "C-g")))
         (restore-callback (lambda (state)
                             (with-current-buffer (aichat-stream-state-output-buffer state)
                               (local-set-key (kbd "C-g")
                                            (aichat-stream-state-original-quit-binding state))
                               (undo-amalgamate-change-group (aichat-stream-state-undo-handle state))
                               (accept-change-group (aichat-stream-state-undo-handle state)))))
         (text-callback (lambda (text state)
                          (with-current-buffer (aichat-stream-state-output-buffer state)
                            (goto-char (aichat-stream-state-insert-position state))
                            (insert text)
                            (setf (aichat-stream-state-insert-position state) (point)))))
         (wrapped-complete-callback (lambda (parsed-blocks state)
                                      (when complete-callback
                                        (funcall complete-callback parsed-blocks)))))

    (activate-change-group undo-handle)

    (aichat-stream--send-request-with-tools
     model dialog tools text-callback wrapped-complete-callback cancel-callback restore-callback
     :output-buffer output-buffer
     :insert-position insert-position
     :undo-handle undo-handle
     :original-quit-binding original-quit-binding)))

(defun aichat-stream--send-request-with-tools (model dialog tools text-callback complete-callback
                                                    cancel-callback restore-callback
                                                    &rest state-args)
  "Internal function to send streaming request with tools."
  (let* ((provider-config (aichat-providers-get-config model))
         (request-spec (aichat-providers-build-request provider-config dialog tools))
         (process (aichat-stream--start-curl-process request-spec))
         (state (apply #'make-aichat-stream-state
                      :accumulated-output ""
                      :complete-response ""
                      :parsed-content-blocks '()
                      :process process
                      :text-callback text-callback
                      :complete-callback complete-callback
                      :cancel-callback cancel-callback
                      :restore-callback restore-callback
                      state-args)))

    (set-process-filter process
                       (lambda (proc output)
                         (declare (ignore proc))
                         (aichat-stream--process-output-chunk output state provider-config)))

    (set-process-sentinel process
                         (lambda (proc event)
                           (declare (ignore event))
                           (aichat-stream--handle-completion proc state provider-config)))

    (set-process-query-on-exit-flag process nil)

    (when (aichat-stream-state-output-buffer state)
      (aichat-stream--setup-cancel-binding state))

    state))

(defun aichat-stream-to-buffer (model dialog complete-callback &optional cancel-callback)
  "Send streaming request for MODEL with DIALOG, inserting text into current buffer.
COMPLETE-CALLBACK is called when done (with no arguments).
CANCEL-CALLBACK is called if cancelled."
  (let* ((output-buffer (current-buffer))
         (insert-position (point))
         (undo-handle (prepare-change-group output-buffer))
         (original-quit-binding (local-key-binding (kbd "C-g")))
         (restore-callback (lambda (state)
                             (with-current-buffer (aichat-stream-state-output-buffer state)
                               (local-set-key (kbd "C-g")
                                            (aichat-stream-state-original-quit-binding state))
                               (undo-amalgamate-change-group (aichat-stream-state-undo-handle state))
                               (accept-change-group (aichat-stream-state-undo-handle state)))))
         (text-callback (lambda (text state)
                          (with-current-buffer (aichat-stream-state-output-buffer state)
                            (goto-char (aichat-stream-state-insert-position state))
                            (insert text)
                            (setf (aichat-stream-state-insert-position state) (point)))))
         (wrapped-complete-callback (lambda (parsed-blocks state)
                                      ;; Ignore parsed blocks for backward compatibility
                                      (declare (ignore parsed-blocks))
                                      (when complete-callback (funcall complete-callback)))))

    (activate-change-group undo-handle)

    (aichat-stream--send-request-internal
     model dialog text-callback wrapped-complete-callback cancel-callback restore-callback
     :output-buffer output-buffer
     :insert-position insert-position
     :undo-handle undo-handle
     :original-quit-binding original-quit-binding)))

(defun aichat-stream-request (request response-callback &optional error-callback)
  "Send streaming REQUEST and call RESPONSE-CALLBACK with complete response.
REQUEST should be an alist with request parameters.
RESPONSE-CALLBACK is called with the parsed response.
ERROR-CALLBACK is called with error message if request fails."
  (let* ((model (alist-get 'model request))
         (dialog (aichat-stream--convert-request-to-dialog request))
         (provider-config (aichat-providers-get-config model))
         (complete-callback (lambda (parsed-blocks state)
                              (declare (ignore state))
                              (when response-callback
                                (condition-case err
                                    (let ((response `((content . ,(apply #'vector parsed-blocks)))))
                                      (funcall response-callback response))
                                  (error
                                   (when error-callback
                                     (funcall error-callback
                                            (format "Error parsing response: %s"
                                                   (error-message-string err))))))))))

    (aichat-stream--send-request-internal
     model dialog nil complete-callback error-callback (lambda (state) nil))))

;;; Internal implementation

(defun aichat-stream--send-request-internal (model dialog text-callback complete-callback
                                                  cancel-callback restore-callback
                                                  &rest state-args)
  "Internal function to send streaming request.
STATE-ARGS are additional arguments to initialize the state struct."
  (let* ((provider-config (aichat-providers-get-config model))
         (request-spec (aichat-providers-build-request provider-config dialog))
         (process (aichat-stream--start-curl-process request-spec))
         (state (apply #'make-aichat-stream-state
                      :accumulated-output ""
                      :complete-response ""
                      :parsed-content-blocks '()
                      :process process
                      :text-callback text-callback
                      :complete-callback complete-callback
                      :cancel-callback cancel-callback
                      :restore-callback restore-callback
                      state-args)))

    (set-process-filter process
                       (lambda (proc output)
                         (declare (ignore proc))
                         (aichat-stream--process-output-chunk output state provider-config)))

    (set-process-sentinel process
                         (lambda (proc event)
                           (declare (ignore event))
                           (aichat-stream--handle-completion proc state provider-config)))

    (set-process-query-on-exit-flag process nil)

    (when (aichat-stream-state-output-buffer state)
      (aichat-stream--setup-cancel-binding state))

    state))

(defun aichat-stream--setup-cancel-binding (state)
  "Setup C-g binding for cancellation in the output buffer."
  (with-current-buffer (aichat-stream-state-output-buffer state)
    (local-set-key (kbd "C-g")
                   (lambda ()
                     (interactive)
                     (aichat-stream--cancel-request state)))))

(defun aichat-stream--process-output-chunk (output state provider-config)
  "Process a chunk of OUTPUT using STATE."
  ;; Always accumulate for complete response
  (setf (aichat-stream-state-complete-response state)
        (concat (aichat-stream-state-complete-response state) output))

  ;; Update working buffer for chunk processing
  (setf (aichat-stream-state-accumulated-output state)
        (concat (aichat-stream-state-accumulated-output state) output))

  (aichat-stream--process-claude-events state))

(defun aichat-stream--process-claude-events (state)
  "Process Claude streaming events from accumulated output in STATE."
  (let ((accumulated (aichat-stream-state-accumulated-output state))
        (remaining ""))

    ;; Process complete lines (events)
    (while (string-match "\n" accumulated)
      (let* ((line-end (match-end 0))
             (line (substring accumulated 0 (1- line-end))))

        ;; Process the line if it's a data event
        (when (string-prefix-p "data: " line)
          (let ((data-json (substring line 6)))
            (unless (string= data-json "[DONE]")
              (aichat-stream--handle-claude-event data-json state))))

        ;; Remove processed line
        (setq accumulated (substring accumulated line-end))))

    ;; Store remaining incomplete data
    (setf (aichat-stream-state-accumulated-output state) accumulated)))

(defun aichat-stream--handle-claude-event (data-json state)
  "Handle a single Claude event with DATA-JSON using STATE."
  (condition-case nil
      (let* ((data (json-read-from-string data-json))
             (type (alist-get 'type data)))
        (cond
         ;; Content block start - create new content block
         ((string= type "content_block_start")
          (let* ((index (alist-get 'index data))
                 (content-block (copy-alist (alist-get 'content_block data)))
                 (blocks (aichat-stream-state-parsed-content-blocks state)))

            ;; Initialize content for accumulation
            (cond
             ((string= (alist-get 'type content-block) "tool_use")
              (setf (alist-get 'input content-block) ""))
             ((string= (alist-get 'type content-block) "text")
              (setf (alist-get 'text content-block) "")))

            ;; Add block at the right index
            (aichat-stream--ensure-block-at-index blocks index content-block state)))

         ;; Content block delta - update existing content block
         ((string= type "content_block_delta")
          (let* ((index (alist-get 'index data))
                 (delta (alist-get 'delta data))
                 (delta-type (alist-get 'type delta))
                 (blocks (aichat-stream-state-parsed-content-blocks state)))

            (when (< index (length blocks))
              (let ((block (nth index blocks)))
                (cond
                 ;; Text delta
                 ((string= delta-type "text_delta")
                  (let ((text (alist-get 'text delta)))
                    (setf (alist-get 'text block)
                          (concat (alist-get 'text block) text))
                    ;; Call text callback for live display
                    (when (aichat-stream-state-text-callback state)
                      (funcall (aichat-stream-state-text-callback state) text state))))

                 ;; Tool input delta
                 ((string= delta-type "input_json_delta")
                  (let ((partial-json (alist-get 'partial_json delta)))
                    (setf (alist-get 'input block)
                          (concat (alist-get 'input block) partial-json)))))))))

         ;; Content block stop - finalize tool input if needed
         ((string= type "content_block_stop")
          (let* ((index (alist-get 'index data))
                 (blocks (aichat-stream-state-parsed-content-blocks state)))

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
                       (setf (alist-get 'input block) '())))))))))))
    (error nil))) ; Ignore parse errors

(defun aichat-stream--ensure-block-at-index (blocks index new-block state)
  "Ensure BLOCKS list has NEW-BLOCK at INDEX, extending if necessary."
  (let ((current-blocks (aichat-stream-state-parsed-content-blocks state)))
    ;; Extend list if needed
    (while (<= (length current-blocks) index)
      (setq current-blocks (append current-blocks (list nil))))

    ;; Set the block at index
    (setf (nth index current-blocks) new-block)
    (setf (aichat-stream-state-parsed-content-blocks state) current-blocks)))

(defun aichat-stream--handle-completion (proc state provider-config)
  "Handle process completion for PROC using STATE."
  (when (memq (process-status proc) '(exit signal))
    (funcall (aichat-stream-state-restore-callback state) state)

    (if (= (process-exit-status proc) 0)
        (when (aichat-stream-state-complete-callback state)
          (let ((parsed-blocks (aichat-stream-state-parsed-content-blocks state)))
            (funcall (aichat-stream-state-complete-callback state) parsed-blocks state)))
      (when (aichat-stream-state-cancel-callback state)
        (funcall (aichat-stream-state-cancel-callback state))))))

(defun aichat-stream--cancel-request (state)
  "Cancel streaming request using STATE."
  (let ((process (aichat-stream-state-process state)))
    (when (process-live-p process)
      (message "Interrupting generation")
      (interrupt-process process)
      (sit-for 0.1)
      (delete-process process)
      (when (aichat-stream-state-cancel-callback state)
        (funcall (aichat-stream-state-cancel-callback state))))
    (funcall (aichat-stream-state-restore-callback state) state)))

;;; Utility functions

(defun aichat-stream--convert-request-to-dialog (request)
  "Convert agent REQUEST format to dialog format."
  (let ((messages (alist-get 'messages request))
        (system (alist-get 'system request)))
    (append
     (when system
       (list `((role . "system")
              (content . ,(alist-get 'text (aref system 0))))))
     (cl-map 'list
             (lambda (msg)
               `((role . ,(alist-get 'role msg))
                 (content . ,(alist-get 'content msg))))
             messages))))

(defun aichat-stream--start-curl-process (request-spec)
  "Start curl process with REQUEST-SPEC."
  (start-process-shell-command
   "aichat-curl" nil
   (aichat-stream--build-curl-command request-spec)))

(defun aichat-stream--build-curl-command (request-spec)
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
                (format "-d '%s'" (replace-regexp-in-string "'" "'\\\\''" data))
              "")
            url)))

(provide 'aichat-stream)

;;; aichat-stream.el ends here
