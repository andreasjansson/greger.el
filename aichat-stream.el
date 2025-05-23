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

;; Add this function to aichat-stream.el

(defun aichat-stream-to-buffer-with-tools (model dialog tools complete-callback &optional cancel-callback)
  "Send streaming request for MODEL with DIALOG and TOOLS, inserting text into current buffer.
COMPLETE-CALLBACK is called when done with the complete response.
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
         (wrapped-complete-callback (lambda (complete-response state)
                                      (when complete-callback
                                        (funcall complete-callback complete-response)))))

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
         (wrapped-complete-callback (lambda (complete-response state)
                                      ;; Throw away complete response for backward compatibility
                                      (declare (ignore complete-response))
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
         (complete-callback (lambda (complete-response state)
                              (declare (ignore state))
                              (when response-callback
                                (condition-case err
                                    (let ((response (aichat-stream--parse-complete-response
                                                   complete-response provider-config)))
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

  (aichat-stream--extract-and-process-chunks state provider-config))

(defun aichat-stream--extract-and-process-chunks (state provider-config)
  "Extract complete chunks from STATE and process them."
  (let ((accumulated (aichat-stream-state-accumulated-output state)))
    (cl-loop while (string-match-p "\n\n" accumulated)
             for separator-pos = (string-match-p "\n\n" accumulated)
             for chunk = (substring accumulated 0 separator-pos)
             for text = (aichat-providers-extract-text provider-config chunk)
             do (progn
                  (when (and text (aichat-stream-state-text-callback state))
                    (funcall (aichat-stream-state-text-callback state) text state))
                  (setq accumulated (substring accumulated (+ separator-pos 2))))
             finally (setf (aichat-stream-state-accumulated-output state) accumulated))))

(defun aichat-stream--handle-completion (proc state provider-config)
  "Handle process completion for PROC using STATE."
  (when (memq (process-status proc) '(exit signal))
    ;; Process any remaining accumulated output
    (let ((remaining (aichat-stream-state-accumulated-output state)))
      (unless (string-empty-p remaining)
        (ignore-errors
          (aichat-providers-extract-text provider-config remaining))))

    (funcall (aichat-stream-state-restore-callback state) state)

    (if (= (process-exit-status proc) 0)
        (when (aichat-stream-state-complete-callback state)
          (funcall (aichat-stream-state-complete-callback state)
                   (aichat-stream-state-complete-response state)
                   state))
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

(defun aichat-stream--parse-complete-response (response-text provider-config)
  "Parse complete RESPONSE-TEXT using PROVIDER-CONFIG."
  (or (ignore-errors
        (let ((parsed (json-read-from-string response-text)))
          (if (alist-get 'content parsed)
              parsed
            `((content . [((type . "text") (text . ,response-text))])))))
      ;; Fallback if JSON parsing fails
      `((content . [((type . "text") (text . ,response-text))]))))

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

;; Backward compatibility
(defalias 'aichat-stream-send-request 'aichat-stream-to-buffer)

(provide 'aichat-stream)

;;; aichat-stream.el ends here
