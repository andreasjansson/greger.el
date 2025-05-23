;;; aichat-stream.el --- Streaming support for aichat -*- lexical-binding: t -*-

;;; Commentary:
;; Handles streaming responses from AI providers

;;; Code:

(require 'json)
(require 'aichat-providers)

(defun aichat-stream-send-request (model dialog complete-callback &optional cancel-callback)
  "Send streaming request for MODEL with DIALOG.
COMPLETE-CALLBACK is called when done.
CANCEL-CALLBACK is called if cancelled."
  (let* ((provider-config (aichat-providers-get-config model))
         (request-spec (aichat-providers-build-request provider-config dialog))
         (output-buffer (current-buffer))
         (insert-position (point))
         (undo-handle (prepare-change-group output-buffer))
         (original-quit-binding (local-key-binding (kbd "C-g")))
         (process (aichat-stream--start-curl-process request-spec))
         (accumulated-output "")
         (restore-callback (lambda ()
                             (with-current-buffer output-buffer
                               (local-set-key (kbd "C-g") original-quit-binding)
                               (undo-amalgamate-change-group undo-handle)
                               (accept-change-group undo-handle)))))

    (activate-change-group undo-handle)

    (set-process-filter
     process
     (lambda (_proc output)
       (setq accumulated-output
             (aichat-stream--process-output
              output
              provider-config
              (lambda (text)
                (with-current-buffer output-buffer
                  (goto-char insert-position)
                  (insert text)
                  (setq insert-position (point))))
              accumulated-output))))

    (set-process-sentinel
     process
     (lambda (proc _event)
       (aichat-stream--handle-completion
        proc provider-config complete-callback cancel-callback
        restore-callback accumulated-output)))

    (set-process-query-on-exit-flag process nil)

    (with-current-buffer output-buffer
      (local-set-key (kbd "C-g")
                     (lambda ()
                       (interactive)
                       (aichat-stream--cancel-request process cancel-callback restore-callback))))))

(defun aichat-stream--start-curl-process (request-spec)
  "Start curl process with REQUEST-SPEC."
  (let ((curl-command (aichat-stream--build-curl-command request-spec)))
    (start-process-shell-command "aichat-curl" nil curl-command)))

(defun aichat-stream--build-curl-command (request-spec)
  "Build curl command from REQUEST-SPEC."
  (let ((url (plist-get request-spec :url))
        (method (plist-get request-spec :method))
        (headers (plist-get request-spec :headers))
        (data (plist-get request-spec :data)))
    (let ((header-strings (mapconcat
                           (lambda (header)
                             (format "-H \"%s: %s\"" (car header) (cdr header)))
                           headers " "))
          (data-string (if data
                           (format "-d '%s'" (replace-regexp-in-string "'" "'\\\\''" data))
                         "")))
      (format "curl -s -X %s %s %s %s" method header-strings data-string url))))

(defun aichat-stream--process-output (output provider-config text-callback accumulated-output)
  "Process OUTPUT from provider using PROVIDER-CONFIG.
TEXT-CALLBACK is called with extracted text.
ACCUMULATED-OUTPUT is the running output buffer."
  (setq accumulated-output (concat accumulated-output output))
  (let ((separator-position (string-match-p "\n\n" accumulated-output)))
    (while separator-position
      (let* ((chunk (substring accumulated-output 0 separator-position))
             (text (aichat-providers-extract-text provider-config chunk)))
        (when text
          (funcall text-callback text))
        (setq accumulated-output (substring accumulated-output (+ separator-position 2)))
        (setq separator-position (string-match-p "\n\n" accumulated-output)))))
  accumulated-output)

(defun aichat-stream--handle-completion (proc provider-config complete-callback cancel-callback restore-callback accumulated-output)
  "Handle process completion for PROC."
  (when (memq (process-status proc) '(exit signal))
    (unless (string-empty-p accumulated-output)
      (condition-case nil
          (let ((text (aichat-providers-extract-text provider-config accumulated-output)))
            (when text
              ;; Insert final text if any
              ))
        (error nil)))
    (funcall restore-callback)
    (if (= (process-exit-status proc) 0)
        (when complete-callback (funcall complete-callback))
      (when cancel-callback (funcall cancel-callback)))))

(defun aichat-stream--cancel-request (process cancel-callback restore-callback)
  "Cancel streaming REQUEST."
  (when (process-live-p process)
    (message "Interrupting generation")
    (interrupt-process process)
    (sit-for 0.1)
    (delete-process process)
    (when cancel-callback (funcall cancel-callback)))
  (funcall restore-callback))

(provide 'aichat-stream)

;;; aichat-stream.el ends here
