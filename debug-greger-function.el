;;; debug-greger-function.el --- Debug the greger function buffer creation

(add-to-list 'load-path ".")
(require 'greger)

;; Call greger and see what buffer content we get
(let ((original-buffers (buffer-list)))
  (greger)
  (let ((new-buffers (cl-remove-if (lambda (buf) (memq buf original-buffers))
                                   (buffer-list))))
    (when (> (length new-buffers) 0)
      (let ((greger-buffer (car new-buffers)))
        (message "Buffer name: %s" (buffer-name greger-buffer))
        (message "Major mode: %s" (with-current-buffer greger-buffer major-mode))
        (message "Buffer content:")
        (message "%s" (with-current-buffer greger-buffer (buffer-string)))
        ;; Clean up
        (kill-buffer greger-buffer)))))

;;; debug-greger-function.el ends here
