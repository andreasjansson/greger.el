;;; Fixed vterm test with proper scoping

(add-to-list 'load-path ".")
(add-to-list 'load-path "~/.emacs.d/elpa/vterm-20241218.331")

(require 'vterm)

(defvar test-vterm-output "")

(defun test-simple-vterm ()
  "Test vterm directly to see what happens."
  (interactive)
  (let* ((buffer-name " *test-vterm*")
         (vterm-buffer (get-buffer-create buffer-name))
         (completion-marker "GREGER_DONE_12345")
         (found-completion nil))
    
    (with-current-buffer vterm-buffer
      (let ((vterm-kill-buffer-on-exit nil)
            (vterm-shell "bash"))
        (vterm-mode)
        
        ;; Set up process for non-interactive cleanup
        (when vterm--process
          (set-process-query-on-exit-flag vterm--process nil))
        
        ;; Monitor buffer changes
        (add-hook 'after-change-functions
                  (lambda (start end old-len)
                    (let ((content (buffer-string)))
                      (setq test-vterm-output content)
                      (message "Buffer changed. Length: %d" (length content))
                      (when (and (not found-completion)
                                (string-match completion-marker content))
                        (setq found-completion t)
                        (message "Found completion marker!")
                        (message "Final content: %s" content)
                        (run-with-timer 0.1 nil
                                       (lambda ()
                                         (when (buffer-live-p vterm-buffer)
                                           (with-current-buffer vterm-buffer
                                             (when vterm--process
                                               (set-process-query-on-exit-flag vterm--process nil)
                                               (delete-process vterm--process)))
                                           (kill-buffer vterm-buffer)))))))
                  nil t)
        
        ;; Wait for shell to be ready
        (run-with-timer 0.5 nil
                       (lambda ()
                         (when (buffer-live-p vterm-buffer)
                           (with-current-buffer vterm-buffer
                             (message "Executing command...")
                             (vterm-send-string (format "ls && echo %s" completion-marker))
                             (vterm-send-return)))))))))

;; Run the test
(test-simple-vterm)

;; Wait for completion
(sleep-for 5)
(message "=== FINAL OUTPUT ===")
(message "%s" test-vterm-output)
(message "=== END OUTPUT ===")
(message "Test completed")
