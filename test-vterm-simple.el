;;; Simple vterm test to debug completion detection

(add-to-list 'load-path ".")
(add-to-list 'load-path "~/.emacs.d/elpa/vterm-20241218.331")

(require 'vterm)

(defun test-simple-vterm ()
  "Test vterm directly to see what happens."
  (interactive)
  (let* ((buffer-name " *test-vterm*")
         (vterm-buffer (get-buffer-create buffer-name))
         (completion-marker "GREGER_DONE_12345"))
    
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
                      (message "=== BUFFER CONTENT ===")
                      (message "%s" content)
                      (message "=== END CONTENT ===")
                      (when (string-match completion-marker content)
                        (message "Found completion marker!")
                        (when vterm--process
                          (set-process-query-on-exit-flag vterm--process nil)
                          (delete-process vterm--process))
                        (kill-buffer vterm-buffer))))
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
(sleep-for 3)
(message "Test completed")
