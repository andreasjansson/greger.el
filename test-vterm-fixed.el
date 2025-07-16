#!/usr/bin/env emacs --script
;; Test script to create a working vterm implementation

;; Load the necessary files
(add-to-list 'load-path "/Users/andreas/projects/greger.el")

(defun test-vterm-working (command callback)
  "Create a working vterm implementation"
  (require 'vterm)
  (let* ((buffer-name (format " *test-vterm-%s*" (random 100000)))
         (vterm-buffer (get-buffer-create buffer-name))
         (command-completed nil))
    
    (with-current-buffer vterm-buffer
      ;; Initialize vterm
      (vterm-mode)
      
      ;; Get the process
      (let ((process vterm--process))
        (when process
          (set-process-query-on-exit-flag process nil))
        
        ;; Set up process sentinel
        (when process
          (set-process-sentinel process
                                (lambda (proc event)
                                  (when (and (not command-completed)
                                            (string-match "\\(finished\\|exited\\)" event))
                                    (setq command-completed t)
                                    
                                    ;; Get the final output
                                    (let ((final-output (if (buffer-live-p vterm-buffer)
                                                           (with-current-buffer vterm-buffer
                                                             (buffer-string))
                                                         "")))
                                      (funcall callback final-output nil))
                                    
                                    (when (buffer-live-p vterm-buffer)
                                      (kill-buffer vterm-buffer))))))
        
        ;; Execute command after a short delay
        (run-with-timer 0.5 nil
                       (lambda ()
                         (when (buffer-live-p vterm-buffer)
                           (with-current-buffer vterm-buffer
                             ;; Clear the buffer
                             (vterm-clear)
                             
                             ;; Execute the command
                             (vterm-send-string command)
                             (vterm-send-return)
                             
                             ;; Exit to terminate shell
                             (vterm-send-string "exit")
                             (vterm-send-return)))))))))

;; Test the function
(let ((test-completed nil)
      (test-result nil)
      (test-error nil))
  
  (defun test-callback (result error)
    (setq test-completed t)
    (setq test-result result)
    (setq test-error error))
  
  (message "Testing working vterm command...")
  (test-vterm-working "echo 'Hello working vterm!'" #'test-callback)
  
  ;; Wait for completion
  (let ((counter 0))
    (while (and (not test-completed) (< counter 100))
      (sleep-for 0.1)
      (setq counter (1+ counter))))
  
  (if test-completed
      (progn
        (message "SUCCESS: Test completed!")
        (message "Result: %S" test-result)
        (message "Error: %S" test-error))
    (message "FAILED: Test did not complete within timeout")))
