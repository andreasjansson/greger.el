#!/usr/bin/env emacs --script
;; Simple test for the clean vterm function

;; Load the necessary files
(add-to-list 'load-path "/Users/andreas/projects/greger.el")
(require 'vterm)

;; Define a simple clean vterm function
(defun test-vterm-command (command callback)
  "Simple test vterm command execution"
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
        
        ;; Function to extract clean output
        (defun extract-clean-output ()
          "Extract command output, filtering out shell prompts and command echoes."
          (let ((content (buffer-string)))
            (message "RAW CONTENT: %S" content)
            ;; Remove shell prompts, command echoes, and empty lines
            (let ((lines (split-string content "\n")))
              (let ((filtered-lines
                     (seq-filter 
                      (lambda (line)
                        (let ((trimmed (string-trim line)))
                          (and (not (string-empty-p trimmed))
                               (not (string-match "^[^@]*@[^:]*:" trimmed))  ; Shell prompts
                               (not (string-match "^\\$" trimmed))           ; $ prompts
                               (not (string-match "^>" trimmed))             ; > prompts
                               (not (string-prefix-p command trimmed))       ; Command echo
                               (not (string-match "^exit" trimmed)))))       ; Exit command
                      lines)))
                (string-trim (string-join filtered-lines "\n"))))))
        
        ;; Set up process sentinel
        (when process
          (set-process-sentinel process
                                (lambda (proc event)
                                  (message "PROCESS EVENT: %S" event)
                                  (when (and (not command-completed)
                                            (string-match "\\(finished\\|exited\\)" event))
                                    (setq command-completed t)
                                    
                                    ;; Get the final output
                                    (let ((final-output (if (buffer-live-p vterm-buffer)
                                                           (with-current-buffer vterm-buffer
                                                             (extract-clean-output))
                                                         "")))
                                      (message "FINAL OUTPUT: %S" final-output)
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
  
  (message "Testing simple vterm command...")
  (test-vterm-command "echo 'Hello vterm!'" #'test-callback)
  
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
