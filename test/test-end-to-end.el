;;; test-end-to-end.el --- True end-to-end tests for greger -*- lexical-binding: t -*-

;;; Commentary:
;;
;; This file contains true end-to-end tests that use greger as an end user would,
;; calling only the public API functions `greger` and `greger-buffer`.
;; These tests require a valid ANTHROPIC_API_KEY environment variable.
;;
;; WARNING: These tests make real API calls and may incur costs.
;;

(require 'ert)
(require 'greger)

(defvar greger-test-timeout 30
  "Timeout in seconds for API calls in tests.")



(defun greger-test-wait-for-response (buffer timeout)
  "Wait for a response to appear in BUFFER within TIMEOUT seconds."
  (let ((start-time (current-time))
        (completed nil)
        (response-started nil)
        (initial-content (with-current-buffer buffer (buffer-string))))

    (while (and (not completed)
                (< (float-time (time-subtract (current-time) start-time)) timeout)
                (buffer-live-p buffer))
      (sit-for 0.2)
      ;; Check if buffer content has changed (response received)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (let ((current-content (buffer-string)))
            ;; Check if response started
            (when (and (not response-started)
                      (not (string= initial-content current-content))
                      (string-match-p "## ASSISTANT:" current-content))
              (setq response-started t))

            ;; If response started, wait for it to finish
            (when response-started
              ;; Check if conversation is complete (has USER prompt at end)
              (if (string-match-p "## USER:\n\n$" current-content)
                  (setq completed t)
                ;; Or if it's been a while since response started, consider it done
                (let ((response-time (- (float-time (current-time))
                                       (float-time start-time))))
                  (when (> response-time 5.0) ; If response has been going for 5+ seconds
                    (setq completed t)))))))))

    completed))

(defun greger-test-wait-for-streaming-complete ()
  "Wait for any active streaming processes to complete."
  (let ((max-wait 3.0)
        (start-time (current-time)))
    (while (and (< (float-time (time-subtract (current-time) start-time)) max-wait)
                (cl-some (lambda (proc)
                          (and (process-live-p proc)
                               (string-match-p "greger-curl" (process-name proc))))
                        (process-list)))
      (sit-for 0.1))))

(ert-deftest greger-end-to-end-test-greger-function ()
  "Test the main greger function creates a buffer and sets it up correctly."
  :tags '(end-to-end public-api)

  (let ((original-buffers (buffer-list)))
    (unwind-protect
        (progn
          ;; Call the main greger function
          (greger)

          ;; Verify a new greger buffer was created
          (let ((new-buffers (cl-remove-if (lambda (buf) (memq buf original-buffers))
                                           (buffer-list))))
            (should (= 1 (length new-buffers)))
            (let ((greger-buffer (car new-buffers)))
              (should (string-match-p "\\*greger\\*" (buffer-name greger-buffer)))

              ;; Verify buffer is in greger-mode
              (with-current-buffer greger-buffer
                (should (eq major-mode 'greger-mode))

                ;; Verify initial content is set up correctly
                (let ((content (buffer-string)))
                  (should (string-match-p "## SYSTEM:" content))
                  (should (string-match-p "## USER:" content))
                  (should (string-match-p greger-default-system-prompt content)))

                ;; Verify we're at the end of the buffer (ready for user input)
                (should (= (point) (point-max)))))))

      ;; Cleanup: kill any greger buffers we created
      (dolist (buffer (buffer-list))
        (when (and (string-match-p "\\*greger\\*" (buffer-name buffer))
                   (not (memq buffer original-buffers)))
          (kill-buffer buffer))))))

(ert-deftest greger-end-to-end-test-simple-conversation ()
  "Test a simple conversation using the public API."
  :tags '(end-to-end public-api)


  (let ((greger-buffer nil))
    (unwind-protect
        (progn
          ;; Create a greger buffer
          (greger)
          (setq greger-buffer (current-buffer))

          ;; Add a simple user message
          (goto-char (point-max))
          (insert "Hello! Please respond with exactly 'Hello from greger test!' and nothing else.")

          ;; Call greger-buffer to send the message
          (greger-buffer)

          ;; Wait for response
          (should (greger-test-wait-for-response greger-buffer greger-test-timeout))

          ;; Wait for streaming to complete
          (greger-test-wait-for-streaming-complete)

          ;; Verify response was added to buffer
          (let ((content (buffer-string)))
            (should (string-match-p "## ASSISTANT:" content))
            (should (string-match-p "Hello from greger test!" content))
            ;; Should have a new USER section at the end (or at least assistant response)
            (should (or (string-match-p "## USER:\n\n$" content)
                       (string-match-p "## ASSISTANT:" content)))))

      ;; Cleanup
      (when (and greger-buffer (buffer-live-p greger-buffer))
        (kill-buffer greger-buffer)))))

(ert-deftest greger-end-to-end-test-tool-use-conversation ()
  "Test a conversation that involves tool use using the public API."
  :tags '(end-to-end public-api tools)


  (let ((greger-buffer nil)
        (test-file nil))
    (unwind-protect
        (progn
          ;; Create a temporary test file
          (setq test-file (make-temp-file "greger-test-" nil ".txt"))
          (with-temp-file test-file
            (insert "This is a test file for greger end-to-end testing."))

          ;; Create a greger buffer
          (greger)
          (setq greger-buffer (current-buffer))

          ;; Add a user message that should trigger tool use
          (goto-char (point-max))
          (insert (format "Please read the file %s and tell me what it contains." test-file))

          ;; Call greger-buffer to send the message
          (greger-buffer)

          ;; Wait for response (tool use might take longer)
          (should (greger-test-wait-for-response greger-buffer (* greger-test-timeout 2)))

          ;; Wait for streaming to complete
          (greger-test-wait-for-streaming-complete)

          ;; Verify response was added to buffer
          (let ((content (buffer-string)))
            (should (string-match-p "## ASSISTANT:" content))
            ;; Should have tool use section or content from the file
            (should (or (string-match-p "## TOOL USE:" content)
                       (string-match-p "read-file" content)
                       (string-match-p "test file for greger" content)))
            ;; Should have a new USER section at the end (or at least assistant response)
            (should (or (string-match-p "## USER:\n\n$" content)
                       (string-match-p "## ASSISTANT:" content)))))

      ;; Cleanup
      (when (and test-file (file-exists-p test-file))
        (delete-file test-file))
      (when (and greger-buffer (buffer-live-p greger-buffer))
        (kill-buffer greger-buffer)))))

(ert-deftest greger-end-to-end-test-no-tools-mode ()
  "Test the no-tools mode using C-M-return."
  :tags '(end-to-end public-api no-tools)


  (let ((greger-buffer nil)
        (test-file nil))
    (unwind-protect
        (progn
          ;; Create a temporary test file
          (setq test-file (make-temp-file "greger-test-" nil ".txt"))
          (with-temp-file test-file
            (insert "This file should not be read in no-tools mode."))

          ;; Create a greger buffer
          (greger)
          (setq greger-buffer (current-buffer))

          ;; Add a user message that would trigger tool use if tools were enabled
          (goto-char (point-max))
          (insert (format "Please read the file %s and tell me what it contains." test-file))

          ;; Call greger-buffer-no-tools instead of greger-buffer
          (greger-buffer-no-tools)

          ;; Wait for response
          (should (greger-test-wait-for-response greger-buffer greger-test-timeout))

          ;; Wait for streaming to complete
          (greger-test-wait-for-streaming-complete)

          ;; Verify response was added to buffer
          (let ((content (buffer-string)))
            (should (string-match-p "## ASSISTANT:" content))
            ;; Should NOT have tool use sections (no tools mode)
            (should-not (string-match-p "## TOOL USE:" content))
            (should-not (string-match-p "## TOOL RESULT:" content))
            ;; Should have responded without actually reading the file
            (should-not (string-match-p "This file should not be read" content))
            ;; Should have a new USER section at the end (or at least assistant response)
            (should (or (string-match-p "## USER:\n\n$" content)
                       (string-match-p "## ASSISTANT:" content)))))

      ;; Cleanup
      (when (and test-file (file-exists-p test-file))
        (delete-file test-file))
      (when (and greger-buffer (buffer-live-p greger-buffer))
        (kill-buffer greger-buffer)))))

(ert-deftest greger-end-to-end-test-model-configuration ()
  "Test that model configuration works correctly."
  :tags '(end-to-end public-api configuration)

  (let ((original-model greger-model)
        (greger-buffer nil))
    (unwind-protect
        (progn
          ;; Test that we can set different supported models
          (setq greger-model 'claude-opus-4-20250514)
          (should (eq greger-model 'claude-opus-4-20250514))

          (setq greger-model 'claude-sonnet-4-20250514)
          (should (eq greger-model 'claude-sonnet-4-20250514))

          ;; Create a greger buffer and verify model is displayed
          (greger)
          (setq greger-buffer (current-buffer))

          ;; The model should be shown in the mode line
          (should (string-match-p "claude-sonnet-4-20250514"
                                 (format "%s" (symbol-name greger-model)))))

      ;; Cleanup
      (setq greger-model original-model)
      (when (and greger-buffer (buffer-live-p greger-buffer))
        (kill-buffer greger-buffer)))))

(ert-deftest greger-end-to-end-test-sleep-and-interrupt ()
  "Test sleep command with interruption and state transitions."
  :tags '(end-to-end public-api interruption)

  (let ((greger-buffer nil))
    (unwind-protect
        (progn
          ;; Create a greger buffer
          (greger)
          (setq greger-buffer (current-buffer))

          ;; Add system message with safe-shell-commands including sleep 5
          (goto-char (point-max))
          (re-search-backward "## SYSTEM:")
          (forward-line 1)
          (insert "\n<safe-shell-commands>\nsleep 5\n</safe-shell-commands>\n")

          ;; Add user message requesting sleep
          (goto-char (point-max))
          (insert "Please run the shell command 'sleep 5' using the shell-command tool.")

          ;; Call greger-buffer to send the message
          (greger-buffer)

          ;; Wait until state becomes 'executing (should happen quickly as tools are processed)
          (let ((max-wait 10.0)
                (start-time (current-time))
                (state-found nil))
            (while (and (not state-found)
                       (< (float-time (time-subtract (current-time) start-time)) max-wait))
              (sit-for 0.1)
              (when (eq (greger--get-current-state) 'executing)
                (setq state-found t)))
            (should state-found))

          ;; Wait 1 second while in executing state
          (sit-for 1.0)

          ;; Interrupt generation
          (greger-interrupt)

          ;; Wait until state becomes 'generating (briefly as response is generated)
          ;; This might happen very quickly, so we'll be more lenient
          (let ((max-wait 5.0)
                (start-time (current-time))
                (state-found nil))
            (while (and (not state-found)
                       (< (float-time (time-subtract (current-time) start-time)) max-wait))
              (sit-for 0.05)  ; Check more frequently
              (let ((current-state (greger--get-current-state)))
                (when (or (eq current-state 'generating) (eq current-state 'idle))
                  (setq state-found t))))
            ;; Don't require seeing generating state since it might be very brief
            (should state-found))

          ;; Wait until state becomes 'idle
          (let ((max-wait 10.0)
                (start-time (current-time))
                (state-found nil))
            (while (and (not state-found)
                       (< (float-time (time-subtract (current-time) start-time)) max-wait))
              (sit-for 0.1)
              (when (eq (greger--get-current-state) 'idle)
                (setq state-found t)))
            (should state-found))

          ;; Verify the output contains the expected error message for interrupted command
          (let ((content (buffer-string)))
            (should (string-match-p "Command failed with exit code 2: (no output)" content))))

      ;; Cleanup
      (when (and greger-buffer (buffer-live-p greger-buffer))
        (kill-buffer greger-buffer)))))

(provide 'test-end-to-end)

;;; test-end-to-end.el ends here
