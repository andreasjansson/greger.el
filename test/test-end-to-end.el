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

(defun greger-test-has-api-key ()
  "Check if we have a valid API key for testing."
  (not (string-empty-p (or (getenv "ANTHROPIC_API_KEY") ""))))

(defun greger-test-wait-for-response (buffer timeout)
  "Wait for a response to appear in BUFFER within TIMEOUT seconds."
  (let ((start-time (current-time))
        (completed nil)
        (initial-content (with-current-buffer buffer (buffer-string))))

    (while (and (not completed)
                (< (float-time (time-subtract (current-time) start-time)) timeout))
      (sit-for 0.5)
      ;; Check if buffer content has changed (response received)
      (with-current-buffer buffer
        (let ((current-content (buffer-string)))
          (when (not (string= initial-content current-content))
            ;; Content changed, but let's wait a bit more to ensure completion
            (when (string-match-p "## USER:" current-content)
              (setq completed t))))))

    completed))

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
  (skip-unless (greger-test-has-api-key))

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

          ;; Verify response was added to buffer
          (let ((content (buffer-string)))
            (should (string-match-p "## ASSISTANT:" content))
            (should (string-match-p "Hello from greger test!" content))
            ;; Should have a new USER section at the end
            (should (string-match-p "## USER:\n\n$" content))))

      ;; Cleanup
      (when (and greger-buffer (buffer-live-p greger-buffer))
        (kill-buffer greger-buffer)))))

(ert-deftest greger-end-to-end-test-tool-use-conversation ()
  "Test a conversation that involves tool use using the public API."
  :tags '(end-to-end public-api tools)
  (skip-unless (greger-test-has-api-key))

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

          ;; Verify response was added to buffer
          (let ((content (buffer-string)))
            (should (string-match-p "## ASSISTANT:" content))
            ;; Should have tool use section
            (should (or (string-match-p "## TOOL USE:" content)
                       (string-match-p "read-file" content)))
            ;; Should mention the test file content
            (should (string-match-p "test file for greger" content))
            ;; Should have a new USER section at the end
            (should (string-match-p "## USER:\n\n$" content))))

      ;; Cleanup
      (when (and test-file (file-exists-p test-file))
        (delete-file test-file))
      (when (and greger-buffer (buffer-live-p greger-buffer))
        (kill-buffer greger-buffer)))))

(ert-deftest greger-end-to-end-test-no-tools-mode ()
  "Test the no-tools mode using C-M-return."
  :tags '(end-to-end public-api no-tools)
  (skip-unless (greger-test-has-api-key))

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

          ;; Verify response was added to buffer
          (let ((content (buffer-string)))
            (should (string-match-p "## ASSISTANT:" content))
            ;; Should NOT have tool use sections (no tools mode)
            (should-not (string-match-p "## TOOL USE:" content))
            (should-not (string-match-p "## TOOL RESULT:" content))
            ;; Should have responded without actually reading the file
            (should-not (string-match-p "This file should not be read" content))
            ;; Should have a new USER section at the end
            (should (string-match-p "## USER:\n\n$" content))))

      ;; Cleanup
      (when (and test-file (file-exists-p test-file))
        (delete-file test-file))
      (when (and greger-buffer (buffer-live-p greger-buffer))
        (kill-buffer greger-buffer)))))

(ert-deftest greger-end-to-end-test-markdown-features ()
  "Test that markdown features work correctly in greger buffers."
  :tags '(end-to-end public-api markdown)

  (let ((greger-buffer nil))
    (unwind-protect
        (progn
          ;; Create a greger buffer
          (greger)
          (setq greger-buffer (current-buffer))

          ;; Verify we're in greger-mode which inherits from gfm-mode
          (should (eq major-mode 'greger-mode))
          (should (derived-mode-p 'gfm-mode))

          ;; Test that code blocks are handled
          (goto-char (point-max))
          (insert "Here's some code:\n\n```python\nprint('hello')\n```\n\n")

          ;; Test that markdown fontification is working
          (should markdown-fontify-code-blocks-natively)

          ;; Test key bindings are set up
          (should (keymapp greger-mode-map))
          (should (eq (lookup-key greger-mode-map (kbd "M-<return>")) 'greger-buffer))
          (should (eq (lookup-key greger-mode-map (kbd "C-M-<return>")) 'greger-buffer-no-tools)))

      ;; Cleanup
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
                                 (format "%s" mode-line-misc-info))))

      ;; Cleanup
      (setq greger-model original-model)
      (when (and greger-buffer (buffer-live-p greger-buffer))
        (kill-buffer greger-buffer)))))

(provide 'test-end-to-end)

;;; test-end-to-end.el ends here
