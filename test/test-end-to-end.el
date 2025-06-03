;;; test-end-to-end.el --- End-to-end tests for greger -*- lexical-binding: t -*-

;;; Commentary:
;;
;; This file contains end-to-end tests that actually call the Claude API.
;; These tests require a valid ANTHROPIC_API_KEY environment variable.
;;
;; WARNING: These tests make real API calls and may incur costs.
;;

(require 'ert)
(require 'greger-client)
(require 'greger-parser)

(defvar greger-test-timeout 30
  "Timeout in seconds for API calls in tests.")

(defun greger-test-has-api-key ()
  "Check if we have a valid API key for testing."
  (not (string-empty-p (or (getenv "ANTHROPIC_API_KEY") ""))))

(defun greger-test-wait-for-completion (state timeout)
  "Wait for streaming STATE to complete within TIMEOUT seconds."
  (let ((start-time (current-time))
        (completed nil))

    (while (and (not completed)
                (< (float-time (time-subtract (current-time) start-time)) timeout))
      (sit-for 0.1)
      ;; Check if process is still running
      (when (not (process-live-p (greger-client-state-process state)))
        (setq completed t)))

    completed))

(ert-deftest greger-end-to-end-test-simple-text-generation ()
  "Test simple text generation with Claude API."
  :tags '(end-to-end api)
  (skip-unless (greger-test-has-api-key))

  (let ((response-received nil)
        (text-chunks '())
        (final-blocks nil)
        (test-model 'claude-sonnet-4-20250514)
        (test-dialog '(((role . "user")
                        (content . "Say exactly 'Hello from Greger test!' and nothing else.")))))

    (with-temp-buffer
      (let ((test-buffer (current-buffer)))

        ;; Make the API call
        (let ((state (greger-client-stream
                      :model test-model
                      :dialog test-dialog
                      :buffer test-buffer
                      :text-callback (lambda (text)
                                       (push text text-chunks))
                      :complete-callback (lambda (blocks)
                                           (setq final-blocks blocks
                                                 response-received t)))))

          ;; Wait for completion
          (should (greger-test-wait-for-completion state greger-test-timeout))

          ;; Check that we got a response
          (should response-received)
          (should final-blocks)
          (should (> (length final-blocks) 0))

          ;; Check that the first block is text
          (let ((first-block (car final-blocks)))
            (should (string= (alist-get 'type first-block) "text"))
            (should (alist-get 'text first-block))

            ;; Verify the response contains our expected text
            (let ((response-text (alist-get 'text first-block)))
              (should (string-match-p "Hello from Greger test!" response-text))))

          ;; Verify text was written to buffer
          (should (> (buffer-size) 0))
          (let ((buffer-content (buffer-string)))
            (should (string-match-p "Hello from Greger test!" buffer-content))))))))

(ert-deftest greger-end-to-end-test-tool-use ()
  "Test tool use functionality with Claude API."
  :tags '(end-to-end api tools)
  (skip-unless (greger-test-has-api-key))

  (let ((response-received nil)
        (final-blocks nil)
        (test-model 'claude-sonnet-4-20250514)
        (test-dialog '(((role . "user")
                        (content . "What is 2 + 2? Please use the calculator tool to compute this."))))
        (test-tools '(((type . "function")
                       (function . ((name . "calculator")
                                   (description . "Performs basic arithmetic calculations")
                                   (parameters . ((type . "object")
                                                 (properties . ((expression . ((type . "string")
                                                                               (description . "Mathematical expression to evaluate")))))
                                                 (required . ["expression"])))))))))

    (with-temp-buffer
      (let ((test-buffer (current-buffer)))

        ;; Make the API call with tools
        (let ((state (greger-client-stream
                      :model test-model
                      :dialog test-dialog
                      :tools test-tools
                      :buffer test-buffer
                      :complete-callback (lambda (blocks)
                                           (setq final-blocks blocks
                                                 response-received t)))))

          ;; Wait for completion
          (should (greger-test-wait-for-completion state greger-test-timeout))

          ;; Check that we got a response
          (should response-received)
          (should final-blocks)
          (should (> (length final-blocks) 0))

          ;; Look for tool use in the response
          (let ((has-tool-use nil))
            (dolist (block final-blocks)
              (when (string= (alist-get 'type block) "tool_use")
                (setq has-tool-use t)
                ;; Verify tool use structure
                (should (alist-get 'id block))
                (should (alist-get 'name block))
                (should (alist-get 'input block))))

            ;; We should have at least attempted to use a tool
            ;; (Note: Claude might not always use the tool, but this tests the capability)
            (should (or has-tool-use
                       ;; Or at least responded with text
                       (cl-some (lambda (block)
                                  (string= (alist-get 'type block) "text"))
                                final-blocks)))))))))

(ert-deftest greger-end-to-end-test-error-handling ()
  "Test error handling with invalid model."
  :tags '(end-to-end error-handling)
  (skip-unless (greger-test-has-api-key))

  (let ((error-caught nil))

    ;; Test with invalid model - should fail validation
    (condition-case err
        (greger-client-stream
         :model 'invalid-model
         :dialog '(((role . "user") (content . "test"))))
      (error
       (setq error-caught t)
       (should (string-match-p "Unsupported model" (error-message-string err)))))

    (should error-caught)))

(ert-deftest greger-end-to-end-test-supported-models ()
  "Test that supported models are accepted."
  :tags '(end-to-end models)

  ;; Test that both supported models are accepted (we won't actually call API)
  (should (memq 'claude-sonnet-4-20250514 greger-client-supported-models))
  (should (memq 'claude-opus-4-20250514 greger-client-supported-models))

  ;; Test that only these models are supported
  (should (= 2 (length greger-client-supported-models))))

(ert-deftest greger-end-to-end-test-request-building ()
  "Test that request building works correctly."
  :tags '(end-to-end request-building)
  (skip-unless (greger-test-has-api-key))

  (let* ((test-model 'claude-sonnet-4-20250514)
         (test-dialog '(((role . "user") (content . "Hello"))
                        ((role . "assistant") (content . "Hi there!"))
                        ((role . "user") (content . "How are you?"))))
         (test-tools '(((type . "function")
                        (function . ((name . "test_tool")
                                    (description . "A test tool"))))))
         (request-spec (greger-client--build-request test-model test-dialog test-tools)))

    ;; Verify request structure
    (should (plist-get request-spec :url))
    (should (string= (plist-get request-spec :url) greger-client-api-url))
    (should (string= (plist-get request-spec :method) "POST"))
    (should (plist-get request-spec :headers))
    (should (plist-get request-spec :data))

    ;; Verify headers
    (let ((headers (plist-get request-spec :headers)))
      (should (assoc "Content-Type" headers))
      (should (assoc "x-api-key" headers))
      (should (assoc "anthropic-version" headers)))

    ;; Verify data is valid JSON
    (let ((data (plist-get request-spec :data)))
      (should (stringp data))
      (should (json-read-from-string data)))))

(provide 'test-end-to-end)

;;; test-end-to-end.el ends here
