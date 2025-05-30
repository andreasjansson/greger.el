;;; test-greger-agent.el --- Tests for greger agent functionality -*- lexical-binding: t -*-

(require 'ert)
(require 'greger-agent)
(require 'greger-tools)
(require 'greger-parser)

(ert-deftest greger-agent-test-tool-placeholder ()
  "Test the tool placeholder helper function."
  (let ((expected1 "<!-- TOOL_RESULT_PLACEHOLDER_test123 -->")
        (expected2 "<!-- TOOL_RESULT_PLACEHOLDER_tool_abc_def -->"))
    (should (string= expected1 (greger-agent--tool-placeholder "test123")))
    (should (string= expected2 (greger-agent--tool-placeholder "tool_abc_def")))))

(ert-deftest greger-agent-test-single-tool-execution ()
  "Test execution of a single tool with callback."
  (let ((test-completed nil)
        (expected-content-patterns '("test-simple" "Tool executed: Hello World")))

    ;; Define a simple test function
    (defun greger-test-simple-tool (message)
      (format "Tool executed: %s" message))

    ;; Register test tool
    (greger-register-tool "test-simple"
      :description "Simple test tool"
      :properties '((message . ((type . "string")
                                (description . "Test message"))))
      :required '("message")
      :function 'greger-test-simple-tool)

    ;; Create test buffer
    (with-temp-buffer
      (let ((agent-state (make-greger-agent-state
                          :current-iteration 0
                          :chat-buffer (current-buffer)
                          :directory default-directory))
            (tool-calls `(((type . "tool_use")
                          (id . "test_001")
                          (name . "test-simple")
                          (input . ((message . "Hello World")))))))

        ;; Mock greger-agent--run-agent-loop to capture completion
        (cl-letf (((symbol-function 'greger-agent--run-agent-loop)
                   (lambda (state)
                     (setq test-completed t))))

          ;; Execute tools
          (greger-agent--execute-tools tool-calls agent-state)

          ;; Check that the function completed
          (should test-completed)

          ;; Check buffer contents against expected patterns
          (let ((actual-content (buffer-substring-no-properties (point-min) (point-max))))
            (dolist (pattern expected-content-patterns)
              (should (string-match-p pattern actual-content)))))))

    ;; Clean up
    (remhash "test-simple" greger-tools-registry)))

(ert-deftest greger-agent-test-multiple-tools-parallel ()
  "Test execution of multiple tools in parallel."
  (let ((tools-completed nil)
        (expected-content-patterns '("Tool A result: input-a"
                                   "Tool B result: input-b"
                                   "test-tool-a"
                                   "test-tool-b")))

    ;; Define test functions with different execution times
    (defun greger-test-tool-a (value)
      (format "Tool A result: %s" value))

    (defun greger-test-tool-b (value)
      (format "Tool B result: %s" value))

    ;; Register test tools
    (greger-register-tool "test-tool-a"
      :description "Test tool A"
      :properties '((value . ((type . "string")
                              (description . "Input value"))))
      :required '("value")
      :function 'greger-test-tool-a)

    (greger-register-tool "test-tool-b"
      :description "Test tool B"
      :properties '((value . ((type . "string")
                              (description . "Input value"))))
      :required '("value")
      :function 'greger-test-tool-b)

    ;; Create test buffer
    (with-temp-buffer
      (let ((agent-state (make-greger-agent-state
                          :current-iteration 0
                          :chat-buffer (current-buffer)
                          :directory default-directory))
            (tool-calls `(((type . "tool_use")
                          (id . "test_a")
                          (name . "test-tool-a")
                          (input . ((value . "input-a"))))
                         ((type . "tool_use")
                          (id . "test_b")
                          (name . "test-tool-b")
                          (input . ((value . "input-b")))))))

        ;; Mock greger-agent--run-agent-loop to capture completion
        (cl-letf (((symbol-function 'greger-agent--run-agent-loop)
                   (lambda (state)
                     (setq tools-completed t))))

          ;; Execute tools
          (greger-agent--execute-tools tool-calls agent-state)

          ;; Check that all tools completed
          (should tools-completed)

          ;; Check buffer contents against expected patterns
          (let ((actual-content (buffer-substring-no-properties (point-min) (point-max))))
            (dolist (pattern expected-content-patterns)
              (should (string-match-p pattern actual-content)))))))

    ;; Clean up
    (remhash "test-tool-a" greger-tools-registry)
    (remhash "test-tool-b" greger-tools-registry)))

(ert-deftest greger-agent-test-tool-error-handling ()
  "Test that tool errors are properly handled and displayed."
  (let ((test-completed nil)
        (expected-error-patterns '("Error executing tool"
                                 "Simulated tool error"
                                 "bad-input")))

    ;; Define a tool function that throws an error
    (defun greger-test-error-tool (input)
      (error "Simulated tool error: %s" input))

    ;; Register test tool
    (greger-register-tool "test-error"
      :description "Tool that throws an error"
      :properties '((input . ((type . "string")
                              (description . "Input that will cause error"))))
      :required '("input")
      :function 'greger-test-error-tool)

    ;; Create test buffer
    (with-temp-buffer
      (let ((agent-state (make-greger-agent-state
                          :current-iteration 0
                          :chat-buffer (current-buffer)
                          :directory default-directory))
            (tool-calls `(((type . "tool_use")
                          (id . "error_test")
                          (name . "test-error")
                          (input . ((input . "bad-input")))))))

        ;; Mock greger-agent--run-agent-loop to capture completion
        (cl-letf (((symbol-function 'greger-agent--run-agent-loop)
                   (lambda (state)
                     (setq test-completed t))))

          ;; Execute tools
          (greger-agent--execute-tools tool-calls agent-state)

          ;; Check that execution completed despite error
          (should test-completed)

          ;; Check buffer contents against expected error patterns
          (let ((actual-content (buffer-substring-no-properties (point-min) (point-max))))
            (dolist (pattern expected-error-patterns)
              (should (string-match-p pattern actual-content)))))))

    ;; Clean up
    (remhash "test-error" greger-tools-registry)))

(ert-deftest greger-agent-test-placeholder-replacement ()
  "Test that placeholders are correctly replaced with tool results."
  (let ((test-completed nil)
        (expected-result-content "Processed: test-data")
        (unexpected-placeholder "TOOL_RESULT_PLACEHOLDER_placeholder_test"))

    ;; Define a simple test function
    (defun greger-test-placeholder-tool (data)
      (format "Processed: %s" data))

    ;; Register test tool
    (greger-register-tool "test-placeholder"
      :description "Test placeholder replacement"
      :properties '((data . ((type . "string")
                             (description . "Data to process"))))
      :required '("data")
      :function 'greger-test-placeholder-tool)

    ;; Create test buffer
    (with-temp-buffer
      (let ((agent-state (make-greger-agent-state
                          :current-iteration 0
                          :chat-buffer (current-buffer)
                          :directory default-directory))
            (tool-calls `(((type . "tool_use")
                          (id . "placeholder_test")
                          (name . "test-placeholder")
                          (input . ((data . "test-data")))))))

        ;; Mock greger-agent--run-agent-loop to capture completion
        (cl-letf (((symbol-function 'greger-agent--run-agent-loop)
                   (lambda (state)
                     (setq test-completed t))))

          ;; Execute tools
          (greger-agent--execute-tools tool-calls agent-state)

          ;; Check that execution completed
          (should test-completed)

          ;; Check that placeholder was replaced with expected content
          (let ((actual-content (buffer-substring-no-properties (point-min) (point-max))))
            ;; Placeholder should not exist anymore
            (should-not (string-match-p unexpected-placeholder actual-content))
            ;; Expected result should be present
            (should (string-match-p expected-result-content actual-content))))))

    ;; Clean up
    (remhash "test-placeholder" greger-tools-registry)))

(ert-deftest greger-agent-test-unknown-tool-error ()
  "Test handling of unknown tool execution."
  (let ((test-completed nil)
        (expected-error-message "Unknown tool: nonexistent-tool"))

    ;; Create test buffer
    (with-temp-buffer
      (let ((agent-state (make-greger-agent-state
                          :current-iteration 0
                          :chat-buffer (current-buffer)
                          :directory default-directory))
            (tool-calls `(((type . "tool_use")
                          (id . "unknown_test")
                          (name . "nonexistent-tool")
                          (input . ((param . "value")))))))

        ;; Mock greger-agent--run-agent-loop to capture completion
        (cl-letf (((symbol-function 'greger-agent--run-agent-loop)
                   (lambda (state)
                     (setq test-completed t))))

          ;; Execute tools
          (greger-agent--execute-tools tool-calls agent-state)

          ;; Check that execution completed despite unknown tool
          (should test-completed)

          ;; Check buffer contents for expected error message
          (let ((actual-content (buffer-substring-no-properties (point-min) (point-max))))
            (should (string-match-p expected-error-message actual-content))))))))

(ert-deftest greger-agent-test-exact-buffer-content ()
  "Test with exact expected buffer content using string= comparison."
  (let ((test-completed nil))

    ;; Define a predictable test function
    (defun greger-test-exact-tool (input)
      "EXACT_OUTPUT")

    ;; Register test tool
    (greger-register-tool "test-exact"
      :description "Tool with exact output"
      :properties '((input . ((type . "string")
                              (description . "Input parameter"))))
      :required '("input")
      :function 'greger-test-exact-tool)

    ;; Create test buffer
    (with-temp-buffer
      ;; Pre-insert a placeholder that will be replaced
      (insert "<!-- TOOL_RESULT_PLACEHOLDER_exact_001 -->")

      (let ((agent-state (make-greger-agent-state
                          :current-iteration 0
                          :chat-buffer (current-buffer)
                          :directory default-directory))
            (tool-calls `(((type . "tool_use")
                          (id . "exact_001")
                          (name . "test-exact")
                          (input . ((input . "test")))))))

        ;; Expected exact content after tool execution and placeholder replacement
        (let ((expected-exact-content "EXACT_OUTPUT"))

          ;; Mock greger-agent--run-agent-loop to capture completion
          (cl-letf (((symbol-function 'greger-agent--run-agent-loop)
                     (lambda (state)
                       (setq test-completed t))))

            ;; Execute tools
            (greger-agent--execute-tools tool-calls agent-state)

            ;; Check that execution completed
            (should test-completed)

            ;; Check exact buffer content using string=
            (let ((actual-content (buffer-substring-no-properties (point-min) (point-max))))
              (should (string= expected-exact-content actual-content)))))))

    ;; Clean up
    (remhash "test-exact" greger-tools-registry)))

(provide 'test-greger-agent)

;;; test-greger-agent.el ends here
