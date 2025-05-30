;;; test-greger-agent.el --- Tests for greger agent functionality -*- lexical-binding: t -*-

(require 'ert)
(require 'greger-agent)
(require 'greger-tools)
(require 'greger-parser)

(ert-deftest greger-agent-test-tool-placeholder ()
  "Test the tool placeholder helper function."
  (let ((expected-1 "<!-- TOOL_RESULT_PLACEHOLDER_test123 -->")
        (expected-2 "<!-- TOOL_RESULT_PLACEHOLDER_tool_abc_def -->"))
    (should (string= expected-1
                     (greger-agent--tool-placeholder "test123")))
    (should (string= expected-2
                     (greger-agent--tool-placeholder "tool_abc_def")))))

(ert-deftest greger-agent-test-single-tool-execution ()
  "Test execution of a single tool with callback."
  (let ((test-completed nil)
        (expected-content-parts '("test-simple" "Tool executed: Hello World")))

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

          ;; Check buffer contents against expected parts
          (let ((actual-content (buffer-substring-no-properties (point-min) (point-max))))
            (dolist (expected-part expected-content-parts)
              (should (string-match-p expected-part actual-content)))))))

    ;; Clean up
    (remhash "test-simple" greger-tools-registry)))

(ert-deftest greger-agent-test-multiple-tools-parallel ()
  "Test execution of multiple tools in parallel."
  (let ((tools-completed nil)
        (expected-content-parts '("test-tool-a" "test-tool-b"
                                 "Tool A result: input-a" "Tool B result: input-b")))

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

          ;; Check buffer contents against expected parts
          (let ((actual-content (buffer-substring-no-properties (point-min) (point-max))))
            (dolist (expected-part expected-content-parts)
              (should (string-match-p expected-part actual-content)))))))

    ;; Clean up
    (remhash "test-tool-a" greger-tools-registry)
    (remhash "test-tool-b" greger-tools-registry)))

(ert-deftest greger-agent-test-tool-error-handling ()
  "Test that tool errors are properly handled and displayed."
  (let ((test-completed nil)
        (expected-error-parts '("Error executing tool" "Simulated tool error" "bad-input")))

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

          ;; Check buffer contents against expected error parts
          (let ((actual-content (buffer-substring-no-properties (point-min) (point-max))))
            (dolist (expected-part expected-error-parts)
              (should (string-match-p expected-part actual-content)))))))

    ;; Clean up
    (remhash "test-error" greger-tools-registry)))

(ert-deftest greger-agent-test-placeholder-replacement ()
  "Test that placeholders are correctly replaced with tool results."
  (let ((test-completed nil)
        (expected-result "Processed: test-data")
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

          ;; Check that placeholder was replaced and result is present
          (let ((actual-content (buffer-substring-no-properties (point-min) (point-max))))
            ;; Placeholder should not exist anymore
            (should-not (string-match-p unexpected-placeholder actual-content))
            ;; Expected result should be present
            (should (string-match-p expected-result actual-content))))))

    ;; Clean up
    (remhash "test-placeholder" greger-tools-registry)))

(ert-deftest greger-agent-test-unknown-tool-error ()
  "Test handling of unknown tool execution."
  (let ((test-completed nil)
        (expected-error "Unknown tool: nonexistent-tool"))

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

          ;; Check buffer contents against expected error
          (let ((actual-content (buffer-substring-no-properties (point-min) (point-max))))
            (should (string-match-p expected-error actual-content))))))))

(provide 'test-greger-agent)

;;; test-greger-agent.el ends here
