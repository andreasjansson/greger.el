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
  (let ((test-completed nil))

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
                          (input . ((message . "Hello World"))))))
            (expected-content "## TOOL RESULT:\n\nID: test_001\n\n<tool.test_001>\nTool executed: Hello World\n</tool.test_001>"))

        ;; Mock greger-agent--run-agent-loop to capture completion
        (cl-letf (((symbol-function 'greger-agent--run-agent-loop)
                   (lambda (state)
                     (setq test-completed t))))

          ;; Execute tools
          (greger-agent--execute-tools tool-calls agent-state)

          ;; Check that the function completed
          (should test-completed)

          ;; Check buffer contents match expected output exactly
          (let ((actual-content (buffer-substring-no-properties (point-min) (point-max))))
            (should (string= expected-content actual-content))))))

    ;; Clean up
    (remhash "test-simple" greger-tools-registry)))

(ert-deftest greger-agent-test-multiple-tools-parallel ()
  "Test execution of multiple tools in parallel."
  (let ((tools-completed nil))

    ;; Define test functions with deterministic output
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
                          (input . ((value . "input-b"))))))
            ;; Expected content contains both tool results
            (expected-patterns '("## TOOL RESULT:\n\nID: test_a\n\n<tool.test_a>\nTool A result: input-a\n</tool.test_a>"
                               "## TOOL RESULT:\n\nID: test_b\n\n<tool.test_b>\nTool B result: input-b\n</tool.test_b>")))

        ;; Mock greger-agent--run-agent-loop to capture completion
        (cl-letf (((symbol-function 'greger-agent--run-agent-loop)
                   (lambda (state)
                     (setq tools-completed t))))

          ;; Execute tools
          (greger-agent--execute-tools tool-calls agent-state)

          ;; Check that all tools completed
          (should tools-completed)

          ;; Check buffer contains both expected tool results
          (let ((actual-content (buffer-substring-no-properties (point-min) (point-max))))
            (dolist (expected-pattern expected-patterns)
              (should (string-match-p (regexp-quote expected-pattern) actual-content)))))))

    ;; Clean up
    (remhash "test-tool-a" greger-tools-registry)
    (remhash "test-tool-b" greger-tools-registry)))

(ert-deftest greger-agent-test-tool-error-handling ()
  "Test that tool errors are properly handled and displayed."
  (let ((test-completed nil))

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
                          (input . ((input . "bad-input"))))))
            (expected-error-content "## TOOL RESULT:\n\nID: error_test\n\n<tool.error_test>\nError executing tool test-error: Simulated tool error: bad-input\n</tool.error_test>"))

        ;; Mock greger-agent--run-agent-loop to capture completion
        (cl-letf (((symbol-function 'greger-agent--run-agent-loop)
                   (lambda (state)
                     (setq test-completed t))))

          ;; Execute tools
          (greger-agent--execute-tools tool-calls agent-state)

          ;; Check that execution completed despite error
          (should test-completed)

          ;; Check buffer contents match expected error format
          (let ((actual-content (buffer-substring-no-properties (point-min) (point-max))))
            (should (string= expected-error-content actual-content))))))

    ;; Clean up
    (remhash "test-error" greger-tools-registry)))

(ert-deftest greger-agent-test-placeholder-replacement ()
  "Test that placeholders are correctly replaced with tool results."
  (let ((test-completed nil))

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

    ;; Create test buffer with placeholder
    (with-temp-buffer
      (insert "<!-- TOOL_RESULT_PLACEHOLDER_placeholder_test -->")

      (let ((agent-state (make-greger-agent-state
                          :current-iteration 0
                          :chat-buffer (current-buffer)
                          :directory default-directory))
            (tool-calls `(((type . "tool_use")
                          (id . "placeholder_test")
                          (name . "test-placeholder")
                          (input . ((data . "test-data"))))))
            (expected-content "## TOOL RESULT:\n\nID: placeholder_test\n\n<tool.placeholder_test>\nProcessed: test-data\n</tool.placeholder_test>"))

        ;; Mock greger-agent--run-agent-loop to capture completion
        (cl-letf (((symbol-function 'greger-agent--run-agent-loop)
                   (lambda (state)
                     (setq test-completed t))))

          ;; Execute tools
          (greger-agent--execute-tools tool-calls agent-state)

          ;; Check that execution completed
          (should test-completed)

          ;; Check that placeholder was replaced with exact expected content
          (let ((actual-content (buffer-substring-no-properties (point-min) (point-max))))
            (should (string= expected-content actual-content))))))

    ;; Clean up
    (remhash "test-placeholder" greger-tools-registry)))

(ert-deftest greger-agent-test-unknown-tool-error ()
  "Test handling of unknown tool execution."
  (let ((test-completed nil))

    ;; Create test buffer
    (with-temp-buffer
      (let ((agent-state (make-greger-agent-state
                          :current-iteration 0
                          :chat-buffer (current-buffer)
                          :directory default-directory))
            (tool-calls `(((type . "tool_use")
                          (id . "unknown_test")
                          (name . "nonexistent-tool")
                          (input . ((param . "value"))))))
            (expected-error-content "## TOOL RESULT:\n\nID: unknown_test\n\n<tool.unknown_test>\nUnknown tool: nonexistent-tool\n</tool.unknown_test>"))

        ;; Mock greger-agent--run-agent-loop to capture completion
        (cl-letf (((symbol-function 'greger-agent--run-agent-loop)
                   (lambda (state)
                     (setq test-completed t))))

          ;; Execute tools
          (greger-agent--execute-tools tool-calls agent-state)

          ;; Check that execution completed despite unknown tool
          (should test-completed)

          ;; Check buffer contents match expected error message exactly
          (let ((actual-content (buffer-substring-no-properties (point-min) (point-max))))
            (should (string= expected-error-content actual-content))))))))

(ert-deftest greger-agent-test-exact-tool-output-formatting ()
  "Test exact tool output formatting with multiple scenarios."
  (let ((test-completed nil))

    ;; Define a tool that returns multi-line content
    (defun greger-test-multiline-tool (content)
      (format "Line 1: %s\nLine 2: More content\nLine 3: End" content))

    ;; Register test tool
    (greger-register-tool "test-multiline"
      :description "Tool with multi-line output"
      :properties '((content . ((type . "string")
                                (description . "Content for first line"))))
      :required '("content")
      :function 'greger-test-multiline-tool)

    ;; Create test buffer
    (with-temp-buffer
      (let ((agent-state (make-greger-agent-state
                          :current-iteration 0
                          :chat-buffer (current-buffer)
                          :directory default-directory))
            (tool-calls `(((type . "tool_use")
                          (id . "multiline_test")
                          (name . "test-multiline")
                          (input . ((content . "Start"))))))
            (expected-content "## TOOL RESULT:\n\nID: multiline_test\n\n<tool.multiline_test>\nLine 1: Start\nLine 2: More content\nLine 3: End\n</tool.multiline_test>"))

        ;; Mock greger-agent--run-agent-loop to capture completion
        (cl-letf (((symbol-function 'greger-agent--run-agent-loop)
                   (lambda (state)
                     (setq test-completed t))))

          ;; Execute tools
          (greger-agent--execute-tools tool-calls agent-state)

          ;; Check that execution completed
          (should test-completed)

          ;; Check exact multi-line formatting
          (let ((actual-content (buffer-substring-no-properties (point-min) (point-max))))
            (should (string= expected-content actual-content))))))

    ;; Clean up
    (remhash "test-multiline" greger-tools-registry)))

(ert-deftest greger-agent-test-simple-string-comparison ()
  "Test simple tool execution with clear before/after string comparison."
  (let ((test-completed nil))

    ;; Define a predictable tool function
    (defun greger-test-simple-echo (input)
      (format "Echo: %s" input))

    ;; Register test tool
    (greger-register-tool "test-echo"
      :description "Simple echo tool"
      :properties '((input . ((type . "string")
                              (description . "Input to echo"))))
      :required '("input")
      :function 'greger-test-simple-echo)

    ;; Create test buffer
    (with-temp-buffer
      ;; Initial buffer state (empty)
      (let ((initial-content "")
            (agent-state (make-greger-agent-state
                          :current-iteration 0
                          :chat-buffer (current-buffer)
                          :directory default-directory))
            (tool-calls `(((type . "tool_use")
                          (id . "echo_001")
                          (name . "test-echo")
                          (input . ((input . "hello world"))))))
            ;; Expected content after tool execution
            (expected-final-content "## TOOL RESULT:\n\nID: echo_001\n\n<tool.echo_001>\nEcho: hello world\n</tool.echo_001>"))

        ;; Verify initial state
        (should (string= initial-content (buffer-substring-no-properties (point-min) (point-max))))

        ;; Mock greger-agent--run-agent-loop to capture completion
        (cl-letf (((symbol-function 'greger-agent--run-agent-loop)
                   (lambda (state)
                     (setq test-completed t))))

          ;; Execute tools
          (greger-agent--execute-tools tool-calls agent-state)

          ;; Verify completion
          (should test-completed)

          ;; Verify exact final content using string= comparison
          (let ((actual-final-content (buffer-substring-no-properties (point-min) (point-max))))
            (should (string= expected-final-content actual-final-content))))))

    ;; Clean up
    (remhash "test-echo" greger-tools-registry)))

(provide 'test-greger-agent)

;;; test-greger-agent.el ends here
