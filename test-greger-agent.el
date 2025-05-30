;;; test-greger-agent.el --- Tests for greger agent functionality -*- lexical-binding: t -*-

;;; Commentary:
;;
;; This file contains tests for the greger agent functionality.
;;
;; Testing Approach:
;; - Each test has explicit "expected" content that defines the exact buffer state after execution
;; - Tests use string= for exact content comparison where possible
;; - Expected content follows the greger tool result format:
;;   ## TOOL RESULT:
;;
;;   ID: <tool_id>
;;
;;   <tool.<tool_id>>
;;   <tool_output>
;;   </tool.<tool_id>>
;; - This makes tests more readable and maintainable by clearly showing what the expected output should be

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
                          :directory default-directory
                          :metadata nil))
            (tool-calls `(((type . "tool_use")
                          (id . "test_001")
                          (name . "test-simple")
                          (input . ((message . "Hello World"))))))
            (expected-content "

## TOOL USE:

Name: test-simple
ID: test_001

### message

<tool.test_001>
Hello World
</tool.test_001>

## TOOL RESULT:

ID: test_001

<tool.test_001>
Tool executed: Hello World
</tool.test_001>"))

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
            ;; Expected content has each tool use followed by its result
            (expected-content "

## TOOL USE:

Name: test-tool-a
ID: test_a

### value

<tool.test_a>
input-a
</tool.test_a>

## TOOL RESULT:

ID: test_a

<tool.test_a>
Tool A result: input-a
</tool.test_a>

## TOOL USE:

Name: test-tool-b
ID: test_b

### value

<tool.test_b>
input-b
</tool.test_b>

## TOOL RESULT:

ID: test_b

<tool.test_b>
Tool B result: input-b
</tool.test_b>"))

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
            (should (string= expected-content actual-content))))))

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
            (expected-error-content "

## TOOL USE:

Name: test-error
ID: error_test

### input

<tool.error_test>
bad-input
</tool.error_test>

## TOOL RESULT:

ID: error_test

<tool.error_test>
Error executing tool: Simulated tool error: bad-input
</tool.error_test>"))

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

(ert-deftest greger-agent-test-tool-execution-with-existing-content ()
  "Test tool execution when buffer already has content."
  (let ((test-completed nil))

    ;; Define a simple test function
    (defun greger-test-content-tool (data)
      (format "Processed: %s" data))

    ;; Register test tool
    (greger-register-tool "test-content"
      :description "Test tool with existing content"
      :properties '((data . ((type . "string")
                             (description . "Data to process"))))
      :required '("data")
      :function 'greger-test-content-tool)

    ;; Create test buffer with existing content
    (with-temp-buffer
      (insert "Existing content in buffer")

      (let ((agent-state (make-greger-agent-state
                          :current-iteration 0
                          :chat-buffer (current-buffer)
                          :directory default-directory))
            (tool-calls `(((type . "tool_use")
                          (id . "content_test")
                          (name . "test-content")
                          (input . ((data . "test-data"))))))
            (expected-content "Existing content in buffer

## TOOL USE:

Name: test-content
ID: content_test

### data

<tool.content_test>
test-data
</tool.content_test>

## TOOL RESULT:

ID: content_test

<tool.content_test>
Processed: test-data
</tool.content_test>"))

        ;; Mock greger-agent--run-agent-loop to capture completion
        (cl-letf (((symbol-function 'greger-agent--run-agent-loop)
                   (lambda (state)
                     (setq test-completed t))))

          ;; Execute tools
          (greger-agent--execute-tools tool-calls agent-state)

          ;; Check that execution completed
          (should test-completed)

          ;; Check that content was appended correctly
          (let ((actual-content (buffer-substring-no-properties (point-min) (point-max))))
            (should (string= expected-content actual-content))))))

    ;; Clean up
    (remhash "test-content" greger-tools-registry)))

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
            (expected-error-content "

## TOOL USE:

Name: nonexistent-tool
ID: unknown_test

### param

<tool.unknown_test>
value
</tool.unknown_test>

## TOOL RESULT:

ID: unknown_test

<tool.unknown_test>
Unknown tool: nonexistent-tool
</tool.unknown_test>"))

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
            (expected-content "

## TOOL USE:

Name: test-multiline
ID: multiline_test

### content

<tool.multiline_test>
Start
</tool.multiline_test>

## TOOL RESULT:

ID: multiline_test

<tool.multiline_test>
Line 1: Start
Line 2: More content
Line 3: End
</tool.multiline_test>"))

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
            (expected-final-content "

## TOOL USE:

Name: test-echo
ID: echo_001

### input

<tool.echo_001>
hello world
</tool.echo_001>

## TOOL RESULT:

ID: echo_001

<tool.echo_001>
Echo: hello world
</tool.echo_001>"))

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
