;;; test-greger.el --- Tests for greger functionality -*- lexical-binding: t -*-

;;; Commentary:
;;
;; This file contains tests for the greger functionality (including agent capabilities).
;;
;; Testing Approach:
;; - Each test has explicit "expected" content that defines the exact buffer state after execution
;; - Tests use string= for exact content comparison where possible
;; - Expected content follows the greger tool result format:
;;   # TOOL RESULT:
;;
;;   ID: <tool_id>
;;
;;   <tool.<tool_id>>
;;   <tool_output>
;;   </tool.<tool_id>>
;; - This makes tests more readable and maintainable by clearly showing what the expected output should be

(require 'ert)
(require 'greger)
(require 'greger-tools)
(require 'greger-parser)

(ert-deftest greger-test-single-tool-execution ()
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
      (greger-mode) ; Set up greger-mode for tree-sitter support
      (let ((agent-state (make-greger-state
                          :current-iteration 0
                          :chat-buffer (current-buffer)
                          :directory default-directory
                          :tool-use-metadata '(:safe-shell-commands () :allow-all-shell-commands nil)))
            (tool-calls `(((type . "tool_use")
                           (id . "test_001")
                           (name . "test-simple")
                           (input . ((message . "Hello World"))))))
            (expected-content "

# TOOL USE

Name: test-simple
ID: test_001

## message

<tool.test_001>
Hello World
</tool.test_001>

# TOOL RESULT

ID: test_001

<tool.test_001>
Tool executed: Hello World
</tool.test_001>"))

        ;; Mock greger--run-agent-loop to capture completion
        (cl-letf (((symbol-function 'greger--run-agent-loop)
                   (lambda (state)
                     (setq test-completed t))))

          ;; First insert the tool use markdown (simulating what greger--append-nonstreaming-content-block does)
          (dolist (tool-call tool-calls)
            (let ((tool-use-markdown (greger-parser--tool-use-to-markdown tool-call))
                  (tool-id (alist-get 'id tool-call)))
              (insert "\n\n" tool-use-markdown)
              (insert "\n\n" (greger--tool-placeholder tool-id))))

          ;; Execute tools
          (greger--execute-tools tool-calls agent-state)

          ;; Check that the function completed
          (should test-completed)

          ;; Check buffer contents match expected output exactly
          (let ((actual-content (buffer-substring-no-properties (point-min) (point-max))))
            (should (string= expected-content actual-content))))))

    ;; Clean up
    (remhash "test-simple" greger-tools-registry)))

(ert-deftest greger-test-multiple-tools-parallel ()
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
      (greger-mode) ; Set up greger-mode for tree-sitter support
      (let ((agent-state (make-greger-state
                          :current-iteration 0
                          :chat-buffer (current-buffer)
                          :directory default-directory :tool-use-metadata '(:safe-shell-commands () :allow-all-shell-commands nil)))
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

# TOOL USE

Name: test-tool-a
ID: test_a

## value

<tool.test_a>
input-a
</tool.test_a>

# TOOL RESULT

ID: test_a

<tool.test_a>
Tool A result: input-a
</tool.test_a>

# TOOL USE

Name: test-tool-b
ID: test_b

## value

<tool.test_b>
input-b
</tool.test_b>

# TOOL RESULT

ID: test_b

<tool.test_b>
Tool B result: input-b
</tool.test_b>"))

        ;; Mock greger--run-agent-loop to capture completion
        (cl-letf (((symbol-function 'greger--run-agent-loop)
                   (lambda (state)
                     (setq tools-completed t))))

          ;; First insert the tool use markdown (simulating what greger--append-nonstreaming-content-block does)
          (dolist (tool-call tool-calls)
            (let ((tool-use-markdown (greger-parser--tool-use-to-markdown tool-call))
                  (tool-id (alist-get 'id tool-call)))
              (insert "\n\n" tool-use-markdown)
              (insert "\n\n" (greger--tool-placeholder tool-id))))

          ;; Execute tools
          (greger--execute-tools tool-calls agent-state)

          ;; Check that all tools completed
          (should tools-completed)

          ;; Check buffer contains both expected tool results
          (let ((actual-content (buffer-substring-no-properties (point-min) (point-max))))
            (should (string= expected-content actual-content))))))

    ;; Clean up
    (remhash "test-tool-a" greger-tools-registry)
    (remhash "test-tool-b" greger-tools-registry)))

(ert-deftest greger-test-tool-error-handling ()
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
      (greger-mode) ; Set up greger-mode for tree-sitter support
      (let ((agent-state (make-greger-state
                          :current-iteration 0
                          :chat-buffer (current-buffer)
                          :directory default-directory :tool-use-metadata nil))
            (tool-calls `(((type . "tool_use")
                           (id . "error_test")
                           (name . "test-error")
                           (input . ((input . "bad-input"))))))
            (expected-error-content "

# TOOL USE

Name: test-error
ID: error_test

## input

<tool.error_test>
bad-input
</tool.error_test>

# TOOL RESULT

ID: error_test

<tool.error_test>
Simulated tool error: bad-input
</tool.error_test>"))

        ;; Mock greger--run-agent-loop to capture completion
        (cl-letf (((symbol-function 'greger--run-agent-loop)
                   (lambda (state)
                     (setq test-completed t))))

          ;; First insert the tool use markdown (simulating what greger--append-nonstreaming-content-block does)
          (dolist (tool-call tool-calls)
            (let ((tool-use-markdown (greger-parser--tool-use-to-markdown tool-call))
                  (tool-id (alist-get 'id tool-call)))
              (insert "\n\n" tool-use-markdown)
              (insert "\n\n" (greger--tool-placeholder tool-id))))

          ;; Execute tools
          (greger--execute-tools tool-calls agent-state)

          ;; Check that execution completed despite error
          (should test-completed)

          ;; Check buffer contents match expected error format
          (let ((actual-content (buffer-substring-no-properties (point-min) (point-max))))
            (should (string= expected-error-content actual-content))))))

    ;; Clean up
    (remhash "test-error" greger-tools-registry)))

(ert-deftest greger-test-tool-execution-with-existing-content ()
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
      (greger-mode) ; Set up greger-mode for tree-sitter support
      (insert "Existing content in buffer")

      (let ((agent-state (make-greger-state
                          :current-iteration 0
                          :chat-buffer (current-buffer)
                          :directory default-directory :tool-use-metadata nil))
            (tool-calls `(((type . "tool_use")
                           (id . "content_test")
                           (name . "test-content")
                           (input . ((data . "test-data"))))))
            (expected-content "Existing content in buffer

# TOOL USE

Name: test-content
ID: content_test

## data

<tool.content_test>
test-data
</tool.content_test>

# TOOL RESULT

ID: content_test

<tool.content_test>
Processed: test-data
</tool.content_test>"))

        ;; Mock greger--run-agent-loop to capture completion
        (cl-letf (((symbol-function 'greger--run-agent-loop)
                   (lambda (state)
                     (setq test-completed t))))

          ;; First insert the tool use markdown (simulating what greger--append-nonstreaming-content-block does)
          (dolist (tool-call tool-calls)
            (let ((tool-use-markdown (greger-parser--tool-use-to-markdown tool-call))
                  (tool-id (alist-get 'id tool-call)))
              (insert "\n\n" tool-use-markdown)
              (insert "\n\n" (greger--tool-placeholder tool-id))))

          ;; Execute tools
          (greger--execute-tools tool-calls agent-state)

          ;; Check that execution completed
          (should test-completed)

          ;; Check that content was appended correctly
          (let ((actual-content (buffer-substring-no-properties (point-min) (point-max))))
            (should (string= expected-content actual-content))))))

    ;; Clean up
    (remhash "test-content" greger-tools-registry)))

(ert-deftest greger-test-unknown-tool-error ()
  "Test handling of unknown tool execution."
  (let ((test-completed nil))

    ;; Create test buffer
    (with-temp-buffer
      (greger-mode) ; Set up greger-mode for tree-sitter support
      (let ((agent-state (make-greger-state
                          :current-iteration 0
                          :chat-buffer (current-buffer)
                          :directory default-directory :tool-use-metadata nil))
            (tool-calls `(((type . "tool_use")
                           (id . "unknown_test")
                           (name . "nonexistent-tool")
                           (input . ((param . "value"))))))
            (expected-error-content "

# TOOL USE

Name: nonexistent-tool
ID: unknown_test

## param

<tool.unknown_test>
value
</tool.unknown_test>

# TOOL RESULT

ID: unknown_test

<tool.unknown_test>
Unknown tool: nonexistent-tool
</tool.unknown_test>"))

        ;; Mock greger--run-agent-loop to capture completion
        (cl-letf (((symbol-function 'greger--run-agent-loop)
                   (lambda (state)
                     (setq test-completed t))))

          ;; First insert the tool use markdown (simulating what greger--append-nonstreaming-content-block does)
          (dolist (tool-call tool-calls)
            (let ((tool-use-markdown (greger-parser--tool-use-to-markdown tool-call))
                  (tool-id (alist-get 'id tool-call)))
              (insert "\n\n" tool-use-markdown)
              (insert "\n\n" (greger--tool-placeholder tool-id))))

          ;; Execute tools
          (greger--execute-tools tool-calls agent-state)

          ;; Check that execution completed despite unknown tool
          (should test-completed)

          ;; Check buffer contents match expected error message exactly
          (let ((actual-content (buffer-substring-no-properties (point-min) (point-max))))
            (should (string= expected-error-content actual-content))))))))

(ert-deftest greger-test-exact-tool-output-formatting ()
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
      (greger-mode) ; Set up greger-mode for tree-sitter support
      (let ((agent-state (make-greger-state
                          :current-iteration 0
                          :chat-buffer (current-buffer)
                          :directory default-directory :tool-use-metadata nil))
            (tool-calls `(((type . "tool_use")
                           (id . "multiline_test")
                           (name . "test-multiline")
                           (input . ((content . "Start"))))))
            (expected-content "

# TOOL USE

Name: test-multiline
ID: multiline_test

## content

<tool.multiline_test>
Start
</tool.multiline_test>

# TOOL RESULT

ID: multiline_test

<tool.multiline_test>
Line 1: Start
Line 2: More content
Line 3: End
</tool.multiline_test>"))

        ;; Mock greger--run-agent-loop to capture completion
        (cl-letf (((symbol-function 'greger--run-agent-loop)
                   (lambda (state)
                     (setq test-completed t))))

          ;; First insert the tool use markdown (simulating what greger--append-nonstreaming-content-block does)
          (dolist (tool-call tool-calls)
            (let ((tool-use-markdown (greger-parser--tool-use-to-markdown tool-call))
                  (tool-id (alist-get 'id tool-call)))
              (insert "\n\n" tool-use-markdown)
              (insert "\n\n" (greger--tool-placeholder tool-id))))

          ;; Execute tools
          (greger--execute-tools tool-calls agent-state)

          ;; Check that execution completed
          (should test-completed)

          ;; Check exact multi-line formatting
          (let ((actual-content (buffer-substring-no-properties (point-min) (point-max))))
            (should (string= expected-content actual-content))))))

    ;; Clean up
    (remhash "test-multiline" greger-tools-registry)))

(ert-deftest greger-test-simple-string-comparison ()
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
      (greger-mode) ; Set up greger-mode for tree-sitter support
      ;; Initial buffer state (empty)
      (let ((initial-content "")
            (agent-state (make-greger-state
                          :current-iteration 0
                          :chat-buffer (current-buffer)
                          :directory default-directory :tool-use-metadata nil))
            (tool-calls `(((type . "tool_use")
                           (id . "echo_001")
                           (name . "test-echo")
                           (input . ((input . "hello world"))))))
            ;; Expected content after tool execution
            (expected-final-content "

# TOOL USE

Name: test-echo
ID: echo_001

## input

<tool.echo_001>
hello world
</tool.echo_001>

# TOOL RESULT

ID: echo_001

<tool.echo_001>
Echo: hello world
</tool.echo_001>"))

        ;; Verify initial state
        (should (string= initial-content (buffer-substring-no-properties (point-min) (point-max))))

        ;; Mock greger--run-agent-loop to capture completion
        (cl-letf (((symbol-function 'greger--run-agent-loop)
                   (lambda (state)
                     (setq test-completed t))))

          ;; First insert the tool use markdown (simulating what greger--append-nonstreaming-content-block does)
          (dolist (tool-call tool-calls)
            (let ((tool-use-markdown (greger-parser--tool-use-to-markdown tool-call))
                  (tool-id (alist-get 'id tool-call)))
              (insert "\n\n" tool-use-markdown)
              (insert "\n\n" (greger--tool-placeholder tool-id))))

          ;; Execute tools
          (greger--execute-tools tool-calls agent-state)

          ;; Verify completion
          (should test-completed)

          ;; Verify exact final content using string= comparison
          (let ((actual-final-content (buffer-substring-no-properties (point-min) (point-max))))
            (should (string= expected-final-content actual-final-content))))))

    ;; Clean up
    (remhash "test-echo" greger-tools-registry)))

(ert-deftest greger-test-interrupt-without-active-generation ()
  "Test greger-interrupt behavior when no generation is active."
  ;; Test that greger-interrupt calls keyboard-quit when no active generation
  (with-temp-buffer
    (greger-mode)
    (let ((keyboard-quit-called nil))
      ;; Mock keyboard-quit to track if it's called
      (cl-letf (((symbol-function 'keyboard-quit)
                 (lambda () (setq keyboard-quit-called t))))
        ;; Call greger-interrupt when no active generation
        (greger-interrupt)
        ;; Should have called keyboard-quit
        (should keyboard-quit-called)))))

(ert-deftest greger-test-interrupt-with-active-generation ()
  "Test greger-interrupt behavior when generation is active."
  ;; Test that greger-interrupt cancels active generation
  (with-temp-buffer
    (greger-mode)
    (let ((cancel-called nil)
          (keyboard-quit-called nil)
          ;; Create a mock client state
          (mock-client-state '(mock-state)))

      ;; Create agent state with active client state
      (let ((agent-state (make-greger-state
                          :current-iteration 1
                          :chat-buffer (current-buffer)
                          :directory default-directory
                          :tool-use-metadata '(:safe-shell-commands () :allow-all-shell-commands nil)
                          :client-state mock-client-state)))

        ;; Set buffer-local agent state
        (setq greger--current-state agent-state)

        ;; Mock functions
        (cl-letf (((symbol-function 'greger-client--cancel-request)
                   (lambda (state) (setq cancel-called t)))
                  ((symbol-function 'keyboard-quit)
                   (lambda () (setq keyboard-quit-called t))))

          ;; Call greger-interrupt
          (greger-interrupt)

          ;; Should have called cancel but not keyboard-quit
          (should cancel-called)
          (should-not keyboard-quit-called)
          ;; Client state should be nil after cancellation
          (should (null (greger-state-client-state agent-state))))))))

(ert-deftest greger-test-interrupt-with-executing-tools ()
  "Test greger-interrupt behavior with executing tools."
  ;; Test that greger-interrupt calls cancel functions but doesn't clear the map
  (with-temp-buffer
    (greger-mode)
    ;; Use defvar to create dynamically scoped variables for the closure
    (defvar greger-test-cancel-called nil)
    (defvar greger-test-callback-called nil)

    (let ((keyboard-quit-called nil)
          ;; Create a mock greger-tool with cancel function
          (mock-greger-tool (make-greger-tool
                             :cancel-fn (lambda ()
                                          (setq greger-test-cancel-called t)
                                          (setq greger-test-callback-called t))))
          (executing-tools-map (make-hash-table :test 'equal)))

      ;; Set up executing tools map with one tool
      (puthash "test-tool-id" mock-greger-tool executing-tools-map)

      ;; Create agent state with executing tools
      (let ((agent-state (make-greger-state
                          :current-iteration 1
                          :chat-buffer (current-buffer)
                          :directory default-directory
                          :tool-use-metadata nil
                          :client-state nil
                          :executing-tools executing-tools-map)))

        ;; Set buffer-local agent state
        (setq greger--current-state agent-state)

        ;; Mock keyboard-quit
        (cl-letf (((symbol-function 'keyboard-quit)
                   (lambda () (setq keyboard-quit-called t))))

          ;; Call greger-interrupt
          (greger-interrupt)

          ;; Should have called cancel function
          (should greger-test-cancel-called)
          ;; Should not have called keyboard-quit
          (should-not keyboard-quit-called)
          ;; The executing-tools map should still contain the tool
          ;; (it should only be removed when callback is actually called)
          (should (gethash "test-tool-id" executing-tools-map)))))))

(ert-deftest greger-test-with-context ()
  "Test calling (greger-buffer t) from a temp file with specific cursor position."
  (let ((test-file (make-temp-file "greger-test-context" nil ".txt"))
        (test-content "function calculateSum(a, b) {\n  return a + b;\n}\n\n// TODO: Add error handling here\nconsole.log('Hello world');")
        (source-buffer nil))
    (unwind-protect
        (progn
          ;; Write test content to file
          (with-temp-file test-file
            (insert test-content))
          
          ;; Open the file and position cursor at a specific location
          (setq source-buffer (find-file-noselect test-file))
          (with-current-buffer source-buffer
            ;; Position cursor at line 5, column 9 (in the TODO comment)
            (goto-char (point-min))
            (forward-line 4) ; Move to line 5 (TODO comment)
            (forward-char 9)  ; Move to column 9 (just after "// TODO: ")
            
            ;; Mock the agent loop to capture what would be sent
            (let ((captured-state nil))
              (cl-letf (((symbol-function 'greger--run-agent-loop)
                         (lambda (state)
                           (setq captured-state state)
                           "Mocked agent response"))
                        ((symbol-function 'greger--ensure-buffer-can-be-submitted)
                         #'ignore))
                
                ;; Call greger-buffer with t (no-tools parameter)
                (greger-buffer t)
                
                ;; Verify the state was created correctly
                (should captured-state)
                (should (eq (greger-state-chat-buffer captured-state) source-buffer))
                (should (string= (greger-state-directory captured-state) default-directory))
                
                ;; Verify the current buffer is the source buffer with content
                (should (eq (current-buffer) source-buffer))
                (should (string= (buffer-string) test-content))
                
                ;; Verify cursor position
                (should (= (line-number-at-pos) 5))
                (should (= (current-column) 9))
                
                ;; Verify buffer has the test file name
                (should (string= (buffer-file-name) test-file))))))
      
      ;; Clean up
      (when (and source-buffer (buffer-live-p source-buffer))
        (kill-buffer source-buffer))
      (when (file-exists-p test-file)
        (delete-file test-file)))))



(ert-deftest greger-test-set-model ()
  "Test greger-set-model functionality."
  (let ((original-model greger-model)
        (selected-model 'claude-opus-4-20250514)
        (completing-read-called nil)
        (customize-set-variable-called nil)
        (set-variable nil)
        (set-value nil))

    (unwind-protect
        (progn
          ;; Mock completing-read to return a specific model
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (prompt collection &optional predicate require-match)
                       (setq completing-read-called t)
                       (should (string= prompt "Choose model: "))
                       (should (equal collection greger-available-models))
                       (should require-match)
                       "claude-opus-4-20250514"))
                    ;; Mock customize-set-variable to track the call
                    ((symbol-function 'customize-set-variable)
                     (lambda (variable value)
                       (setq customize-set-variable-called t)
                       (setq set-variable variable)
                       (setq set-value value)))
                    ;; Mock message to avoid output during tests
                    ((symbol-function 'message) #'ignore))

            ;; Call greger-set-model
            (greger-set-model)

            ;; Verify completing-read was called
            (should completing-read-called)

            ;; Verify customize-set-variable was called with correct parameters
            (should customize-set-variable-called)
            (should (eq set-variable 'greger-model))
            (should (eq set-value selected-model))))

      ;; Restore original model
      (setq greger-model original-model))))

(ert-deftest greger-test-debug-request ()
  "Test greger-debug-request functionality."
  (let ((temp-file nil)
        (request-data-saved nil)
        (read-string-called nil)
        (find-file-called nil)
        (filename-used nil)
        (opened-filename nil))

    (unwind-protect
        (with-temp-buffer
          (greger-mode)
          ;; Set up a basic greger buffer with some content
          (insert greger-parser-system-tag "\n\nTest system prompt\n\n"
                  greger-parser-user-tag "\n\nTest user message\n\n"
                  greger-parser-assistant-tag "\n\nTest response")

          ;; Mock functions
          (cl-letf (((symbol-function 'read-string)
                     (lambda (prompt &optional initial-input history default-value)
                       (setq read-string-called t)
                       (should (string-match "Save to filename" prompt))
                       (should (string= default-value "request.json"))
                       (setq filename-used (or default-value "request.json"))
                       filename-used))
                    ;; Mock the greger-client--build-data function to return predictable JSON
                    ((symbol-function 'greger-client--build-data)
                     (lambda (model dialog tools server-tools thinking-budget max-tokens)
                       (setq request-data-saved t)
                       "{\"model\":\"test-model\",\"messages\":[]}"))
                    ;; Mock find-file to track file opening
                    ((symbol-function 'find-file)
                     (lambda (filename)
                       (setq find-file-called t)
                       (setq opened-filename filename)))
                    ;; Mock message to avoid output during tests
                    ((symbol-function 'message) #'ignore))

            ;; Call greger-debug-request
            (greger-debug-request)

            ;; Verify read-string was called
            (should read-string-called)

            ;; Verify request data was built
            (should request-data-saved)

            ;; Verify find-file was called with the correct filename
            (should find-file-called)
            (should (string= opened-filename filename-used))

            ;; Verify file was created with expected content
            (should (file-exists-p filename-used))
            (with-temp-buffer
              (insert-file-contents filename-used)
              (let ((file-content (buffer-substring-no-properties (point-min) (point-max))))
                ;; Should contain pretty-printed JSON
                (should (string-match "model" file-content))
                (should (string-match "messages" file-content))))

            (setq temp-file filename-used)))

      ;; Clean up temp file
      (when (and temp-file (file-exists-p temp-file))
        (delete-file temp-file)))))

(provide 'test-greger)

;;; test-greger.el ends here
