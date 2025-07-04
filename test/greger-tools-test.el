;;; greger-tools-test.el --- Tests for greger tools -*- lexical-binding: t -*-

(require 'ert)
(require 'greger-tools)

(ert-deftest greger-tools-test-tool-registration-and-execution ()
  "Test that tools can be registered and executed correctly."
  ;; Define a simple test function
  (defun greger-test-subtract-numbers (a b)
    (- a b))

  ;; Register a test tool
  (greger-register-tool "test-subtract"
                        :description "Subtract second number from first number"
                        :properties '((a . ((type . "integer")
                                            (description . "First number")))
                                      (b . ((type . "integer")
                                            (description . "Second number"))))
                        :required '("a" "b")
                        :function 'greger-test-subtract-numbers)

  ;; Test that the tool was registered
  (should (gethash "test-subtract" greger-tools-registry))

  ;; Test getting tool schema
  (let ((schemas (greger-tools-get-schemas '("test-subtract"))))
    (should (= 1 (length schemas)))
    (let ((schema (car schemas)))
      (should (string= "test-subtract" (alist-get 'name schema)))
      (should (string= "Subtract second number from first number" (alist-get 'description schema)))))

  ;; Test tool execution
  (let ((result nil)
        (error nil))
    (greger-tools-execute :tool-name "test-subtract"
                          :args '((a . 5) (b . 3))
                          :callback (lambda (r e) (setq result r error e))
                          :buffer nil)
    (should (= 2 result))
    (should (null error)))

  ;; Test execution with different parameters
  (let ((result nil)
        (error nil))
    (greger-tools-execute :tool-name "test-subtract"
                          :args '((b . 1) (a . 4))
                          :callback (lambda (r e) (setq result r error e))
                          :buffer nil)
    (should (= 3 result))
    (should (null error)))

  ;; Clean up - remove test tool from registry
  (remhash "test-subtract" greger-tools-registry))

(ert-deftest greger-tools-test-unknown-tool-error ()
  "Test that executing unknown tools calls callback with error."
  (let ((result nil)
        (error nil))
    (greger-tools-execute :tool-name "nonexistent-tool"
                          :args '((param . "value"))
                          :callback (lambda (r e) (setq result r error e))
                          :buffer nil)
    (should (null result))
    (should (stringp error))
    (should (string-match "Unknown tool" error))))

(ert-deftest greger-tools-test-parameter-mapping ()
  "Test that parameters are correctly mapped from underscores to hyphens."
  ;; Define a test function with hyphenated parameter names
  (defun greger-test-hyphenated-params (path commit-message)
    "Test function with hyphenated parameters."
    (format "file: %s, message: %s" path commit-message))

  ;; Register tool with underscore parameter names (as they come from JSON)
  (greger-register-tool "test-hyphens"
                        :description "Test hyphenated parameter mapping"
                        :properties '((path . ((type . "string")
                                               (description . "File path")))
                                      (commit-message . ((type . "string")
                                                         (description . "Commit message"))))
                        :required '("path" "commit-message")
                        :function 'greger-test-hyphenated-params)

  ;; Test execution with underscore parameters
  (let ((result nil)
        (error nil))
    (greger-tools-execute :tool-name "test-hyphens"
                          :args '((path . "/path/to/file")
                                  (commit-message . "test commit"))
                          :callback (lambda (r e) (setq result r error e))
                          :buffer nil)
    (should (string= "file: /path/to/file, message: test commit" result))
    (should (null error)))

  ;; Clean up
  (remhash "test-hyphens" greger-tools-registry))

(provide 'test-greger-tools)

(ert-deftest greger-tools-test-optional-parameters ()
  "Test that tools work correctly with optional parameters."
  ;; Define a test function with optional parameters
  (defun greger-test-optional-params (required-param &optional optional-param1 optional-param2)
    "Test function with optional parameters."
    (format "required: %s, opt1: %s, opt2: %s"
            required-param
            (or optional-param1 "default1")
            (or optional-param2 "default2")))

  ;; Register tool with some optional parameters
  (greger-register-tool "test-optional"
                        :description "Test optional parameter handling"
                        :properties '((required-param . ((type . "string")
                                                         (description . "Required parameter")))
                                      (optional-param1 . ((type . "string")
                                                          (description . "First optional parameter")))
                                      (optional-param2 . ((type . "string")
                                                          (description . "Second optional parameter"))))
                        :required '("required-param")
                        :function 'greger-test-optional-params)

  ;; Test with only required parameter
  (let ((result nil)
        (error nil))
    (greger-tools-execute :tool-name "test-optional"
                          :args '((required-param . "test"))
                          :callback (lambda (r e) (setq result r error e))
                          :buffer nil)
    (should (string= "required: test, opt1: default1, opt2: default2" result))
    (should (null error)))

  ;; Test with required + one optional parameter
  (let ((result nil)
        (error nil))
    (greger-tools-execute :tool-name "test-optional"
                          :args '((required-param . "test")
                                  (optional-param1 . "provided1"))
                          :callback (lambda (r e) (setq result r error e))
                          :buffer nil)
    (should (string= "required: test, opt1: provided1, opt2: default2" result))
    (should (null error)))

  ;; Test with all parameters provided
  (let ((result nil)
        (error nil))
    (greger-tools-execute :tool-name "test-optional"
                          :args '((required-param . "test")
                                  (optional-param1 . "provided1")
                                  (optional-param2 . "provided2"))
                          :callback (lambda (r e) (setq result r error e))
                          :buffer nil)
    (should (string= "required: test, opt1: provided1, opt2: provided2" result))
    (should (null error)))

  ;; Clean up
  (remhash "test-optional" greger-tools-registry))

(ert-deftest greger-tools-test-default-parameter-values ()
  "Test that tools work correctly with default parameter values."
  ;; Define a test function with default parameters
  (defun greger-test-default-params (message &optional count prefix)
    "Test function with default parameters."
    (let ((actual-prefix (or prefix ">>>")))
      (format "%s %s (repeated %d times)"
              actual-prefix message count)))

  ;; Register tool with default values in the schema
  (greger-register-tool "test-defaults"
                        :description "Test default parameter handling"
                        :properties '((message . ((type . "string")
                                                  (description . "Message to format")))
                                      (count . ((type . "integer")
                                                (description . "Number of repetitions")
                                                (default . 5)))
                                      (prefix . ((type . "string")
                                                 (description . "Prefix for message"))))
                        :required '("message")
                        :function 'greger-test-default-params)

  ;; Test with only required parameter - should use defaults
  (let ((result nil)
        (error nil))
    (greger-tools-execute :tool-name "test-defaults"
                          :args '((message . "hello"))
                          :callback (lambda (r e) (setq result r error e))
                          :buffer nil)
    (should (string= ">>> hello (repeated 5 times)" result))
    (should (null error)))

  ;; Test with one default overridden
  (let ((result nil)
        (error nil))
    (greger-tools-execute :tool-name "test-defaults"
                          :args '((message . "hello")
                                  (count . 2))
                          :callback (lambda (r e) (setq result r error e))
                          :buffer nil)
    (should (string= ">>> hello (repeated 2 times)" result))
    (should (null error)))

  ;; Test with both defaults overridden
  (let ((result nil)
        (error nil))
    (greger-tools-execute :tool-name "test-defaults"
                          :args '((message . "hello")
                                  (count . 2)
                                  (prefix . "***"))
                          :callback (lambda (r e) (setq result r error e))
                          :buffer nil)
    (should (string= "*** hello (repeated 2 times)" result))
    (should (null error)))

  ;; Clean up
  (remhash "test-defaults" greger-tools-registry))

(ert-deftest greger-tools-test-missing-required-parameter-error ()
  "Test that missing required parameters throw an error."
  ;; Define a test function with required and optional parameters
  (defun greger-test-required-params (required-param1 required-param2 &optional optional-param)
    "Test function with required parameters."
    (format "req1: %s, req2: %s, opt: %s"
            required-param1 required-param2 (or optional-param "default")))

  ;; Register tool with multiple required parameters
  (greger-register-tool "test-required"
                        :description "Test required parameter validation"
                        :properties '((required-param1 . ((type . "string")
                                                          (description . "First required parameter")))
                                      (required-param2 . ((type . "string")
                                                          (description . "Second required parameter")))
                                      (optional-param . ((type . "string")
                                                         (description . "Optional parameter"))))
                        :required '("required-param1" "required-param2")
                        :function 'greger-test-required-params)

  ;; Test that missing first required parameter calls callback with error
  (let ((result nil)
        (error nil))
    (greger-tools-execute :tool-name "test-required"
                          :args '((required-param2 . "value2")
                                  (optional-param . "optional"))
                          :callback (lambda (r e) (setq result r error e))
                          :buffer nil)
    (should (null result))
    (should error))

  ;; Test that missing second required parameter calls callback with error
  (let ((result nil)
        (error nil))
    (greger-tools-execute :tool-name "test-required"
                          :args '((required-param1 . "value1")
                                  (optional-param . "optional"))
                          :callback (lambda (r e) (setq result r error e))
                          :buffer nil)
    (should (null result))
    (should error))

  ;; Test that missing both required parameters calls callback with error
  (let ((result nil)
        (error nil))
    (greger-tools-execute :tool-name "test-required"
                          :args '((optional-param . "optional"))
                          :callback (lambda (r e) (setq result r error e))
                          :buffer nil)
    (should (null result))
    (should error))

  ;; Test that providing all required parameters works (even without optional)
  (let ((result nil)
        (error nil))
    (greger-tools-execute :tool-name "test-required"
                          :args '((required-param1 . "value1")
                                  (required-param2 . "value2"))
                          :callback (lambda (r e) (setq result r error e))
                          :buffer nil)
    (should (string= "req1: value1, req2: value2, opt: default" result))
    (should (null error)))

  ;; Test that providing all parameters works
  (let ((result nil)
        (error nil))
    (greger-tools-execute :tool-name "test-required"
                          :args '((required-param1 . "value1")
                                  (required-param2 . "value2")
                                  (optional-param . "provided"))
                          :callback (lambda (r e) (setq result r error e))
                          :buffer nil)
    (should (string= "req1: value1, req2: value2, opt: provided" result))
    (should (null error)))

  ;; Clean up
  (remhash "test-required" greger-tools-registry))

(ert-deftest greger-tools-test-pass-buffer-functionality ()
  "Test that tools can receive buffer parameter when :pass-buffer is set."
  ;; Define a test function that expects a buffer parameter
  (defun greger-test-buffer-param (message &optional buffer)
    "Test function that accepts a buffer parameter."
    (if buffer
        (format "message: %s, buffer: %s" message (buffer-name buffer))
      (format "message: %s, buffer: none" message)))

  ;; Register tool without :pass-buffer
  (greger-register-tool "test-no-buffer"
                        :description "Test tool without buffer passing"
                        :properties '((message . ((type . "string")
                                                  (description . "Test message"))))
                        :required '("message")
                        :function 'greger-test-buffer-param)

  ;; Register tool with :pass-buffer t
  (greger-register-tool "test-with-buffer"
                        :description "Test tool with buffer passing"
                        :properties '((message . ((type . "string")
                                                  (description . "Test message"))))
                        :required '("message")
                        :function 'greger-test-buffer-param
                        :pass-buffer t)

  ;; Create a test buffer
  (with-temp-buffer
    (rename-buffer "*test-buffer*")

    ;; Test tool without :pass-buffer - should not receive buffer
    (let ((result nil)
          (error nil))
      (greger-tools-execute :tool-name "test-no-buffer"
                            :args '((message . "hello"))
                            :callback (lambda (r e) (setq result r error e))
                            :buffer (current-buffer))
      (should (string= "message: hello, buffer: none" result))
      (should (null error)))

    ;; Test tool with :pass-buffer t - should receive buffer
    (let ((result nil)
          (error nil))
      (greger-tools-execute :tool-name "test-with-buffer"
                            :args '((message . "hello"))
                            :callback (lambda (r e) (setq result r error e))
                            :buffer (current-buffer))
      (should (string= "message: hello, buffer: *test-buffer*" result))
      (should (null error))))

  ;; Clean up
  (remhash "test-no-buffer" greger-tools-registry)
  (remhash "test-with-buffer" greger-tools-registry))

(ert-deftest greger-tools-test-pass-callback-functionality ()
  "Test that tools can receive and use callback parameter when :pass-callback is set."
  ;; Define a test function that accepts a callback parameter and calls it asynchronously
  (defun greger-test-callback-param (message callback)
    "Test function that accepts and uses a callback parameter."
    ;; Simulate some processing, then call the callback
    (let ((result (format "processed: %s" message)))
      (funcall callback result nil)))

  ;; Define a test function that doesn't use callback
  (defun greger-test-no-callback-param (message)
    "Test function that returns a result normally."
    (format "result: %s" message))

  ;; Register tool without :pass-callback (normal behavior)
  (greger-register-tool "test-normal-callback"
                        :description "Test tool with normal callback handling"
                        :properties '((message . ((type . "string")
                                                  (description . "Test message"))))
                        :required '("message")
                        :function 'greger-test-no-callback-param)

  ;; Register tool with :pass-callback set to 'callback
  (greger-register-tool "test-pass-callback"
                        :description "Test tool with callback parameter passing"
                        :properties '((message . ((type . "string")
                                                  (description . "Test message"))))
                        :required '("message")
                        :function 'greger-test-callback-param
                        :pass-callback t)

  ;; Test normal tool - greger-tools-execute calls callback with result
  (let ((result nil)
        (error nil))
    (greger-tools-execute :tool-name "test-normal-callback"
                          :args '((message . "hello"))
                          :callback (lambda (r e) (setq result r error e))
                          :buffer nil)
    (should (string= "result: hello" result))
    (should (null error)))

  ;; Test tool with :pass-callback - function calls callback directly
  (let ((result nil)
        (error nil)
        (callback-called nil))
    (greger-tools-execute :tool-name "test-pass-callback"
                          :args '((message . "world"))
                          :callback (lambda (r e)
                                      (setq result r error e callback-called t))
                          :buffer nil)
    ;; Since the callback is called synchronously in our test function,
    ;; we can check the results immediately
    (should callback-called)
    (should (string= "processed: world" result))
    (should (null error)))

  ;; Test combining :pass-callback with :pass-buffer
  (defun greger-test-callback-with-buffer (message callback &optional buffer)
    "Test function that accepts both callback and buffer parameters."
    (let* ((buffer-name (if buffer (buffer-name buffer) "no-buffer"))
           (result (format "message: %s, buffer: %s" message buffer-name)))
      (funcall callback result nil)))

  (greger-register-tool "test-callback-with-buffer"
                        :description "Test tool with both callback and buffer passing"
                        :properties '((message . ((type . "string")
                                                  (description . "Test message"))))
                        :required '("message")
                        :function 'greger-test-callback-with-buffer
                        :pass-callback t
                        :pass-buffer t)

  ;; Test with both buffer and callback
  (with-temp-buffer
    (rename-buffer "*test-callback-buffer*")
    (let ((result nil)
          (error nil)
          (callback-called nil))
      (greger-tools-execute :tool-name "test-callback-with-buffer"
                            :args '((message . "test"))
                            :callback (lambda (r e)
                                        (setq result r error e callback-called t))
                            :buffer (current-buffer))
      ;; Since the callback is called synchronously in our test function,
      ;; we can check the results immediately
      (should callback-called)
      (should (string= "message: test, buffer: *test-callback-buffer*" result))
      (should (null error))))

  ;; Clean up
  (remhash "test-normal-callback" greger-tools-registry)
  (remhash "test-pass-callback" greger-tools-registry)
  (remhash "test-callback-with-buffer" greger-tools-registry))

(ert-deftest greger-tools-test-json-parsing-arrays ()
  "Test that array parameters are correctly parsed from JSON strings."
  ;; Define a test function that expects a list
  (defun greger-test-array-param (items)
    "Test function that expects an array parameter."
    (format "received %d items: %s" (length items) (mapconcat 'identity items ", ")))

  ;; Register tool with array parameter
  (greger-register-tool "test-array-parsing"
                        :description "Test array parameter JSON parsing"
                        :properties '((items . ((type . "array")
                                                (items . ((type . "string")))
                                                (description . "List of items"))))
                        :required '("items")
                        :function 'greger-test-array-param)

  ;; Test with JSON array string
  (let ((result nil)
        (error nil))
    (greger-tools-execute :tool-name "test-array-parsing"
                          :args '((items . "[\"apple\", \"banana\", \"cherry\"]"))
                          :callback (lambda (r e) (setq result r error e))
                          :buffer nil)
    (should (string= "received 3 items: apple, banana, cherry" result))
    (should (null error)))

  ;; Test with already parsed list (should work fine)
  (let ((result nil)
        (error nil))
    (greger-tools-execute :tool-name "test-array-parsing"
                          :args '((items . ("apple" "banana")))
                          :callback (lambda (r e) (setq result r error e))
                          :buffer nil)
    (should (string= "received 2 items: apple, banana" result))
    (should (null error)))

  ;; Clean up
  (remhash "test-array-parsing" greger-tools-registry))

(ert-deftest greger-tools-test-json-parsing-booleans ()
  "Test that boolean parameters are correctly parsed from JSON strings."
  ;; Define a test function that expects booleans
  (defun greger-test-boolean-params (flag1 &optional flag2)
    "Test function that expects boolean parameters."
    (format "flag1: %s, flag2: %s" flag1 flag2))

  ;; Register tool with boolean parameters
  (greger-register-tool "test-boolean-parsing"
                        :description "Test boolean parameter JSON parsing"
                        :properties '((flag1 . ((type . "boolean")
                                                (description . "First boolean flag")))
                                      (flag2 . ((type . "boolean")
                                                (description . "Second boolean flag"))))
                        :required '("flag1")
                        :function 'greger-test-boolean-params)

  ;; Test with JSON boolean strings
  (let ((result nil)
        (error nil))
    (greger-tools-execute :tool-name "test-boolean-parsing"
                          :args '((flag1 . "true") (flag2 . "false"))
                          :callback (lambda (r e) (setq result r error e))
                          :buffer nil)
    (should (string= "flag1: t, flag2: nil" result))
    (should (null error)))

  ;; Test with :json-true/:json-false format
  (let ((result nil)
        (error nil))
    (greger-tools-execute :tool-name "test-boolean-parsing"
                          :args '((flag1 . ":json-false") (flag2 . ":json-true"))
                          :callback (lambda (r e) (setq result r error e))
                          :buffer nil)
    (should (string= "flag1: nil, flag2: t" result))
    (should (null error)))

  ;; Test with already parsed booleans (should work fine)
  (let ((result nil)
        (error nil))
    (greger-tools-execute :tool-name "test-boolean-parsing"
                          :args '((flag1 . t) (flag2 . nil))
                          :callback (lambda (r e) (setq result r error e))
                          :buffer nil)
    (should (string= "flag1: t, flag2: nil" result))
    (should (null error)))

  ;; Clean up
  (remhash "test-boolean-parsing" greger-tools-registry))

(ert-deftest greger-tools-test-json-parsing-numbers ()
  "Test that number parameters are correctly parsed from JSON strings."
  ;; Define a test function that expects numbers
  (defun greger-test-number-params (count &optional rate)
    "Test function that expects number parameters."
    (format "count: %s (type: %s), rate: %s (type: %s)"
            count (type-of count) rate (type-of rate)))

  ;; Register tool with number parameters
  (greger-register-tool "test-number-parsing"
                        :description "Test number parameter JSON parsing"
                        :properties '((count . ((type . "integer")
                                                (description . "Integer count")))
                                      (rate . ((type . "number")
                                               (description . "Floating point rate"))))
                        :required '("count")
                        :function 'greger-test-number-params)

  ;; Test with JSON number strings
  (let ((result nil)
        (error nil))
    (greger-tools-execute :tool-name "test-number-parsing"
                          :args '((count . "42") (rate . "3.14"))
                          :callback (lambda (r e) (setq result r error e))
                          :buffer nil)
    (should (string= "count: 42 (type: integer), rate: 3.14 (type: float)" result))
    (should (null error)))

  ;; Test with negative numbers
  (let ((result nil)
        (error nil))
    (greger-tools-execute :tool-name "test-number-parsing"
                          :args '((count . "-5") (rate . "-2.5"))
                          :callback (lambda (r e) (setq result r error e))
                          :buffer nil)
    (should (string= "count: -5 (type: integer), rate: -2.5 (type: float)" result))
    (should (null error)))

  ;; Test with already parsed numbers (should work fine)
  (let ((result nil)
        (error nil))
    (greger-tools-execute :tool-name "test-number-parsing"
                          :args '((count . 10) (rate . 1.5))
                          :callback (lambda (r e) (setq result r error e))
                          :buffer nil)
    (should (string= "count: 10 (type: integer), rate: 1.5 (type: float)" result))
    (should (null error)))

  ;; Clean up
  (remhash "test-number-parsing" greger-tools-registry))

(ert-deftest greger-tools-test-json-parsing-mixed-types ()
  "Test that mixed parameter types are correctly handled."
  ;; Define a test function with mixed parameter types
  (defun greger-test-mixed-params (name items count enabled &optional rate)
    "Test function with mixed parameter types."
    (format "name: %s, items: %d, count: %s, enabled: %s, rate: %s"
            name (length items) count enabled rate))

  ;; Register tool with mixed parameter types
  (greger-register-tool "test-mixed-parsing"
                        :description "Test mixed parameter type JSON parsing"
                        :properties '((name . ((type . "string")
                                               (description . "String name")))
                                      (items . ((type . "array")
                                                (items . ((type . "string")))
                                                (description . "List of items")))
                                      (count . ((type . "integer")
                                                (description . "Integer count")))
                                      (enabled . ((type . "boolean")
                                                  (description . "Boolean flag")))
                                      (rate . ((type . "number")
                                               (description . "Optional rate"))))
                        :required '("name" "items" "count" "enabled")
                        :function 'greger-test-mixed-params)

  ;; Test with mixed JSON strings and parsed values
  (let ((result nil)
        (error nil))
    (greger-tools-execute :tool-name "test-mixed-parsing"
                          :args '((name . "test")
                                  (items . "[\"a\", \"b\", \"c\"]")
                                  (count . "5")
                                  (enabled . "true")
                                  (rate . "2.5"))
                          :callback (lambda (r e) (setq result r error e))
                          :buffer nil)
    (should (string= "name: test, items: 3, count: 5, enabled: t, rate: 2.5" result))
    (should (null error)))

  ;; Clean up
  (remhash "test-mixed-parsing" greger-tools-registry))

(ert-deftest greger-tools-test-argument-extraction-edge-cases ()
  "Test edge cases in argument extraction."
  ;; Define a test function with various parameter patterns
  (defun greger-test-edge-cases (required-param &optional optional-param1 optional-param2)
    "Test function with edge case parameter patterns."
    (format "required: %s, opt1: %s, opt2: %s"
            required-param optional-param1 optional-param2))

  ;; Register tool
  (greger-register-tool "test-edge-cases"
                        :description "Test edge cases in argument extraction"
                        :properties '((required-param . ((type . "string")
                                                         (description . "Required parameter")))
                                      (optional-param1 . ((type . "boolean")
                                                          (description . "Optional boolean")))
                                      (optional-param2 . ((type . "integer")
                                                          (description . "Optional integer"))))
                        :required '("required-param")
                        :function 'greger-test-edge-cases)

  ;; Test with only required parameter
  (let ((result nil)
        (error nil))
    (greger-tools-execute :tool-name "test-edge-cases"
                          :args '((required-param . "test"))
                          :callback (lambda (r e) (setq result r error e))
                          :buffer nil)
    (should (string= "required: test, opt1: nil, opt2: nil" result))
    (should (null error)))

  ;; Test with invalid JSON that should be returned as-is
  (let ((result nil)
        (error nil))
    (greger-tools-execute :tool-name "test-edge-cases"
                          :args '((required-param . "test")
                                  (optional-param1 . "not-a-boolean")
                                  (optional-param2 . "not-a-number"))
                          :callback (lambda (r e) (setq result r error e))
                          :buffer nil)
    (should (string= "required: test, opt1: nil, opt2: not-a-number" result))
    (should (null error)))

  ;; Clean up
  (remhash "test-edge-cases" greger-tools-registry))

(ert-deftest greger-tools-test-json-parsing-fallback ()
  "Test that invalid JSON falls back gracefully."
  ;; Define a test function
  (defun greger-test-fallback (items)
    "Test function for fallback behavior."
    (format "received: %s (type: %s)" items (type-of items)))

  ;; Register tool with array parameter
  (greger-register-tool "test-fallback"
                        :description "Test JSON parsing fallback"
                        :properties '((items . ((type . "array")
                                                (description . "List of items"))))
                        :required '("items")
                        :function 'greger-test-fallback)

  ;; Test with invalid JSON - should return original string
  (let ((result nil)
        (error nil))
    (greger-tools-execute :tool-name "test-fallback"
                          :args '((items . "[invalid json"))
                          :callback (lambda (r e) (setq result r error e))
                          :buffer nil)
    (should (string= "received: [invalid json (type: string)" result))
    (should (null error)))

  ;; Clean up
  (remhash "test-fallback" greger-tools-registry))

(ert-deftest greger-tools-test-greger-tool-struct-creation ()
  "Test that greger-tools-execute returns a greger-tool struct."
  ;; Define a simple test function
  (defun greger-test-simple-return ()
    "Just return a string")

  ;; Register a test tool
  (greger-register-tool "test-simple-struct"
                        :description "Simple tool for testing greger-tool struct"
                        :properties '()
                        :required '()
                        :function 'greger-test-simple-return)

  (let ((result nil)
        (error nil)
        (greger-tool nil))

    ;; Execute the tool and capture the greger-tool struct
    (setq greger-tool (greger-tools-execute :tool-name "test-simple-struct"
                                            :args '()
                                            :callback (lambda (r e) (setq result r error e))
                                            :buffer nil))

    ;; Should get a greger-tool struct
    (should (greger-tool-p greger-tool))
    (should (null (greger-tool-cancel-fn greger-tool)))
    (should (string= "Just return a string" result))
    (should (null error)))

  ;; Clean up
  (remhash "test-simple-struct" greger-tools-registry))

(ert-deftest greger-tools-test-greger-tool-with-cancel-function ()
  "Test that greger-tools-execute captures cancel functions."
  ;; Define a test function that returns a cancel function
  (defun greger-test-with-cancel ()
    (lambda () "cancelled"))

  ;; Register a test tool
  (greger-register-tool "test-with-cancel"
                        :description "Tool that returns a cancel function"
                        :properties '()
                        :required '()
                        :function 'greger-test-with-cancel)

  (let ((result nil)
        (error nil)
        (greger-tool nil))

    ;; Execute the tool and capture the greger-tool struct
    (setq greger-tool (greger-tools-execute :tool-name "test-with-cancel"
                                            :args '()
                                            :callback (lambda (r e) (setq result r error e))
                                            :buffer nil))

    ;; Should get a greger-tool struct with a cancel function
    (should (greger-tool-p greger-tool))
    (should (functionp (greger-tool-cancel-fn greger-tool)))
    ;; The result should be the cancel function
    (should (functionp result))
    (should (null error))

    ;; Test that the cancel function works
    (let ((cancel-result (funcall (greger-tool-cancel-fn greger-tool))))
      (should (string= "cancelled" cancel-result))))

  ;; Clean up
  (remhash "test-with-cancel" greger-tools-registry))

(ert-deftest greger-tools-test-greger-tool-with-pass-callback ()
  "Test that greger-tools-execute works with pass-callback tools."
  ;; Define a test function that takes a callback and returns a cancel function
  (defun greger-test-with-callback-and-cancel (callback)
    (funcall callback "callback result" nil)
    (lambda () "callback-cancelled"))

  ;; Register a test tool with pass-callback
  (greger-register-tool "test-callback-cancel"
                        :description "Tool with callback that returns cancel function"
                        :properties '()
                        :required '()
                        :pass-callback t
                        :function 'greger-test-with-callback-and-cancel)

  (let ((result nil)
        (error nil)
        (greger-tool nil))

    ;; Execute the tool and capture the greger-tool struct
    (setq greger-tool (greger-tools-execute :tool-name "test-callback-cancel"
                                            :args '()
                                            :callback (lambda (r e) (setq result r error e))
                                            :buffer nil))

    ;; Should get a greger-tool struct with a cancel function
    (should (greger-tool-p greger-tool))
    (should (functionp (greger-tool-cancel-fn greger-tool)))
    ;; The callback should have been called
    (should (string= "callback result" result))
    (should (null error))

    ;; Test that the cancel function works
    (let ((cancel-result (funcall (greger-tool-cancel-fn greger-tool))))
      (should (string= "callback-cancelled" cancel-result))))

  ;; Clean up
  (remhash "test-callback-cancel" greger-tools-registry))

(ert-deftest greger-tools-test-cancellation-calls-callback ()
  "Test that cancelling a tool properly calls the callback with an error."
  ;; Define a test function that simulates a long-running process
  (defun greger-test-long-running-with-cancel (callback)
    (let ((cancelled nil))
      ;; Return cancel function that sets cancelled flag and calls callback
      (lambda ()
        (unless cancelled
          (setq cancelled t)
          (funcall callback nil "Operation was cancelled")))))

  ;; Register a test tool with pass-callback
  (greger-register-tool "test-cancellable"
                        :description "Tool that can be cancelled"
                        :properties '()
                        :required '()
                        :pass-callback t
                        :function 'greger-test-long-running-with-cancel)

  (let ((result nil)
        (error nil)
        (greger-tool nil))

    ;; Execute the tool and capture the greger-tool struct
    (setq greger-tool (greger-tools-execute :tool-name "test-cancellable"
                                            :args '()
                                            :callback (lambda (r e) (setq result r error e))
                                            :buffer nil))

    ;; Should get a greger-tool struct with a cancel function
    (should (greger-tool-p greger-tool))
    (should (functionp (greger-tool-cancel-fn greger-tool)))

    ;; Initially, no callback should have been called
    (should (null result))
    (should (null error))

    ;; Cancel the operation
    (funcall (greger-tool-cancel-fn greger-tool))

    ;; Now the callback should have been called with an error
    (should (null result))
    (should (string= "Operation was cancelled" error)))

  ;; Clean up
  (remhash "test-cancellable" greger-tools-registry))

(ert-deftest greger-tools-test-server-tool-registration ()
  "Test that server tools can be registered and schemas retrieved as JSON."
  ;; Register a test server tool
  (greger-register-server-tool 'test-web-search
                               :type "web_search_20250305"
                               :max_uses 5
                               :allowed_domains ["example.com" "trusteddomain.org"]
                               :user_location ((type . "approximate")
                                               (city . "San Francisco")
                                               (region . "California")
                                               (country . "US")
                                               (timezone . "America/Los_Angeles")))

  ;; Test that the server tool was registered
  (should (gethash "test-web-search" greger-server-tools-registry))

  ;; Test getting server tool schemas as JSON
  (let ((schemas (greger-server-tools-get-schemas '(test-web-search))))
    (should (= 1 (length schemas)))
    (let ((json-string (car schemas)))
      (should (stringp json-string))
      ;; Verify it's valid JSON by parsing it
      (let ((parsed (json-parse-string json-string :object-type 'alist)))
        (should (string= "web_search_20250305" (alist-get 'type parsed)))
        (should (string= "test-web-search" (alist-get 'name parsed)))
        (should (= 5 (alist-get 'max_uses parsed)))
        (should (equal ["example.com" "trusteddomain.org"] (alist-get 'allowed_domains parsed))))))

  ;; Test getting all server tool schemas
  (let ((all-schemas (greger-server-tools-get-all-schemas)))
    (should (>= (length all-schemas) 1))
    (should (cl-every #'stringp all-schemas)))

  ;; Clean up - remove test server tool from registry
  (remhash "test-web-search" greger-server-tools-registry))

;;; greger-tools-test.el ends here
