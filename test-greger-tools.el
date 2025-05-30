;;; test-greger-tools.el --- Tests for greger tools -*- lexical-binding: t -*-

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
    (greger-tools-execute "test-subtract" '((a . 5) (b . 3))
                          (lambda (r e) (setq result r error e)) nil)
    (should (= 2 result))
    (should (null error)))

  ;; Test execution with different parameters
  (let ((result nil)
        (error nil))
    (greger-tools-execute "test-subtract" '((b . 1) (a . 4))
                          (lambda (r e) (setq result r error e)) nil)
    (should (= 3 result))
    (should (null error)))

  ;; Clean up - remove test tool from registry
  (remhash "test-subtract" greger-tools-registry))

(ert-deftest greger-tools-test-unknown-tool-error ()
  "Test that executing unknown tools calls callback with error."
  (let ((result nil)
        (error nil))
    (greger-tools-execute "nonexistent-tool" '((param . "value"))
                          (lambda (r e) (setq result r error e)) nil)
    (should (null result))
    (should (stringp error))
    (should (string-match "Unknown tool" error))))

(ert-deftest greger-tools-test-parameter-mapping ()
  "Test that parameters are correctly mapped from underscores to hyphens."
  ;; Define a test function with hyphenated parameter names
  (defun greger-test-hyphenated-params (file-path commit-message)
    "Test function with hyphenated parameters."
    (format "file: %s, message: %s" file-path commit-message))

  ;; Register tool with underscore parameter names (as they come from JSON)
  (greger-register-tool "test-hyphens"
    :description "Test hyphenated parameter mapping"
    :properties '((file_path . ((type . "string")
                                (description . "File path")))
                  (commit_message . ((type . "string")
                                     (description . "Commit message"))))
    :required '("file_path" "commit_message")
    :function 'greger-test-hyphenated-params)

  ;; Test execution with underscore parameters
  (let ((result nil)
        (error nil))
    (greger-tools-execute "test-hyphens"
                          '((file_path . "/path/to/file")
                            (commit_message . "test commit"))
                          (lambda (r e) (setq result r error e)) nil)
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
    :properties '((required_param . ((type . "string")
                                     (description . "Required parameter")))
                  (optional_param1 . ((type . "string")
                                      (description . "First optional parameter")))
                  (optional_param2 . ((type . "string")
                                      (description . "Second optional parameter"))))
    :required '("required_param")
    :function 'greger-test-optional-params)

  ;; Test with only required parameter
  (let ((result nil)
        (error nil))
    (greger-tools-execute "test-optional"
                          '((required_param . "test"))
                          (lambda (r e) (setq result r error e)) nil)
    (should (string= "required: test, opt1: default1, opt2: default2" result))
    (should (null error)))

  ;; Test with required + one optional parameter
  (let ((result nil)
        (error nil))
    (greger-tools-execute "test-optional"
                          '((required_param . "test")
                            (optional_param1 . "provided1"))
                          (lambda (r e) (setq result r error e)) nil)
    (should (string= "required: test, opt1: provided1, opt2: default2" result))
    (should (null error)))

  ;; Test with all parameters provided
  (let ((result nil)
        (error nil))
    (greger-tools-execute "test-optional"
                          '((required_param . "test")
                            (optional_param1 . "provided1")
                            (optional_param2 . "provided2"))
                          (lambda (r e) (setq result r error e)) nil)
    (should (string= "required: test, opt1: provided1, opt2: provided2" result))
    (should (null error)))

  ;; Clean up
  (remhash "test-optional" greger-tools-registry))

(ert-deftest greger-tools-test-default-parameter-values ()
  "Test that tools work correctly with default parameter values."
  ;; Define a test function with default parameters
  (defun greger-test-default-params (message &optional count prefix)
    "Test function with default parameters."
    ;; TODO: remove debug
    (message (format "count: %s" count))
    ;; TODO: remove debug
    (message (format "prefix: %s" prefix))
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
    (greger-tools-execute "test-defaults"
                          '((message . "hello"))
                          (lambda (r e) (setq result r error e)) nil)
    (should (string= ">>> hello (repeated 5 times)" result))
    (should (null error)))

  ;; Test with one default overridden
  (let ((result nil)
        (error nil))
    (greger-tools-execute "test-defaults"
                          '((message . "hello")
                            (count . 2))
                          (lambda (r e) (setq result r error e)) nil)
    (should (string= ">>> hello (repeated 2 times)" result))
    (should (null error)))

  ;; Test with both defaults overridden
  (let ((result nil)
        (error nil))
    (greger-tools-execute "test-defaults"
                          '((message . "hello")
                            (count . 2)
                            (prefix . "***"))
                          (lambda (r e) (setq result r error e)) nil)
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
    :properties '((required_param1 . ((type . "string")
                                      (description . "First required parameter")))
                  (required_param2 . ((type . "string")
                                      (description . "Second required parameter")))
                  (optional_param . ((type . "string")
                                     (description . "Optional parameter"))))
    :required '("required_param1" "required_param2")
    :function 'greger-test-required-params)

  ;; Test that missing first required parameter calls callback with error
  (let ((result nil)
        (error nil))
    (greger-tools-execute "test-required"
                          '((required_param2 . "value2")
                            (optional_param . "optional"))
                          (lambda (r e) (setq result r error e)) nil)
    (should (null result))
    (should error))

  ;; Test that missing second required parameter calls callback with error
  (let ((result nil)
        (error nil))
    (greger-tools-execute "test-required"
                          '((required_param1 . "value1")
                            (optional_param . "optional"))
                          (lambda (r e) (setq result r error e)) nil)
    (should (null result))
    (should error))

  ;; Test that missing both required parameters calls callback with error
  (let ((result nil)
        (error nil))
    (greger-tools-execute "test-required"
                          '((optional_param . "optional"))
                          (lambda (r e) (setq result r error e)) nil)
    (should (null result))
    (should error))

  ;; Test that providing all required parameters works (even without optional)
  (let ((result nil)
        (error nil))
    (greger-tools-execute "test-required"
                          '((required_param1 . "value1")
                            (required_param2 . "value2"))
                          (lambda (r e) (setq result r error e)) nil)
    (should (string= "req1: value1, req2: value2, opt: default" result))
    (should (null error)))

  ;; Test that providing all parameters works
  (let ((result nil)
        (error nil))
    (greger-tools-execute "test-required"
                          '((required_param1 . "value1")
                            (required_param2 . "value2")
                            (optional_param . "provided"))
                          (lambda (r e) (setq result r error e)) nil)
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
      (greger-tools-execute "test-no-buffer"
                            '((message . "hello"))
                            (lambda (r e) (setq result r error e))
                            (current-buffer))
      (should (string= "message: hello, buffer: none" result))
      (should (null error)))

    ;; Test tool with :pass-buffer t - should receive buffer
    (let ((result nil)
          (error nil))
      (greger-tools-execute "test-with-buffer"
                            '((message . "hello"))
                            (lambda (r e) (setq result r error e))
                            (current-buffer))
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
    :pass-callback callback)

  ;; Test normal tool - greger-tools-execute calls callback with result
  (let ((result nil)
        (error nil))
    (greger-tools-execute "test-normal-callback"
                          '((message . "hello"))
                          (lambda (r e) (setq result r error e))
                          nil)
    (should (string= "result: hello" result))
    (should (null error)))

  ;; Test tool with :pass-callback - function calls callback directly
  (let ((result nil)
        (error nil))
    (greger-tools-execute "test-pass-callback"
                          '((message . "world"))
                          (lambda (r e) (setq result r error e))
                          nil)
    (should (string= "processed: world" result))
    (should (null error)))

  ;; Clean up
  (remhash "test-normal-callback" greger-tools-registry)
  (remhash "test-pass-callback" greger-tools-registry))

;;; test-greger-tools.el ends here
