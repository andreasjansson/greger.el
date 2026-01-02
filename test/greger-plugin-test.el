;;; greger-plugin-test.el --- Tests for greger-plugin -*- lexical-binding: t -*-

(require 'ert)
(require 'greger-plugin)

;; Test helper functions

(defun greger-plugin-test--cleanup ()
  "Clean up test plugins and tools from registries."
  (remhash "test-plugin" greger-plugin-registry)
  (remhash "test-plugin-2" greger-plugin-registry)
  (remhash "test-tool-1" greger-tools-registry)
  (remhash "test-tool-2" greger-tools-registry)
  (remhash "test-tool-3" greger-tools-registry))

;; Test functions for plugin tools

(defun greger-plugin-test--echo (message)
  "Simple test function that echoes MESSAGE."
  (format "echo: %s" message))

(defun greger-plugin-test--add (a b)
  "Test function that adds A and B."
  (+ a b))

(defun greger-plugin-test--greet (name &optional greeting)
  "Test function with optional parameter."
  (format "%s, %s!" (or greeting "Hello") name))


;; Tests

(ert-deftest greger-plugin-test-tool-definition ()
  "Test that greger-plugin-tool creates proper tool definition plists."
  (let ((tool-def (greger-plugin-tool "my-tool"
                    :description "A test tool"
                    :properties '((param . ((type . "string"))))
                    :required '("param")
                    :function 'my-function)))
    (should (equal "my-tool" (plist-get tool-def :name)))
    (should (equal "A test tool" (plist-get tool-def :description)))
    (should (equal '((param . ((type . "string")))) (plist-get tool-def :properties)))
    (should (equal '("param") (plist-get tool-def :required)))
    (should (equal 'my-function (plist-get tool-def :function)))))

(ert-deftest greger-plugin-test-tool-definition-with-optional-args ()
  "Test that greger-plugin-tool handles optional args."
  (let ((tool-def (greger-plugin-tool "my-tool"
                    :description "A test tool"
                    :properties '()
                    :required '()
                    :function 'my-function
                    :pass-buffer t
                    :pass-callback t
                    :streaming t
                    :pass-metadata t)))
    (should (eq t (plist-get tool-def :pass-buffer)))
    (should (eq t (plist-get tool-def :pass-callback)))
    (should (eq t (plist-get tool-def :streaming)))
    (should (eq t (plist-get tool-def :pass-metadata)))))

(ert-deftest greger-plugin-test-plugin-registration ()
  "Test that greger-plugin registers plugins and their tools."
  (unwind-protect
      (progn
        (greger-plugin 'test-plugin
          :tools
          ((greger-plugin-tool "test-tool-1"
             :description "First test tool"
             :properties ((message . ((type . "string") (description . "Message"))))
             :required ("message")
             :function greger-plugin-test--echo)))

        ;; Plugin should be registered
        (should (greger-plugin-exists-p "test-plugin"))

        ;; Plugin tools should be listed
        (should (equal '("test-tool-1") (greger-plugin-tools "test-plugin")))

        ;; Tool should be registered in tools registry
        (should (gethash "test-tool-1" greger-tools-registry)))
    (greger-plugin-test--cleanup)))

(ert-deftest greger-plugin-test-plugin-with-multiple-tools ()
  "Test that plugins can have multiple tools."
  (unwind-protect
      (progn
        (greger-plugin 'test-plugin
          :tools
          ((greger-plugin-tool "test-tool-1"
             :description "First test tool"
             :properties ((message . ((type . "string"))))
             :required ("message")
             :function greger-plugin-test--echo)
           (greger-plugin-tool "test-tool-2"
             :description "Second test tool"
             :properties ((a . ((type . "integer")))
                          (b . ((type . "integer"))))
             :required ("a" "b")
             :function greger-plugin-test--add)))

        ;; Plugin should have both tools
        (should (equal '("test-tool-1" "test-tool-2") (greger-plugin-tools "test-plugin")))

        ;; Both tools should be registered
        (should (gethash "test-tool-1" greger-tools-registry))
        (should (gethash "test-tool-2" greger-tools-registry)))
    (greger-plugin-test--cleanup)))

(ert-deftest greger-plugin-test-plugin-tools-are-executable ()
  "Test that plugin tools can be executed via greger-tools-execute."
  (unwind-protect
      (progn
        (greger-plugin 'test-plugin
          :tools
          ((greger-plugin-tool "test-tool-1"
             :description "Echo tool"
             :properties ((message . ((type . "string") (description . "Message"))))
             :required ("message")
             :function greger-plugin-test--echo)
           (greger-plugin-tool "test-tool-2"
             :description "Add tool"
             :properties ((a . ((type . "integer") (description . "First number")))
                          (b . ((type . "integer") (description . "Second number"))))
             :required ("a" "b")
             :function greger-plugin-test--add)))

        ;; Execute first tool
        (let ((result nil) (error nil))
          (greger-tools-execute :tool-name "test-tool-1"
                                :args '((message . "hello"))
                                :callback (lambda (r e) (setq result r error e)))
          (should (equal "echo: hello" result))
          (should (null error)))

        ;; Execute second tool
        (let ((result nil) (error nil))
          (greger-tools-execute :tool-name "test-tool-2"
                                :args '((a . 5) (b . 3))
                                :callback (lambda (r e) (setq result r error e)))
          (should (equal 8 result))
          (should (null error))))
    (greger-plugin-test--cleanup)))

(ert-deftest greger-plugin-test-plugin-tool-with-optional-params ()
  "Test that plugin tools work with optional parameters."
  (unwind-protect
      (progn
        (greger-plugin 'test-plugin
          :tools
          ((greger-plugin-tool "test-tool-3"
             :description "Greet tool"
             :properties ((name . ((type . "string") (description . "Name")))
                          (greeting . ((type . "string") (description . "Greeting") (default . nil))))
             :required ("name")
             :function greger-plugin-test--greet)))

        ;; With only required param
        (let ((result nil) (error nil))
          (greger-tools-execute :tool-name "test-tool-3"
                                :args '((name . "World"))
                                :callback (lambda (r e) (setq result r error e)))
          (should (equal "Hello, World!" result))
          (should (null error)))

        ;; With optional param
        (let ((result nil) (error nil))
          (greger-tools-execute :tool-name "test-tool-3"
                                :args '((name . "World") (greeting . "Goodbye"))
                                :callback (lambda (r e) (setq result r error e)))
          (should (equal "Goodbye, World!" result))
          (should (null error))))
    (greger-plugin-test--cleanup)))

(ert-deftest greger-plugin-test-plugin-with-skills-path ()
  "Test that plugins can have a skills path."
  (unwind-protect
      (let ((skill-path "/tmp/test-skill.md"))
        ;; Create a test skill file
        (with-temp-file skill-path
          (insert "# Test Skill\n\nThis is a test skill."))

        (greger-plugin 'test-plugin
          :skills skill-path
          :tools
          ((greger-plugin-tool "test-tool-1"
             :description "Test tool"
             :properties ()
             :required ()
             :function (lambda () "test"))))

        ;; Skill content should be retrievable
        (let ((skill-content (greger-plugin-skill "test-plugin")))
          (should (stringp skill-content))
          (should (string-match-p "Test Skill" skill-content)))

        ;; Clean up skill file
        (delete-file skill-path))
    (greger-plugin-test--cleanup)))

(ert-deftest greger-plugin-test-plugin-skill-nonexistent-file ()
  "Test that greger-plugin-skill returns nil for nonexistent files."
  (unwind-protect
      (progn
        (greger-plugin 'test-plugin
          :skills "/nonexistent/path/SKILL.md"
          :tools
          ((greger-plugin-tool "test-tool-1"
             :description "Test tool"
             :properties ()
             :required ()
             :function (lambda () "test"))))

        (should (null (greger-plugin-skill "test-plugin"))))
    (greger-plugin-test--cleanup)))

(ert-deftest greger-plugin-test-plugin-list ()
  "Test that greger-plugin-list returns all registered plugins."
  (unwind-protect
      (progn
        (greger-plugin 'test-plugin
          :tools
          ((greger-plugin-tool "test-tool-1"
             :description "Test"
             :properties ()
             :required ()
             :function (lambda () "test"))))

        (greger-plugin 'test-plugin-2
          :tools
          ((greger-plugin-tool "test-tool-2"
             :description "Test 2"
             :properties ()
             :required ()
             :function (lambda () "test2"))))

        (let ((plugins (greger-plugin-list)))
          (should (member "test-plugin" plugins))
          (should (member "test-plugin-2" plugins))))
    (greger-plugin-test--cleanup)))

(ert-deftest greger-plugin-test-plugin-exists-p ()
  "Test greger-plugin-exists-p function."
  (unwind-protect
      (progn
        (should-not (greger-plugin-exists-p "test-plugin"))

        (greger-plugin 'test-plugin
          :tools
          ((greger-plugin-tool "test-tool-1"
             :description "Test"
             :properties ()
             :required ()
             :function (lambda () "test"))))

        (should (greger-plugin-exists-p "test-plugin"))
        (should-not (greger-plugin-exists-p "nonexistent-plugin")))
    (greger-plugin-test--cleanup)))

(ert-deftest greger-plugin-test-plugin-quoted-name ()
  "Test that plugin name can be quoted."
  (unwind-protect
      (progn
        (greger-plugin 'test-plugin
          :tools
          ((greger-plugin-tool "test-tool-1"
             :description "Test"
             :properties ()
             :required ()
             :function (lambda () "test"))))

        (should (greger-plugin-exists-p "test-plugin")))
    (greger-plugin-test--cleanup)))

(ert-deftest greger-plugin-test-tool-schema-generation ()
  "Test that plugin tools generate proper schemas."
  (unwind-protect
      (progn
        (greger-plugin 'test-plugin
          :tools
          ((greger-plugin-tool "test-tool-1"
             :description "A descriptive message"
             :properties ((param1 . ((type . "string") (description . "First param")))
                          (param2 . ((type . "integer") (description . "Second param") (default . 10))))
             :required ("param1")
             :function greger-plugin-test--echo)))

        (let* ((schemas (greger-tools-get-schemas '("test-tool-1")))
               (schema (car schemas)))
          (should (equal "test-tool-1" (alist-get 'name schema)))
          (should (equal "A descriptive message" (alist-get 'description schema)))
          (let ((input-schema (alist-get 'input_schema schema)))
            (should (equal "object" (alist-get 'type input-schema)))
            (should (equal '("param1") (alist-get 'required input-schema))))))
    (greger-plugin-test--cleanup)))

(provide 'greger-plugin-test)

;;; greger-plugin-test.el ends here
