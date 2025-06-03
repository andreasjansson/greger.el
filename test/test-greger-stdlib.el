;;; greger-test-stdlib.el --- Tests for greger stdlib tools -*- lexical-binding: t -*-

;;; Commentary:
;; Tests for the tools defined in greger-stdlib.el

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'greger-stdlib)
(require 'greger-web)

(ert-deftest greger-test-read-webpage-valid-url ()
  "Test reading a webpage with a valid URL."
  (let ((test-url "https://pub-b88c9764a4fc46baa90b9e8e1544f59e.r2.dev/hello.html"))

    ;; Test with text extraction (default)
    (let ((result (greger-stdlib--read-webpage test-url t)))
      (should (stringp result))
      (should (string= "Hello world!\n" result)))

    ;; Test with raw HTML
    (let ((result (greger-stdlib--read-webpage test-url nil)))
      (should (stringp result))
      (should (string= "<h1>Hello world!</h1>\n" result)))

    ;; Test with readability enhancement
    (let ((result (greger-stdlib--read-webpage test-url t t)))
      (should (stringp result))
      (should (string= "Hello world!\n" result)))))

(ert-deftest greger-test-read-webpage-invalid-url ()
  "Test reading a webpage with invalid URLs."
  ;; Test empty URL
  (should-error (greger-stdlib--read-webpage ""))
  (should-error (greger-stdlib--read-webpage "   "))

  ;; Test non-string URL
  (should-error (greger-stdlib--read-webpage nil))
  (should-error (greger-stdlib--read-webpage 123))

  ;; Test invalid URL format
  (should-error (greger-stdlib--read-webpage "ftp://example.com"))
  (should-error (greger-stdlib--read-webpage "not-a-url"))
  (should-error (greger-stdlib--read-webpage "file:///path/to/file")))

(ert-deftest greger-test-read-webpage-network-error ()
  "Test reading a webpage when network error occurs."
  ;; Mock the web download function to simulate network error
  (cl-letf (((symbol-function 'greger-web-download-page)
             (lambda (url extract-text use-highest-readability)
               (error "Network timeout"))))

    (let ((result (greger-stdlib--read-webpage "https://pub-b88c9764a4fc46baa90b9e8e1544f59e.r2.dev/hello.html")))
      (should (stringp result))
      (should (string-match-p "Failed to read webpage" result))
      (should (string-match-p "Network timeout" result)))))

(ert-deftest greger-test-web-url-validation ()
  "Test the web URL validation function."
  ;; Valid URLs
  (should (greger-web-is-web-url-p "http://example.com"))
  (should (greger-web-is-web-url-p "https://example.com"))
  (should (greger-web-is-web-url-p "https://subdomain.example.com/path"))

  ;; Invalid URLs
  (should-not (greger-web-is-web-url-p "ftp://example.com"))
  (should-not (greger-web-is-web-url-p "file:///path/to/file"))
  (should-not (greger-web-is-web-url-p "/local/path"))
  (should-not (greger-web-is-web-url-p "example.com"))
  (should-not (greger-web-is-web-url-p "")))

(ert-deftest greger-test-shell-command-simple ()
  "Test shell-command tool with a simple command."
  (let ((result nil)
        (error nil)
        (callback-called nil))

    ;; Mock the permission prompt to always return yes
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (prompt) t)))

      ;; Execute a simple echo command
      (greger-stdlib--shell-command
       "echo hello world"
       (lambda (output err)
         (setq result output error err callback-called t)))

      ;; Wait for async operation to complete
      (let ((timeout 0))
        (while (and (not callback-called) (< timeout 50))  ; 5 second timeout
          (sit-for 0.1)
          (setq timeout (1+ timeout))))

      ;; Verify the results
      (should callback-called)
      (should (null error))
      (should (stringp result))
      (should (string-match "Command executed successfully" result))
      (should (string-match "hello world" result)))))

(ert-deftest greger-test-shell-command-with-pipe ()
  "Test shell-command tool with a command containing a pipe."
  (let ((result nil)
        (error nil)
        (callback-called nil))

    ;; Mock the permission prompt to always return yes
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (prompt) t)))

      ;; Execute a command with a pipe
      (greger-stdlib--shell-command
       "echo 'apple\nbanana\ncherry' | grep 'an'"
       (lambda (output err)
         (setq result output error err callback-called t)))

      ;; Wait for async operation to complete
      (let ((timeout 0))
        (while (and (not callback-called) (< timeout 50))  ; 5 second timeout
          (sit-for 0.1)
          (setq timeout (1+ timeout))))

      ;; Verify the results
      (should callback-called)
      (should (null error))
      (should (stringp result))
      (should (string-match "Command executed successfully" result))
      (should (string-match "banana" result)))))

(ert-deftest greger-test-shell-command-permission-denied ()
  "Test shell-command tool when user denies permission."
  (let ((result nil)
        (error nil)
        (callback-called nil))

    ;; Mock the permission prompt to always return no
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (prompt) nil)))

      ;; Try to execute a command
      (greger-stdlib--shell-command
       "echo test"
       (lambda (output err)
         (setq result output error err callback-called t)))

      ;; Should call callback immediately with error
      (should callback-called)
      (should (null result))
      (should (stringp error))
      (should (string-match "cancelled by user" error)))))

(ert-deftest greger-test-shell-command-command-failure ()
  "Test shell-command tool when command fails."
  (let ((result nil)
        (error nil)
        (callback-called nil))

    ;; Mock the permission prompt to always return yes
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (prompt) t)))

      ;; Execute a command that should fail
      (greger-stdlib--shell-command
       "false"  ; Command that always exits with code 1
       (lambda (output err)
         (setq result output error err callback-called t)))

      ;; Wait for async operation to complete
      (let ((timeout 0))
        (while (and (not callback-called) (< timeout 50))  ; 5 second timeout
          (sit-for 0.1)
          (setq timeout (1+ timeout))))

      ;; Verify the results
      (should callback-called)
      (should (null result))
      (should (stringp error))
      (should (string-match "failed with exit code" error)))))

(ert-deftest greger-test-shell-command-safe-commands ()
  "Test shell-command tool with safe-shell-commands metadata to skip permission prompt."
  (let ((result nil)
        (error nil)
        (callback-called nil)
        (prompt-called nil))

    ;; Mock the permission prompt to track if it's called
    (cl-letf (((symbol-function 'y-or-n-p)
               (lambda (prompt)
                 (setq prompt-called t)
                 t)))

      ;; Create metadata with safe shell commands
      (let ((metadata '(:safe-shell-commands ("echo safe command" "pwd" "ls -la"))))

        ;; Execute a command that's in the safe list
        (greger-stdlib--shell-command
         "echo safe command"
         (lambda (output err)
           (setq result output error err callback-called t))
         "."  ; working directory
         metadata)

        ;; Wait for async operation to complete
        (let ((timeout 0))
          (while (and (not callback-called) (< timeout 50))  ; 5 second timeout
            (sit-for 0.1)
            (setq timeout (1+ timeout))))

        ;; Verify the results
        (should callback-called)
        (should (null error))
        (should (stringp result))
        (should (string-match "Command executed successfully" result))
        (should (string-match "safe command" result))
        ;; Most importantly: permission prompt should NOT have been called
        (should (null prompt-called))))))

(ert-deftest greger-test-shell-command-unsafe-commands-with-metadata ()
  "Test shell-command tool with metadata but command not in safe list still prompts."
  (let ((result nil)
        (error nil)
        (callback-called nil)
        (prompt-called nil))

    ;; Mock the permission prompt to track if it's called and return yes
    (cl-letf (((symbol-function 'y-or-n-p)
               (lambda (prompt)
                 (setq prompt-called t)
                 t)))

      ;; Create metadata with safe shell commands
      (let ((metadata '(:safe-shell-commands ("echo safe command" "pwd"))))

        ;; Execute a command that's NOT in the safe list
        (greger-stdlib--shell-command
         "echo unsafe command"
         (lambda (output err)
           (setq result output error err callback-called t))
         "."  ; working directory
         metadata)

        ;; Wait for async operation to complete
        (let ((timeout 0))
          (while (and (not callback-called) (< timeout 50))  ; 5 second timeout
            (sit-for 0.1)
            (setq timeout (1+ timeout))))

        ;; Verify the results
        (should callback-called)
        (should (null error))
        (should (stringp result))
        (should (string-match "Command executed successfully" result))
        (should (string-match "unsafe command" result))
        ;; Permission prompt SHOULD have been called since command not in safe list
        (should prompt-called)))))

(ert-deftest greger-test-shell-command-no-metadata-still-prompts ()
  "Test shell-command tool without metadata still prompts for permission."
  (let ((result nil)
        (error nil)
        (callback-called nil)
        (prompt-called nil))

    ;; Mock the permission prompt to track if it's called and return yes
    (cl-letf (((symbol-function 'y-or-n-p)
               (lambda (prompt)
                 (setq prompt-called t)
                 t)))

        ;; Execute a command without any metadata
        (greger-stdlib--shell-command
         "echo no metadata"
         (lambda (output err)
           (setq result output error err callback-called t))
         "."  ; working directory
         nil)  ; no metadata

        ;; Wait for async operation to complete
        (let ((timeout 0))
          (while (and (not callback-called) (< timeout 50))  ; 5 second timeout
            (sit-for 0.1)
            (setq timeout (1+ timeout))))

        ;; Verify the results
        (should callback-called)
        (should (null error))
        (should (stringp result))
        (should (string-match "Command executed successfully" result))
        (should (string-match "no metadata" result))
        ;; Permission prompt SHOULD have been called since no metadata provided
        (should prompt-called))))

(ert-deftest greger-test-count-paren-balance ()
  "Test the paren balance counting function."
  ;; Test balanced expressions
  (should (= 0 (greger-stdlib--count-paren-balance "()")))
  (should (= 0 (greger-stdlib--count-paren-balance "(foo)")))
  (should (= 0 (greger-stdlib--count-paren-balance "(foo (bar) baz)")))
  (should (= 0 (greger-stdlib--count-paren-balance "(let ((x 1) (y 2)) (+ x y))")))

  ;; Test unbalanced expressions
  (should (= 1 (greger-stdlib--count-paren-balance "(")))
  (should (= -1 (greger-stdlib--count-paren-balance ")")))
  (should (= -1 (greger-stdlib--count-paren-balance "(foo))")))  ; 1 open, 2 close = -1
  (should (= 1 (greger-stdlib--count-paren-balance "((foo)")))   ; 2 open, 1 close = 1
  (should (= 3 (greger-stdlib--count-paren-balance "(((")))
  (should (= -3 (greger-stdlib--count-paren-balance ")))")))

  ;; Test with strings (parens in strings should be ignored)
  (should (= 0 (greger-stdlib--count-paren-balance "\"()\"")))
  (should (= 0 (greger-stdlib--count-paren-balance "\"(((\"")))
  (should (= 0 (greger-stdlib--count-paren-balance "(message \"hello (world)\")")))  ; Fixed: this should be 0

  ;; Test with comments (parens in comments should be ignored)
  (should (= 0 (greger-stdlib--count-paren-balance "; (((")))
  (should (= 0 (greger-stdlib--count-paren-balance ";; This has (parens) in comment")))
  (should (= 0 (greger-stdlib--count-paren-balance "(foo) ; comment with (parens)")))  ; Fixed: this should be 0

  ;; Test mixed content
  (should (= 0 (greger-stdlib--count-paren-balance "(foo \"string with (parens)\" bar) ; comment (with parens)")))
  (should (= 1 (greger-stdlib--count-paren-balance "((foo \"string with )\" bar) ; comment (with parens)")))

  ;; Test empty content
  (should (= 0 (greger-stdlib--count-paren-balance "")))
  (should (= 0 (greger-stdlib--count-paren-balance "   ")))
  (should (= 0 (greger-stdlib--count-paren-balance "foo bar baz"))))

(ert-deftest greger-test-str-replace-paren-balance-check ()
  "Test str-replace paren balance checking for .el files."
  (let ((test-file (make-temp-file "test" nil ".el"))
        (original-content "(defun foo () (+ 1 2))")
        (new-content-balanced "(defun bar () (+ 3 4))")
        (new-content-unbalanced "(defun bar () (+ 3 4"))

    (unwind-protect
        (progn
          ;; Write test content to file
          (with-temp-file test-file
            (insert original-content))

          ;; Mock git operations to avoid actual git commits
          (cl-letf (((symbol-function 'greger-stdlib--git-stage-and-commit)
                     (lambda (files commit-message buffer) "Mocked git result")))

            ;; Test successful replacement with balanced parens
            (should (stringp (greger-stdlib--str-replace
                             test-file
                             original-content
                             new-content-balanced
                             "Test commit")))

            ;; Reset file content
            (with-temp-file test-file
              (insert original-content))

            ;; Test failed replacement with unbalanced parens
            (should-error (greger-stdlib--str-replace
                          test-file
                          original-content
                          new-content-unbalanced
                          "Test commit")
                         :type 'error)))

      ;; Clean up
      (when (file-exists-p test-file)
        (delete-file test-file)))))

(ert-deftest greger-test-str-replace-non-el-files-skip-paren-check ()
  "Test that str-replace skips paren checking for non-.el files."
  (let ((test-file (make-temp-file "test" nil ".txt"))
        (original-content "Some text with (unbalanced parens")
        (new-content "Some other text with (((more unbalanced"))

    (unwind-protect
        (progn
          ;; Write test content to file
          (with-temp-file test-file
            (insert original-content))

          ;; Mock git operations to avoid actual git commits
          (cl-letf (((symbol-function 'greger-stdlib--git-stage-and-commit)
                     (lambda (files commit-message buffer) "Mocked git result")))

            ;; Should succeed even with unbalanced parens since it's not a .el file
            (should (stringp (greger-stdlib--str-replace
                             test-file
                             original-content
                             new-content
                             "Test commit")))))

      ;; Clean up
      (when (file-exists-p test-file)
        (delete-file test-file)))))

;;; greger-test-stdlib.el ends here
