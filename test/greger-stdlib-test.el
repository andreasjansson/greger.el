;;; greger-stdlib-test.el --- Tests for greger stdlib tools -*- lexical-binding: t -*-

;;; Commentary:
;; Tests for the tools defined in greger-stdlib.el

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'greger-stdlib)
(require 'greger-web)

;; Helper function for creating controlled test directories
(defun greger-stdlib-test--make-controlled-temp-dir (prefix)
  "Create a temporary directory with controlled parent permissions.
Creates a parent directory, sets its permissions to 0700, then creates
the actual test directory inside it. Returns the test directory path.
This ensures the '..' entry has predictable permissions in tests."
  (let* ((parent-dir (make-temp-file (concat prefix "-parent") t))
         (test-dir (expand-file-name "testdir" parent-dir)))
    ;; Set parent directory permissions to 0700
    (set-file-modes parent-dir #o700)
    ;; Create the actual test directory
    (make-directory test-dir)
    ;; Set test directory permissions to 0700
    (set-file-modes test-dir #o700)
    test-dir))

(ert-deftest greger-stdlib-test-read-webpage-valid-url ()
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

(ert-deftest greger-stdlib-test-read-webpage-invalid-url ()
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

(ert-deftest greger-stdlib-test-read-webpage-network-error ()
  "Test reading a webpage when network error occurs."
  ;; Mock the web download function to simulate network error
  (cl-letf (((symbol-function 'greger-web-download-page)
             (lambda (url extract-text use-highest-readability)
               (error "Network timeout"))))

    (let ((result (greger-stdlib--read-webpage "https://pub-b88c9764a4fc46baa90b9e8e1544f59e.r2.dev/hello.html")))
      (should (stringp result))
      (should (string-match-p "Network timeout" result)))))

(ert-deftest greger-stdlib-test-web-url-validation ()
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

(ert-deftest greger-stdlib-test-shell-command-simple ()
  "Test shell-command tool with a simple command."
  (let ((result "")
        (error nil)
        (callback-called nil))

    ;; Mock the permission prompt to always return yes
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (prompt) t)))

      ;; Execute a simple echo command
      (greger-stdlib--shell-command
       "echo hello world"
       (lambda (output err)
         (setq error err callback-called t))
       "."  ; working-directory
       nil  ; timeout
       nil  ; enable-environment
       (lambda (chunk)  ; streaming-callback
         (setq result (concat result chunk)))
       nil) ; metadata

      ;; Wait for async operation to complete
      (let ((timeout 0))
        (while (and (not callback-called) (< timeout 50))  ; 5 second timeout
          (sit-for 0.1)
          (setq timeout (1+ timeout))))

      ;; Verify the results
      (should callback-called)
      (should (null error))
      (should (stringp result))
      (should (string-match "hello world" result)))))

(ert-deftest greger-stdlib-test-shell-command-with-pipe ()
  "Test shell-command tool with a command containing a pipe."
  (let ((result "")
        (error nil)
        (callback-called nil))

    ;; Mock the permission prompt to always return yes
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (prompt) t)))

      ;; Execute a command with a pipe
      (greger-stdlib--shell-command
       "echo 'apple\nbanana\ncherry' | grep 'an'"
       (lambda (output err)
         (setq error err callback-called t))
       "."  ; working-directory
       nil  ; timeout
       nil  ; enable-environment
       (lambda (chunk)  ; streaming-callback
         (setq result (concat result chunk)))
       nil) ; metadata

      ;; Wait for async operation to complete
      (let ((timeout 0))
        (while (and (not callback-called) (< timeout 50))  ; 5 second timeout
          (sit-for 0.1)
          (setq timeout (1+ timeout))))

      ;; Verify the results
      (should callback-called)
      (should (null error))
      (should (stringp result))
      (should (string-match "banana" result)))))

(ert-deftest greger-stdlib-test-shell-command-permission-denied ()
  "Test shell-command tool when user denies permission."
  ;; Mock the permission prompt to always return no
  (cl-letf (((symbol-function 'y-or-n-p) (lambda (prompt) nil)))
    ;; Should signal an error when permission is denied
    (should-error
     (greger-stdlib--shell-command
      "echo test"
      (lambda (output err) nil)  ; callback shouldn't be called
      "."  ; working-directory
      nil  ; timeout
      nil  ; enable-environment
      nil  ; streaming-callback
      '(:allow-all-shell-commands nil)) ; metadata
     :type 'error)))

(ert-deftest greger-stdlib-test-shell-command-command-failure ()
  "Test shell-command tool when command fails."
  (let ((result "")
        (error nil)
        (callback-called nil))

    ;; Mock the permission prompt to always return yes
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (prompt) t)))

      ;; Execute a command that should fail
      (greger-stdlib--shell-command
       "false"  ; Command that always exits with code 1
       (lambda (output err)
         (setq error err callback-called t))
       "."  ; working-directory
       nil  ; timeout
       nil  ; enable-environment
       (lambda (chunk)  ; streaming-callback
         (setq result (concat result chunk)))
       nil) ; metadata

      ;; Wait for async operation to complete
      (let ((timeout 0))
        (while (and (not callback-called) (< timeout 50))  ; 5 second timeout
          (sit-for 0.1)
          (setq timeout (1+ timeout))))

      ;; Verify the results
      (should callback-called)
      (should (string= result ""))  ; false command produces no output
      (should (stringp error))
      (should (string-match "failed with exit code" error)))))

(ert-deftest greger-stdlib-test-shell-command-safe-commands ()
  "Test shell-command tool with safe-shell-commands metadata to skip permission prompt."
  (let ((result "")
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
           (setq error err callback-called t))
         "."  ; working directory
         nil  ; timeout (use default)
         nil  ; enable-environment
         (lambda (chunk)  ; streaming-callback
           (setq result (concat result chunk)))
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
        (should (string-match "safe command" result))
        ;; Most importantly: permission prompt should NOT have been called
        (should (null prompt-called))))))

(ert-deftest greger-stdlib-test-shell-command-unsafe-commands-with-metadata ()
  "Test shell-command tool with metadata but command not in safe list still prompts."
  (let ((result "")
        (error nil)
        (callback-called nil)
        (prompt-called nil))

    ;; Mock the permission prompt to track if it's called and return yes
    (cl-letf (((symbol-function 'y-or-n-p)
               (lambda (prompt)
                 (setq prompt-called t)
                 t)))

      ;; Create metadata with safe shell commands
      (let ((metadata '(:safe-shell-commands ("echo safe command" "pwd") :allow-all-shell-commands nil)))

        ;; Execute a command that's NOT in the safe list
        (greger-stdlib--shell-command
         "echo unsafe command"
         (lambda (output err)
           (setq error err callback-called t))
         "."  ; working directory
         nil  ; timeout (use default)
         nil  ; enable-environment
         (lambda (chunk)  ; streaming-callback
           (setq result (concat result chunk)))
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
        (should (string-match "unsafe command" result))
        ;; Permission prompt SHOULD have been called since command not in safe list
        (should prompt-called)))))
(ert-deftest greger-stdlib-test-shell-command-with-timeout ()
  "Test shell-command tool with timeout parameter."
  (let ((result "")
        (error nil)
        (callback-called nil))

    ;; Mock the permission prompt to always return yes
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (prompt) t)))

      ;; Execute a command that should complete within timeout
      (greger-stdlib--shell-command
       "echo timeout test"
       (lambda (output err)
         (setq error err callback-called t))
       "."  ; working directory
       10   ; timeout 10 seconds
       nil  ; enable-environment
       (lambda (chunk)  ; streaming-callback
         (setq result (concat result chunk)))
       nil) ; metadata

      ;; Wait for async operation to complete
      (let ((timeout 0))
        (while (and (not callback-called) (< timeout 50))  ; 5 second timeout
          (sit-for 0.1)
          (setq timeout (1+ timeout))))

      ;; Verify the results
      (should callback-called)
      (should (null error))
      (should (stringp result))
      (should (string-match "timeout test" result)))))

(ert-deftest greger-stdlib-test-shell-command-timeout-exceeded ()
  "Test shell-command tool when timeout is exceeded."
  (let ((result "")
        (error nil)
        (callback-called nil))

    ;; Mock the permission prompt to always return yes
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (prompt) t)))

      ;; Execute a command that should timeout (sleep longer than timeout)
      (greger-stdlib--shell-command
       "sleep 3"
       (lambda (output err)
         (setq error err callback-called t))
       "."  ; working directory
       1    ; timeout 1 second
       nil  ; enable-environment
       (lambda (chunk)  ; streaming-callback
         (setq result (concat result chunk)))
       nil) ; metadata

      ;; Wait for timeout to occur
      (let ((timeout 0))
        (while (and (not callback-called) (< timeout 30))  ; 3 second timeout
          (sit-for 0.1)
          (setq timeout (1+ timeout))))

      ;; Verify the timeout occurred
      (should callback-called)
      (should error)
      (should (string= result ""))  ; sleep command produces no output
      (should (string-match "timed out after 1 seconds" error)))))

(ert-deftest greger-stdlib-test-shell-command-pager-environment ()
  "Test shell-command tool sets PAGER=cat environment variable."
  (let ((result "")
        (error nil)
        (callback-called nil))

    ;; Mock the permission prompt to always return yes
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (prompt) t)))

      ;; Execute a command that checks the PAGER environment variable
      (greger-stdlib--shell-command
       "echo \"PAGER is: $PAGER\""
       (lambda (output err)
         (setq error err callback-called t))
       "."  ; working directory
       nil  ; timeout (use default)
       nil  ; enable-environment
       (lambda (chunk)  ; streaming-callback
         (setq result (concat result chunk)))
       nil) ; metadata

      ;; Wait for async operation to complete
      (let ((timeout 0))
        (while (and (not callback-called) (< timeout 50))  ; 5 second timeout
          (sit-for 0.1)
          (setq timeout (1+ timeout))))

      ;; Verify the results
      (should callback-called)
      (should (null error))
      (should (stringp result))
      (should (string-match "PAGER is: cat" result)))))

(ert-deftest greger-stdlib-test-shell-command-default-timeout ()
  "Test shell-command tool uses default timeout of 600 seconds when not specified."
  (let ((result "")
        (error nil)
        (callback-called nil))

    ;; Mock the permission prompt to always return yes
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (prompt) t)))

      ;; Execute a quick command without specifying timeout
      (greger-stdlib--shell-command
       "echo default timeout test"
       (lambda (output err)
         (setq error err callback-called t))
       "."  ; working directory
       nil  ; timeout (should default to 600)
       nil  ; enable-environment
       (lambda (chunk)  ; streaming-callback
         (setq result (concat result chunk)))
       nil) ; metadata

      ;; Wait for async operation to complete
      (let ((timeout 0))
        (while (and (not callback-called) (< timeout 50))  ; 5 second timeout
          (sit-for 0.1)
          (setq timeout (1+ timeout))))

      ;; Verify the results - command should complete successfully
      (should callback-called)
      (should (null error))
      (should (stringp result))
      (should (string-match "default timeout test" result)))))

(ert-deftest greger-stdlib-test-shell-command-environment-access ()
  "Test shell-command with enable-environment loads bash initialization files."
  (let ((result-without-env "")
        (result-with-env "")
        (error nil)
        (callback-called nil))

    ;; Mock the permission prompt to always return yes
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (prompt) t)))

      ;; Test WITHOUT enable-environment - PS1 should be unset or minimal
      (greger-stdlib--shell-command
       "echo \"PS1 is: [$PS1]\""
       (lambda (output err)
         (setq error err callback-called t))
       "."  ; working directory
       nil  ; timeout
       nil  ; enable-environment = nil
       (lambda (chunk)  ; streaming-callback
         (setq result-without-env (concat result-without-env chunk)))
       nil) ; metadata

      ;; Wait for async operation to complete
      (let ((timeout-count 0))
        (while (and (not callback-called) (< timeout-count 100))
          (sleep-for 0.1)
          (setq timeout-count (1+ timeout-count))))

      (should callback-called)
      (should (null error))
      (should (stringp result-without-env))

      ;; Reset for next test
      (setq callback-called nil error nil)

      ;; Test WITH enable-environment - PS1 should be set from .bashrc
      (greger-stdlib--shell-command
       "echo \"PS1 is: [$PS1]\""
       (lambda (output err)
         (setq error err callback-called t))
       "."  ; working directory
       nil  ; timeout
       t    ; enable-environment = t
       (lambda (chunk)  ; streaming-callback
         (setq result-with-env (concat result-with-env chunk)))
       nil) ; metadata

      ;; Wait for async operation to complete
      (let ((timeout-count 0))
        (while (and (not callback-called) (< timeout-count 100))
          (sleep-for 0.1)
          (setq timeout-count (1+ timeout-count))))

      (should callback-called)
      (should (null error))
      (should (stringp result-with-env))

      ;; The key test: verify specific PS1 behavior
      ;; Non-interactive (without env) should have empty PS1
      (should (string-match "PS1 is: \\[\\]" result-without-env))

      ;; Interactive (with env) should have PS1 set to some value from .bashrc
      (should-not (string-match "PS1 is: \\[\\]" result-with-env))
      (should (string-match "PS1 is: \\[.+\\]" result-with-env)))))

(ert-deftest greger-stdlib-test-count-paren-balance ()
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

(ert-deftest greger-stdlib-test-str-replace-paren-balance-check ()
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

(ert-deftest greger-stdlib-test-str-replace-non-el-files-skip-paren-check ()
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

(ert-deftest greger-stdlib-test-str-replace-single-occurrence ()
  "Test str-replace with replace-all=false (default behavior)."
  (let ((test-file (make-temp-file "greger-str-replace-single"))
        (original-content "Hello world. Goodbye everyone.")
        (target-string "Hello")
        (replacement "Hi"))
    (unwind-protect
        (progn
          ;; Create test file with original content
          (with-temp-file test-file
            (insert original-content))

          ;; Mock git operations
          (cl-letf (((symbol-function 'greger-stdlib--git-stage-and-commit)
                     (lambda (files commit-message buffer) "Mocked git result")))

            ;; Test replacing first occurrence only (default behavior)
            (let ((result (greger-stdlib--str-replace
                           test-file
                           target-string
                           replacement
                           "Replace first Hello")))
              (should (stringp result))
              (should (string-match "Successfully replaced content" result))
              (should-not (string-match "made.*replacements" result)) ; No count for single replacement

              ;; Verify only first occurrence was replaced
              (with-temp-buffer
                (insert-file-contents test-file)
                (should (string= (buffer-string) "Hi world. Goodbye everyone."))))))

      ;; Clean up
      (when (file-exists-p test-file)
        (delete-file test-file)))))

(ert-deftest greger-stdlib-test-str-replace-all-occurrences ()
  "Test str-replace with replace-all=true."
  (let ((test-file (make-temp-file "greger-str-replace-all"))
        (original-content "Hello world. Hello again. Hello everyone!")
        (target-string "Hello")
        (replacement "Hi"))
    (unwind-protect
        (progn
          ;; Create test file with original content
          (with-temp-file test-file
            (insert original-content))

          ;; Mock git operations
          (cl-letf (((symbol-function 'greger-stdlib--git-stage-and-commit)
                     (lambda (files commit-message buffer) "Mocked git result")))

            ;; Test replacing all occurrences
            (let ((result (greger-stdlib--str-replace
                           test-file
                           target-string
                           replacement
                           "Replace all Hellos"
                           t))) ; replace-all = true
              (should (stringp result))
              (should (string-match "Successfully replaced content" result))
              (should (string-match "made 3 replacements" result)) ; Should show count

              ;; Verify all occurrences were replaced
              (with-temp-buffer
                (insert-file-contents test-file)
                (should (string= (buffer-string) "Hi world. Hi again. Hi everyone!"))))))

      ;; Clean up
      (when (file-exists-p test-file)
        (delete-file test-file)))))

(ert-deftest greger-stdlib-test-str-replace-all-no-matches ()
  "Test str-replace with replace-all=true when no matches found."
  (let ((test-file (make-temp-file "greger-str-replace-no-match"))
        (original-content "Hello world")
        (target-string "Goodbye")
        (replacement "Hi"))
    (unwind-protect
        (progn
          ;; Create test file with original content
          (with-temp-file test-file
            (insert original-content))

          ;; Mock git operations
          (cl-letf (((symbol-function 'greger-stdlib--git-stage-and-commit)
                     (lambda (files commit-message buffer) "Mocked git result")))

            ;; Should error when no matches found
            (should-error (greger-stdlib--str-replace
                           test-file
                           target-string
                           replacement
                           "Should fail - no matches"
                           t)))) ; replace-all = true

      ;; Clean up
      (when (file-exists-p test-file)
        (delete-file test-file)))))

(ert-deftest greger-stdlib-test-str-replace-all-single-match ()
  "Test str-replace with replace-all=true when only one match exists."
  (let ((test-file (make-temp-file "greger-str-replace-single-match"))
        (original-content "Hello world")
        (target-string "Hello")
        (replacement "Hi"))
    (unwind-protect
        (progn
          ;; Create test file with original content
          (with-temp-file test-file
            (insert original-content))

          ;; Mock git operations
          (cl-letf (((symbol-function 'greger-stdlib--git-stage-and-commit)
                     (lambda (files commit-message buffer) "Mocked git result")))

            ;; Test replacing single occurrence with replace-all=true
            (let ((result (greger-stdlib--str-replace
                           test-file
                           target-string
                           replacement
                           "Replace single Hello with replace-all"
                           t))) ; replace-all = true
              (should (stringp result))
              (should (string-match "Successfully replaced content" result))
              (should-not (string-match "made.*replacements" result)) ; No count for single replacement

              ;; Verify content was replaced
              (with-temp-buffer
                (insert-file-contents test-file)
                (should (string= (buffer-string) "Hi world"))))))

      ;; Clean up
      (when (file-exists-p test-file)
        (delete-file test-file)))))

(ert-deftest greger-stdlib-test-str-replace-multiline-content ()
  "Test str-replace with multi-line content and replace-all."
  (let ((test-file (make-temp-file "greger-str-replace-multiline"))
        (original-content "Line 1\nReplace me\nLine 3\nReplace me\nLine 5")
        (target-string "Replace me")
        (replacement "Replaced!"))
    (unwind-protect
        (progn
          ;; Create test file with original content
          (with-temp-file test-file
            (insert original-content))

          ;; Mock git operations
          (cl-letf (((symbol-function 'greger-stdlib--git-stage-and-commit)
                     (lambda (files commit-message buffer) "Mocked git result")))

            ;; Test replacing all occurrences of multi-line pattern
            (let ((result (greger-stdlib--str-replace
                           test-file
                           target-string
                           replacement
                           "Replace all multi-line patterns"
                           t))) ; replace-all = true
              (should (stringp result))
              (should (string-match "Successfully replaced content" result))
              (should (string-match "made 2 replacements" result))

              ;; Verify all occurrences were replaced
              (with-temp-buffer
                (insert-file-contents test-file)
                (should (string= (buffer-string) "Line 1\nReplaced!\nLine 3\nReplaced!\nLine 5"))))))

      ;; Clean up
      (when (file-exists-p test-file)
        (delete-file test-file)))))

(ert-deftest greger-stdlib-test-str-replace-case-sensitive ()
  "Test that str-replace is case-sensitive with replace-all."
  (let ((test-file (make-temp-file "greger-str-replace-case"))
        (original-content "Hello hello HELLO")
        (target-string "hello")
        (replacement "hi"))
    (unwind-protect
        (progn
          ;; Create test file with original content
          (with-temp-file test-file
            (insert original-content))

          ;; Mock git operations
          (cl-letf (((symbol-function 'greger-stdlib--git-stage-and-commit)
                     (lambda (files commit-message buffer) "Mocked git result")))

            ;; Test case-sensitive replacement
            (let ((result (greger-stdlib--str-replace
                           test-file
                           target-string
                           replacement
                           "Case-sensitive replacement"
                           t))) ; replace-all = true
              (should (stringp result))
              (should (string-match "Successfully replaced content" result))
              (should-not (string-match "made.*replacements" result)) ; Only one match

              ;; Verify only lowercase "hello" was replaced
              (with-temp-buffer
                (insert-file-contents test-file)
                (should (string= (buffer-string) "Hello hi HELLO"))))))

      ;; Clean up
      (when (file-exists-p test-file)
        (delete-file test-file)))))

(ert-deftest greger-stdlib-test-str-replace-multiple-occurrences-error ()
  "Test str-replace error when multiple occurrences exist and replace-all is false."
  (let ((test-file (make-temp-file "greger-str-replace-multiple-error"))
        (original-content "Hello world. Hello again. Hello everyone!")
        (target-string "Hello")
        (replacement "Hi"))
    (unwind-protect
        (progn
          ;; Create test file with multiple occurrences
          (with-temp-file test-file
            (insert original-content))

          ;; Mock git operations
          (cl-letf (((symbol-function 'greger-stdlib--git-stage-and-commit)
                     (lambda (files commit-message buffer) "Mocked git result")))

            ;; Should error when multiple occurrences exist and replace-all is false (default)
            (should-error (greger-stdlib--str-replace
                           test-file
                           target-string
                           replacement
                           "Should fail with multiple occurrences")
                          :type 'error)

            ;; Verify the error message mentions the occurrence count
            (condition-case err
                (greger-stdlib--str-replace
                 test-file
                 target-string
                 replacement
                 "Should fail with multiple occurrences")
              (error
               (let ((error-message (error-message-string err)))
                 (should (string-match "Found 3 occurrences" error-message))
                 (should (string-match "replace-all=t" error-message)))))))

      ;; Clean up
      (when (file-exists-p test-file)
        (delete-file test-file)))))

(ert-deftest greger-stdlib-test-str-replace-multiple-occurrences-success-with-all ()
  "Test str-replace succeeds with multiple occurrences when replace-all=true."
  (let ((test-file (make-temp-file "greger-str-replace-multiple-success"))
        (original-content "Hello world. Hello again. Hello everyone!")
        (target-string "Hello")
        (replacement "Hi"))
    (unwind-protect
        (progn
          ;; Create test file with multiple occurrences
          (with-temp-file test-file
            (insert original-content))

          ;; Mock git operations
          (cl-letf (((symbol-function 'greger-stdlib--git-stage-and-commit)
                     (lambda (files commit-message buffer) "Mocked git result")))

            ;; Should succeed when replace-all=true even with multiple occurrences
            (let ((result (greger-stdlib--str-replace
                           test-file
                           target-string
                           replacement
                           "Replace all occurrences"
                           t))) ; replace-all = true
              (should (stringp result))
              (should (string-match "Successfully replaced content" result))
              (should (string-match "made 3 replacements" result))

              ;; Verify all occurrences were replaced
              (with-temp-buffer
                (insert-file-contents test-file)
                (should (string= (buffer-string) "Hi world. Hi again. Hi everyone!"))))))

      ;; Clean up
      (when (file-exists-p test-file)
        (delete-file test-file)))))

(ert-deftest greger-stdlib-test-delete-files-basic ()
  "Test basic delete-files functionality."
  (let ((test-file1 (make-temp-file "test1" nil ".txt"))
        (test-file2 (make-temp-file "test2" nil ".txt"))
        (test-content "Test content"))

    (unwind-protect
        (progn
          ;; Write test content to files
          (with-temp-file test-file1
            (insert test-content))
          (with-temp-file test-file2
            (insert test-content))

          ;; Verify files exist
          (should (file-exists-p test-file1))
          (should (file-exists-p test-file2))

          ;; Mock git operations to avoid actual git commits
          (cl-letf (((symbol-function 'greger-stdlib--git-stage-and-commit)
                     (lambda (files commit-message buffer) "Mocked git result")))

            ;; Test successful deletion with array input
            (let ((result (greger-stdlib--delete-files
                           (vector test-file1 test-file2)
                           "Test commit message")))
              (should (stringp result))
              (should (string-match "Successfully deleted 2 file" result))
              (should (string-match (file-name-nondirectory test-file1) result))
              (should (string-match (file-name-nondirectory test-file2) result)))

            ;; Verify files are deleted
            (should-not (file-exists-p test-file1))
            (should-not (file-exists-p test-file2))))

      ;; Clean up any remaining files
      (when (file-exists-p test-file1)
        (delete-file test-file1))
      (when (file-exists-p test-file2)
        (delete-file test-file2)))))

(ert-deftest greger-stdlib-test-delete-files-list-input ()
  "Test delete-files with list input instead of vector."
  (let ((test-file (make-temp-file "test" nil ".txt"))
        (test-content "Test content"))

    (unwind-protect
        (progn
          ;; Write test content to file
          (with-temp-file test-file
            (insert test-content))

          ;; Verify file exists
          (should (file-exists-p test-file))

          ;; Mock git operations to avoid actual git commits
          (cl-letf (((symbol-function 'greger-stdlib--git-stage-and-commit)
                     (lambda (files commit-message buffer) "Mocked git result")))

            ;; Test successful deletion with vector input
            (let ((result (greger-stdlib--delete-files
                           (vector test-file)
                           "Test commit message")))
              (should (stringp result))
              (should (string-match "Successfully deleted 1 file" result))
              (should (string-match (file-name-nondirectory test-file) result)))

            ;; Verify file is deleted
            (should-not (file-exists-p test-file))))

      ;; Clean up any remaining files
      (when (file-exists-p test-file)
        (delete-file test-file)))))

(ert-deftest greger-stdlib-test-delete-files-nonexistent-file ()
  "Test delete-files with non-existent file."
  (let ((nonexistent-file "/tmp/does-not-exist.txt"))

    ;; Ensure file doesn't exist
    (should-not (file-exists-p nonexistent-file))

    ;; Mock git operations to avoid actual git commits
    (cl-letf (((symbol-function 'greger-stdlib--git-stage-and-commit)
               (lambda (files commit-message buffer) "Mocked git result")))

      ;; Should error when trying to delete non-existent file
      (should-error (greger-stdlib--delete-files
                     (vector nonexistent-file)
                     "Test commit message")
                    :type 'error))))

(ert-deftest greger-stdlib-test-delete-files-directory ()
  "Test delete-files with directory path (should fail)."
  (let ((test-dir (make-temp-file "test-dir" t)))

    (unwind-protect
        (progn
          ;; Verify directory exists
          (should (file-exists-p test-dir))
          (should (file-directory-p test-dir))

          ;; Mock git operations to avoid actual git commits
          (cl-letf (((symbol-function 'greger-stdlib--git-stage-and-commit)
                     (lambda (files commit-message buffer) "Mocked git result")))

            ;; Should error when trying to delete directory
            (should-error (greger-stdlib--delete-files
                           (vector test-dir)
                           "Test commit message")
                          :type 'error)))

      ;; Clean up
      (when (file-exists-p test-dir)
        (delete-directory test-dir)))))

(ert-deftest greger-stdlib-test-delete-files-invalid-input ()
  "Test delete-files with invalid input types."
  ;; Mock git operations to avoid actual git commits
  (cl-letf (((symbol-function 'greger-stdlib--git-stage-and-commit)
             (lambda (files commit-message buffer) "Mocked git result")))

    ;; Test with non-string file paths
    (should-error (greger-stdlib--delete-files
                   (vector 123)
                   "Test commit message")
                  :type 'error)

    ;; Test with non-vector paths
    (should-error (greger-stdlib--delete-files
                   "not-a-vector"
                   "Test commit message")
                  :type 'error)

    ;; Test with non-string commit message
    (should-error (greger-stdlib--delete-files
                   (vector "/tmp/test.txt")
                   123)
                  :type 'error)))

(ert-deftest greger-stdlib-test-delete-files-git-tracking ()
  "Test delete-files git tracking behavior."
  (let ((test-file (make-temp-file "test" nil ".txt"))
        (test-content "Test content")
        (git-tracked-called nil)
        (git-stage-called nil)
        (staged-files nil))

    (unwind-protect
        (progn
          ;; Write test content to file
          (with-temp-file test-file
            (insert test-content))

          ;; Mock git functions
          (cl-letf (((symbol-function 'greger-stdlib--find-git-repo-root)
                     (lambda (dir) "/fake/repo/root"))
                    ((symbol-function 'greger-stdlib--is-file-tracked-by-git)
                     (lambda (file repo-root)
                       (setq git-tracked-called t)
                       t))  ; Simulate file is tracked
                    ((symbol-function 'greger-stdlib--git-stage-and-commit)
                     (lambda (files commit-message buffer)
                       (setq git-stage-called t)
                       (setq staged-files files)
                       "Mocked git result")))

            ;; Test deletion of git-tracked file
            (let ((result (greger-stdlib--delete-files
                           (vector test-file)
                           "Delete test file")))
              (should (stringp result))
              (should (string-match "Successfully deleted 1 file" result))
              (should git-tracked-called)
              (should git-stage-called)
              (should (member test-file staged-files))
              (should-not (file-exists-p test-file)))))

      ;; Clean up any remaining files
      (when (file-exists-p test-file)
        (delete-file test-file)))))

(ert-deftest greger-stdlib-test-delete-files-not-git-tracked ()
  "Test delete-files with files not tracked by git."
  (let ((test-file (make-temp-file "test" nil ".txt"))
        (test-content "Test content")
        (git-stage-called nil))

    (unwind-protect
        (progn
          ;; Write test content to file
          (with-temp-file test-file
            (insert test-content))

          ;; Mock git functions
          (cl-letf (((symbol-function 'greger-stdlib--find-git-repo-root)
                     (lambda (dir) nil))  ; No git repo found
                    ((symbol-function 'greger-stdlib--git-stage-and-commit)
                     (lambda (files commit-message buffer)
                       (setq git-stage-called t)
                       "Should not be called")))

            ;; Test deletion of non-git-tracked file
            (let ((result (greger-stdlib--delete-files
                           (vector test-file)
                           "Delete test file")))
              (should (stringp result))
              (should (string-match "Successfully deleted 1 file" result))
              (should (string-match "No files were tracked by git" result))
              (should-not git-stage-called)
              (should-not (file-exists-p test-file)))))

      ;; Clean up any remaining files
      (when (file-exists-p test-file)
        (delete-file test-file)))))

(ert-deftest greger-stdlib-test-read-file-basic ()
  "Test basic file reading functionality."
  (let ((test-file (make-temp-file "greger-test-read-file")))
    (unwind-protect
        (progn
          ;; Write test content to file
          (with-temp-buffer
            (insert "Line 1
Line 2
Line 3
")
            (write-file test-file))

          ;; Test basic reading without line numbers
          (let ((result (greger-stdlib--read-file test-file nil nil nil))
                (expected "Line 1
Line 2
Line 3"))
            (should (stringp result))
            (should (string= result expected)))

          ;; Test reading with line numbers
          (let ((result (greger-stdlib--read-file test-file t nil nil))
                (expected "1: Line 1
2: Line 2
3: Line 3"))
            (should (stringp result))
            (should (string= result expected)))

          ;; Test reading with start and end lines
          (let ((result (greger-stdlib--read-file test-file nil 2 2))
                (expected "Line 2"))
            (should (stringp result))
            (should (string= result expected)))

          ;; Test reading with line numbers and range
          (let ((result (greger-stdlib--read-file test-file t 1 2))
                (expected "1: Line 1
2: Line 2"))
            (should (stringp result))
            (should (string= result expected))))

      ;; Clean up
      (when (file-exists-p test-file)
        (delete-file test-file)))))

(ert-deftest greger-stdlib-test-read-file-line-number-formatting ()
  "Test that line numbers are properly formatted with padding."
  (let ((test-file (make-temp-file "greger-test-read-file-padding")))
    (unwind-protect
        (progn
          ;; Write test content with 12 lines to test padding
          (with-temp-buffer
            (dotimes (i 12)
              (insert (format "Line %d\n" (1+ i))))
            (write-file test-file))

          ;; Test reading lines 8-12 with line numbers - should have 2-digit padding
          (let ((result (greger-stdlib--read-file test-file t 8 12))
                (expected " 8: Line 8
 9: Line 9
10: Line 10
11: Line 11
12: Line 12"))
            (should (stringp result))
            (should (string= result expected)))

          ;; Test reading lines 1-3 with line numbers - padding based on range max (3)
          (let ((result (greger-stdlib--read-file test-file t 1 3))
                (expected "1: Line 1
2: Line 2
3: Line 3"))
            (should (stringp result))
            (should (string= result expected)))

          ;; Test reading lines 10-12 with line numbers - should have 2-digit padding
          (let ((result (greger-stdlib--read-file test-file t 10 12))
                (expected "10: Line 10
11: Line 11
12: Line 12"))
            (should (stringp result))
            (should (string= result expected))))

      ;; Clean up
      (when (file-exists-p test-file)
        (delete-file test-file)))))

(ert-deftest greger-stdlib-test-read-file-error-cases ()
  "Test error handling in read-file function."
  ;; Test non-existent file
  (should-error (greger-stdlib--read-file "/path/that/does/not/exist" nil nil nil))

  ;; Test invalid path type
  (should-error (greger-stdlib--read-file 123 nil nil nil))

  ;; Test invalid start-line type
  (should-error (greger-stdlib--read-file "test-file" nil "not-a-number" nil))

  ;; Test invalid end-line type
  (should-error (greger-stdlib--read-file "test-file" nil 1 "not-a-number"))

  ;; Test invalid start-line value
  (should-error (greger-stdlib--read-file "test-file" nil 0 nil))

  ;; Test invalid end-line value
  (should-error (greger-stdlib--read-file "test-file" nil 1 0))

  ;; Test start-line > end-line
  (should-error (greger-stdlib--read-file "test-file" nil 5 3)))

(ert-deftest greger-stdlib-test-list-directory-basic ()
  "Test basic list-directory functionality with detailed output."
  (let ((test-dir (greger-stdlib-test--make-controlled-temp-dir "greger-test-dir"))
        (parent-dir nil))
    (unwind-protect
        (progn
          (setq parent-dir (file-name-directory (directory-file-name test-dir)))
          ;; Create a test file
          (let ((test-file (expand-file-name "test.txt" test-dir)))
            (with-temp-file test-file
              (insert "content"))

            ;; Test basic listing
            (let ((result (greger-stdlib--list-directory test-dir))
                  (expected (format "%s:
drwx------  (dir)  .
drwx------  (dir)  ..
-rw-r--r--         7  test.txt" (file-name-as-directory test-dir))))
              (should (stringp result))
              (should (string= expected result)))))

      ;; Clean up
      (when (and parent-dir (file-exists-p parent-dir))
        (delete-directory parent-dir t)))))

(ert-deftest greger-stdlib-test-list-directory-exclude-directories-recursive ()
  "Test list-directory exclude-directories-recursive functionality."
  (let ((test-dir (greger-stdlib-test--make-controlled-temp-dir "greger-test-dir"))
        (parent-dir nil))
    (unwind-protect
        (progn
          (setq parent-dir (file-name-directory (directory-file-name test-dir)))
          ;; Create test structure with directories and files
          (let ((keep-dir (expand-file-name "keepdir" test-dir))
                (exclude-dir (expand-file-name ".git" test-dir))
                (file1 (expand-file-name "file1.txt" test-dir)))

            (make-directory keep-dir)
            (make-directory exclude-dir)
            (with-temp-file file1 (insert "content"))
            (with-temp-file (expand-file-name "kept.txt" keep-dir) (insert "kept"))
            (with-temp-file (expand-file-name "excluded.txt" exclude-dir) (insert "excluded"))

            ;; Test with default exclude pattern - should exclude .git directory from recursion
            (let ((result (greger-stdlib--list-directory test-dir [".git" "__pycache__"] t))
                  (expected (format "%s:
drwx------  (dir)  .
drwx------  (dir)  ..
drwxr-xr-x  (dir)  .git
-rw-r--r--         7  file1.txt
drwxr-xr-x  (dir)  keepdir

%skeepdir/:
drwxr-xr-x  (dir)  .
drwx------  (dir)  ..
-rw-r--r--         4  kept.txt" (file-name-as-directory test-dir) (file-name-as-directory test-dir))))
              (should (stringp result))
              (should (string= expected result)))

            ;; Test with empty exclude pattern - should recurse into all directories including .git
            (let ((result (greger-stdlib--list-directory test-dir [] t))
                  (expected (format "%s:
drwx------  (dir)  .
drwx------  (dir)  ..
drwxr-xr-x  (dir)  .git
-rw-r--r--         7  file1.txt
drwxr-xr-x  (dir)  keepdir

%s.git/:
drwxr-xr-x  (dir)  .
drwx------  (dir)  ..
-rw-r--r--         8  excluded.txt

%skeepdir/:
drwxr-xr-x  (dir)  .
drwx------  (dir)  ..
-rw-r--r--         4  kept.txt" (file-name-as-directory test-dir) (file-name-as-directory test-dir) (file-name-as-directory test-dir))))
              (should (stringp result))
              (should (string= expected result)))))

      ;; Clean up
      (when (and parent-dir (file-exists-p parent-dir))
        (delete-directory parent-dir t)))))

(ert-deftest greger-stdlib-test-list-directory-recursive ()
  "Test list-directory recursive functionality."
  (let ((test-dir (greger-stdlib-test--make-controlled-temp-dir "greger-test-dir"))
        (parent-dir nil))
    (unwind-protect
        (progn
          (setq parent-dir (file-name-directory (directory-file-name test-dir)))
          ;; Create simple nested directory structure
          (let ((subdir (expand-file-name "testdir" test-dir))
                (file1 (expand-file-name "root.txt" test-dir))
                (file2 (expand-file-name "testdir/nested.txt" test-dir)))

            (make-directory subdir)
            (with-temp-file file1 (insert "Root content"))
            (with-temp-file file2 (insert "Nested content"))

            ;; Test recursive listing
            (let ((result (greger-stdlib--list-directory test-dir nil t))
                  (expected (format "%s:
drwx------  (dir)  .
drwx------  (dir)  ..
-rw-r--r--        12  root.txt
drwxr-xr-x  (dir)  testdir

%stestdir/:
drwxr-xr-x  (dir)  .
drwx------  (dir)  ..
-rw-r--r--        14  nested.txt" (file-name-as-directory test-dir) (file-name-as-directory test-dir))))
              (should (stringp result))
              (should (string= expected result)))))

      ;; Clean up
      (when (and parent-dir (file-exists-p parent-dir))
        (delete-directory parent-dir t)))))

(ert-deftest greger-stdlib-test-list-directory-error-cases ()
  "Test error handling in list-directory function."
  ;; Test non-existent directory
  (should-error (greger-stdlib--list-directory "/path/that/does/not/exist"))

  ;; Test invalid path type
  (should-error (greger-stdlib--list-directory 123))

  ;; Test file instead of directory
  (let ((test-file (make-temp-file "greger-test-file")))
    (unwind-protect
        (progn
          (with-temp-file test-file
            (insert "test content"))
          (should-error (greger-stdlib--list-directory test-file)))
      (when (file-exists-p test-file)
        (delete-file test-file)))))

(ert-deftest greger-stdlib-test-list-directory-empty-dir ()
  "Test list-directory with empty directory."
  (let ((test-dir (greger-stdlib-test--make-controlled-temp-dir "greger-test-empty-dir"))
        (parent-dir nil))
    (unwind-protect
        (progn
          (setq parent-dir (file-name-directory (directory-file-name test-dir)))
          ;; Test empty directory - should show directory header and . .. entries
          (let ((result (greger-stdlib--list-directory test-dir))
                (expected (format "%s:
drwx------  (dir)  .
drwx------  (dir)  .." (file-name-as-directory test-dir))))
            (should (stringp result))
            (should (string= expected result))))

      ;; Clean up
      (when (and parent-dir (file-exists-p parent-dir))
        (delete-directory parent-dir t)))))

(ert-deftest greger-stdlib-test-list-directory-file-mode-string ()
  "Test the file mode string formatting function."
  (let ((temp-dir (make-temp-file "greger-test-mode" t))
        (temp-file (make-temp-file "greger-test-file")))
    (unwind-protect
        (progn
          ;; Test directory formatting
          (let ((dir-info (greger-stdlib--format-file-info temp-dir "testdir" "nomatch"))
                (expected "drwx------  (dir)  testdir"))
            (should (stringp dir-info))
            (should (string= expected dir-info)))

          ;; Test file formatting
          (let ((file-info (greger-stdlib--format-file-info temp-file "testfile" "nomatch"))
                (expected "-rw-------         0  testfile"))
            (should (stringp file-info))
            (should (string= expected file-info))))

      ;; Clean up
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(ert-deftest greger-stdlib-test-list-directory-hidden-directories ()
  "Test list-directory handling of hidden directories with exclude-directories-recursive."
  (let ((test-dir (greger-stdlib-test--make-controlled-temp-dir "greger-test-hidden"))
        (parent-dir nil))
    (unwind-protect
        (progn
          (setq parent-dir (file-name-directory (directory-file-name test-dir)))
          ;; Create hidden and normal directories and files
          (let ((hidden-dir (expand-file-name ".hiddendir" test-dir))
                (normal-dir (expand-file-name "normaldir" test-dir))
                (normal-file (expand-file-name "normalfile.txt" test-dir)))

            (make-directory hidden-dir)
            (make-directory normal-dir)
            (with-temp-file normal-file (insert "Normal content"))
            (with-temp-file (expand-file-name "hidden.txt" hidden-dir) (insert "Hidden content"))
            (with-temp-file (expand-file-name "normal.txt" normal-dir) (insert "Normal content"))

            ;; Test with no exclude pattern (should show all files and directories, and recurse into all)
            (let ((result (greger-stdlib--list-directory test-dir [] t))
                  (expected (format "%s:
drwx------  (dir)  .
drwx------  (dir)  ..
drwxr-xr-x  (dir)  .hiddendir
drwxr-xr-x  (dir)  normaldir
-rw-r--r--        14  normalfile.txt

%s.hiddendir/:
drwxr-xr-x  (dir)  .
drwx------  (dir)  ..
-rw-r--r--        14  hidden.txt

%snormaldir/:
drwxr-xr-x  (dir)  .
drwx------  (dir)  ..
-rw-r--r--        14  normal.txt" (file-name-as-directory test-dir) (file-name-as-directory test-dir) (file-name-as-directory test-dir))))
              (should (stringp result))
              (should (string= expected result)))

            ;; Test with pattern excluding hidden directories (starting with .) from recursion
            (let ((result (greger-stdlib--list-directory test-dir [".hiddendir"] t)))
              (should (stringp result))
              (let ((expected (format "%s:
drwx------  (dir)  .
drwx------  (dir)  ..
drwxr-xr-x  (dir)  .hiddendir
drwxr-xr-x  (dir)  normaldir
-rw-r--r--        14  normalfile.txt

%snormaldir/:
drwxr-xr-x  (dir)  .
drwx------  (dir)  ..
-rw-r--r--        14  normal.txt" (file-name-as-directory test-dir) (file-name-as-directory test-dir))))
                (should (string= expected result))))))

      ;; Clean up
      (when (and parent-dir (file-exists-p parent-dir))
        (delete-directory parent-dir t)))))

;; File manipulation tests

(ert-deftest greger-stdlib-test-write-new-file-basic ()
  "Test basic write-new-file functionality."
  (let ((test-file (expand-file-name "new-test-file.txt"
                                     (make-temp-file "greger-write-test" t)))
        (test-content "Hello, world!\nThis is a new file."))
    (unwind-protect
        (progn
          ;; Ensure file doesn't exist
          (should-not (file-exists-p test-file))

          ;; Mock git operations
          (cl-letf (((symbol-function 'greger-stdlib--git-stage-and-commit)
                     (lambda (files commit-message buffer) "Mocked git result")))

            ;; Test successful file creation
            (let ((result (greger-stdlib--write-new-file
                           test-file
                           test-content
                           "Create new test file")))
              (should (stringp result))
              (should (string-match "Successfully wrote new file" result))
              (should (file-exists-p test-file))

              ;; Verify file contents (write-file adds a newline at the end)
              (with-temp-buffer
                (insert-file-contents test-file)
                (should (string= (buffer-string) (concat test-content "\n")))))))

      ;; Clean up
      (when (file-exists-p test-file)
        (delete-file test-file))
      (when (file-exists-p (file-name-directory test-file))
        (delete-directory (file-name-directory test-file) t)))))

(ert-deftest greger-stdlib-test-write-new-file-already-exists ()
  "Test write-new-file when file already exists."
  (let ((test-file (make-temp-file "greger-write-existing")))
    (unwind-protect
        (progn
          ;; File already exists
          (should (file-exists-p test-file))

          ;; Mock git operations
          (cl-letf (((symbol-function 'greger-stdlib--git-stage-and-commit)
                     (lambda (files commit-message buffer) "Mocked git result")))

            ;; Should error when trying to write to existing file
            (should-error (greger-stdlib--write-new-file
                           test-file
                           "New content"
                           "Should fail"))))

      ;; Clean up
      (when (file-exists-p test-file)
        (delete-file test-file)))))

(ert-deftest greger-stdlib-test-write-new-file-invalid-path ()
  "Test write-new-file with invalid path."
  ;; Mock git operations
  (cl-letf (((symbol-function 'greger-stdlib--git-stage-and-commit)
             (lambda (files commit-message buffer) "Mocked git result")))

    ;; Test with invalid directory
    (should-error (greger-stdlib--write-new-file
                   "/nonexistent/directory/file.txt"
                   "Content"
                   "Should fail"))))

(ert-deftest greger-stdlib-test-replace-file-basic ()
  "Test basic replace-file functionality."
  (let ((test-file (make-temp-file "greger-replace-test"))
        (original-content "Original content")
        (new-content "Replaced content\nWith multiple lines"))
    (unwind-protect
        (progn
          ;; Write initial content
          (with-temp-file test-file
            (insert original-content))

          ;; Verify initial content
          (with-temp-buffer
            (insert-file-contents test-file)
            (should (string= (buffer-string) original-content)))

          ;; Mock git operations
          (cl-letf (((symbol-function 'greger-stdlib--git-stage-and-commit)
                     (lambda (files commit-message buffer) "Mocked git result")))

            ;; Test successful file replacement
            (let ((result (greger-stdlib--replace-file
                           test-file
                           new-content
                           "Replace file content")))
              (should (stringp result))
              (should (string-match "Successfully replaced" result))

              ;; Verify new content
              (with-temp-buffer
                (insert-file-contents test-file)
                (should (string= (buffer-string) new-content))))))

      ;; Clean up
      (when (file-exists-p test-file)
        (delete-file test-file)))))

(ert-deftest greger-stdlib-test-replace-file-nonexistent ()
  "Test replace-file with non-existent file."
  ;; Mock git operations
  (cl-letf (((symbol-function 'greger-stdlib--git-stage-and-commit)
             (lambda (files commit-message buffer) "Mocked git result")))

    ;; Should error when trying to replace non-existent file
    (should-error (greger-stdlib--replace-file
                   "/path/that/does/not/exist.txt"
                   "New content"
                   "Should fail"))))

(ert-deftest greger-stdlib-test-replace-file-empty-content ()
  "Test replace-file with empty content."
  (let ((test-file (make-temp-file "greger-replace-empty"))
        (original-content "Some original content"))
    (unwind-protect
        (progn
          ;; Write initial content
          (with-temp-file test-file
            (insert original-content))

          ;; Mock git operations
          (cl-letf (((symbol-function 'greger-stdlib--git-stage-and-commit)
                     (lambda (files commit-message buffer) "Mocked git result")))

            ;; Test replacing with empty content
            (let ((result (greger-stdlib--replace-file
                           test-file
                           ""
                           "Replace with empty content")))
              (should (stringp result))
              (should (string-match "Successfully replaced" result))

              ;; Verify file is now empty
              (with-temp-buffer
                (insert-file-contents test-file)
                (should (string= (buffer-string) ""))))))

      ;; Clean up
      (when (file-exists-p test-file)
        (delete-file test-file)))))

(ert-deftest greger-stdlib-test-make-directory-basic ()
  "Test basic make-directory functionality."
  (let ((test-dir (expand-file-name "test-new-dir"
                                    (make-temp-file "greger-mkdir-test" t))))
    (unwind-protect
        (progn
          ;; Ensure directory doesn't exist
          (should-not (file-exists-p test-dir))

          ;; Mock git operations
          (cl-letf (((symbol-function 'greger-stdlib--git-stage-and-commit)
                     (lambda (files commit-message buffer) "Mocked git result")))

            ;; Test successful directory creation
            (let ((result (greger-stdlib--make-directory
                           test-dir
                           "Create new directory")))
              (should (stringp result))
              (should (string-match "Successfully created directory" result))
              (should (file-exists-p test-dir))
              (should (file-directory-p test-dir)))))

      ;; Clean up
      (when (file-exists-p test-dir)
        (delete-directory test-dir))
      (when (file-exists-p (file-name-directory test-dir))
        (delete-directory (file-name-directory test-dir) t)))))

(ert-deftest greger-stdlib-test-make-directory-recursive ()
  "Test make-directory with nested directories."
  (let ((parent-dir (make-temp-file "greger-mkdir-recursive" t))
        (nested-dir nil))
    (unwind-protect
        (progn
          (setq nested-dir (expand-file-name "level1/level2/level3" parent-dir))

          ;; Ensure nested path doesn't exist
          (should-not (file-exists-p nested-dir))

          ;; Mock git operations
          (cl-letf (((symbol-function 'greger-stdlib--git-stage-and-commit)
                     (lambda (files commit-message buffer) "Mocked git result")))

            ;; Test recursive directory creation
            (let ((result (greger-stdlib--make-directory
                           nested-dir
                           "Create nested directories")))
              (should (stringp result))
              (should (string-match "Successfully created directory" result))
              (should (file-exists-p nested-dir))
              (should (file-directory-p nested-dir))

              ;; Verify intermediate directories were created
              (should (file-exists-p (expand-file-name "level1" parent-dir)))
              (should (file-exists-p (expand-file-name "level1/level2" parent-dir))))))

      ;; Clean up
      (when (file-exists-p parent-dir)
        (delete-directory parent-dir t)))))

(ert-deftest greger-stdlib-test-make-directory-already-exists ()
  "Test make-directory when directory already exists."
  (let ((test-dir (make-temp-file "greger-mkdir-existing" t)))
    (unwind-protect
        (progn
          ;; Directory already exists
          (should (file-exists-p test-dir))
          (should (file-directory-p test-dir))

          ;; Mock git operations
          (cl-letf (((symbol-function 'greger-stdlib--git-stage-and-commit)
                     (lambda (files commit-message buffer) "Mocked git result")))

            ;; Should succeed even if directory exists (like mkdir -p)
            (let ((result (greger-stdlib--make-directory
                           test-dir
                           "Directory already exists")))
              (should (stringp result))
              (should (string-match "already exists" result)))))

      ;; Clean up
      (when (file-exists-p test-dir)
        (delete-directory test-dir t)))))

(ert-deftest greger-stdlib-test-rename-file-basic ()
  "Test basic rename-file functionality."
  (let ((old-file (make-temp-file "greger-rename-old"))
        (new-file (expand-file-name "renamed-file.txt"
                                    (make-temp-file "greger-rename-test" t)))
        (test-content "File content to preserve"))
    (unwind-protect
        (progn
          ;; Write content to original file
          (with-temp-file old-file
            (insert test-content))

          ;; Ensure new file doesn't exist
          (should-not (file-exists-p new-file))

          ;; Mock git operations
          (cl-letf (((symbol-function 'greger-stdlib--git-stage-and-commit)
                     (lambda (files commit-message buffer) "Mocked git result")))

            ;; Test successful file rename
            (let ((result (greger-stdlib--rename-file
                           old-file
                           new-file
                           "Rename test file")))
              (should (stringp result))
              (should (string-match "Successfully renamed" result))
              (should-not (file-exists-p old-file))
              (should (file-exists-p new-file))

              ;; Verify content was preserved
              (with-temp-buffer
                (insert-file-contents new-file)
                (should (string= (buffer-string) test-content))))))

      ;; Clean up
      (when (file-exists-p old-file)
        (delete-file old-file))
      (when (file-exists-p new-file)
        (delete-file new-file))
      (when (file-exists-p (file-name-directory new-file))
        (delete-directory (file-name-directory new-file) t)))))

(ert-deftest greger-stdlib-test-rename-file-move-to-different-directory ()
  "Test rename-file moving to different directory."
  (let ((source-dir (make-temp-file "greger-rename-source" t))
        (dest-dir (make-temp-file "greger-rename-dest" t))
        (old-file nil)
        (new-file nil)
        (test-content "Content to move"))
    (unwind-protect
        (progn
          (setq old-file (expand-file-name "original.txt" source-dir))
          (setq new-file (expand-file-name "moved.txt" dest-dir))

          ;; Create original file
          (with-temp-file old-file
            (insert test-content))

          ;; Mock git operations
          (cl-letf (((symbol-function 'greger-stdlib--git-stage-and-commit)
                     (lambda (files commit-message buffer) "Mocked git result")))

            ;; Test moving file to different directory
            (let ((result (greger-stdlib--rename-file
                           old-file
                           new-file
                           "Move file to different directory")))
              (should (stringp result))
              (should (string-match "Successfully renamed" result))
              (should-not (file-exists-p old-file))
              (should (file-exists-p new-file))

              ;; Verify content was preserved
              (with-temp-buffer
                (insert-file-contents new-file)
                (should (string= (buffer-string) test-content))))))

      ;; Clean up
      (when (file-exists-p source-dir)
        (delete-directory source-dir t))
      (when (file-exists-p dest-dir)
        (delete-directory dest-dir t)))))

(ert-deftest greger-stdlib-test-rename-file-nonexistent-source ()
  "Test rename-file with non-existent source file."
  (let ((new-file (make-temp-file "greger-rename-target")))
    (unwind-protect
        (progn
          ;; Mock git operations
          (cl-letf (((symbol-function 'greger-stdlib--git-stage-and-commit)
                     (lambda (files commit-message buffer) "Mocked git result")))

            ;; Should error when source file doesn't exist
            (should-error (greger-stdlib--rename-file
                           "/path/that/does/not/exist.txt"
                           new-file
                           "Should fail"))))

      ;; Clean up
      (when (file-exists-p new-file)
        (delete-file new-file)))))

(ert-deftest greger-stdlib-test-rename-file-target-exists ()
  "Test rename-file when target file already exists."
  (let ((old-file (make-temp-file "greger-rename-old"))
        (new-file (make-temp-file "greger-rename-existing"))
        (old-content "Original content")
        (existing-content "Existing content"))
    (unwind-protect
        (progn
          ;; Create files with different content
          (with-temp-file old-file
            (insert old-content))
          (with-temp-file new-file
            (insert existing-content))

          ;; Mock git operations
          (cl-letf (((symbol-function 'greger-stdlib--git-stage-and-commit)
                     (lambda (files commit-message buffer) "Mocked git result")))

            ;; Should fail when target file already exists
            (should-error (greger-stdlib--rename-file
                           old-file
                           new-file
                           "Should fail when target exists"))))

      ;; Clean up
      (when (file-exists-p old-file)
        (delete-file old-file))
      (when (file-exists-p new-file)
        (delete-file new-file)))))

(ert-deftest greger-stdlib-test-rename-file-directory ()
  "Test that rename-file works for directories too."
  (let ((test-dir (make-temp-file "greger-rename-dir" t))
        (new-path (expand-file-name "new-name" (make-temp-file "greger-rename-parent" t))))
    (unwind-protect
        (progn
          ;; Ensure target doesn't exist
          (should-not (file-exists-p new-path))

          ;; Mock git operations
          (cl-letf (((symbol-function 'greger-stdlib--git-stage-and-commit)
                     (lambda (files commit-message buffer) "Mocked git result")))

            ;; Should succeed when renaming a directory
            (let ((result (greger-stdlib--rename-file
                           test-dir
                           new-path
                           "Rename directory")))
              (should (stringp result))
              (should (string-match "Successfully renamed" result))
              (should-not (file-exists-p test-dir))
              (should (file-exists-p new-path))
              (should (file-directory-p new-path)))))

      ;; Clean up
      (when (file-exists-p test-dir)
        (delete-directory test-dir t))
      (when (file-exists-p new-path)
        (delete-directory new-path t))
      (when (file-exists-p (file-name-directory new-path))
        (delete-directory (file-name-directory new-path) t)))))

;; Ripgrep tests

(ert-deftest greger-stdlib-test-ripgrep-basic-search ()
  "Test basic ripgrep functionality with simple pattern matching."
  (let ((test-dir (make-temp-file "greger-ripgrep-test" t))
        (result nil)
        (error nil)
        (callback-called nil))
    (unwind-protect
        (progn
          ;; Create test files with content
          (with-temp-file (expand-file-name "test1.txt" test-dir)
            (insert "Hello world\nThis is a test\nAnother line"))
          (with-temp-file (expand-file-name "test2.py" test-dir)
            (insert "def hello():\n    print('world')\n    return True"))

          ;; Test basic pattern search
          (greger-stdlib--ripgrep
           "hello"
           test-dir
           (lambda (output err)
             (setq result output error err callback-called t))
           nil nil 0 nil nil nil 50)

          ;; Wait for async operation
          (let ((timeout 0))
            (while (and (not callback-called) (< timeout 50))
              (sit-for 0.1)
              (setq timeout (1+ timeout))))

          ;; Verify results
          (should callback-called)
          (should (null error))
          (should (stringp result))
          (should (string-match "Hello world" result))
          (should (string-match "def hello" result)))

      ;; Clean up
      (when (file-exists-p test-dir)
        (delete-directory test-dir t)))))

(ert-deftest greger-stdlib-test-ripgrep-case-sensitive ()
  "Test ripgrep case sensitivity options."
  (let ((test-dir (make-temp-file "greger-ripgrep-case" t))
        (result-sensitive nil)
        (result-insensitive nil)
        (callback-count 0))
    (unwind-protect
        (progn
          ;; Create test file with mixed case content
          (with-temp-file (expand-file-name "case-test.txt" test-dir)
            (insert "Hello World\nhello world\nHELLO WORLD"))

          ;; Test case-sensitive search (should only match exact case)
          (greger-stdlib--ripgrep
           "Hello"
           test-dir
           (lambda (output err)
             (setq result-sensitive output)
             (setq callback-count (1+ callback-count)))
           t nil 0 nil nil nil 50) ; case-sensitive = t

          ;; Test case-insensitive search (should match all cases)
          (greger-stdlib--ripgrep
           "Hello"
           test-dir
           (lambda (output err)
             (setq result-insensitive output)
             (setq callback-count (1+ callback-count)))
           nil nil 0 nil nil nil 50) ; case-sensitive = nil

          ;; Wait for both operations
          (let ((timeout 0))
            (while (and (< callback-count 2) (< timeout 100))
              (sit-for 0.1)
              (setq timeout (1+ timeout))))

          ;; Verify case-sensitive results (should only find "Hello World")
          (should (stringp result-sensitive))
          (should (string-match ":Hello World" result-sensitive))
          ;; Count lines - should only have 1 match line for case sensitive search
          (let ((lines (split-string result-sensitive "\n" t)))
            (should (= (length lines) 1)))

          ;; Verify case-insensitive results (should find all variants)
          (should (stringp result-insensitive))
          (should (string-match ":Hello World" result-insensitive))
          (should (string-match ":hello world" result-insensitive))
          (should (string-match ":HELLO WORLD" result-insensitive)))

      ;; Clean up
      (when (file-exists-p test-dir)
        (delete-directory test-dir t)))))

(ert-deftest greger-stdlib-test-ripgrep-file-type-filter ()
  "Test ripgrep file type filtering."
  (let ((test-dir (make-temp-file "greger-ripgrep-filetype" t))
        (result nil)
        (error nil)
        (callback-called nil))
    (unwind-protect
        (progn
          ;; Create test files of different types
          (with-temp-file (expand-file-name "test.py" test-dir)
            (insert "print('Python file')"))
          (with-temp-file (expand-file-name "test.js" test-dir)
            (insert "console.log('JavaScript file');"))
          (with-temp-file (expand-file-name "test.txt" test-dir)
            (insert "Plain text file"))

          ;; Search only in Python files
          (greger-stdlib--ripgrep
           "file"
           test-dir
           (lambda (output err)
             (setq result output error err callback-called t))
           nil "py" 0 nil nil nil 50) ; case-sensitive, file-type, context-lines, fixed-strings, word-regexp, line-regexp, max-results

          ;; Wait for operation
          (let ((timeout 0))
            (while (and (not callback-called) (< timeout 50))
              (sit-for 0.1)
              (setq timeout (1+ timeout))))

          ;; Verify results - should only find matches in Python file
          (should callback-called)
          (should (null error))
          (should (stringp result))
          (should (string-match "Python file" result))
          (should-not (string-match "JavaScript file" result))
          (should-not (string-match "Plain text file" result)))

      ;; Clean up
      (when (file-exists-p test-dir)
        (delete-directory test-dir t)))))

(ert-deftest greger-stdlib-test-ripgrep-context-lines ()
  "Test ripgrep context lines functionality."
  (let ((test-dir (make-temp-file "greger-ripgrep-context" t))
        (result nil)
        (error nil)
        (callback-called nil))
    (unwind-protect
        (progn
          ;; Create test file with multiple lines
          (with-temp-file (expand-file-name "context-test.txt" test-dir)
            (insert "Line 1\nLine 2\nMATCH LINE\nLine 4\nLine 5"))

          ;; Search with 1 context line
          (greger-stdlib--ripgrep
           "MATCH"
           test-dir
           (lambda (output err)
             (setq result output error err callback-called t))
           nil nil 1 nil nil nil 50)  ; case-sensitive, file-type, context-lines, fixed-strings, word-regexp, line-regexp, max-results

          ;; Wait for operation
          (let ((timeout 0))
            (while (and (not callback-called) (< timeout 50))
              (sit-for 0.1)
              (setq timeout (1+ timeout))))

          ;; Verify results - should include context lines
          (should callback-called)
          (should (null error))
          (should (stringp result))
          (should (string-match "MATCH LINE" result))
          (should (string-match "Line 2" result))  ; Before context
          (should (string-match "Line 4" result))) ; After context

      ;; Clean up
      (when (file-exists-p test-dir)
        (delete-directory test-dir t)))))

(ert-deftest greger-stdlib-test-ripgrep-fixed-strings ()
  "Test ripgrep fixed strings vs regex patterns."
  (let ((test-dir (make-temp-file "greger-ripgrep-fixed" t))
        (result-regex nil)
        (result-fixed nil)
        (callback-count 0))
    (unwind-protect
        (progn
          ;; Create test file with regex special characters
          (with-temp-file (expand-file-name "special-chars.txt" test-dir)
            (insert "test.txt\ntest[txt]\ntest(txt)\ntest+txt"))

          ;; Search as regex pattern (should match multiple lines due to . wildcard)
          (greger-stdlib--ripgrep
           "test.txt"
           test-dir
           (lambda (output err)
             (setq result-regex output)
             (setq callback-count (1+ callback-count)))
           nil nil 0 nil nil nil 50) ; case-sensitive, file-type, context-lines, fixed-strings, word-regexp, line-regexp, max-results

          ;; Search as literal/fixed string (should only match exact string)
          (greger-stdlib--ripgrep
           "test.txt"
           test-dir
           (lambda (output err)
             (setq result-fixed output)
             (setq callback-count (1+ callback-count)))
           nil nil 0 t nil nil 50)  ; case-sensitive, file-type, context-lines, fixed-strings=t, word-regexp, line-regexp, max-results

          ;; Wait for both operations
          (let ((timeout 0))
            (while (and (< callback-count 2) (< timeout 100))
              (sit-for 0.1)
              (setq timeout (1+ timeout))))

          ;; Verify regex results (. matches any character, so should match multiple lines)
          (should (stringp result-regex))
          (should (string-match "test.txt" result-regex))
          (should (string-match "test\\[txt\\]" result-regex))  ; . matches [
          (should (string-match "test(txt)" result-regex))     ; . matches (
          (should (string-match "test\\+txt" result-regex))    ; . matches +

          ;; Verify fixed string results (should only match literal "test.txt")
          (should (stringp result-fixed))
          (should (string-match "test\\.txt" result-fixed))
          (should-not (string-match "test\\[txt\\]" result-fixed))
          (should-not (string-match "test(txt)" result-fixed))
          (should-not (string-match "test\\+txt" result-fixed)))

      ;; Clean up
      (when (file-exists-p test-dir)
        (delete-directory test-dir t)))))

(ert-deftest greger-stdlib-test-ripgrep-word-regexp ()
  "Test ripgrep word boundary matching."
  (let ((test-dir (make-temp-file "greger-ripgrep-word" t))
        (result-word nil)
        (result-normal nil)
        (callback-count 0))
    (unwind-protect
        (progn
          ;; Create test file with word boundary scenarios
          (with-temp-file (expand-file-name "word-test.txt" test-dir)
            (insert "test testing untested\ntest-case\ntest123"))

          ;; Search with word boundaries
          (greger-stdlib--ripgrep
           "test"
           test-dir
           (lambda (output err)
             (setq result-word output)
             (setq callback-count (1+ callback-count)))
           nil nil 0 nil t nil 50)  ; case-sensitive, file-type, context-lines, fixed-strings, word-regexp=t, line-regexp, max-results

          ;; Search without word boundaries
          (greger-stdlib--ripgrep
           "test"
           test-dir
           (lambda (output err)
             (setq result-normal output)
             (setq callback-count (1+ callback-count)))
           nil nil 0 nil nil nil 50) ; case-sensitive, file-type, context-lines, fixed-strings, word-regexp=nil, line-regexp, max-results

          ;; Wait for both operations
          (let ((timeout 0))
            (while (and (< callback-count 2) (< timeout 100))
              (sit-for 0.1)
              (setq timeout (1+ timeout))))

          ;; Verify word boundary results (should only match whole word "test")
          (should (stringp result-word))
          ;; Should match "test" at word boundaries
          (should (string-match-p ":test testing untested" result-word))
          (should (string-match-p ":test-case" result-word))
          ;; Should NOT match "test123" since "test" is not at a word boundary there
          (should-not (string-match-p ":test123" result-word))

          ;; Verify normal results (should match "test" anywhere including inside words)
          (should (stringp result-normal))
          (should (string-match-p ":test testing untested" result-normal))
          (should (string-match-p ":test-case" result-normal))
          (should (string-match-p ":test123" result-normal)))

      ;; Clean up
      (when (file-exists-p test-dir)
        (delete-directory test-dir t)))))

(ert-deftest greger-stdlib-test-ripgrep-line-regexp ()
  "Test ripgrep line matching (entire line must match)."
  (let ((test-dir (make-temp-file "greger-ripgrep-line" t))
        (result nil)
        (error nil)
        (callback-called nil))
    (unwind-protect
        (progn
          ;; Create test file
          (with-temp-file (expand-file-name "line-test.txt" test-dir)
            (insert "test\nexact test match\ntest with more text"))

          ;; Search with line regexp (entire line must match pattern)
          (greger-stdlib--ripgrep
           "test"
           test-dir
           (lambda (output err)
             (setq result output error err callback-called t))
           nil nil 0 nil nil t 50)  ; case-sensitive, file-type, context-lines, fixed-strings, word-regexp, line-regexp=t, max-results

          ;; Wait for operation
          (let ((timeout 0))
            (while (and (not callback-called) (< timeout 50))
              (sit-for 0.1)
              (setq timeout (1+ timeout))))

          ;; Verify results - should only match lines where entire line matches
          (should callback-called)
          (should (null error))
          (should (stringp result))
          ;; Should match the line that contains only "test" (with file path prefix)
          (should (string-match-p ":test$" result))
          ;; Should NOT match lines with additional text
          (should-not (string-match-p ":exact test match" result))
          (should-not (string-match-p ":test with more text" result)))

      ;; Clean up
      (when (file-exists-p test-dir)
        (delete-directory test-dir t)))))

(ert-deftest greger-stdlib-test-ripgrep-max-results ()
  "Test ripgrep max results limiting."
  (let ((test-dir (make-temp-file "greger-ripgrep-max" t))
        (result nil)
        (error nil)
        (callback-called nil))
    (unwind-protect
        (progn
          ;; Create test file with many matches
          (with-temp-file (expand-file-name "many-matches.txt" test-dir)
            (insert (mapconcat (lambda (i) (format "match line %d" i))
                               (number-sequence 1 20) "\n")))

          ;; Search with max results limit
          (greger-stdlib--ripgrep
           "match"
           test-dir
           (lambda (output err)
             (setq result output error err callback-called t))
           nil nil 0 nil nil nil 3)  ; case-sensitive, file-type, context-lines, fixed-strings, word-regexp, line-regexp, max-results=3

          ;; Wait for operation
          (let ((timeout 0))
            (while (and (not callback-called) (< timeout 50))
              (sit-for 0.1)
              (setq timeout (1+ timeout))))

          ;; Verify results - should be limited to max results
          (should callback-called)
          (should (null error))
          (should (stringp result))
          ;; Count the number of match lines in result
          (let ((match-count (length (split-string result "\n" t))))
            ;; Should have around 3 results (give or take for headers/formatting)
            (should (<= match-count 5))  ; Allow some flexibility for ripgrep output format
            (should (>= match-count 1))))

      ;; Clean up
      (when (file-exists-p test-dir)
        (delete-directory test-dir t)))))

(ert-deftest greger-stdlib-test-ripgrep-nonexistent-path ()
  "Test ripgrep with non-existent path."
  (let ((result nil)
        (error nil)
        (callback-called nil))

    ;; Search in non-existent directory
    (greger-stdlib--ripgrep
     "test"
     "/path/that/does/not/exist"
     (lambda (output err)
       (setq result output error err callback-called t))
     nil nil 0 nil nil nil 50)

    ;; Wait for operation
    (let ((timeout 0))
      (while (and (not callback-called) (< timeout 50))
        (sit-for 0.1)
        (setq timeout (1+ timeout))))

    ;; Verify error handling
    (should callback-called)
    (should (null result))
    (should (stringp error))
    (should (string-match-p "No such file or directory\\|not found\\|does not exist" error))))

(ert-deftest greger-stdlib-test-ripgrep-empty-pattern ()
  "Test ripgrep with empty pattern."
  (let ((test-dir (make-temp-file "greger-ripgrep-empty" t))
        (result nil)
        (error nil)
        (callback-called nil))
    (unwind-protect
        (progn
          ;; Create test file
          (with-temp-file (expand-file-name "test.txt" test-dir)
            (insert "Some content\nMore content"))

          ;; Search with empty pattern
          (greger-stdlib--ripgrep
           ""
           test-dir
           (lambda (output err)
             (setq result output error err callback-called t))
           nil nil 0 nil nil nil 50)

          ;; Wait for operation
          (let ((timeout 0))
            (while (and (not callback-called) (< timeout 50))
              (sit-for 0.1)
              (setq timeout (1+ timeout))))

          ;; Verify handling of empty pattern
          (should callback-called)
          ;; Empty pattern might either return all lines or an error, depending on ripgrep version
          (should (or result error)))

      ;; Clean up
      (when (file-exists-p test-dir)
        (delete-directory test-dir t)))))

(ert-deftest greger-stdlib-test-ripgrep-regex-patterns ()
  "Test ripgrep with various regex patterns."
  (let ((test-dir (make-temp-file "greger-ripgrep-regex" t))
        (result nil)
        (error nil)
        (callback-called nil))
    (unwind-protect
        (progn
          ;; Create test file with various patterns
          (with-temp-file (expand-file-name "regex-test.txt" test-dir)
            (insert "Email: user@example.com\nPhone: 123-456-7890\nDate: 2023-12-25\nNo match here"))

          ;; Search with email regex pattern
          (greger-stdlib--ripgrep
           "[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}"
           test-dir
           (lambda (output err)
             (setq result output error err callback-called t))
           nil nil 0 nil nil nil 50)

          ;; Wait for operation
          (let ((timeout 0))
            (while (and (not callback-called) (< timeout 50))
              (sit-for 0.1)
              (setq timeout (1+ timeout))))

          ;; Verify regex matching
          (should callback-called)
          (should (null error))
          (should (stringp result))
          (should (string-match-p "user@example.com" result))
          (should-not (string-match-p "123-456-7890" result))
          (should-not (string-match-p "2023-12-25" result)))

      ;; Clean up
      (when (file-exists-p test-dir)
        (delete-directory test-dir t)))))

(ert-deftest greger-stdlib-test-ripgrep-multiple-files ()
  "Test ripgrep searching across multiple files."
  (let ((test-dir (make-temp-file "greger-ripgrep-multi" t))
        (result nil)
        (error nil)
        (callback-called nil))
    (unwind-protect
        (progn
          ;; Create multiple test files
          (with-temp-file (expand-file-name "file1.txt" test-dir)
            (insert "target in file 1"))
          (with-temp-file (expand-file-name "file2.txt" test-dir)
            (insert "target in file 2"))
          (with-temp-file (expand-file-name "file3.txt" test-dir)
            (insert "no matches here"))

          ;; Search across all files
          (greger-stdlib--ripgrep
           "target"
           test-dir
           (lambda (output err)
             (setq result output error err callback-called t))
           nil nil 0 nil nil nil 50)

          ;; Wait for operation
          (let ((timeout 0))
            (while (and (not callback-called) (< timeout 50))
              (sit-for 0.1)
              (setq timeout (1+ timeout))))

          ;; Verify results from multiple files
          (should callback-called)
          (should (null error))
          (should (stringp result))
          (should (string-match-p "file1.txt" result))
          (should (string-match-p "file2.txt" result))
          (should (string-match-p "target in file 1" result))
          (should (string-match-p "target in file 2" result))
          (should-not (string-match-p "file3.txt" result)))

      ;; Clean up
      (when (file-exists-p test-dir)
        (delete-directory test-dir t)))))

(ert-deftest greger-stdlib-test-ripgrep-invalid-regex ()
  "Test ripgrep with invalid regex pattern."
  (let ((test-dir (make-temp-file "greger-ripgrep-invalid" t))
        (result nil)
        (error nil)
        (callback-called nil))
    (unwind-protect
        (progn
          ;; Create test file
          (with-temp-file (expand-file-name "test.txt" test-dir)
            (insert "Some test content"))

          ;; Search with invalid regex (unmatched bracket)
          (greger-stdlib--ripgrep
           "[unclosed"
           test-dir
           (lambda (output err)
             (setq result output error err callback-called t))
           nil nil 0 nil nil nil 50)

          ;; Wait for operation
          (let ((timeout 0))
            (while (and (not callback-called) (< timeout 50))
              (sit-for 0.1)
              (setq timeout (1+ timeout))))

          ;; Verify error handling for invalid regex
          (should callback-called)
          (should (null result))
          (should (stringp error))
          (should (string-match-p "regex\\|pattern\\|syntax" error)))

      ;; Clean up
      (when (file-exists-p test-dir)
        (delete-directory test-dir t)))))

(ert-deftest greger-stdlib-test-ripgrep-binary-files ()
  "Test ripgrep behavior with binary files."
  (let ((test-dir (make-temp-file "greger-ripgrep-binary" t))
        (result nil)
        (error nil)
        (callback-called nil))
    (unwind-protect
        (progn
          ;; Create a binary file (with null bytes)
          (with-temp-file (expand-file-name "binary.bin" test-dir)
            (set-buffer-multibyte nil)
            (insert "text\0binary\0content"))

          ;; Create a normal text file for comparison
          (with-temp-file (expand-file-name "text.txt" test-dir)
            (insert "text content here"))

          ;; Search for pattern that exists in both files
          (greger-stdlib--ripgrep
           "text"
           test-dir
           (lambda (output err)
             (setq result output error err callback-called t))
           nil nil 0 nil nil nil 50)

          ;; Wait for operation
          (let ((timeout 0))
            (while (and (not callback-called) (< timeout 50))
              (sit-for 0.1)
              (setq timeout (1+ timeout))))

          ;; Verify results - ripgrep typically skips binary files by default
          (should callback-called)
          (should (null error))
          (should (stringp result))
          (should (string-match-p "text.txt" result))
          ;; Binary file might be mentioned as skipped or not appear at all
          (should (string-match-p "text content here" result)))

      ;; Clean up
      (when (file-exists-p test-dir)
        (delete-directory test-dir t)))))

;; Git operation tests

(ert-deftest greger-stdlib-test-git-stage-and-commit-basic ()
  "Test basic git staging and committing functionality."
  (let ((test-dir (make-temp-file "greger-git-test" t))
        (test-file nil)
        (test-content "Initial file content\nLine 2\nLine 3"))
    (unwind-protect
        (progn
          ;; Initialize git repository
          (let ((default-directory test-dir))
            (should (= 0 (call-process "git" nil nil nil "init")))
            (should (= 0 (call-process "git" nil nil nil "config" "user.name" "Test User")))
            (should (= 0 (call-process "git" nil nil nil "config" "user.email" "test@example.com"))))
          
          ;; Create test file
          (setq test-file (expand-file-name "test.txt" test-dir))
          (with-temp-file test-file
            (insert test-content))
          
          ;; Test git stage and commit
          (let ((result (greger-stdlib--git-stage-and-commit 
                         (list test-file)
                         "Initial commit: add test file")))
            (should (stringp result))
            (should (string-match "Successfully staged 1 file(s) and committed" result))
            (should (string-match "Initial commit: add test file" result)))
          
          ;; Verify file is committed
          (let ((default-directory test-dir))
            (should (= 0 (call-process "git" nil nil nil "ls-files" "--error-unmatch" "test.txt")))))
      
      ;; Clean up
      (when (file-exists-p test-dir)
        (delete-directory test-dir t)))))

(ert-deftest greger-stdlib-test-git-stage-and-commit-multiple-files ()
  "Test git staging and committing with multiple files."
  (let ((test-dir (make-temp-file "greger-git-multi" t))
        (test-file1 nil)
        (test-file2 nil))
    (unwind-protect
        (progn
          ;; Initialize git repository
          (let ((default-directory test-dir))
            (should (= 0 (call-process "git" nil nil nil "init")))
            (should (= 0 (call-process "git" nil nil nil "config" "user.name" "Test User")))
            (should (= 0 (call-process "git" nil nil nil "config" "user.email" "test@example.com"))))
          
          ;; Create test files
          (setq test-file1 (expand-file-name "file1.txt" test-dir))
          (setq test-file2 (expand-file-name "file2.txt" test-dir))
          (with-temp-file test-file1 (insert "Content of file 1"))
          (with-temp-file test-file2 (insert "Content of file 2"))
          
          ;; Test git stage and commit multiple files
          (let ((result (greger-stdlib--git-stage-and-commit 
                         (list test-file1 test-file2)
                         "Add multiple test files")))
            (should (stringp result))
            (should (string-match "Successfully staged 2 file(s) and committed" result))
            (should (string-match "Add multiple test files" result)))
          
          ;; Verify both files are committed
          (let ((default-directory test-dir))
            (should (= 0 (call-process "git" nil nil nil "ls-files" "--error-unmatch" "file1.txt")))
            (should (= 0 (call-process "git" nil nil nil "ls-files" "--error-unmatch" "file2.txt")))))
      
      ;; Clean up
      (when (file-exists-p test-dir)
        (delete-directory test-dir t)))))

(ert-deftest greger-stdlib-test-git-stage-and-commit-non-git-directory ()
  "Test git operations in directory that is not a git repository."
  (let ((test-dir (make-temp-file "greger-non-git" t))
        (test-file nil))
    (unwind-protect
        (progn
          ;; Create test file (but no git repo)
          (setq test-file (expand-file-name "test.txt" test-dir))
          (with-temp-file test-file
            (insert "Test content"))
          
          ;; Should return error message when trying to stage and commit
          (let ((result (greger-stdlib--git-stage-and-commit 
                         (list test-file)
                         "This should fail")))
            (should (stringp result))
            (should (string-match "not in a git repository" result))))
      
      ;; Clean up
      (when (file-exists-p test-dir)
        (delete-directory test-dir t)))))

(ert-deftest greger-stdlib-test-git-stage-and-commit-with-chat-buffer-tracked ()
  "Test git staging with chat buffer that is tracked by git."
  (let ((test-dir (make-temp-file "greger-git-chat" t))
        (test-file nil)
        (chat-file nil)
        (chat-buffer nil))
    (unwind-protect
        (progn
          ;; Initialize git repository
          (let ((default-directory test-dir))
            (should (= 0 (call-process "git" nil nil nil "init")))
            (should (= 0 (call-process "git" nil nil nil "config" "user.name" "Test User")))
            (should (= 0 (call-process "git" nil nil nil "config" "user.email" "test@example.com"))))
          
          ;; Create and commit chat file first (to make it tracked)
          (setq chat-file (expand-file-name "chat.greger" test-dir))
          (with-temp-file chat-file
            (insert "Initial chat content"))
          (let ((default-directory test-dir))
            (should (= 0 (call-process "git" nil nil nil "add" "chat.greger")))
            (should (= 0 (call-process "git" nil nil nil "commit" "-m" "Initial chat"))))
          
          ;; Create chat buffer and modify it
          (setq chat-buffer (find-file-noselect chat-file))
          (with-current-buffer chat-buffer
            (goto-char (point-max))
            (insert "\nUpdated chat content"))
          
          ;; Create test file
          (setq test-file (expand-file-name "test.txt" test-dir))
          (with-temp-file test-file
            (insert "Test content"))
          
          ;; Test git stage and commit with chat buffer
          (let ((result (greger-stdlib--git-stage-and-commit 
                         (list test-file)
                         "Add test file and update chat"
                         chat-buffer)))
            (should (stringp result))
            (should (string-match "Successfully staged 2 file(s) and committed" result))
            (should (string-match "Add test file and update chat" result)))
          
          ;; Verify both files are committed
          (let ((default-directory test-dir))
            (should (= 0 (call-process "git" nil nil nil "ls-files" "--error-unmatch" "test.txt")))
            (should (= 0 (call-process "git" nil nil nil "ls-files" "--error-unmatch" "chat.greger")))))
      
      ;; Clean up
      (when chat-buffer (kill-buffer chat-buffer))
      (when (file-exists-p test-dir)
        (delete-directory test-dir t)))))

(ert-deftest greger-stdlib-test-git-stage-and-commit-with-chat-buffer-untracked ()
  "Test git staging with chat buffer that is not tracked by git."
  (let ((test-dir (make-temp-file "greger-git-untracked" t))
        (test-file nil)
        (chat-file nil)
        (chat-buffer nil))
    (unwind-protect
        (progn
          ;; Initialize git repository
          (let ((default-directory test-dir))
            (should (= 0 (call-process "git" nil nil nil "init")))
            (should (= 0 (call-process "git" nil nil nil "config" "user.name" "Test User")))
            (should (= 0 (call-process "git" nil nil nil "config" "user.email" "test@example.com"))))
          
          ;; Create chat file but don't track it
          (setq chat-file (expand-file-name "untracked-chat.greger" test-dir))
          (with-temp-file chat-file
            (insert "Untracked chat content"))
          
          ;; Create chat buffer
          (setq chat-buffer (find-file-noselect chat-file))
          
          ;; Create test file
          (setq test-file (expand-file-name "test.txt" test-dir))
          (with-temp-file test-file
            (insert "Test content"))
          
          ;; Test git stage and commit with untracked chat buffer
          (let ((result (greger-stdlib--git-stage-and-commit 
                         (list test-file)
                         "Add test file (chat untracked)"
                         chat-buffer)))
            (should (stringp result))
            ;; Should only stage the test file, not the untracked chat file
            (should (string-match "Successfully staged 1 file(s) and committed" result))
            (should (string-match "Add test file (chat untracked)" result)))
          
          ;; Verify only test file is committed
          (let ((default-directory test-dir))
            (should (= 0 (call-process "git" nil nil nil "ls-files" "--error-unmatch" "test.txt")))
            ;; Chat file should not be tracked
            (should-not (= 0 (call-process "git" nil nil nil "ls-files" "--error-unmatch" "untracked-chat.greger")))))
      
      ;; Clean up
      (when chat-buffer (kill-buffer chat-buffer))
      (when (file-exists-p test-dir)
        (delete-directory test-dir t)))))

(ert-deftest greger-stdlib-test-git-stage-and-commit-no-chat-buffer ()
  "Test git staging when no chat buffer is provided."
  (let ((test-dir (make-temp-file "greger-git-no-chat" t))
        (test-file nil))
    (unwind-protect
        (progn
          ;; Initialize git repository
          (let ((default-directory test-dir))
            (should (= 0 (call-process "git" nil nil nil "init")))
            (should (= 0 (call-process "git" nil nil nil "config" "user.name" "Test User")))
            (should (= 0 (call-process "git" nil nil nil "config" "user.email" "test@example.com"))))
          
          ;; Create test file
          (setq test-file (expand-file-name "test.txt" test-dir))
          (with-temp-file test-file
            (insert "Test content"))
          
          ;; Test git stage and commit without chat buffer
          (let ((result (greger-stdlib--git-stage-and-commit 
                         (list test-file)
                         "Add test file (no chat buffer)"
                         nil)))
            (should (stringp result))
            (should (string-match "Successfully staged 1 file(s) and committed" result))
            (should (string-match "Add test file (no chat buffer)" result)))
          
          ;; Verify file is committed
          (let ((default-directory test-dir))
            (should (= 0 (call-process "git" nil nil nil "ls-files" "--error-unmatch" "test.txt")))))
      
      ;; Clean up
      (when (file-exists-p test-dir)
        (delete-directory test-dir t)))))

(ert-deftest greger-stdlib-test-git-stage-and-commit-staging-failure ()
  "Test git staging failure handling."
  (let ((test-dir (make-temp-file "greger-git-fail" t))
        (test-file nil))
    (unwind-protect
        (progn
          ;; Initialize git repository
          (let ((default-directory test-dir))
            (should (= 0 (call-process "git" nil nil nil "init")))
            (should (= 0 (call-process "git" nil nil nil "config" "user.name" "Test User")))
            (should (= 0 (call-process "git" nil nil nil "config" "user.email" "test@example.com"))))
          
          ;; Try to stage a non-existent file
          (setq test-file (expand-file-name "nonexistent.txt" test-dir))
          
          ;; Should return error message when trying to stage non-existent file
          (let ((result (greger-stdlib--git-stage-and-commit 
                         (list test-file)
                         "This should fail - file doesn't exist")))
            (should (stringp result))
            (should (string-match "Git operation failed" result))
            (should (string-match "Failed to stage file" result))))
      
      ;; Clean up
      (when (file-exists-p test-dir)
        (delete-directory test-dir t)))))

(ert-deftest greger-stdlib-test-git-stage-and-commit-relative-paths ()
  "Test git staging with relative paths."
  (let ((test-dir (make-temp-file "greger-git-relative" t))
        (subdir nil)
        (test-file nil))
    (unwind-protect
        (progn
          ;; Initialize git repository
          (let ((default-directory test-dir))
            (should (= 0 (call-process "git" nil nil nil "init")))
            (should (= 0 (call-process "git" nil nil nil "config" "user.name" "Test User")))
            (should (= 0 (call-process "git" nil nil nil "config" "user.email" "test@example.com"))))
          
          ;; Create subdirectory and file
          (setq subdir (expand-file-name "subdir" test-dir))
          (make-directory subdir)
          (setq test-file (expand-file-name "test.txt" subdir))
          (with-temp-file test-file
            (insert "Test content in subdirectory"))
          
          ;; Test git stage and commit with file in subdirectory
          (let ((result (greger-stdlib--git-stage-and-commit 
                         (list test-file)
                         "Add file in subdirectory")))
            (should (stringp result))
            (should (string-match "Successfully staged 1 file(s) and committed" result))
            (should (string-match "Add file in subdirectory" result)))
          
          ;; Verify file is committed with relative path
          (let ((default-directory test-dir))
            (should (= 0 (call-process "git" nil nil nil "ls-files" "--error-unmatch" "subdir/test.txt")))))
      
      ;; Clean up
      (when (file-exists-p test-dir)
        (delete-directory test-dir t)))))

;;; greger-stdlib-test.el ends here
