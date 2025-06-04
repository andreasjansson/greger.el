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

(ert-deftest greger-test-delete-files-basic ()
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

(ert-deftest greger-test-delete-files-list-input ()
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

            ;; Test successful deletion with list input
            (let ((result (greger-stdlib--delete-files
                          (list test-file)
                          "Test commit message")))
              (should (stringp result))
              (should (string-match "Successfully deleted 1 file" result))
              (should (string-match (file-name-nondirectory test-file) result)))

            ;; Verify file is deleted
            (should-not (file-exists-p test-file))))

      ;; Clean up any remaining files
      (when (file-exists-p test-file)
        (delete-file test-file)))))

(ert-deftest greger-test-delete-files-nonexistent-file ()
  "Test delete-files with non-existent file."
  (let ((nonexistent-file "/tmp/does-not-exist.txt"))

    ;; Ensure file doesn't exist
    (should-not (file-exists-p nonexistent-file))

    ;; Mock git operations to avoid actual git commits
    (cl-letf (((symbol-function 'greger-stdlib--git-stage-and-commit)
               (lambda (files commit-message buffer) "Mocked git result")))

      ;; Should error when trying to delete non-existent file
      (should-error (greger-stdlib--delete-files
                    (list nonexistent-file)
                    "Test commit message")
                   :type 'error))))

(ert-deftest greger-test-delete-files-directory ()
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
                          (list test-dir)
                          "Test commit message")
                         :type 'error)))

      ;; Clean up
      (when (file-exists-p test-dir)
        (delete-directory test-dir)))))

(ert-deftest greger-test-delete-files-invalid-input ()
  "Test delete-files with invalid input types."
  ;; Mock git operations to avoid actual git commits
  (cl-letf (((symbol-function 'greger-stdlib--git-stage-and-commit)
             (lambda (files commit-message buffer) "Mocked git result")))

    ;; Test with non-string file paths
    (should-error (greger-stdlib--delete-files
                  (list 123)
                  "Test commit message")
                 :type 'error)

    ;; Test with non-string/non-list file-paths
    (should-error (greger-stdlib--delete-files
                  "not-a-list"
                  "Test commit message")
                 :type 'error)

    ;; Test with non-string commit message
    (should-error (greger-stdlib--delete-files
                  (list "/tmp/test.txt")
                  123)
                 :type 'error)))

(ert-deftest greger-test-delete-files-git-tracking ()
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
                          (list test-file)
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

(ert-deftest greger-test-delete-files-not-git-tracked ()
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
                          (list test-file)
                          "Delete test file")))
              (should (stringp result))
              (should (string-match "Successfully deleted 1 file" result))
              (should (string-match "No files were tracked by git" result))
              (should-not git-stage-called)
              (should-not (file-exists-p test-file)))))

      ;; Clean up any remaining files
      (when (file-exists-p test-file)
        (delete-file test-file)))))

(ert-deftest greger-test-read-file-basic ()
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
          (let ((result (greger-stdlib--read-file test-file))
                (expected "Line 1
Line 2
Line 3"))
            (should (stringp result))
            (should (string= result expected)))

          ;; Test reading with line numbers
          (let ((result (greger-stdlib--read-file test-file t))
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

(ert-deftest greger-test-read-file-line-number-formatting ()
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

(ert-deftest greger-test-read-file-error-cases ()
  "Test error handling in read-file function."
  ;; Test non-existent file
  (should-error (greger-stdlib--read-file "/path/that/does/not/exist"))

  ;; Test invalid path type
  (should-error (greger-stdlib--read-file 123))

  ;; Test invalid start-line type
  (should-error (greger-stdlib--read-file "test-file" nil "not-a-number"))

  ;; Test invalid end-line type
  (should-error (greger-stdlib--read-file "test-file" nil 1 "not-a-number"))

  ;; Test invalid start-line value
  (should-error (greger-stdlib--read-file "test-file" nil 0))

  ;; Test invalid end-line value
  (should-error (greger-stdlib--read-file "test-file" nil 1 0))

  ;; Test start-line > end-line
  (should-error (greger-stdlib--read-file "test-file" nil 5 3)))

(ert-deftest greger-test-list-directory-basic ()
  "Test basic list-directory functionality with detailed output."
  (let ((test-dir (make-temp-file "greger-test-dir" t)))
    (unwind-protect
        (progn
          ;; Create a test file
          (let ((test-file (expand-file-name "test.txt" test-dir)))
            (with-temp-file test-file
              (insert "content"))

            ;; Test basic listing
            (let ((result (greger-stdlib--list-directory test-dir))
                  (expected "-rw-r--r--        7  test.txt"))
              (should (stringp result))
              (should (string= expected result)))))

      ;; Clean up
      (when (file-exists-p test-dir)
        (delete-directory test-dir t)))))

(ert-deftest greger-test-list-directory-exclude-pattern ()
  "Test list-directory exclude pattern functionality."
  (let ((test-dir (make-temp-file "greger-test-dir" t)))
    (unwind-protect
        (progn
          ;; Create test files
          (let ((include-file (expand-file-name "keep.txt" test-dir))
                (exclude-file (expand-file-name "remove.log" test-dir)))

            (with-temp-file include-file
              (insert "keep this"))
            (with-temp-file exclude-file
              (insert "exclude this"))

            ;; Test with exclude pattern for .log files
            (let ((result (greger-stdlib--list-directory test-dir "\\.log$"))
                  (expected "-rw-r--r--        9  keep.txt"))
              (should (stringp result))
              (should (string= expected result)))

            ;; Test with empty exclude pattern - should include all files
            (let ((result (greger-stdlib--list-directory test-dir ""))
                  (expected "-rw-r--r--        9  keep.txt
-rw-r--r--       12  remove.log"))
              (should (stringp result))
              (should (string= expected result)))))

      ;; Clean up
      (when (file-exists-p test-dir)
        (delete-directory test-dir t)))))

(ert-deftest greger-test-list-directory-recursive ()
  "Test list-directory recursive functionality."
  (let ((test-dir (make-temp-file "greger-test-dir" t)))
    (unwind-protect
        (progn
          ;; Create simple nested directory structure
          (let ((subdir (expand-file-name "testdir" test-dir))
                (file1 (expand-file-name "root.txt" test-dir))
                (file2 (expand-file-name "testdir/nested.txt" test-dir)))

            (make-directory subdir)
            (with-temp-file file1 (insert "Root content"))
            (with-temp-file file2 (insert "Nested content"))

            ;; Test recursive listing
            (let ((result (greger-stdlib--list-directory test-dir nil t))
                  (expected (format "
testdir/testdir:
-rw-r--r--       14  nested.txt
%s:
-rw-r--r--       12  root.txt
drwxr-xr-x       96  testdir" (file-name-as-directory test-dir))))
              (should (stringp result))
              (should (string= expected result)))))

      ;; Clean up
      (when (file-exists-p test-dir)
        (delete-directory test-dir t)))))

(ert-deftest greger-test-list-directory-error-cases ()
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

(ert-deftest greger-test-list-directory-empty-dir ()
  "Test list-directory with empty directory."
  (let ((test-dir (make-temp-file "greger-test-empty-dir" t)))
    (unwind-protect
        (progn
          ;; Test empty directory - should return empty directory message
          (let ((result (greger-stdlib--list-directory test-dir))
                (expected "Directory is empty"))
            (should (stringp result))
            (should (string= expected result))))

      ;; Clean up
      (when (file-exists-p test-dir)
        (delete-directory test-dir)))))

(ert-deftest greger-test-list-directory-file-mode-string ()
  "Test the file mode string formatting function."
  (let ((temp-dir (make-temp-file "greger-test-mode" t))
        (temp-file (make-temp-file "greger-test-file")))
    (unwind-protect
        (progn
          ;; Test directory formatting
          (let ((dir-info (greger-stdlib--format-file-info temp-dir "testdir" "nomatch"))
                (expected "drwx------       64  testdir"))
            (should (stringp dir-info))
            (should (string= expected dir-info)))

          ;; Test file formatting
          (let ((file-info (greger-stdlib--format-file-info temp-file "testfile" "nomatch"))
                (expected "-rw-------        0  testfile"))
            (should (stringp file-info))
            (should (string= expected file-info))))

      ;; Clean up
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(ert-deftest greger-test-list-directory-hidden-files ()
  "Test list-directory handling of hidden files with exclude patterns."
  (let ((test-dir (make-temp-file "greger-test-hidden" t)))
    (unwind-protect
        (progn
          ;; Create hidden and normal files
          (let ((hidden-file (expand-file-name ".hiddenfile" test-dir))
                (normal-file (expand-file-name "normalfile.txt" test-dir)))

            (with-temp-file hidden-file (insert "Hidden content"))
            (with-temp-file normal-file (insert "Normal content"))

            ;; Test with no exclude pattern (should show all files including hidden)
            (let ((result (greger-stdlib--list-directory test-dir ""))
                  (expected "-rw-r--r--       14  .hiddenfile
-rw-r--r--       14  normalfile.txt"))
              (should (stringp result))
              (should (string= expected result)))

            ;; Test with pattern excluding hidden files (starting with .)
            (let ((result (greger-stdlib--list-directory test-dir "^\\.")))
              (should (stringp result))
              (let ((expected "-rw-r--r--       14  normalfile.txt"))
                (should (string= expected result))))))

      ;; Clean up
      (when (file-exists-p test-dir)
        (delete-directory test-dir t)))))

;;; greger-test-stdlib.el ends here
