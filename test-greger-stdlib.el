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
    (let ((result (greger-tools--read-webpage test-url t)))
      (should (stringp result))
      (should (string= "Hello world!\n" result)))

    ;; Test with raw HTML
    (let ((result (greger-tools--read-webpage test-url nil)))
      (should (stringp result))
      (should (string= "<h1>Hello world!</h1>\n" result)))

    ;; Test with readability enhancement
    (let ((result (greger-tools--read-webpage test-url t t)))
      (should (stringp result))
      (should (string= "Hello world!\n" result)))))

(ert-deftest greger-test-read-webpage-invalid-url ()
  "Test reading a webpage with invalid URLs."
  ;; Test empty URL
  (should-error (greger-tools--read-webpage ""))
  (should-error (greger-tools--read-webpage "   "))

  ;; Test non-string URL
  (should-error (greger-tools--read-webpage nil))
  (should-error (greger-tools--read-webpage 123))

  ;; Test invalid URL format
  (should-error (greger-tools--read-webpage "ftp://example.com"))
  (should-error (greger-tools--read-webpage "not-a-url"))
  (should-error (greger-tools--read-webpage "file:///path/to/file")))

(ert-deftest greger-test-read-webpage-network-error ()
  "Test reading a webpage when network error occurs."
  ;; Mock the web download function to simulate network error
  (cl-letf (((symbol-function 'greger-web-download-page)
             (lambda (url extract-text use-highest-readability)
               (error "Network timeout"))))

    (let ((result (greger-tools--read-webpage "https://pub-b88c9764a4fc46baa90b9e8e1544f59e.r2.dev/hello.html")))
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
      (greger-tools--shell-command
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
      (greger-tools--shell-command
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
      (greger-tools--shell-command
       "echo test"
       (lambda (output err)
         (setq result output error err callback-called t)))

      ;; Should call callback immediately with error
      (should callback-called)
      (should (null result))
      (should (stringp error))
      (should (string-match "cancelled by user" error)))))

;;; greger-test-stdlib.el ends here
