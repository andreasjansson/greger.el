;;; greger-test-stdlib.el --- Tests for greger stdlib tools -*- lexical-binding: t -*-

;;; Commentary:
;; Tests for the tools defined in greger-stdlib.el

;;; Code:

(require 'ert)
(require 'greger-stdlib)
(require 'greger-web)

(ert-deftest greger-test-read-webpage-valid-url ()
  "Test reading a webpage with a valid URL."
  ;; Mock the web download function to avoid actual network calls in tests
  (cl-letf (((symbol-function 'greger-web-download-page)
             (lambda (url extract-text use-highest-readability)
               (if extract-text
                   "This is sample extracted text from the webpage."
                 "<html><body>This is sample HTML content.</body></html>"))))

    ;; Test with text extraction (default)
    (let ((result (greger-tools--read-webpage "https://example.com")))
      (should (stringp result))
      (should (string-match-p "Successfully read and extracted text" result))
      (should (string-match-p "This is sample extracted text" result)))

    ;; Test with raw HTML
    (let ((result (greger-tools--read-webpage "https://example.com" nil)))
      (should (stringp result))
      (should (string-match-p "Successfully read raw HTML" result))
      (should (string-match-p "<html><body>" result)))

    ;; Test with readability enhancement
    (let ((result (greger-tools--read-webpage "https://example.com" t t)))
      (should (stringp result))
      (should (string-match-p "Successfully read and extracted text" result)))))

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

    (let ((result (greger-tools--read-webpage "https://example.com")))
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

;;; greger-test-stdlib.el ends here
