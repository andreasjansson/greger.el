;;; greger-diff-test.el --- Tests for greger-diff.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Andreas Jansson

;; Author: Andreas Jansson <andreas@jansson.me.uk>
;; Version: 0.1.0
;; URL: https://github.com/andreasjansson/greger.el
;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Tests for greger-diff.el functions.

;;; Code:

(require 'ert)
(require 'greger-diff)
(require 'greger-parser)

(ert-deftest greger-diff-test-basic ()
  "Test basic diff and undiff functionality."
  (let* ((original "a\nb\nc\n1\n2\n3")
         (new "a\nb\nd\n4\n2\n3")
         (diff-result (greger-diff-strings original new))
         (undiff-result (greger-diff-undiff-strings diff-result)))
    
    ;; Verify the diff result contains expected diff content (headers deleted)
    (should (string-match-p "^ a" diff-result))
    (should (string-match-p "^-c" diff-result))
    (should (string-match-p "^\\+d" diff-result))
    (should (string-match-p "^ b" diff-result))
    (should (string-match-p "^-c" diff-result))
    (should (string-match-p "^\\+d" diff-result))
    
    ;; Verify undiff reconstructs the original strings correctly
    (should (string= original (car undiff-result)))
    (should (string= new (cdr undiff-result)))))

(ert-deftest greger-diff-test-empty-strings ()
  "Test diff with empty strings."
  (let* ((original "")
         (new "hello\nworld")
         (diff-result (greger-diff-strings original new))
         (undiff-result (greger-diff-undiff-strings diff-result)))
    
    ;; Verify undiff reconstructs correctly
    (should (string= original (car undiff-result)))
    (should (string= new (cdr undiff-result)))))

(ert-deftest greger-diff-test-identical-strings ()
  "Test diff with identical strings."
  (let* ((original "hello\nworld\ntest")
         (new "hello\nworld\ntest")
         (diff-result (greger-diff-strings original new))
         (undiff-result (greger-diff-undiff-strings diff-result)))
    
    ;; When strings are identical, diff should be empty or minimal
    ;; but undiff should still reconstruct correctly
    (should (string= original (car undiff-result)))
    (should (string= new (cdr undiff-result)))))

(ert-deftest greger-diff-test-single-line ()
  "Test diff with single line strings."
  (let* ((original "hello")
         (new "world")
         (diff-result (greger-diff-strings original new))
         (undiff-result (greger-diff-undiff-strings diff-result)))
    
    ;; Verify undiff reconstructs correctly
    (should (string= original (car undiff-result)))
    (should (string= new (cdr undiff-result)))))

(ert-deftest greger-diff-test-additions-only ()
  "Test diff with only additions."
  (let* ((original "line1\nline2")
         (new "line1\nline2\nline3\nline4")
         (diff-result (greger-diff-strings original new))
         (undiff-result (greger-diff-undiff-strings diff-result)))
    
    ;; Verify undiff reconstructs correctly
    (should (string= original (car undiff-result)))
    (should (string= new (cdr undiff-result)))))

(ert-deftest greger-diff-test-deletions-only ()
  "Test diff with only deletions."
  (let* ((original "line1\nline2\nline3\nline4")
         (new "line1\nline2")
         (diff-result (greger-diff-strings original new))
         (undiff-result (greger-diff-undiff-strings diff-result)))
    
    ;; Verify undiff reconstructs correctly
    (should (string= original (car undiff-result)))
    (should (string= new (cdr undiff-result)))))

(ert-deftest greger-diff-test-complex-changes ()
  "Test diff with complex mix of changes."
  (let* ((original "#!/bin/bash\necho \"hello\"\necho \"world\"\necho \"test\"\nexit 0")
         (new "#!/bin/bash\necho \"hi there\"\necho \"universe\"\necho \"testing\"\necho \"done\"\nexit 1")
         (diff-result (greger-diff-strings original new))
         (undiff-result (greger-diff-undiff-strings diff-result)))
    
    ;; Verify undiff reconstructs correctly
    (should (string= original (car undiff-result)))
    (should (string= new (cdr undiff-result)))))

(ert-deftest greger-diff-test-inverse-property ()
  "Test that diff and undiff are true inverses."
  (let ((test-cases '(
                      ("" "")
                      ("hello" "world")
                      ("a\nb\nc" "a\nb\nd")
                      ("line1\nline2\nline3" "line1\nmodified\nline3")
                      ("# Code\nfunction test() {\n  return true;\n}" "# Code\nfunction test() {\n  return false;\n}")
                      )))
    (dolist (test-case test-cases)
      (let* ((original (car test-case))
             (new (cadr test-case))
             (diff-result (greger-diff-strings original new))
             (undiff-result (greger-diff-undiff-strings diff-result)))
        
        ;; Verify the inverse property holds
        (should (string= original (car undiff-result)))
        (should (string= new (cdr undiff-result)))))))

(ert-deftest greger-diff-test-parser-integration ()
  "Test integration with greger-parser for str-replace tools."
  (let* ((original-content "function hello() {\n  return \"hello\";\n}")
         (new-content "function hello() {\n  return \"hi there\";\n}")
         (tool-use `((name . "str-replace")
                     (id . "test-123")
                     (input . ((path . "test.js")
                               (original-content . ,original-content)
                               (new-content . ,new-content)))))
         ;; Convert to markdown
         (markdown (greger-parser--tool-use-to-markdown tool-use)))
    
    ;; The markdown should contain a diff parameter instead of original/new content
    (should (string-match-p "## diff" markdown))
    (should (string-match-p "## path" markdown))
    (should-not (string-match-p "## original-content" markdown))
    (should-not (string-match-p "## new-content" markdown))
    
    ;; Parse the markdown back to verify round-trip
    (let* ((parsed (greger-parser-markdown-to-dialog markdown))
           (content (alist-get 'content (car parsed)))
           (tool-content (car content))
           (input (alist-get 'input tool-content)))
      
      ;; Verify the parsed result has original-content and new-content back
      (should (alist-get 'original-content input))
      (should (alist-get 'new-content input))
      (should (string= original-content (alist-get 'original-content input)))
      (should (string= new-content (alist-get 'new-content input))))))

(ert-deftest greger-diff-test-non-str-replace-unchanged ()
  "Test that non-str-replace tools are unchanged by diff processing."
  (let* ((tool-use `((name . "shell-command")
                     (id . "test-456")
                     (input . ((command . "ls -la")
                               (working-directory . "/tmp")))))
         ;; Convert to markdown
         (markdown (greger-parser--tool-use-to-markdown tool-use)))
    
    ;; The markdown should not be affected by diff processing
    (should (string-match-p "## command" markdown))
    (should (string-match-p "## working-directory" markdown))
    (should-not (string-match-p "## diff" markdown))
    
    ;; Parse the markdown back to verify it's unchanged
    (let* ((parsed (greger-parser-markdown-to-dialog markdown))
           (content (alist-get 'content (car parsed)))
           (tool-content (car content))
           (input (alist-get 'input tool-content)))
      
      ;; Verify the parsed result is unchanged
      (should (string= "ls -la" (alist-get 'command input)))
      (should (string= "/tmp" (alist-get 'working-directory input))))))

(ert-deftest greger-diff-test-empty-diff-string ()
  "Test that empty diff string returns empty strings instead of error."
  (let ((result (greger-diff-undiff-strings "")))
    (should (consp result))
    (should (string= "" (car result)))
    (should (string= "" (cdr result))))
  
  ;; Test with whitespace-only string
  (let ((result (greger-diff-undiff-strings "   \n  \t  ")))
    (should (consp result))
    (should (string= "" (car result)))
    (should (string= "" (cdr result)))))

(ert-deftest greger-diff-test-fontification ()
  "Test that diff strings are properly fontified."
  (let* ((original "hello\nworld")
         (new "hello\nmodified")
         (diff-result (greger-diff-strings original new)))
    
    ;; Should contain diff content (headers deleted)
    (should (string-match-p "^ hello" diff-result))
    (should (string-match-p "^-world" diff-result))
    (should (string-match-p "^\\+modified" diff-result))
    
    ;; Should have fontification properties
    (should (not (null (text-property-not-all 0 (length diff-result) 'font-lock-face nil diff-result))))
    
    ;; Round-trip should work and return clean strings
    (let ((undiff-result (greger-diff-undiff-strings diff-result)))
      (should (string= original (car undiff-result)))
      (should (string= new (cdr undiff-result)))
      
      ;; Undiffed strings should be clean (no text properties)
      (should (= 0 (length (text-properties-at 0 (car undiff-result)))))
      (should (= 0 (length (text-properties-at 0 (cdr undiff-result)))))))
  
  ;; Test identical strings also get fontified
  (let* ((original "hello")
         (new "hello")
         (diff-result (greger-diff-strings original new)))
    (should (text-property-any 0 (length diff-result) 'font-lock-face nil diff-result))))

(ert-deftest greger-diff-test-invisible-metadata ()
  "Test that diff headers are deleted and 'No newline' messages are made invisible."
  ;; Test with strings that don't end with newlines
  (let* ((original "hello")
         (new "world") 
         (diff-result (greger-diff-strings original new)))
    
    ;; Headers should be deleted (not present)
    (should-not (string-match-p "^--- " diff-result))
    (should-not (string-match-p "^\\+\\+\\+ " diff-result))
    (should-not (string-match-p "^@@.*@@" diff-result))
    
    ;; Should start directly with diff content
    (should (string-match-p "^-hello" diff-result))
    (should (string-match-p "^\\+world" diff-result))
    
    ;; Should contain the "No newline" message and it should be invisible
    (should (string-match-p "\\\\ No newline at end of file" diff-result))
    (let ((pos (string-match "\\\\ No newline at end of file" diff-result)))
      (should pos)
      (should (get-text-property pos 'invisible diff-result))))
  
  ;; Test with strings that end with newlines (should not have "No newline" message)
  (let* ((original "hello\n")
         (new "world\n")
         (diff-result (greger-diff-strings original new)))
    ;; Should NOT contain the "No newline" message
    (should-not (string-match-p "\\\\ No newline at end of file" diff-result))
    
    ;; Headers should still be deleted
    (should-not (string-match-p "^---" diff-result))))

(provide 'greger-diff-test)
;;; greger-diff-test.el ends here
