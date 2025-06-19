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

(ert-deftest greger-diff-test-basic ()
  "Test basic diff and undiff functionality."
  (let* ((original "a\nb\nc\n1\n2\n3")
         (new "a\nb\nd\n4\n2\n3")
         (diff-result (greger-diff-strings original new))
         (undiff-result (greger-diff-undiff-strings diff-result)))
    
    ;; Verify the diff result contains expected diff format
    (should (string-match-p "^---" diff-result))
    (should (string-match-p "^\\+\\+\\+" diff-result))
    (should (string-match-p "^@@" diff-result))
    (should (string-match-p "^ a" diff-result))
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
        (should (string= original (car undiff-result))
                (format "Original reconstruction failed for case: %S" test-case))
        (should (string= new (cdr undiff-result))
                (format "New reconstruction failed for case: %S" test-case))))))

(provide 'greger-diff-test)
;;; greger-diff-test.el ends here
