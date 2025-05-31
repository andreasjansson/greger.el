;;; test-greger-lib-lsp.el --- Tests for greger LSP tools -*- lexical-binding: t -*-

;;; Commentary:
;; Tests for the LSP-powered tools defined in greger-lib-lsp.el

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'greger-lib-lsp)
(require 'lsp-mode)
(require 'lsp-pyright)  ; Python LSP server

;;; Test utilities

(defvar greger-lsp-test-temp-dir nil
  "Temporary directory for LSP tests.")

(defvar greger-lsp-test-python-file nil
  "Path to test Python file.")

(defvar greger-lsp-test-project-root nil
  "Root directory of test project.")

(defun greger-lsp-test-setup ()
  "Set up test environment with temporary Python project."
  (setq greger-lsp-test-temp-dir (make-temp-file "greger-lsp-test-" t))
  (setq greger-lsp-test-project-root greger-lsp-test-temp-dir)

  ;; Ensure we have a clean LSP session
  (when (bound-and-true-p lsp--session)
    ;; Clear any existing workspace folders that might interfere
    (setf (lsp-session-folders lsp--session)
          (cl-remove-if (lambda (folder)
                         (string-prefix-p "/tmp" folder))
                        (lsp-session-folders lsp--session))))

  ;; Create a simple Python project structure
  (let ((src-dir (file-name-as-directory (expand-file-name "src" greger-lsp-test-temp-dir))))
    (make-directory src-dir)

    ;; Create main.py with some example code
    (setq greger-lsp-test-python-file (expand-file-name "main.py" src-dir))
    (with-temp-file greger-lsp-test-python-file
      (insert "#!/usr/bin/env python3
\"\"\"Example Python module for testing LSP tools.\"\"\"

import os
import sys
from typing import List, Optional


class Calculator:
    \"\"\"A simple calculator class.\"\"\"

    def __init__(self, precision: int = 2):
        self.precision = precision
        self.history: List[str] = []

    def add_numbers(self, a: float, b: float) -> float:
        \"\"\"Add two numbers together.\"\"\"
        result = a + b
        self.history.append(f\"{a} + {b} = {result}\")
        return round(result, self.precision)

    def multiply_numbers(self, a: float, b: float) -> float:
        \"\"\"Multiply two numbers.\"\"\"
        result = a * b
        self.history.append(f\"{a} * {b} = {result}\")
        return round(result, self.precision)

    def get_history(self) -> List[str]:
        \"\"\"Get calculation history.\"\"\"
        return self.history.copy()


def create_calculator(precision: Optional[int] = None) -> Calculator:
    \"\"\"Factory function to create a calculator.\"\"\"
    if precision is None:
        precision = 2
    return Calculator(precision)


def main():
    \"\"\"Main function for testing.\"\"\"
    calc = create_calculator(3)
    result1 = calc.add_numbers(10.5, 20.3)
    result2 = calc.multiply_numbers(result1, 2.0)

    print(f\"Final result: {result2}\")
    print(\"History:\")
    for entry in calc.get_history():
        print(f\"  {entry}\")


if __name__ == \"__main__\":
    main()
"))

    ;; Create utils.py for reference testing
    (let ((utils-file (expand-file-name "utils.py" src-dir)))
      (with-temp-file utils-file
        (insert "#!/usr/bin/env python3
\"\"\"Utility functions.\"\"\"

from main import Calculator, create_calculator


def advanced_calculation(x: float, y: float) -> float:
    \"\"\"Perform advanced calculation using Calculator.\"\"\"
    calc = create_calculator(4)
    intermediate = calc.add_numbers(x, y)
    return calc.multiply_numbers(intermediate, 1.5)


def format_result(value: float) -> str:
    \"\"\"Format a calculation result.\"\"\"
    return f\"Result: {value:.2f}\"
")))

    ;; Create a simple pyproject.toml for LSP to recognize the project
    (with-temp-file (expand-file-name "pyproject.toml" greger-lsp-test-temp-dir)
      (insert "[project]
name = \"greger-lsp-test\"
version = \"0.1.0\"
description = \"Test project for greger LSP tools\"
"))))

(defun greger-lsp-test-teardown ()
  "Clean up test environment."
  (when greger-lsp-test-temp-dir
    ;; Clean up LSP session
    (when (bound-and-true-p lsp--session)
      (condition-case nil
          (lsp-workspace-folders-remove greger-lsp-test-project-root)
        (error nil)))

    ;; Kill any buffers visiting test files
    (when greger-lsp-test-python-file
      (let ((buffer (get-file-buffer greger-lsp-test-python-file)))
        (when buffer
          (with-current-buffer buffer
            (when (bound-and-true-p lsp-mode)
              (condition-case nil (lsp-disconnect) (error nil))))
          (kill-buffer buffer))))

    ;; Remove temp directory
    (delete-directory greger-lsp-test-temp-dir t)
    (setq greger-lsp-test-temp-dir nil
          greger-lsp-test-python-file nil
          greger-lsp-test-project-root nil)))

(defun greger-lsp-test-ensure-lsp-started ()
  "Ensure LSP is started for the test Python file."
  (let ((buffer (find-file-noselect greger-lsp-test-python-file)))
    (with-current-buffer buffer
      (python-mode)

      ;; Add the test project to LSP workspace folders to avoid prompts
      (unless (member greger-lsp-test-project-root (lsp-session-folders (lsp-session)))
        (lsp-workspace-folders-add greger-lsp-test-project-root))

      ;; Temporarily disable lsp prompts and UI features
      (let ((lsp-auto-guess-root t)
            (lsp-enable-file-watchers nil)
            (lsp-signature-auto-activate nil)
            (lsp-eldoc-enable-hover nil)
            (lsp-enable-symbol-highlighting nil)
            (lsp-headerline-breadcrumb-enable nil)
            (lsp-ui-doc-enable nil)
            (lsp-ui-sideline-enable nil)
            (lsp-restart 'ignore)  ; Don't prompt for restart
            (lsp-warn-no-matched-clients nil)) ; Don't warn about no clients

        ;; Mock the project root finder to return our test directory
        (cl-letf (((symbol-function 'lsp--calculate-root)
                   (lambda (session file-name)
                     greger-lsp-test-project-root))
                  ((symbol-function 'lsp--suggest-project-root)
                   (lambda ()
                     greger-lsp-test-project-root))
                  ((symbol-function 'lsp--find-root-interactively)
                   (lambda (session)
                     greger-lsp-test-project-root)))

          ;; Start LSP
          (condition-case err
              (progn
                (lsp)
                ;; Wait for LSP to initialize with longer timeout
                (let ((timeout 0))
                  (while (and (not lsp--buffer-workspaces) (< timeout 200))
                    (sit-for 0.1)
                    (setq timeout (1+ timeout))))
                (unless lsp--buffer-workspaces
                  (error "Failed to start LSP server for test")))
            (error
             (message "LSP startup error: %s" (error-message-string err))
             (error "Failed to start LSP server for test: %s" (error-message-string err)))))))
    buffer))

;;; Helper functions for test requirements

(defun greger-lsp-test-requirements-met-p ()
  "Check if requirements for LSP tests are met."
  (and (fboundp 'lsp)
       (fboundp 'python-mode)
       (or (executable-find "pyright")
           (executable-find "pylsp")
           (executable-find "python-lsp-server"))))

(defun greger-lsp-test-skip-if-requirements-not-met ()
  "Skip test if LSP requirements are not met."
  (unless (greger-lsp-test-requirements-met-p)
    (ert-skip "LSP mode or Python LSP server not available")))

;;; Helper macros

(defmacro greger-lsp-test-with-setup (&rest body)
  "Execute BODY with LSP test setup and teardown."
  `(progn
     (greger-lsp-test-skip-if-requirements-not-met)
     (unwind-protect
         (progn
           (greger-lsp-test-setup)
           (greger-lsp-test-ensure-lsp-started)
           ,@body)
       (greger-lsp-test-teardown))))

;;; Tests for helper functions

(ert-deftest greger-lsp-test-ensure-server ()
  "Test LSP server initialization helper."
  (greger-lsp-test-with-setup
   (let ((buffer (greger-lsp--ensure-server greger-lsp-test-python-file)))
     (should (bufferp buffer))
     (with-current-buffer buffer
       (should (bound-and-true-p lsp-mode))
       (should lsp--buffer-workspaces)))))

(ert-deftest greger-lsp-test-with-buffer-at-position ()
  "Test executing code at specific buffer position."
  (greger-lsp-test-with-setup
   (let ((result nil))
     (greger-lsp--with-buffer-at-position
      greger-lsp-test-python-file 10 0  ; Line with class definition
      (lambda ()
        (setq result (thing-at-point 'symbol))))
     (should (string= result "class")))))

;;; Tests for lsp-rename tool

(ert-deftest greger-lsp-test-rename-success ()
  "Test successful symbol rename."
  (greger-lsp-test-with-setup
   ;; Rename the Calculator class to MathCalculator
   (let ((result (greger-tools--lsp-rename
                  "MathCalculator"
                  greger-lsp-test-python-file
                  9 0)))  ; Line with "class Calculator:"
     (should (stringp result))
     (should (string-match-p "Successfully renamed" result))
     (should (string-match-p "Calculator.*MathCalculator" result))

     ;; Verify the file was actually changed
     (with-temp-buffer
       (insert-file-contents greger-lsp-test-python-file)
       (goto-char (point-min))
       (should (search-forward "class MathCalculator:" nil t))))))

(ert-deftest greger-lsp-test-rename-invalid-symbol ()
  "Test rename on invalid symbol position."
  (greger-lsp-test-with-setup
   ;; Try to rename at a comment line
   (let ((result (greger-tools--lsp-rename
                  "NewName"
                  greger-lsp-test-python-file
                  3 0)))  ; Line with comment
     (should (stringp result))
     (should (or (string-match-p "No changes made" result)
             (string-match-p "failed" result))))))

;;; Tests for lsp-format tool

(ert-deftest greger-lsp-test-format-file ()
  "Test formatting entire file."
  (greger-lsp-test-with-setup
   ;; First, mess up the formatting
   (with-current-buffer (find-file-noselect greger-lsp-test-python-file)
     (goto-char (point-min))
     (search-forward "def add_numbers")
     (beginning-of-line)
     (insert "   ")  ; Add extra indentation
     (save-buffer))

   ;; Now format the file
   (let ((result (greger-tools--lsp-format greger-lsp-test-python-file)))
     (should (stringp result))
     (should (or (string-match-p "Successfully formatted" result)
             (string-match-p "No formatting changes needed" result))))))

(ert-deftest greger-lsp-test-format-range ()
  "Test formatting a specific range."
  (greger-lsp-test-with-setup
   (let ((result (greger-tools--lsp-format
                  greger-lsp-test-python-file
                  10 15)))  ; Format lines 10-15
     (should (stringp result))
     (should (or (string-match-p "Successfully formatted" result)
             (string-match-p "No formatting changes needed" result)
             (string-match-p "does not support range formatting" result))))))

;;; Tests for lsp-find-definition tool

(ert-deftest greger-lsp-test-find-definition ()
  "Test finding symbol definition."
  (greger-lsp-test-with-setup
   ;; Find definition of Calculator usage
   (let ((result (greger-tools--lsp-find-definition
                  greger-lsp-test-python-file
                  50 11)))  ; Line with "calc = create_calculator"
     (should (stringp result))
     (should (string-match-p "Definition.*create_calculator" result))
     (should (string-match-p "main.py:" result)))))

(ert-deftest greger-lsp-test-find-definition-with-declaration ()
  "Test finding definition with declarations."
  (greger-lsp-test-with-setup
   (let ((result (greger-tools--lsp-find-definition
                  greger-lsp-test-python-file
                  50 11  ; Line with "calc = create_calculator"
                  t)))   ; Include declarations
     (should (stringp result))
     (should (string-match-p "Definition.*create_calculator" result)))))

;;; Tests for lsp-find-references tool

(ert-deftest greger-lsp-test-find-references ()
  "Test finding symbol references."
  (greger-lsp-test-with-setup
   ;; Find references to Calculator class
   (let ((result (greger-tools--lsp-find-references
                  greger-lsp-test-python-file
                  9 6)))  ; Line with "class Calculator", column at "Calculator"
     (should (stringp result))
     (should (string-match-p "References.*Calculator" result))
     (should (string-match-p "main.py:" result)))))

(ert-deftest greger-lsp-test-find-references-limited ()
  "Test finding references with result limit."
  (greger-lsp-test-with-setup
   (let ((result (greger-tools--lsp-find-references
                  greger-lsp-test-python-file
                  9 6      ; Calculator class
                  t        ; Include declaration
                  5)))     ; Max 5 results
     (should (stringp result))
     (should (string-match-p "References.*Calculator" result)))))

;;; Tests for lsp-document-symbols tool

(ert-deftest greger-lsp-test-document-symbols ()
  "Test getting document symbols."
  (greger-lsp-test-with-setup
   (let ((result (greger-tools--lsp-document-symbols greger-lsp-test-python-file)))
     (should (stringp result))
     (should (string-match-p "Document symbols.*main.py" result))
     (should (string-match-p "Calculator.*Class" result))
     (should (string-match-p "add_numbers.*Method\\|Function" result))
     (should (string-match-p "main.*Function" result)))))

(ert-deftest greger-lsp-test-document-symbols-filtered ()
  "Test getting filtered document symbols."
  (greger-lsp-test-with-setup
   ;; Filter for only classes
   (let ((result (greger-tools--lsp-document-symbols
                  greger-lsp-test-python-file
                  "Class")))
     (should (stringp result))
     (should (string-match-p "Calculator.*Class" result))
     ;; Should not contain functions if filtering works
     (should-not (string-match-p "main.*Function" result)))))

(ert-deftest greger-lsp-test-document-symbols-hierarchical ()
  "Test getting hierarchical document symbols."
  (greger-lsp-test-with-setup
   (let ((result (greger-tools--lsp-document-symbols
                  greger-lsp-test-python-file
                  nil  ; No type filter
                  t))) ; Hierarchical
     (should (stringp result))
     (should (string-match-p "Calculator.*Class" result))
     ;; Methods should be indented under the class
     (should (string-match-p "  add_numbers.*Method\\|Function" result)))))

;;; Tests for lsp-workspace-symbols tool

(ert-deftest greger-lsp-test-workspace-symbols ()
  "Test searching workspace symbols."
  (greger-lsp-test-with-setup
   (let ((result (greger-tools--lsp-workspace-symbols "Calculator")))
     (should (stringp result))
     (should (string-match-p "Workspace symbols.*Calculator" result))
     (should (string-match-p "Calculator.*Class" result))
     (should (string-match-p "main.py:" result)))))

(ert-deftest greger-lsp-test-workspace-symbols-limited ()
  "Test searching workspace symbols with limits."
  (greger-lsp-test-with-setup
   (let ((result (greger-tools--lsp-workspace-symbols
                  "add"  ; Search for "add"
                  3)))   ; Max 3 results
     (should (stringp result))
     (should (string-match-p "Workspace symbols.*add" result)))))

(ert-deftest greger-lsp-test-workspace-symbols-typed ()
  "Test searching workspace symbols by type."
  (greger-lsp-test-with-setup
   (let ((result (greger-tools--lsp-workspace-symbols
                  "main"     ; Search for "main"
                  nil        ; No result limit
                  "Function"))) ; Only functions
     (should (stringp result))
     (should (string-match-p "Workspace symbols.*main.*Function" result)))))

;;; Tests for error handling

(ert-deftest greger-lsp-test-no-lsp-server ()
  "Test behavior when LSP server is not available."
  ;; Create a temporary file without starting LSP
  (let ((temp-file (make-temp-file "test" nil ".py")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert "# Simple Python file\nprint('hello')\n"))

          ;; Should gracefully handle missing LSP
          (let ((result (greger-tools--lsp-rename "new_name" temp-file 2 0)))
            (should (stringp result))
            (should (string-match-p "failed\\|not available" result))))
      (delete-file temp-file))))

(ert-deftest greger-lsp-test-invalid-file ()
  "Test behavior with invalid file path."
  (let ((result (greger-tools--lsp-find-definition
                 "/nonexistent/file.py" 1 0)))
    (should (stringp result))
    (should (string-match-p "failed\\|not available\\|No such file" result))))

(ert-deftest greger-lsp-test-unsupported-feature ()
  "Test behavior when LSP server doesn't support a feature."
  (greger-lsp-test-with-setup
   ;; Mock feature detection to return false
   (cl-letf (((symbol-function 'greger-lsp--feature-supported-p)
              (lambda (method) nil)))
     (let ((result (greger-tools--lsp-rename "newname" greger-lsp-test-python-file 9 6)))
       (should (stringp result))
       (should (string-match-p "does not support" result))))))

;;; Integration tests

(ert-deftest greger-lsp-test-rename-and-find-references ()
  "Integration test: rename a symbol and verify references are updated."
  (greger-lsp-test-with-setup
   ;; First, find references to the original name
   (let ((original-refs (greger-tools--lsp-find-references
                         greger-lsp-test-python-file 9 6))) ; Calculator class
     (should (string-match-p "Calculator" original-refs))

     ;; Rename the symbol
     (greger-tools--lsp-rename "MathEngine" greger-lsp-test-python-file 9 6)

     ;; Find references to the new name (after a brief delay for LSP to update)
     (sit-for 0.5)
     (let ((new-refs (greger-tools--lsp-find-references
                      greger-lsp-test-python-file 9 6)))
       (should (string-match-p "MathEngine" new-refs))))))

(ert-deftest greger-lsp-test-cross-file-references ()
  "Test finding references across multiple files."
  (greger-lsp-test-with-setup
   ;; Create the utils.py file with references to Calculator
   (let ((utils-file (expand-file-name "src/utils.py" greger-lsp-test-project-root)))
     ;; Find references to create_calculator (used in both files)
     (let ((result (greger-tools--lsp-find-references
                    greger-lsp-test-python-file
                    45 11))) ; Line with create_calculator definition
       (should (stringp result))
       (should (string-match-p "create_calculator" result))
       ;; Should find references in both main.py and utils.py
       (should (string-match-p "main.py" result))))))

(provide 'test-greger-lib-lsp)

;;; test-greger-lib-lsp.el ends here
