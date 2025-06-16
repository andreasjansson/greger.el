;;; greger-ui-test.el --- Tests for greger tools -*- lexical-binding: t -*-

(require 'ert)
(require 'greger-ui)
(require 'greger)

(defun greger-ui-test--visible-text ()
  "Extract only the visible text from the current buffer.
Text with the 'invisible property set to t is excluded."
  (let ((result "")
        (pos (point-min)))
    (while (< pos (point-max))
      (let* ((next-change (next-single-property-change pos 'invisible nil (point-max)))
             (invisible (get-text-property pos 'invisible)))
        (unless invisible
          (setq result (concat result (buffer-substring-no-properties pos next-change))))
        (setq pos next-change)))
    result))

(ert-deftest greger-ui-test-citations-folding ()
  (with-current-buffer (greger)
    (insert "# ASSISTANT

Einstein developed the theory of relativity

## https://physics.com/einstein

Title: Einstein
Cited text: Albert Einstein developed the theory of relativity in the early 20th century...
Encrypted index: def456

# ASSISTANT

 while

# ASSISTANT

 Newton formulated the laws of motion

## https://physics.com/newton

Title: Newton Biography
Cited text: laws of motion
Encrypted index: ghi789

")
    (let ((expected-visible "# ASSISTANT

Einstein developed the theory of relativity while Newton formulated the laws of motion"))
      (should (string= expected-visible (greger-ui-test--visible-text))))

    (goto-char (point-min))
    (re-search-forward "Newton")
    (execute-kbd-macro (kbd "TAB"))

    (let ((expected-visible "# ASSISTANT

Einstein developed the theory of relativity while Newton formulated the laws of motion

## https://physics.com/newton

Title: Newton Biography
Cited text: laws of motion
Encrypted index: ghi789

"))
      (should (string= expected-visible (greger-ui-test-visible-text))))))

;;; greger-ui-test.el ends here
