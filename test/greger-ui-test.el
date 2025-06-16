;;; greger-ui-test.el --- Tests for greger tools -*- lexical-binding: t -*-

(require 'ert)
(require 'greger-ui)

(defun greger-ui--visible-text (input)
  "Extract visible text from INPUT as it would appear with greger-ui folding.
This simulates the folding behavior where citation entries are hidden,
leaving only the main assistant content visible."
  (with-temp-buffer
    (insert input)
    (goto-char (point-min))
    
    ;; Remove citation entries (lines starting with ## followed by content until next # or end)
    (while (re-search-forward "^## " nil t)
      (let ((citation-start (line-beginning-position)))
        ;; Find the end of the citation entry (next # line or end of buffer)
        (forward-line 1)
        (let ((citation-content-start (point)))
          (while (and (not (eobp))
                      (not (looking-at "^#")))
            (forward-line 1))
          ;; Delete the entire citation entry
          (delete-region citation-start (point)))))
    
    ;; Clean up extra whitespace and normalize newlines
    (goto-char (point-min))
    ;; Remove multiple consecutive newlines, keeping structure
    (while (re-search-forward "\n\n+" nil t)
      (replace-match "\n\n"))
    
    ;; Remove trailing whitespace at end
    (goto-char (point-max))
    (while (and (> (point) (point-min))
                (memq (char-before) '(?\s ?\t ?\n)))
      (delete-char -1))
    
    (buffer-string)))

(ert-deftest greger-ui-test-citations-folding ()
  (let ((input "# ASSISTANT

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
"))
    (expected-visible "# ASSISTANT

Einstein developed the theory of relativity while Newton formulated the laws of motion")
    (actual-visible (greger-ui-test--visible-text input)))
  (should (string= expected-visible actual-visible)))

;;; greger-ui-test.el ends here
