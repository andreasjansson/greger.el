;;; greger-ui-test.el --- Tests for greger tools -*- lexical-binding: t -*-

(require 'ert)
(require 'greger-ui)
(require 'greger)

(defun greger-ui--visible-text (input)
  "Extract visible text from INPUT after greger-ui folding is applied.
This function sets up a buffer with greger-mode, applies the folding,
and returns only the text that would be visible to the user."
  (with-temp-buffer
    (insert input)
    
    ;; Enable greger-mode to get tree-sitter parsing and folding
    (greger-mode)
    
    ;; Force font-lock to apply all the folding functions
    (font-lock-ensure)
    
    ;; Extract only visible text by checking the 'invisible property
    (let ((result ""))
      (goto-char (point-min))
      (while (< (point) (point-max))
        (let* ((char (char-after))
               (invisible (get-text-property (point) 'invisible)))
          (unless invisible
            (setq result (concat result (char-to-string char))))
          (forward-char 1)))
      
      ;; Clean up trailing whitespace
      (string-trim result))))

(ert-deftest greger-ui-test-citations-folding ()
  (let* ((input "# ASSISTANT

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
         (expected-visible "# ASSISTANT

Einstein developed the theory of relativity while Newton formulated the laws of motion")
         (actual-visible (greger-ui--visible-text input)))
    (should (string= expected-visible actual-visible))))

;;; greger-ui-test.el ends here
