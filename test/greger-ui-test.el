;;; greger-ui-test.el --- Tests for greger tools -*- lexical-binding: t -*-

(require 'ert)
(require 'greger-ui)
(require 'greger)

(defun greger-ui-test--key-binding-at-point (key)
  "Get the effective key binding for KEY at point.
This checks text properties, overlays, and local/global keymaps."
  (or 
   ;; Check keymap text property
   (let ((keymap (get-char-property (point) 'keymap)))
     (when keymap (lookup-key keymap key)))
   ;; Check local-map text property
   (let ((keymap (get-char-property (point) 'local-map)))
     (when keymap (lookup-key keymap key)))
   ;; Fall back to normal key lookup
   (key-binding key)))


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

(defun greger-ui-test--send-key (key)
  (let ((fn (greger-ui-test--key-binding-at-point key)))
    (funcall fn))

  ;; force font-lock to update
  (font-lock-flush (point-min) (point-max))
  (font-lock-ensure (point-min) (point-max)))

(ert-deftest greger-ui-test-citations-folding ()
  (with-current-buffer (greger)
    (erase-buffer)
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
    ;; Force font-lock to process the buffer
    (font-lock-ensure)
    
    (let ((actual (greger-ui-test--visible-text))
          (expected "# ASSISTANT

Einstein developed the theory of relativity while Newton formulated the laws of motion

"))
      (should (string= expected actual)))

    ;; Test expanding a citation
    (goto-char (point-min))
    (re-search-forward "Newton")
    
    (greger-ui-test--send-key (kbd "TAB"))

    (let ((actual (greger-ui-test--visible-text))
          (expected "# ASSISTANT

Einstein developed the theory of relativity while Newton formulated the laws of motion

## https://physics.com/newton

Title: Newton Biography
Cited text: laws of motion
Encrypted index: ghi789

"))
      (should (string= expected actual)))))

(ert-deftest greger-ui-test-tool-content-folding ()
  (with-current-buffer (greger)
    (erase-buffer)
    (insert "# TOOL USE

Name: read-file
ID: toolu_999

## path

<tool.toolu_999>
line1
line2
line3
line4
line5
line6
</tool.toolu_999>

")
    ;; Force font-lock to process the buffer
    (font-lock-ensure)
    
    (let ((actual (greger-ui-test--visible-text))
          (expected "# TOOL USE

Name: read-file
ID: toolu_999

## path

<tool.toolu_999>
line1
line2
line3
line4
</tool.toolu_999>

"))
      (should (string= expected actual)))

    ;; Test expanding a citation
    (goto-char (point-min))
    (re-search-forward "line1")
    
    (greger-ui-test--send-key (kbd "TAB"))

    (let ((actual (greger-ui-test--visible-text))
          (expected "# TOOL USE

Name: read-file
ID: toolu_999

## path

<tool.toolu_999>
line1
line2
line3
line4
line5
line6
</tool.toolu_999>

"))
      (should (string= expected actual)))))

(ert-deftest greger-ui-test-thinking-signature-invisible ()
  (with-current-buffer (greger)
    (erase-buffer)
    (insert "# USER

Let me think about this

# THINKING

Signature: abc123

I need to consider all the options carefully before responding.

")
    ;; Force font-lock to process the buffer
    (font-lock-ensure)
    
    (let ((actual (greger-ui-test--visible-text))
          (expected "# USER

Let me think about this

# THINKING

I need to consider all the options carefully before responding.

"))
      (should (string= expected actual)))))

;;; greger-ui-test.el ends here
