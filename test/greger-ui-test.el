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

(ert-deftest greger-ui-test-citations-folding-simple ()
  "Test basic citation folding with a single citation."
  (with-current-buffer (greger)
    (erase-buffer)
    (insert "# ASSISTANT

Einstein developed the theory of relativity

## https://physics.com/einstein

Title: Einstein
Cited text: Albert Einstein developed the theory of relativity in the early 20th century...
Encrypted index: def456

")
    ;; Force font-lock to process the buffer
    (font-lock-ensure)
    (let ((visible-text (greger-ui-test--visible-text)))
      ;; Should hide the citation details but keep the main text
      (should (string-match-p "Einstein developed the theory of relativity" visible-text))
      (should-not (string-match-p "https://physics.com/einstein" visible-text)))))

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
    
    ;; Debug: let's see what we actually get
    (let ((actual-visible (greger-ui-test--visible-text)))
      (message "Actual visible text: %S" actual-visible)
      
      ;; Test that citation sections are hidden initially
      (should (string-match-p "Einstein developed the theory of relativity" actual-visible))
      (should (string-match-p "Newton formulated the laws of motion" actual-visible))
      (should-not (string-match-p "https://physics.com/einstein" actual-visible)))

    ;; Test expanding a citation
    (goto-char (point-min))
    (re-search-forward "Newton")
    
    ;; Send TAB key directly - simulate the key event
    (push 'tab unread-command-events)
    
    (let ((expanded-visible (greger-ui-test--visible-text)))
      ;; After TAB, the Newton section should be visible
      (should (string-match-p "https://physics.com/newton" expanded-visible)))))

;;; greger-ui-test.el ends here
