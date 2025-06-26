;;; minimal-repro.el --- Minimal reproduction of Emacs 29.4 vs 30.1 difference

;; Test what happens when we modify buffer content during tree-sitter font-lock

(defun test-treesit-font-lock-modification ()
  "Test buffer modification during tree-sitter font-lock similar to greger-ui."
  (with-temp-buffer
    ;; Insert test content similar to the failing test
    (insert "# TOOL USE

Name: str-replace
ID: test_123

## original-content

old content here

## new-content

new content here

")
    
    ;; Define a font-lock function that modifies content (simulating greger-ui--apply-str-replace-diff-content)
    (defun test-modify-content (start end)
      (let ((inhibit-read-only t))
        (message "Modifying buffer content from %d to %d" start end)
        (goto-char start)
        (let ((original-text (buffer-substring start end)))
          (message "Original text: %s" (substring original-text 0 (min 50 (length original-text))))
          (delete-region start end)
          (insert "## diff

-old content here
+new content here

")
          (message "Inserted replacement text")
          (message "Buffer now: %s" (buffer-substring (max 1 (- start 10)) (min (point-max) (+ (point) 20)))))))
    
    ;; Try to modify the content
    (let ((start (search-forward "## original-content"))
          (end (progn (search-forward "## new-content") 
                     (forward-line 3)
                     (point))))
      (goto-char start)
      (test-modify-content (line-beginning-position) end))
    
    (message "Final buffer content:")
    (message "%s" (buffer-string))
    (message "Buffer size: %d" (buffer-size))
    
    (buffer-string)))

(message "Testing tree-sitter-like buffer modification...")
(let ((result (test-treesit-font-lock-modification)))
  (message "Length of result: %d" (length result)))
