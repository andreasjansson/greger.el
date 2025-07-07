;; Visual debug script
(add-to-list 'load-path ".")
(require 'greger)

;; Load the test file
(let ((test-file (expand-file-name "test-eval.greger")))
  (when (file-exists-p test-file)
    (find-file test-file)
    (greger-mode)
    
    ;; Enable folding mode
    (setq greger-ui-folding-mode t)
    
    ;; Force fontification
    (font-lock-flush)
    (font-lock-ensure)
    
    ;; Show the buffer content as seen by the user
    (goto-char (point-min))
    (forward-line 2)  ; Go to the line with the first eval
    (let ((line-start (line-beginning-position))
          (line-end (line-end-position)))
      (message "Line content: '%s'" (buffer-substring line-start line-end)))
    
    ;; Save the buffer to a file to examine
    (write-file "/tmp/debug-output.txt")))
