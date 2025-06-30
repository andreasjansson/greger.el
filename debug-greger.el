;; Debug script to test greger function

(load-file "greger.el")
(load-file "greger-parser.el")

(message "greger-parser-system-tag: %s" greger-parser-system-tag)
(message "greger-default-system-prompt: %s" greger-default-system-prompt)

;; Test greger without context
(let ((buffer (greger)))
  (with-current-buffer buffer
    (message "Buffer contents: '%s'" (buffer-substring-no-properties (point-min) (point-max))))
  (kill-buffer buffer))

;; Test greger with context
(with-temp-buffer
  (insert "test file content")
  (cl-letf (((symbol-function 'save-buffer) #'ignore)
            ((symbol-function 'buffer-file-name) 
             (lambda () "/test/path/test-file.txt"))
            ((symbol-function 'window-list)
             (lambda () '(window1))) ; Single window
            ((symbol-function 'split-window-right) #'ignore)
            ((symbol-function 'other-window) #'ignore)
            ((symbol-function 'switch-to-buffer) #'ignore)
            ((symbol-function 'greger-mode) #'ignore))
    
    (let ((buffer (greger t)))
      (with-current-buffer buffer
        (message "Buffer with context contents: '%s'" (buffer-substring-no-properties (point-min) (point-max))))
      (kill-buffer buffer))))
