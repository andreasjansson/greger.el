;; Interactive test for eval folding
(add-to-list 'load-path ".")
(require 'greger)

(defun test-eval-folding ()
  "Test eval folding interactively."
  (interactive)
  (let ((test-buffer (get-buffer-create "*test-eval-folding*")))
    (with-current-buffer test-buffer
      (erase-buffer)
      (insert "# SYSTEM\n\n")
      (insert "<eval>1. single-line<eval-result-abc123>hello</eval-result-abc123></eval>\n\n")
      (insert "<eval>\n2. newlines-and-single-line\n<eval-result-abc123>hello</eval-result-abc123>\n</eval>\n\n")
      (insert "<eval>\n3. multi-\nline\n<eval-result-abc123>\nhello\n</eval-result-abc123>\n</eval>\n\n")
      (insert "<eval>\n5. four-line-head\n<eval-result-abc123>\nline1\nline2\nline3\nline4\n</eval-result-abc123>\n</eval>\n\n")
      (insert "<eval>\n6. long-result-with-tail\n<eval-result-def456>\nline1\nline2\nline3\nline4\nline5\nline6\nline7\nline8\n</eval-result-def456>\n</eval>\n\n")
      (insert "# USER\n\nHow does it look?")
      
      (greger-mode)
      (setq greger-ui-folding-mode t)
      (font-lock-flush)
      (font-lock-ensure)
      
      (switch-to-buffer test-buffer)
      
      (message "Test buffer created. Check the display and press any key to continue.")
      (read-char)
      
      ;; Show some debug info
      (goto-char (point-min))
      (when (search-forward "<eval-result-abc123>" nil t)
        (let ((start (match-beginning 0))
              (end (match-end 0)))
          (message "Tag at %d-%d: invisible=%s display=%s" 
                   start end
                   (get-text-property start 'invisible)
                   (get-text-property start 'display)))))))

;; Run the test
(test-eval-folding)
