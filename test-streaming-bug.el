;;; Test to reproduce the streaming bug

(defun test-streaming-bug ()
  "Test if the streaming function properly handles VTERM_FULL_REPLACE markers."
  (let* ((buffer (get-buffer-create "*test-streaming*"))
         (content-start nil)
         (content-end nil))
    
    (with-current-buffer buffer
      (erase-buffer)
      (insert "<function_results>\n")
      (setq content-start (point))
      (insert "initial content")
      (insert "\n</function_results>")
      (setq content-end (- (point) (length "\n</function_results>")))
      
      (message "Initial buffer: '%s'" (buffer-string))
      (message "Content area: %d to %d" content-start content-end)
      (message "Initial content: '%s'" (buffer-substring content-start content-end))
      
      ;; Test the problematic case
      (let ((text "VTERM_FULL_REPLACE:Hello World"))
        (if (string-prefix-p "VTERM_FULL_REPLACE:" text)
            (let ((vterm-content (substring text (length "VTERM_FULL_REPLACE:"))))
              (message "Replacing with: '%s'" vterm-content)
              (delete-region content-start (1- content-end))
              (goto-char content-start)
              (insert vterm-content))
          (message "Not a VTERM_FULL_REPLACE")))
      
      (message "After replacement: '%s'" (buffer-string))
      (message "Final content: '%s'" (buffer-substring content-start (- (point-max) (length "\n</function_results>"))))
      
      ;; Test with another replacement
      (let ((text "VTERM_FULL_REPLACE:Final Output"))
        (if (string-prefix-p "VTERM_FULL_REPLACE:" text)
            (let ((vterm-content (substring text (length "VTERM_FULL_REPLACE:"))))
              (message "Second replace with: '%s'" vterm-content)
              (setq content-end (- (point-max) (length "\n</function_results>")))
              (delete-region content-start (1- content-end))
              (goto-char content-start)
              (insert vterm-content))))
      
      (message "Final buffer: '%s'" (buffer-string))
      (let ((final-content (buffer-substring content-start (- (point-max) (length "\n</function_results>")))))
        (message "Final content: '%s'" final-content)
        (if (string-match "VTERM_FULL_REPLACE:" final-content)
            (message "❌ BUG: VTERM_FULL_REPLACE markers still in content!")
          (message "✅ GOOD: Clean content without markers"))))))

(test-streaming-bug)
