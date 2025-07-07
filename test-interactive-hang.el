;;; test-interactive-hang.el --- Test to reproduce the exact hanging scenario

(require 'greger)

(defun test-interactive-hang ()
  "Test the exact scenario that causes the hang."
  (interactive)
  
  ;; Create a buffer with the exact content
  (with-temp-buffer
    (insert "# SYSTEM\n\nYou are an expert coding agent.\n\n# USER\n\n<eval>\n")
    (greger-mode)
    
    ;; Position at the end (after <eval>)
    (goto-char (point-max))
    
    ;; Insert a newline to make it more like the real scenario
    (insert "\n")
    
    ;; Now simulate typing < which should trigger the hang
    (message "About to type < after <eval>...")
    
    ;; Try to catch infinite recursion or hanging
    (let ((start-time (current-time)))
      (condition-case err
          (progn
            (insert "<")
            (let ((elapsed (time-subtract (current-time) start-time)))
              (message "Successfully inserted < in %s seconds" elapsed)
              (message "Buffer content:\n%s" (buffer-string))))
        (error
         (message "Error occurred: %s" err))
        (quit
         (message "Quit signal received - possible hang detected"))))))

;; Run the test
(test-interactive-hang)
