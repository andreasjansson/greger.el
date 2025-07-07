;;; test-reproduce-bug.el --- Test to reproduce the eval tag bug

;; This script reproduces the bug where typing < after <eval> causes Emacs to hang

(require 'greger)

(defun test-reproduce-eval-bug ()
  "Reproduce the eval tag bug by typing characters one by one."
  (interactive)
  
  ;; Open the test file
  (find-file "test-bug-reproduction.greger")
  
  ;; Enable greger-mode
  (greger-mode)
  
  ;; Go to the end of the buffer (after <eval>)
  (goto-char (point-max))
  
  ;; Add a newline first
  (insert "\n")
  (message "Inserted newline after <eval>")
  (sit-for 0.1)  ; Small delay to see if font-lock processes
  
  ;; Now type < which should trigger the bug
  (message "About to insert < character...")
  (sit-for 0.5)
  
  ;; This should cause the hang
  (insert "<")
  (message "Successfully inserted < - no hang occurred")
  
  ;; Show the buffer content
  (message "Buffer content: %s" (buffer-string)))

;; Run the test
(test-reproduce-eval-bug)
