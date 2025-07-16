#!/usr/bin/env emacs --script
;; Test script to debug vterm issues

(load-file "greger-stdlib.el")

(defun test-vterm-manually ()
  "Test the vterm implementation manually."
  (interactive)
  (let ((callback (lambda (result error)
                    (message "CALLBACK: result=%S error=%S" result error)
                    (if error
                        (message "ERROR: %s" error)
                      (message "SUCCESS: %s" result)))))
    (greger-stdlib--run-shell-command-with-vterm
     "echo 'TEST OUTPUT'"
     default-directory
     callback
     10
     nil
     (lambda (text) (message "STREAMING: %s" text)))))

(test-vterm-manually)
