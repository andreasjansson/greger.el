;; Debug script for Emacs 29.4 issue
(require 'greger-ui)

(defun debug-generate-diff-content ()
  "Debug function to test diff generation directly."
  (let ((original "def old_function():
    print('old implementation')
    return False")
        (new "def new_function():
    print('new implementation')
    return True")
        (path "example.py"))
    (greger-ui--generate-diff-content original new path)))

(message "Testing diff generation...")
(let ((diff-result (debug-generate-diff-content)))
  (with-temp-buffer
    (insert diff-result)
    (message "Generated diff:")
    (message "%s" (buffer-string))
    (message "Diff length: %d" (length diff-result))))
