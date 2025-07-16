;;; Simple test

(message "Starting test...")

(defun simple-test ()
  (message "Simple test function called")
  (let ((result "test"))
    (message "Result: %s" result)))

(simple-test)
(message "Test completed")
