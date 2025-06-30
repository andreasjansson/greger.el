;; Debug script to test the cl-loop function argument extraction

(load-file "greger-tools.el")

;; Test function matching the failing test
(defun greger-test-optional-params (required-param &optional optional-param1 optional-param2)
  "Test function with optional parameters."
  (format "required: %s, opt1: %s, opt2: %s"
          required-param
          (or optional-param1 "default1")
          (or optional-param2 "default2")))

;; Get the argument list
(let ((func 'greger-test-optional-params)
      (args '((required-param . "test")))
      (tool-def nil))
  
  (message "Function: %s" func)
  (message "Args: %s" args)
  (message "help-function-arglist: %s" (help-function-arglist func t))
  
  ;; Test our function
  (let ((result (greger-tools--extract-function-args func args tool-def)))
    (message "Extracted args: %s" result)
    (message "Applying function: %s" (apply func result))
    result))
