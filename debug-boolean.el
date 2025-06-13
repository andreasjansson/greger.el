;;; debug-boolean.el --- Debug boolean parameter processing

(add-to-list 'load-path ".")
(require 'greger-tools)

;; Create a simple tool definition for testing
(defun test-bool-func (flag)
  (format "flag: %s" flag))

(greger-register-tool "debug-bool"
  :description "Debug boolean tool"
  :properties '((flag . ((type . "boolean") (description . "A boolean flag"))))
  :required '("flag")
  :function 'test-bool-func)

;; Get the tool definition to pass to the function
(let ((tool-def (gethash "debug-bool" greger-tools-registry)))
  (message "Tool def: %S" tool-def)
  
  ;; Test different values
  (let ((test-cases '(("string true" . "true")
                     ("string false" . "false") 
                     ("symbol t" . t)
                     ("symbol nil" . nil)
                     ("json-true symbol" . :json-true)
                     ("json-false symbol" . :json-false))))
    
    (dolist (test-case test-cases)
      (let* ((description (car test-case))
             (value (cdr test-case))
             (result (greger-tools--maybe-parse-json-value value 'flag tool-def)))
        (message "%s: %S -> %S" description value result)))))

;;; debug-boolean.el ends here
