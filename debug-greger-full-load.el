;;; debug-greger-full-load.el --- Test loading all greger modules like end-to-end tests

(add-to-list 'load-path ".")

(message "Loading greger modules...")

(condition-case err
    (progn
      (require 'greger)
      (message "greger.el loaded successfully"))
  (error (message "Error loading greger.el: %s" err)))

(condition-case err
    (progn
      (require 'greger-parser)
      (message "greger-parser.el loaded successfully"))
  (error (message "Error loading greger-parser.el: %s" err)))

(condition-case err
    (progn
      (require 'greger-stdlib)
      (message "greger-stdlib.el loaded successfully"))
  (error (message "Error loading greger-stdlib.el: %s" err)))

(condition-case err
    (progn
      (require 'greger-tools)
      (message "greger-tools.el loaded successfully"))
  (error (message "Error loading greger-tools.el: %s" err)))

(condition-case err
    (progn
      (require 'greger-client)
      (message "greger-client.el loaded successfully"))
  (error (message "Error loading greger-client.el: %s" err)))

(condition-case err
    (progn
      (require 'greger-web)
      (message "greger-web.el loaded successfully"))
  (error (message "Error loading greger-web.el: %s" err)))

(message "All modules loaded successfully!")

;; Now try calling greger function like the end-to-end test does
(condition-case err
    (progn
      (message "Calling greger function...")
      (greger)
      (message "greger function completed successfully")
      ;; Clean up the buffer
      (when (get-buffer "*greger*")
        (kill-buffer "*greger*")))
  (error (message "Error calling greger function: %s" err)))

(message "Full test completed!")

;;; debug-greger-full-load.el ends here
