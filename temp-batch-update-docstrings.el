;;; temp-batch-update-docstrings.el --- Batch update docstrings tool -*- lexical-binding: t -*-

;;; Commentary:
;; Tool for batch updating docstrings in Emacs Lisp files

;;; Code:

(require 'greger-tools)

(greger-register-tool "batch-update-docstrings"
  :description "Batch update docstrings for multiple functions in a file"
  :properties '((file-path . ((type . "string")
                              (description . "Path to the Emacs Lisp file to update")))
                (docstring-updates . ((type . "object")
                                      (description . "Dictionary mapping function names to their new docstrings"))))
  :required '("file-path" "docstring-updates")
  :function 'greger-temp--batch-update-docstrings)

(defun greger-temp--batch-update-docstrings (file-path docstring-updates)
  "Update docstrings for multiple functions in FILE-PATH.
DOCSTRING-UPDATES is a hash table mapping function names to new docstrings."
  (unless (stringp file-path)
    (error "file-path must be a string"))
  
  (unless (hash-table-p docstring-updates)
    (error "docstring-updates must be a hash table"))

  (let ((expanded-path (expand-file-name file-path)))
    ;; Check if file exists
    (unless (file-exists-p expanded-path)
      (error "File does not exist: %s" expanded-path))

    ;; Check if it's actually a file
    (when (file-directory-p expanded-path)
      (error "Path is a directory, not a file: %s" expanded-path))

    ;; Check if it's an Emacs Lisp file
    (unless (string-suffix-p ".el" expanded-path)
      (error "File must be an Emacs Lisp file (.el): %s" expanded-path))

    (with-current-buffer (find-file-noselect expanded-path)
      (let ((updates-made 0)
            (functions-not-found '())
            (original-point (point)))
        
        ;; Iterate through each function to update
        (maphash
         (lambda (function-name new-docstring)
           (goto-char (point-min))
           
           ;; Search for the function definition
           (let ((function-pattern (format "^\\s-*(\\(defun\\|defmacro\\|ert-deftest\\)\\s-+%s\\s-*("
                                          (regexp-quote function-name))))
             (if (re-search-forward function-pattern nil t)
                 (progn
                   ;; Move to beginning of defun
                   (beginning-of-defun)
                   
                   ;; Try to update the docstring
                   (if (greger-temp--update-function-docstring new-docstring)
                       (setq updates-made (1+ updates-made))
                     (push function-name functions-not-found)))
               
               ;; Function not found
               (push function-name functions-not-found))))
         docstring-updates)
        
        ;; Save the file if any updates were made
        (when (> updates-made 0)
          (save-buffer))
        
        ;; Restore original point
        (goto-char original-point)
        
        ;; Return results
        (format "Updated %d function docstrings in %s. %s"
                updates-made
                expanded-path
                (if functions-not-found
                    (format "Functions not found: %s" 
                            (mapconcat #'identity functions-not-found ", "))
                  "All functions found and updated."))))))

(defun greger-temp--update-function-docstring (new-docstring)
  "Update the docstring of the function at point with NEW-DOCSTRING.
Assumes point is at the beginning of a function definition.
Returns t if successful, nil otherwise."
  (save-excursion
    (condition-case err
        (progn
          ;; Move past the function signature
          (forward-sexp) ; Skip (defun function-name
          (forward-sexp) ; Skip parameters
          
          ;; Skip any declare forms or interactive forms before the docstring
          (while (and (not (eobp))
                      (looking-at "\\s-*\\((declare\\|(interactive\\)"))
            (forward-sexp))
          
          ;; Skip whitespace
          (skip-chars-forward " \t\n")
          
          ;; Check if we have a docstring (string literal)
          (if (looking-at "\"")
              (progn
                ;; We have an existing docstring, replace it
                (let ((start (point)))
                  (forward-sexp) ; Skip over the string
                  (delete-region start (point))
                  (insert (format "%S" new-docstring))
                  t))
            
            ;; No existing docstring, insert one
            (insert (format "%S\n  " new-docstring))
            t))
      
      (error
       (message "Error updating docstring: %s" (error-message-string err))
       nil))))

(provide 'temp-batch-update-docstrings)

;;; temp-batch-update-docstrings.el ends here
