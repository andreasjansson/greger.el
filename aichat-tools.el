;;; aichat-tools.el --- Tool definitions for aichat agent -*- lexical-binding: t -*-

;;; Commentary:
;; Defines tools available to the aichat agent

;;; Code:

(require 'json)

(setq aichat-tools-registry
  '((read-file . ((name . "read-file")
                  (description . "Read the contents of a file from the filesystem")
                  (input_schema . ((type . "object")
                                   (properties . ((path . ((type . "string")
                                                          (description . "Path to the file to read")))))
                                   (required . ["path"])))))

    (write-file . ((name . "write-file")
                   (description . "Write content to a file, creating it if it doesn't exist")
                   (input_schema . ((type . "object")
                                    (properties . ((path . ((type . "string")
                                                           (description . "Path to the file to write")))
                                                   (content . ((type . "string")
                                                             (description . "Content to write to the file")))
                                                   (append . ((type . "boolean")
                                                            (description . "Whether to append to existing file instead of overwriting")
                                                            (default . nil)))))
                                    (required . ["path" "content"])))))

    (list-directory . ((name . "list-directory")
                       (description . "List files and directories in a given directory")
                       (input_schema . ((type . "object")
                                        (properties . ((path . ((type . "string")
                                                               (description . "Path to the directory to list. Defaults to current directory.")
                                                               (default . ".")))
                                                       (show-hidden . ((type . "boolean")
                                                                     (description . "Whether to show hidden files starting with .")
                                                                     (default . nil)))
                                                       (recursive . ((type . "boolean")
                                                                   (description . "Whether to list files recursively")
                                                                   (default . nil)))))
                                        (required . [])))))

    (ripgrep . ((name . "ripgrep")
                (description . "Search for patterns in files using ripgrep (rg) command line tool")
                (input_schema . ((type . "object")
                                 (properties . ((pattern . ((type . "string")
                                                           (description . "The search pattern (regex or literal string)")))
                                                (path . ((type . "string")
                                                       (description . "Directory or file path to search in")
                                                       (default . ".")))
                                                (case-sensitive . ((type . "boolean")
                                                                 (description . "Whether the search should be case-sensitive")
                                                                 (default . nil)))
                                                (file-type . ((type . "string")
                                                            (description . "Restrict search to specific file types (e.g., 'py', 'js', 'md')")))
                                                (context-lines . ((type . "integer")
                                                                (description . "Number of context lines to show around matches")
                                                                (default . 0)))
                                                (max-results . ((type . "integer")
                                                              (description . "Maximum number of results to return")
                                                              (default . 50)))))
                                 (required . ["pattern"])))))))

(defun aichat-tools-get-schemas (tool-names)
  "Get tool schemas for TOOL-NAMES."
  (mapcar (lambda (tool-name)
            (or (alist-get tool-name aichat-tools-registry)
                (error "Unknown tool: %s" tool-name)))
          tool-names))

(defun aichat-tools-execute (tool-name args)
  "Execute TOOL-NAME with ARGS."
  (let ((tool-symbol (intern tool-name)))
    (cond
     ((eq tool-symbol 'read-file)
      (aichat-tools--read-file (alist-get 'path args)))

     ((eq tool-symbol 'write-file)
      (aichat-tools--write-file
       (alist-get 'path args)
       (alist-get 'content args)
       (alist-get 'append args)))

     ((eq tool-symbol 'list-directory)
      (aichat-tools--list-directory
       (or (alist-get 'path args) ".")
       (alist-get 'show-hidden args)
       (alist-get 'recursive args)))

     ((eq tool-symbol 'ripgrep)
      (aichat-tools--ripgrep
       (alist-get 'pattern args)
       (or (alist-get 'path args) ".")
       (alist-get 'case-sensitive args)
       (alist-get 'file-type args)
       (or (alist-get 'context-lines args) 0)
       (or (alist-get 'max-results args) 50)))

     (t
      (error "Unknown tool: %s" tool-name)))))

(defun aichat-tools--read-file (path)
  "Read file at PATH."
  (unless (stringp path)
    (error "Path must be a string"))

  (let ((expanded-path (expand-file-name path)))
    (unless (file-exists-p expanded-path)
      (error "File does not exist: %s" expanded-path))

    (unless (file-readable-p expanded-path)
      (error "File is not readable: %s" expanded-path))

    (when (file-directory-p expanded-path)
      (error "Path is a directory, not a file: %s" expanded-path))

    (condition-case err
        (with-temp-buffer
          (insert-file-contents expanded-path)
          (buffer-string))
      (error (format "Failed to read file: %s" (error-message-string err))))))

(defun aichat-tools--write-file (path content &optional append)
  "Write CONTENT to file at PATH. If APPEND is non-nil, append to existing file."
  (unless (stringp path)
    (error "Path must be a string"))

  (unless (stringp content)
    (error "Content must be a string"))

  (let ((expanded-path (expand-file-name path)))
    ;; Create parent directories if they don't exist
    (let ((parent-dir (file-name-directory expanded-path)))
      (when parent-dir
        (make-directory parent-dir t)))

    (condition-case err
        (if append
            (append-to-file content nil expanded-path)
          (with-temp-file expanded-path
            (insert content)))
      (error (format "Failed to write file: %s" (error-message-string err))))

    (format "Successfully %s %d characters to %s"
            (if append "appended" "wrote")
            (length content)
            expanded-path)))

(defun aichat-tools--list-directory (path &optional show-hidden recursive)
  "List directory contents at PATH."
  (unless (stringp path)
    (error "Path must be a string"))

  (let ((expanded-path (expand-file-name path)))
    (unless (file-exists-p expanded-path)
      (error "Directory does not exist: %s" expanded-path))

    (unless (file-directory-p expanded-path)
      (error "Path is not a directory: %s" expanded-path))

    (unless (file-readable-p expanded-path)
      (error "Directory is not readable: %s" expanded-path))

    (condition-case err
        (let ((files (if recursive
                        (aichat-tools--list-directory-recursive expanded-path show-hidden)
                      (directory-files expanded-path nil
                                      (if show-hidden "^[^.]\\|^\\.[^.]" "^[^.]")))))
          (if files
              (mapconcat (lambda (file)
                          (let ((full-path (expand-file-name file expanded-path)))
                            (format "%s%s"
                                   file
                                   (if (file-directory-p full-path) "/" ""))))
                        files "\n")
            "Directory is empty"))
      (error (format "Failed to list directory: %s" (error-message-string err))))))

(defun aichat-tools--list-directory-recursive (path show-hidden &optional prefix)
  "Recursively list directory contents at PATH."
  (let ((files '())
        (prefix (or prefix "")))

    (dolist (file (directory-files path nil
                                  (if show-hidden "^[^.]\\|^\\.[^.]" "^[^.]")))
      (let ((full-path (expand-file-name file path))
            (display-name (concat prefix file)))

        (if (file-directory-p full-path)
            (progn
              (push (concat display-name "/") files)
              (setq files (append files
                                 (aichat-tools--list-directory-recursive
                                  full-path show-hidden (concat prefix file "/")))))
          (push display-name files))))

    (reverse files)))

(defun aichat-tools--ripgrep (pattern path &optional case-sensitive file-type context-lines max-results)
  "Search for PATTERN in PATH using ripgrep."
  (unless (stringp pattern)
    (error "Pattern must be a string"))

  (unless (stringp path)
    (error "Path must be a string"))

  ;; Check if rg is available
  (unless (executable-find "rg")
    (error "ripgrep (rg) command not found. Please install ripgrep"))

  (let ((expanded-path (expand-file-name path)))
    (unless (file-exists-p expanded-path)
      (error "Path does not exist: %s" expanded-path))

    (let ((cmd-args (list "rg"
                         "--color=never"
                         "--no-heading"
                         "--with-filename"
                         "--line-number")))

      ;; Add case sensitivity option
      (unless case-sensitive
        (push "--ignore-case" cmd-args))

      ;; Add file type filter if specified
      (when file-type
        (push (format "--type=%s" file-type) cmd-args))

      ;; Add context lines
      (when (and context-lines (> context-lines 0))
        (push (format "--context=%d" context-lines) cmd-args))

      ;; Add max results limit
      (when (and max-results (> max-results 0))
        (push (format "--max-count=%d" max-results) cmd-args))

      ;; Add pattern and path
      (push pattern cmd-args)
      (push expanded-path cmd-args)

      ;; Reverse to get correct order
      (setq cmd-args (reverse cmd-args))

      (condition-case err
          (let ((result (with-temp-buffer
                         (let ((exit-code (apply #'call-process "rg" nil t nil (cdr cmd-args))))
                           (if (= exit-code 0)
                               (buffer-string)
                             (if (= exit-code 1)
                                 "No matches found"
                               (error "ripgrep failed with exit code %d: %s" exit-code (buffer-string))))))))
            (if (string-empty-p (string-trim result))
                "No matches found"
              result))
        (error (format "Failed to execute ripgrep: %s" (error-message-string err)))))))

(provide 'aichat-tools)

;;; aichat-tools.el ends here
