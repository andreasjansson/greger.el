;;; aichat-tools.el --- Tool definitions for aichat agent -*- lexical-binding: t -*-

;;; Commentary:
;; Defines tools available to the aichat agent

;;; Code:

(require 'json)

(setq aichat-tools-registry
      '(
        (read-file . ((name . "read-file")
                      (description . "Read the contents of a file from the filesystem")
                      (input_schema . ((type . "object")
                                       (properties . ((path . ((type . "string")
                                                               (description . "Path to the file to read")))
                                                      (include_line_numbers . ((type . "boolean")
                                                                               (description . "Whether to include line numbers in the output. If you plan to modify the file with tools that use line numbers, you should include line numbers here.")
                                                                               (default . nil)))))
                                       (required . ["path"])))))

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
                                     (required . ["pattern"])))))

        (replace-function . ((name . "replace-function")
                             (description . "Replace a function in a Python or Elisp file with new contents")
                             (input_schema . ((type . "object")
                                              (properties . ((file_path . ((type . "string")
                                                                           (description . "Absolute path to the .py or .el file")))
                                                             (line_number . ((type . "integer")
                                                                             (description . "Line number where the function is declared")))
                                                             (name . ((type . "string")
                                                                      (description . "Name of the function to replace")))
                                                             (contents . ((type . "string")
                                                                          (description . "New function contents to replace the existing function.")))))
                                              (required . ["file_path" "line_number" "name" "contents"])))))

        (file-replace-region . ((name . "file-replace-region")
                                (description . "Replace arbitrary code sections in a file by line numbers")
                                (input_schema . ((type . "object")
                                                 (properties . ((file_path . ((type . "string")
                                                                              (description . "Absolute path to the file")))
                                                                (line_number_start . ((type . "integer")
                                                                                      (description . "The line to start replacing from")))
                                                                (line_number_end . ((type . "integer")
                                                                                    (description . "The line to end replacing from (exclusive)")))
                                                                (contents . ((type . "string")
                                                                             (description . "Generated code contents to insert instead of the replaced content")))))
                                                 (required . ["file_path" "line_number_start" "line_number_end" "contents"])))))

        (file-insert-text . ((name . "file-insert-text")
                             (description . "Insert text at a specific line number in a file. Use line number = file_length + 1 to append to end of file.")
                             (input_schema . ((type . "object")
                                              (properties . ((file_path . ((type . "string")
                                                                           (description . "Absolute path to the file")))
                                                             (line_number . ((type . "integer")
                                                                             (description . "The line number to insert at (content will be inserted before this line). Use file_length + 1 to append to end of file.")))
                                                             (contents . ((type . "string")
                                                                          (description . "Text contents to insert")))))
                                              (required . ["file_path" "line_number" "contents"])))))

        (file-delete-region . ((name . "file-delete-region")
                               (description . "Delete code sections in a file by line numbers")
                               (input_schema . ((type . "object")
                                                (properties . ((file_path . ((type . "string")
                                                                             (description . "Absolute path to the file")))
                                                               (line_number_start . ((type . "integer")
                                                                                     (description . "The line to start deleting from")))
                                                               (line_number_end . ((type . "integer")
                                                                                   (description . "The line to end deleting from (exclusive)")))))
                                                (required . ["file_path" "line_number_start" "line_number_end"])))))

        (file-prepend-text . ((name . "file-prepend-text")
                              (description . "Prepend text to the beginning of a file")
                              (input_schema . ((type . "object")
                                               (properties . ((file_path . ((type . "string")
                                                                            (description . "Absolute path to the file")))
                                                              (contents . ((type . "string")
                                                                           (description . "Text contents to prepend")))))
                                               (required . ["file_path" "contents"])))))

        (file-append-text . ((name . "file-append-text")
                             (description . "Append text to the end of a file")
                             (input_schema . ((type . "object")
                                              (properties . ((file_path . ((type . "string")
                                                                           (description . "Absolute path to the file")))
                                                             (contents . ((type . "string")
                                                                          (description . "Text contents to append. Note that you may need to start your content with a newline depending on the existing contents of the file.")))))
                                              (required . ["file_path" "contents"])))))

        (write-new-file . ((name . "write-new-file")
                           (description . "Write a new file with the given contents. Fails if the file already exists.")
                           (input_schema . ((type . "object")
                                            (properties . ((file_path . ((type . "string")
                                                                         (description . "Absolute path to the new file")))
                                                           (contents . ((type . "string")
                                                                        (description . "Contents to write to the new file")))))
                                            (required . ["file_path" "contents"])))))

        (make-directory . ((name . "make-directory")
                           (description . "Recursively create a directory and all parent directories if they don't exist")
                           (input_schema . ((type . "object")
                                            (properties . ((path . ((type . "string")
                                                                    (description . "Path to the directory to create")))))
                                            (required . ["path"])))))

        (rename-file . ((name . "rename-file")
                        (description . "Rename or move a file from one path to another")
                        (input_schema . ((type . "object")
                                         (properties . ((old_path . ((type . "string")
                                                                     (description . "Current path of the file")))
                                                        (new_path . ((type . "string")
                                                                     (description . "New path for the file")))))
                                         (required . ["old_path" "new_path"])))))

        ))

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
      (aichat-tools--read-file
       (alist-get 'path args)
       (alist-get 'include_line_numbers args)))

     ((eq tool-symbol 'list-directory)
      (aichat-tools--list-directory
       (or (alist-get 'path args) ".")
       (alist-get 'show-hidden args)
       (alist-get 'recursive args)))

     ((eq tool-symbol 'replace-function)
      (aichat-tools--replace-function
       (alist-get 'file_path args)
       (alist-get 'line_number args)
       (alist-get 'name args)
       (alist-get 'contents args)))

     ((eq tool-symbol 'file-replace-region)
      (aichat-tools--file-replace-region
       (alist-get 'file_path args)
       (alist-get 'line_number_start args)
       (alist-get 'line_number_end args)
       (alist-get 'contents args)))

     ((eq tool-symbol 'ripgrep)
      (aichat-tools--ripgrep
       (alist-get 'pattern args)
       (or (alist-get 'path args) ".")
       (alist-get 'case-sensitive args)
       (alist-get 'file-type args)
       (or (alist-get 'context-lines args) 0)
       (or (alist-get 'max-results args) 50)))

     ((eq tool-symbol 'file-insert-text)
      (aichat-tools--file-insert-text
       (alist-get 'file_path args)
       (alist-get 'line_number args)
       (alist-get 'contents args)))

     ((eq tool-symbol 'file-delete-region)
      (aichat-tools--file-delete-region
       (alist-get 'file_path args)
       (alist-get 'line_number_start args)
       (alist-get 'line_number_end args)))

     ((eq tool-symbol 'file-prepend-text)
      (aichat-tools--prepend-text
       (alist-get 'file_path args)
       (alist-get 'contents args)))

     ((eq tool-symbol 'file-append-text)
      (aichat-tools--append-text
       (alist-get 'file_path args)
       (alist-get 'contents args)))

     ((eq tool-symbol 'write-new-file)
      (aichat-tools--write-new-file
       (alist-get 'file_path args)
       (alist-get 'contents args)))

     ((eq tool-symbol 'make-directory)
      (aichat-tools--make-directory
       (alist-get 'path args)))

     ((eq tool-symbol 'rename-file)
      (aichat-tools--rename-file
       (alist-get 'old_path args)
       (alist-get 'new_path args)))

     (t
      (error "Unknown tool: %s" tool-name)))))


(defun aichat-tools--read-file (path &optional include-line-numbers)
  "Read file at PATH. If INCLUDE-LINE-NUMBERS is non-nil, prepend line numbers."
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
          (if include-line-numbers
              (aichat-tools--add-line-numbers (buffer-string))
            (buffer-string)))
      (error (format "Failed to read file: %s" (error-message-string err))))))

(defun aichat-tools--add-line-numbers (content)
  "Add line numbers to CONTENT string."
  (let ((lines (split-string content "\n"))
        (line-num 1)
        (max-width 0)
        result)

    ;; Calculate the width needed for line numbers
    (setq max-width (length (number-to-string (length lines))))

    ;; Add line numbers to each line
    (dolist (line lines)
      (push (format (concat "%" (number-to-string max-width) "d: %s") line-num line) result)
      (setq line-num (1+ line-num)))

    ;; Join back with newlines
    (mapconcat 'identity (reverse result) "\n")))

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

(defun aichat-tools--replace-function (file-path line-number name contents)
  "Replace function NAME at LINE-NUMBER in FILE-PATH with CONTENTS."
  (unless (stringp file-path)
    (error "file_path must be a string"))
v
  (unless (integerp line-number)
    (error "line_number must be an integer"))

  (unless (stringp name)
    (error "name must be a string"))

  (unless (stringp contents)
    (error "contents must be a string"))

  (let ((expanded-path (expand-file-name file-path))
        (original-buffer (current-buffer))
        (original-window (selected-window)))
    ;; Check file extension
    (unless (or (string-suffix-p ".py" expanded-path)
                (string-suffix-p ".el" expanded-path))
      (error "Only .py and .el files are supported"))

    ;; Check file exists
    (unless (file-exists-p expanded-path)
      (error "File does not exist: %s" expanded-path))

    ;; Split screen if needed and open file
    (aichat-tools--setup-window-and-open-file expanded-path)

    ;; Go to the specified line
    (goto-line line-number)

    ;; Verify function declaration on this line
    (let ((current-line (thing-at-point 'line t)))
      (unless (aichat-tools--line-declares-function-p current-line name expanded-path)
        (error "Function '%s' not declared on line %d" name line-number)))

    ;; Find function boundaries and replace
    (let ((func-start (point))
          (func-end (aichat-tools--find-function-end expanded-path)))

      ;; Delete existing function
      (delete-region func-start func-end)

      ;; Insert new contents
      (insert contents)

      ;; Ensure there's a newline at the end if we're not at end of buffer
      ;; and the next character isn't already a newline
      (when (and (not (eobp))
                 (not (string-suffix-p "\n" contents))
                 (not (looking-at-p "^\n")))
        (insert "\n"))

      ;; Save the buffer
      (save-buffer)

      ;; Return to original chat buffer and window
      (select-window original-window)
      (switch-to-buffer original-buffer)

      (format "Successfully replaced function '%s' in %s" name expanded-path))))

(defun aichat-tools--setup-window-and-open-file (file-path)
  "Setup window split and open FILE-PATH appropriately."
  (let ((chat-window (selected-window)))
    (if (= (length (window-list)) 1)
        ;; Not split, split horizontally
        (progn
          (split-window-horizontally)
          (other-window 1)
          (find-file file-path))
      ;; Already split, find next window after chat buffer
      (let ((windows (window-list))
            (chat-buffer (current-buffer))
            target-window)
        ;; Find the window after the chat window
        (dolist (win windows)
          (when (eq (window-buffer win) chat-buffer)
            (let ((next-win (next-window win)))
              (setq target-window next-win))))

        (if target-window
            (progn
              (select-window target-window)
              (find-file file-path))
          ;; Fallback: use other window
          (other-window 1)
          (find-file file-path))))))

(defun aichat-tools--line-declares-function-p (line func-name file-path)
  "Check if LINE declares function FUNC-NAME based on file type."
  (let ((line-trimmed (string-trim line)))
    (cond
     ;; Python function declaration
     ((string-suffix-p ".py" file-path)
      (string-match-p (format "^def\\s-+%s\\s-*(" (regexp-quote func-name)) line-trimmed))

     ;; Elisp function declaration
     ((string-suffix-p ".el" file-path)
      (string-match-p (format "^(defun\\s-+%s\\s-*(" (regexp-quote func-name)) line-trimmed))

     (t nil))))

(defun aichat-tools--find-function-end (file-path)
  "Find the end of the current function based on file type."
  (cond
   ;; Python: find end by indentation
   ((string-suffix-p ".py" file-path)
    (aichat-tools--find-python-function-end))

   ;; Elisp: find matching closing parenthesis
   ((string-suffix-p ".el" file-path)
    (aichat-tools--find-elisp-function-end))

   (t (error "Unsupported file type"))))

(defun aichat-tools--find-python-function-end ()
  "Find the end of a Python function by indentation."
  (let ((start-column (current-indentation))
        (start-pos (line-beginning-position)))
    (forward-line 1)

    ;; Skip empty lines and comments at start
    (while (and (not (eobp))
                (or (looking-at-p "^\\s-*$")
                    (looking-at-p "^\\s-*#")))
      (forward-line 1))

    ;; Find first line with same or less indentation
    (while (and (not (eobp))
                (or (looking-at-p "^\\s-*$")
                    (looking-at-p "^\\s-*#")
                    (> (current-indentation) start-column)))
      (forward-line 1))

    ;; Move to end of previous line
    (unless (bobp)
      (forward-line -1)
      (end-of-line))

    (point)))

(defun aichat-tools--find-elisp-function-end ()
  "Find the end of an Elisp function by matching parentheses."
  (let ((start-pos (point)))
    ;; Move to the opening parenthesis of defun
    (beginning-of-line)
    (search-forward "(defun" (line-end-position))
    (backward-char 6) ; Go back to the opening paren

    ;; Use forward-sexp to find the matching closing paren
    (condition-case err
        (progn
          (forward-sexp)
          (point))
      (error
       (goto-char start-pos)
       (error "Could not find end of elisp function: %s" (error-message-string err))))))

(defun aichat-tools--file-replace-region (file-path line-start line-end contents)
  "Replace code between LINE-START and LINE-END (exclusive) in FILE-PATH with CONTENTS."
  (unless (stringp file-path)
    (error "file_path must be a string"))

  (unless (integerp line-start)
    (error "line_number_start must be an integer"))

  (unless (integerp line-end)
    (error "line_number_end must be an integer"))

  (unless (stringp contents)
    (error "contents must be a string"))

  (when (< line-end line-start)
    (error "line_number_end (%d) must be >= line_number_start (%d)" line-end line-start))

  (let ((expanded-path (expand-file-name file-path))
        (original-buffer (current-buffer))
        (original-window (selected-window)))

    ;; Check file exists
    (unless (file-exists-p expanded-path)
      (error "File does not exist: %s" expanded-path))

    ;; Split screen if needed and open file
    (aichat-tools--setup-window-and-open-file expanded-path)

    ;; Count total lines in file to validate line numbers
    (let ((total-lines (count-lines (point-min) (point-max))))
      ;; Special case: allow line_start = total_lines + 1 only when line_start == line_end
      ;; This enables appending to the end of the file
      (if (and (= line-start line-end) (= line-start (1+ total-lines)))
          ;; Append case: go to end of file
          (progn
            (goto-char (point-max))
            ;; Ensure we're on a new line if file doesn't end with newline
            (unless (or (bobp) (looking-back "\n" 1))
              (insert "\n"))
            (insert contents)
            ;; Ensure file ends with newline
            (unless (string-suffix-p "\n" contents)
              (insert "\n")))

        ;; Normal case: validate line numbers are within file bounds
        (progn
          (when (> line-start total-lines)
            (error "line_number_start (%d) exceeds file length (%d lines)" line-start total-lines))
          (when (> line-end total-lines)
            (error "line_number_end (%d) exceeds file length (%d lines)" line-end total-lines))

          ;; Go to start line
          (goto-line line-start)
          (let ((region-start (line-beginning-position)))

            ;; Go to end line and move to beginning of that line (exclusive)
            (goto-line line-end)
            (let ((region-end (line-beginning-position)))

              ;; Delete the specified region
              (delete-region region-start region-end)

              ;; Insert new contents at the start position
              (goto-char region-start)
              (insert contents)

              ;; Ensure there's a newline at the end if we're not at end of buffer
              ;; and the contents don't already end with a newline
              (when (and (not (eobp))
                         (not (string-suffix-p "\n" contents))
                         (not (looking-at-p "^\n")))
                (insert "\n"))))))

      ;; Save the buffer
      (save-buffer)

      ;; Return to original chat buffer and window
      (select-window original-window)
      (switch-to-buffer original-buffer)

      (if (and (= line-start line-end) (= line-start (1+ total-lines)))
          (format "Successfully appended %d characters to end of %s"
                  (length contents) expanded-path)
        (format "Successfully replaced lines %d-%d (exclusive) in %s with %d characters"
                line-start line-end expanded-path (length contents))))))

(defun aichat-tools--file-insert-text (file-path line-number contents)
  "Insert CONTENTS at LINE-NUMBER in FILE-PATH."
  (unless (stringp file-path)
    (error "file_path must be a string"))

  (unless (integerp line-number)
    (error "line_number must be an integer"))

  (unless (stringp contents)
    (error "contents must be a string"))

  ;; Use replace-code with same start and end line to insert at that position
  (aichat-tools--file-replace-region file-path line-number line-number contents)

  (format "Successfully inserted %d characters at line %d in %s"
          (length contents) line-number file-path))

(defun aichat-tools--file-delete-region (file-path line-start line-end)
  "Delete code between LINE-START and LINE-END (inclusive) in FILE-PATH."
  (unless (stringp file-path)
    (error "file_path must be a string"))

  (unless (integerp line-start)
    (error "line_number_start must be an integer"))

  (unless (integerp line-end)
    (error "line_number_end must be an integer"))

  (when (< line-end line-start)
    (error "line_number_end (%d) must be >= line_number_start (%d)" line-end line-start))

  ;; Use replace-code with empty contents to delete the range
  (aichat-tools--file-replace-region file-path line-start line-end "")

  (format "Successfully deleted lines %d-%d in %s" line-start line-end file-path))


(defun aichat-tools--prepend-text (file-path contents)
  "Prepend CONTENTS to the beginning of FILE-PATH."
  (unless (stringp file-path)
    (error "file_path must be a string"))

  (unless (stringp contents)
    (error "contents must be a string"))

  (let ((expanded-path (expand-file-name file-path))
        (original-buffer (current-buffer))
        (original-window (selected-window)))

    ;; Check file exists
    (unless (file-exists-p expanded-path)
      (error "File does not exist: %s" expanded-path))

    ;; Split screen if needed and open file
    (aichat-tools--setup-window-and-open-file expanded-path)

    ;; Go to beginning of file
    (goto-char (point-min))

    ;; Insert contents
    (insert contents)

    ;; Ensure there's a newline after the prepended content if it doesn't end with one
    (unless (string-suffix-p "\n" contents)
      (insert "\n"))

    ;; Save the buffer
    (save-buffer)

    ;; Return to original chat buffer and window
    (select-window original-window)
    (switch-to-buffer original-buffer)

    (format "Successfully prepended %d characters to beginning of %s"
            (length contents) expanded-path)))

(defun aichat-tools--append-text (file-path contents)
  "Append CONTENTS to the end of FILE-PATH."
  (unless (stringp file-path)
    (error "file_path must be a string"))

  (unless (stringp contents)
    (error "contents must be a string"))

  (let ((expanded-path (expand-file-name file-path))
        (original-buffer (current-buffer))
        (original-window (selected-window)))

    ;; Check file exists
    (unless (file-exists-p expanded-path)
      (error "File does not exist: %s" expanded-path))

    ;; Split screen if needed and open file
    (aichat-tools--setup-window-and-open-file expanded-path)

    ;; Go to end of file
    (goto-char (point-max))

    ;; Ensure there's a newline before the appended content if file doesn't end with one
    (unless (or (bobp) (looking-back "\n" 1))
      (insert "\n"))

    ;; Insert contents
    (insert contents)

    ;; Ensure file ends with newline
    (unless (string-suffix-p "\n" contents)
      (insert "\n"))

    ;; Save the buffer
    (save-buffer)

    ;; Return to original chat buffer and window
    (select-window original-window)
    (switch-to-buffer original-buffer)

    (format "Successfully appended %d characters to end of %s"
            (length contents) expanded-path)))

(defun aichat-tools--write-new-file (file-path contents)
  "Write CONTENTS to a new file at FILE-PATH. Fails if file already exists."
  (unless (stringp file-path)
    (error "file_path must be a string"))

  (unless (stringp contents)
    (error "contents must be a string"))

  (let ((expanded-path (expand-file-name file-path)))

    ;; Check if file already exists
    (when (file-exists-p expanded-path)
      (error "File already exists: %s" expanded-path))

    ;; Check if parent directory exists, if not create it
    (let ((parent-dir (file-name-directory expanded-path)))
      (unless (file-exists-p parent-dir)
        (make-directory parent-dir t)))

    ;; Write the file
    (condition-case err
        (with-temp-buffer
          (insert contents)
          (write-file expanded-path))
      (error (format "Failed to write file: %s" (error-message-string err))))

    (format "Successfully wrote new file %s with %d characters"
            expanded-path (length contents))))

(defun aichat-tools--make-directory (path)
  "Recursively create directory at PATH."
  (unless (stringp path)
    (error "path must be a string"))

  (let ((expanded-path (expand-file-name path)))

    ;; Check if path already exists
    (if (file-exists-p expanded-path)
        (if (file-directory-p expanded-path)
            (format "Directory already exists: %s" expanded-path)
          (error "Path exists but is not a directory: %s" expanded-path))

      ;; Create directory recursively
      (condition-case err
          (progn
            (make-directory expanded-path t)
            (format "Successfully created directory: %s" expanded-path))
        (error (format "Failed to create directory: %s" (error-message-string err)))))))

(defun aichat-tools--rename-file (old-path new-path)
  "Rename file from OLD-PATH to NEW-PATH."
  (unless (stringp old-path)
    (error "old_path must be a string"))

  (unless (stringp new-path)
    (error "new_path must be a string"))

  (let ((expanded-old-path (expand-file-name old-path))
        (expanded-new-path (expand-file-name new-path)))

    ;; Check if old file exists
    (unless (file-exists-p expanded-old-path)
      (error "Source file does not exist: %s" expanded-old-path))

    ;; Check if new file already exists
    (when (file-exists-p expanded-new-path)
      (error "Destination file already exists: %s" expanded-new-path))

    ;; Ensure destination directory exists
    (let ((parent-dir (file-name-directory expanded-new-path)))
      (unless (file-exists-p parent-dir)
        (make-directory parent-dir t)))

    ;; Rename the file
    (condition-case err
        (progn
          (rename-file expanded-old-path expanded-new-path)
          (format "Successfully renamed %s to %s" expanded-old-path expanded-new-path))
      (error (format "Failed to rename file: %s" (error-message-string err))))))

(provide 'aichat-tools)

;;; aichat-tools.el ends here
