;;; greger-tools.el --- Tool definitions for greger agent -*- lexical-binding: t -*-

;;; Commentary:
;; Defines tools available to the greger agent

;;; Code:

(require 'json)
(require 'magit)
(require 'rg)

(setq greger-tools-registry
      '(
        (read-file . ((name . "read-file")
                      (description . "Read the contents of a file from the filesystem")
                      (input_schema . ((type . "object")
                                       (properties . ((path . ((type . "string")
                                                               (description . "Path to the file to read")))
                                                      (include_line_numbers . ((type . "boolean")
                                                                               (description . "Whether to include line numbers in the output. If you plan to modify the file, you should include line numbers here so you know which lines to edit.")
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
                                                                          (description . "New function contents to replace the existing function.")))
                                                             (git_commit_message . ((type . "string")
                                                                                    (description . "Git commit message for this change")))))
                                              (required . ["file_path" "line_number" "name" "contents" "git_commit_message"])))))

        (file-replace-region . ((name . "file-replace-region")
                                (description . "Replace arbitrary code sections in a file by line numbers")
                                (input_schema . ((type . "object")
                                                 (properties . ((file_path . ((type . "string")
                                                                              (description . "Absolute path to the file")))
                                                                (line_number_start . ((type . "integer")
                                                                                      (description . "The line to start replacing from (inclusive)")))
                                                                (line_number_end . ((type . "integer")
                                                                                    (description . "The line to end replacing from (exclusive, i.e. the line number immediately after the last line to be replaced)")))
                                                                (contents . ((type . "string")
                                                                             (description . "Generated code contents to insert instead of the replaced content")))
                                                                (git_commit_message . ((type . "string")
                                                                                       (description . "Git commit message for this change")))))
                                                 (required . ["file_path" "line_number_start" "line_number_end" "contents" "git_commit_message"])))))

        (file-insert-text . ((name . "file-insert-text")
                             (description . "Insert text at a specific line number in a file. Use line number = file_length + 1 to append to end of file.")
                             (input_schema . ((type . "object")
                                              (properties . ((file_path . ((type . "string")
                                                                           (description . "Absolute path to the file")))
                                                             (line_number . ((type . "integer")
                                                                             (description . "The line number to insert at (content will be inserted before this line). Use file_length + 1 to append to end of file.")))
                                                             (contents . ((type . "string")
                                                                          (description . "Text contents to insert")))
                                                             (git_commit_message . ((type . "string")
                                                                                    (description . "Git commit message for this change")))))
                                              (required . ["file_path" "line_number" "contents" "git_commit_message"])))))

        (file-delete-region . ((name . "file-delete-region")
                               (description . "Delete code sections in a file by line numbers")
                               (input_schema . ((type . "object")
                                                (properties . ((file_path . ((type . "string")
                                                                             (description . "Absolute path to the file")))
                                                               (line_number_start . ((type . "integer")
                                                                                     (description . "The line to start deleting from (inclusive)")))
                                                               (line_number_end . ((type . "integer")
                                                                                   (description . "The line to end deleting from (exclusive, i.e. the line number immediately after the last line to be deleted)")))
                                                               (git_commit_message . ((type . "string")
                                                                                      (description . "Git commit message for this change")))))
                                                (required . ["file_path" "line_number_start" "line_number_end" "git_commit_message"])))))

        (file-prepend-text . ((name . "file-prepend-text")
                              (description . "Prepend text to the beginning of a file")
                              (input_schema . ((type . "object")
                                               (properties . ((file_path . ((type . "string")
                                                                            (description . "Absolute path to the file")))
                                                              (contents . ((type . "string")
                                                                           (description . "Text contents to prepend")))
                                                              (git_commit_message . ((type . "string")
                                                                                     (description . "Git commit message for this change")))))
                                               (required . ["file_path" "contents" "git_commit_message"])))))

        (file-append-text . ((name . "file-append-text")
                             (description . "Append text to the end of a file")
                             (input_schema . ((type . "object")
                                              (properties . ((file_path . ((type . "string")
                                                                           (description . "Absolute path to the file")))
                                                             (contents . ((type . "string")
                                                                          (description . "Text contents to append. Note that you may need to start your content with a newline depending on the existing contents of the file.")))
                                                             (git_commit_message . ((type . "string")
                                                                                    (description . "Git commit message for this change")))))
                                              (required . ["file_path" "contents" "git_commit_message"])))))

        (write-new-file . ((name . "write-new-file")
                           (description . "Write a new file with the given contents. Fails if the file already exists.")
                           (input_schema . ((type . "object")
                                            (properties . ((file_path . ((type . "string")
                                                                         (description . "Absolute path to the new file")))
                                                           (contents . ((type . "string")
                                                                        (description . "Contents to write to the new file")))
                                                           (git_commit_message . ((type . "string")
                                                                                  (description . "Git commit message for this change")))))
                                            (required . ["file_path" "contents" "git_commit_message"])))))

        (make-directory . ((name . "make-directory")
                           (description . "Recursively create a directory and all parent directories if they don't exist")
                           (input_schema . ((type . "object")
                                            (properties . ((path . ((type . "string")
                                                                    (description . "Path to the directory to create")))
                                                           (git_commit_message . ((type . "string")
                                                                                  (description . "Git commit message for this change")))))
                                            (required . ["path" "git_commit_message"])))))

        (rename-file . ((name . "rename-file")
                        (description . "Rename or move a file from one path to another")
                        (input_schema . ((type . "object")
                                         (properties . ((old_path . ((type . "string")
                                                                     (description . "Current path of the file")))
                                                        (new_path . ((type . "string")
                                                                     (description . "New path for the file")))
                                                        (git_commit_message . ((type . "string")
                                                                               (description . "Git commit message for this change")))))
                                         (required . ["old_path" "new_path" "git_commit_message"])))))

        ))

(defun greger-tools-get-schemas (tool-names)
  "Get tool schemas for TOOL-NAMES."
  (mapcar (lambda (tool-name)
            (or (alist-get tool-name greger-tools-registry)
                (error "Unknown tool: %s" tool-name)))
          tool-names))

(defun greger-tools-execute (tool-name args)
  "Execute TOOL-NAME with ARGS."
  (let ((tool-symbol (intern tool-name)))
    (cond
     ((eq tool-symbol 'read-file)
      (greger-tools--read-file
       (alist-get 'path args)
       (alist-get 'include_line_numbers args)))

     ((eq tool-symbol 'list-directory)
      (greger-tools--list-directory
       (or (alist-get 'path args) ".")
       (alist-get 'show-hidden args)
       (alist-get 'recursive args)))

     ((eq tool-symbol 'replace-function)
      (greger-tools--replace-function
       (alist-get 'file_path args)
       (alist-get 'line_number args)
       (alist-get 'name args)
       (alist-get 'contents args)
       (alist-get 'git_commit_message args)))

     ((eq tool-symbol 'file-replace-region)
      (greger-tools--file-replace-region
       (alist-get 'file_path args)
       (alist-get 'line_number_start args)
       (alist-get 'line_number_end args)
       (alist-get 'contents args)
       (alist-get 'git_commit_message args)))

     ((eq tool-symbol 'ripgrep)
      (greger-tools--ripgrep
       (alist-get 'pattern args)
       (or (alist-get 'path args) ".")
       (alist-get 'case-sensitive args)
       (alist-get 'file-type args)
       (or (alist-get 'context-lines args) 0)
       (or (alist-get 'max-results args) 50)))

     ((eq tool-symbol 'file-insert-text)
      (greger-tools--file-insert-text
       (alist-get 'file_path args)
       (alist-get 'line_number args)
       (alist-get 'contents args)
       (alist-get 'git_commit_message args)))

     ((eq tool-symbol 'file-delete-region)
      (greger-tools--file-delete-region
       (alist-get 'file_path args)
       (alist-get 'line_number_start args)
       (alist-get 'line_number_end args)
       (alist-get 'git_commit_message args)))

     ((eq tool-symbol 'file-prepend-text)
      (greger-tools--prepend-text
       (alist-get 'file_path args)
       (alist-get 'contents args)
       (alist-get 'git_commit_message args)))

     ((eq tool-symbol 'file-append-text)
      (greger-tools--append-text
       (alist-get 'file_path args)
       (alist-get 'contents args)
       (alist-get 'git_commit_message args)))

     ((eq tool-symbol 'write-new-file)
      (greger-tools--write-new-file
       (alist-get 'file_path args)
       (alist-get 'contents args)
       (alist-get 'git_commit_message args)))

     ((eq tool-symbol 'make-directory)
      (greger-tools--make-directory
       (alist-get 'path args)
       (alist-get 'git_commit_message args)))

     ((eq tool-symbol 'rename-file)
      (greger-tools--rename-file
       (alist-get 'old_path args)
       (alist-get 'new_path args)
       (alist-get 'git_commit_message args)))

     (t
      (error "Unknown tool: %s" tool-name)))))

(defun greger-tools--git-stage-and-commit (files commit-message)
  "Stage FILES and commit with COMMIT-MESSAGE using magit."
  (condition-case err
      (let* ((first-file (car files))
             (file-dir (file-name-directory (expand-file-name first-file)))
             (repo-root (magit-toplevel file-dir)))
        (unless repo-root
          (error "File %s is not in a git repository" first-file))

        ;; Set default-directory to the repository root for magit operations
        (let ((default-directory repo-root))
          ;; Stage the files
          (magit-stage-files files)

          ;; Create the commit
          (magit-commit-create (list "-m" commit-message))

          (format "Successfully staged %d file(s) and committed with message: %s"
                  (length files) commit-message)))
    (error
     (format "Git operation failed: %s" (error-message-string err)))))

(defun greger-tools--read-file (path &optional include-line-numbers)
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
              (greger-tools--add-line-numbers (buffer-string))
            (buffer-string)))
      (error (format "Failed to read file: %s" (error-message-string err))))))

(defun greger-tools--add-line-numbers (content)
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

(defun greger-tools--list-directory (path &optional show-hidden recursive)
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
                         (greger-tools--list-directory-recursive expanded-path show-hidden)
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

(defun greger-tools--list-directory-recursive (path show-hidden &optional prefix)
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
                                  (greger-tools--list-directory-recursive
                                   full-path show-hidden (concat prefix file "/")))))
          (push display-name files))))

    (reverse files)))

(defun greger-tools--ripgrep (pattern path &optional case-sensitive file-type context-lines max-results)
  "Search for PATTERN in PATH using the rg.el package."
  (unless (stringp pattern)
    (error "Pattern must be a string"))

  (unless (stringp path)
    (error "Path must be a string"))

  ;; Check if rg executable is available
  (unless (executable-find "rg")
    (error "ripgrep (rg) command not found. Please install ripgrep"))

  (let ((expanded-path (expand-file-name path))
        (original-buffer (current-buffer))
        (original-window (selected-window)))

    (unless (file-exists-p expanded-path)
      (error "Path does not exist: %s" expanded-path))

    ;; Set up the search parameters
    (let* ((search-dir (if (file-directory-p expanded-path)
                          expanded-path
                        (file-name-directory expanded-path)))
           (files-pattern (cond
                          ;; If path is a file, use just the filename pattern
                          ((file-regular-p expanded-path)
                           (file-name-nondirectory expanded-path))
                          ;; If file-type is specified, use that
                          (file-type file-type)
                          ;; Otherwise use "everything"
                          (t "everything")))
           (literal (not case-sensitive)) ; rg.el uses literal for case handling
           (flags '()))

      ;; Add case sensitivity flags
      (when case-sensitive
        (setq flags (append flags '("--case-sensitive"))))

      ;; Add context lines if specified
      (when (and context-lines (> context-lines 0))
        (setq flags (append flags (list (format "--context=%d" context-lines)))))

      ;; Add max results limit if specified
      (when (and max-results (> max-results 0))
        (setq flags (append flags (list (format "--max-count=%d" max-results)))))

      ;; Split window if not already split
      (when (= (length (window-list)) 1)
        (split-window-horizontally))

      ;; Run the search using rg.el
      (condition-case err
          (let ((default-directory search-dir))
            ;; Use rg-run to perform the search
            (rg-run pattern files-pattern search-dir literal nil flags)

            ;; Switch to the rg results buffer to show the user
            (other-window 1)
            (switch-to-buffer (rg-buffer-name))

            ;; Wait a moment for the search to complete and collect results
            (sit-for 0.5)

            ;; Get the buffer contents
            (let ((results (with-current-buffer (rg-buffer-name)
                            (buffer-string))))

              ;; Return to original window and buffer
              (select-window original-window)
              (switch-to-buffer original-buffer)

              ;; Return results or indicate no matches
              (if (or (string-empty-p (string-trim results))
                      (string-match-p "No matches found" results)
                      (string-match-p "0 matches" results))
                  "No matches found"
                results)))
        (error (format "Failed to execute ripgrep search: %s" (error-message-string err)))))))

(defun greger-tools--replace-function (file-path line-number name contents git-commit-message)
  "Replace function NAME at LINE-NUMBER in FILE-PATH with CONTENTS."
  (unless (stringp file-path)
    (error "file_path must be a string"))

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
    (greger-tools--setup-window-and-open-file expanded-path)

    ;; Go to the specified line
    (goto-line line-number)

    ;; Verify function declaration on this line
    (let ((current-line (thing-at-point 'line t)))
      (unless (greger-tools--line-declares-function-p current-line name expanded-path)
        (error "Function '%s' not declared on line %d" name line-number)))

    ;; Find function boundaries and replace
    (let ((func-start (point))
          (func-end (greger-tools--find-function-end expanded-path)))

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

      ;; Stage and commit changes - infer the file to stage
      (let ((git-result (greger-tools--git-stage-and-commit (list expanded-path) git-commit-message)))
        (format "Successfully replaced function '%s' in %s. %s" name expanded-path git-result)))))

(defun greger-tools--setup-window-and-open-file (file-path)
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

(defun greger-tools--line-declares-function-p (line func-name file-path)
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

(defun greger-tools--find-function-end (file-path)
  "Find the end of the current function based on file type."
  (cond
   ;; Python: find end by indentation
   ((string-suffix-p ".py" file-path)
    (greger-tools--find-python-function-end))

   ;; Elisp: find matching closing parenthesis
   ((string-suffix-p ".el" file-path)
    (greger-tools--find-elisp-function-end))

   (t (error "Unsupported file type"))))

(defun greger-tools--find-python-function-end ()
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

(defun greger-tools--find-elisp-function-end ()
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

(defun greger-tools--file-replace-region (file-path line-start line-end contents git-commit-message)
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
    (greger-tools--setup-window-and-open-file expanded-path)

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

      ;; Stage and commit changes - infer the file to stage
      (let ((git-result (greger-tools--git-stage-and-commit (list expanded-path) git-commit-message)))
        (if (and (= line-start line-end) (= line-start (1+ total-lines)))
            (format "Successfully appended %d characters to end of %s. %s"
                    (length contents) expanded-path git-result)
          (format "Successfully replaced lines %d-%d (exclusive) in %s with %d characters. %s"
                  line-start line-end expanded-path (length contents) git-result))))))

(defun greger-tools--file-insert-text (file-path line-number contents git-commit-message)
  "Insert CONTENTS at LINE-NUMBER in FILE-PATH."
  (unless (stringp file-path)
    (error "file_path must be a string"))

  (unless (integerp line-number)
    (error "line_number must be an integer"))

  (unless (stringp contents)
    (error "contents must be a string"))

  ;; Use replace-code with same start and end line to insert at that position
  (greger-tools--file-replace-region file-path line-number line-number contents git-commit-message))

(defun greger-tools--file-delete-region (file-path line-start line-end git-commit-message)
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
  (greger-tools--file-replace-region file-path line-start line-end "" git-commit-message))

(defun greger-tools--prepend-text (file-path contents git-commit-message)
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
    (greger-tools--setup-window-and-open-file expanded-path)

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

    ;; Stage and commit changes - infer the file to stage
    (let ((git-result (greger-tools--git-stage-and-commit (list expanded-path) git-commit-message)))
      (format "Successfully prepended %d characters to beginning of %s. %s"
              (length contents) expanded-path git-result))))

(defun greger-tools--append-text (file-path contents git-commit-message)
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
    (greger-tools--setup-window-and-open-file expanded-path)

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

    ;; Stage and commit changes - infer the file to stage
    (let ((git-result (greger-tools--git-stage-and-commit (list expanded-path) git-commit-message)))
      (format "Successfully appended %d characters to end of %s. %s"
              (length contents) expanded-path git-result))))

(defun greger-tools--write-new-file (file-path contents git-commit-message)
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

    ;; Stage and commit changes - infer the file to stage
    (let ((git-result (greger-tools--git-stage-and-commit (list expanded-path) git-commit-message)))
      (format "Successfully wrote new file %s with %d characters. %s"
              expanded-path (length contents) git-result))))

(defun greger-tools--make-directory (path git-commit-message)
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
            ;; For directory creation, we might want to stage a .gitkeep file or similar
            ;; For now, we'll stage the directory path itself (though git doesn't track empty dirs)
            (let ((git-result (greger-tools--git-stage-and-commit (list expanded-path) git-commit-message)))
              (format "Successfully created directory: %s. %s" expanded-path git-result)))
        (error (format "Failed to create directory: %s" (error-message-string err)))))))

(defun greger-tools--rename-file (old-path new-path git-commit-message)
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
          ;; Stage both old and new paths (git mv operation)
          (let ((git-result (greger-tools--git-stage-and-commit
                            (list expanded-old-path expanded-new-path)
                            git-commit-message)))
            (format "Successfully renamed %s to %s. %s" expanded-old-path expanded-new-path git-result)))
      (error (format "Failed to rename file: %s" (error-message-string err))))))

(provide 'greger-tools)

;;; greger-tools.el ends here
