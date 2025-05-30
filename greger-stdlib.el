;;; greger-stdlib.el --- Tool definitions for greger agent -*- lexical-binding: t -*-

;;; Commentary:
;; Defines tools available to the greger agent

;;; Code:

(require 'greger-tools)
(require 'greger-web)

;; Register all tools using the macro
(greger-register-tool "read-file"
  :description "Read the contents of a file from the filesystem"
  :properties '((path . ((type . "string")
                         (description . "Path to the file to read")))
                (include_line_numbers . ((type . "boolean")
                                         (description . "Whether to include line numbers in the output. If you plan to modify the file, you should include line numbers here so you know which lines to edit.")
                                         (default . nil)))
                (start_line . ((type . "integer")
                               (description . "Starting line number (1-based) to begin reading from. If not specified, reads from the beginning of the file.")
                               (default . nil)))
                (end_line . ((type . "integer")
                             (description . "Ending line number (1-based) to stop reading at (inclusive). If not specified, reads to the end of the file.")
                             (default . nil))))
  :required '("path")
  :function 'greger-tools--read-file)

(greger-register-tool "list-directory"
  :description "List files and directories in a given directory"
  :properties '((path . ((type . "string")
                         (description . "Path to the directory to list. Defaults to current directory.")
                         (default . ".")))
                (show-hidden . ((type . "boolean")
                                (description . "Whether to show hidden files starting with .")
                                (default . nil)))
                (recursive . ((type . "boolean")
                              (description . "Whether to list files recursively")
                              (default . nil))))
  :required '()
  :function 'greger-tools--list-directory)

(greger-register-tool "ripgrep"
  :description "Search for patterns in files using ripgrep (rg) command line tool. Note that ripgrep only matches on single lines, so you can't search across multiple lines."
  :properties '((pattern . ((type . "string")
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
                                (default . 50))))
  :required '("pattern")
  :function 'greger-tools--ripgrep
  :pass-callback t)

(greger-register-tool "write-new-file"
  :description "Write a new file with the given contents. Fails if the file already exists."
  :properties '((file_path . ((type . "string")
                              (description . "Absolute path to the new file")))
                (contents . ((type . "string")
                             (description . "Contents to write to the new file")))
                (git_commit_message . ((type . "string")
                                       (description . "Git commit message for this change"))))
  :required '("file_path" "contents" "git_commit_message")
  :function 'greger-tools--write-new-file
  :pass-buffer t)

(greger-register-tool "make-directory"
  :description "Recursively create a directory and all parent directories if they don't exist"
  :properties '((path . ((type . "string")
                         (description . "Path to the directory to create")))
                (git_commit_message . ((type . "string")
                                       (description . "Git commit message for this change"))))
  :required '("path" "git_commit_message")
  :function 'greger-tools--make-directory
  :pass-buffer t)

(greger-register-tool "rename-file"
  :description "Rename or move a file from one path to another"
  :properties '((old_path . ((type . "string")
                             (description . "Current path of the file")))
                (new_path . ((type . "string")
                             (description . "New path for the file")))
                (git_commit_message . ((type . "string")
                                       (description . "Git commit message for this change"))))
  :required '("old_path" "new_path" "git_commit_message")
  :function 'greger-tools--rename-file
  :pass-buffer t)

(greger-register-tool "replace-function"
  :description "Replace a function in a Python or Elisp file with new contents. Fast and reliable - only supports 'def function_name' and '(defun function_name ())' forms. Use replace-file for complete file replacement or str-replace for other specific changes."
  :properties '((file_path . ((type . "string")
                              (description . "Path to the file containing the function")))
                (function_name . ((type . "string")
                                  (description . "Name of the function to replace")))
                (contents . ((type . "string")
                             (description . "New function contents to replace the existing function")))
                (line_number . ((type . "integer")
                                (description . "Line number where the function is defined")))
                (commit_message . ((type . "string")
                                   (description . "Git commit message for this change"))))
  :required '("file_path" "function_name" "contents" "line_number" "commit_message")
  :function 'greger-tools--replace-function
  :pass-buffer t)

(greger-register-tool "replace-file"
  :description "Replace the entire contents of an existing file. Slow but reliable - replaces the complete file contents. Use str-replace for specific changes in large files, or replace-function for Python/Elisp functions."
  :properties '((file_path . ((type . "string")
                              (description . "Path to the file to replace")))
                (contents . ((type . "string")
                             (description . "New contents to replace the entire file")))
                (git_commit_message . ((type . "string")
                                       (description . "Git commit message for this change"))))
  :required '("file_path" "contents" "git_commit_message")
  :function 'greger-tools--replace-file
  :pass-buffer t)

(greger-register-tool "str-replace"
  :description "Replace a specific string or content block in a file with new content. Finds the exact original content and replaces it with new content. Be extra careful to format the original_content exactly correctly, taking extra care with whitespace and newlines. If you're making large swaths of changes, consider using replace-file instead"
  :properties '((file_path . ((type . "string")
                              (description . "Path to the file to modify")))
                (original_content . ((type . "string")
                                     (description . "The exact content to find and replace")))
                (new_content . ((type . "string")
                                (description . "The new content to replace the original content with")))
                (git_commit_message . ((type . "string")
                                       (description . "Git commit message for this change"))))
  :required '("file_path" "original_content" "new_content" "git_commit_message")
  :function 'greger-tools--str-replace
  :pass-buffer t)

(greger-register-tool "insert"
  :description "Insert text at a specific line number in a file. The text will be inserted before the specified line number (use 0 to insert at the beginning of the file, 1 to insert before the first line, etc.). Useful for adding new content, comments, or code blocks at precise locations without replacing existing content."
  :properties '((file_path . ((type . "string")
                              (description . "Path to the file to modify")))
                (line_number . ((type . "integer")
                                (description . "Line number before which to insert the content (0 for beginning of file, 1 to insert before first line, etc.)")))
                (content . ((type . "string")
                            (description . "Content to insert at the specified location")))
                (git_commit_message . ((type . "string")
                                       (description . "Git commit message for this change"))))
  :required '("file_path" "line_number" "content" "git_commit_message")
  :function 'greger-tools--insert
  :pass-buffer t)

(greger-register-tool "git-log"
  :description "View git commit logs."
  :properties '((path . ((type . "string")
                         (description . "Path to the git repository or any file in the repository view logs for")
                         (default . ".")))
                (max_rows . ((type . "integer")
                            (description . "Maximum number of log entries to return")
                            (default . 100))))
  :required '()
  :function 'greger-tools--git-log)

(greger-register-tool "git-show-commit"
  :description "View a specific git commit."
  :properties '((commit_hash . ((type . "string")
                                (description . "The commit hash to view")))
                (path . ((type . "string")
                         (description . "Path to the git repository or any file in the repository")
                         (default . "."))))
  :required '("commit_hash")
  :function 'greger-tools--git-show-commit)

(greger-register-tool "ert-test"
  :description "Execute ERT tests by evaluating Emacs lisp test functions and running them with ert"
  :properties '((test_file_path . ((type . "string")
                                   (description . "Path to the test file containing ERT tests")))
                (function_names . ((type . "array")
                                   (items . ((type . "string")))
                                   (description . "List of ERT test function names to evaluate and run"))))
  :required '("test_file_path" "function_names")
  :function 'greger-tools--ert-test)

(greger-register-tool "eval-elisp-defuns"
  :description "Evaluate Emacs lisp defuns in a specific file. Note that defuns must be evaluated before you run ert-test to test them, if you've updated the code."
  :properties '((file_path . ((type . "string")
                              (description . "Path to the file containing functions/defuns to evaluate")))
                (function_names . ((type . "array")
                                   (items . ((type . "string")))
                                   (description . "List of function names to evaluate and run"))))
  :required '("file_path" "function_names")
  :function 'greger-tools--eval-elisp-defuns)

(greger-register-tool "shell-command"
  :description "Execute an arbitrary shell command and return the output. Prompts for permission before running the command for security."
  :properties '((command . ((type . "string")
                            (description . "The shell command to execute")))
                (working_directory . ((type . "string")
                                      (description . "Directory to run the command in")
                                      (default . "."))))
  :required '("command")
  :function 'greger-tools--shell-command
  :pass-callback t
  :pass-metadata t)

(greger-register-tool "read-webpage"
  :description "Read webpage content from a URL. Can return either extracted text or raw HTML."
  :properties '((url . ((type . "string")
                        (description . "The URL to read content from")))
                (extract_text . ((type . "boolean")
                                 (description . "Whether to extract text content or return raw HTML")
                                 (default . t)))
                (use_highest_readability . ((type . "boolean")
                                            (description . "Whether to use eww's aggressive highest readability setting for better text extraction")
                                            (default . nil))))
  :required '("url")
  :function 'greger-tools--read-webpage)

;; Helper functions

(defun greger-tools--run-async-subprocess (command args working-directory callback)
  "Run COMMAND with ARGS in WORKING-DIRECTORY and call CALLBACK with (output error).
CALLBACK will be called with (output nil) on success or (nil error-message) on failure."
  (let* ((process-name (format "greger-subprocess-%s" (make-temp-name "")))
         (process-buffer (generate-new-buffer (format " *%s*" process-name)))
         (default-directory (expand-file-name (or working-directory ".")))
         (full-command (cons command args)))

    (condition-case err
        (let ((process (apply #'start-process process-name process-buffer command args)))
          (set-process-query-on-exit-flag process nil)

          (set-process-sentinel
           process
           (lambda (proc event)
             (let ((exit-status (process-exit-status proc))
                   (output (with-current-buffer process-buffer
                            (buffer-string))))
               (kill-buffer process-buffer)
               (cond
                ((= exit-status 0)
                 (funcall callback
                         (if (string-empty-p (string-trim output))
                             "(no output)"
                           output)
                         nil))
                (t
                 (funcall callback nil
                         (format "Command failed with exit code %d: %s"
                                exit-status
                                (if (string-empty-p (string-trim output))
                                    "(no output)"
                                  output))))))))
          process)
      (error
       (when (buffer-live-p process-buffer)
         (kill-buffer process-buffer))
       (funcall callback nil (format "Failed to start process: %s" (error-message-string err)))))))

(defun greger-tools--find-git-repo-root (start-dir)
  "Find the git repository root starting from START-DIR."
  (let ((dir (expand-file-name start-dir)))
    (while (and dir
                (not (file-exists-p (expand-file-name ".git" dir)))
                (not (string= dir (directory-file-name dir))))
      (setq dir (file-name-directory (directory-file-name dir))))
    (when (and dir (file-exists-p (expand-file-name ".git" dir)))
      dir)))

(defun greger-tools--is-file-tracked-by-git (file-path repo-root)
  "Check if FILE-PATH is tracked by git in REPO-ROOT.
Returns t if the file is tracked, nil otherwise."
  (let ((default-directory repo-root)
        (relative-path (file-relative-name (expand-file-name file-path) repo-root)))
    (= 0 (call-process "git" nil nil nil "ls-files" "--error-unmatch" relative-path))))

;; Tools below

(defun greger-tools--git-stage-and-commit (files commit-message &optional chat-buffer)
  "Stage FILES and commit with COMMIT-MESSAGE using git command line.
If CHAT-BUFFER is provided, also stage and commit the chat buffer file."
  (condition-case err
      (let* ((first-file (car files))
             (file-dir (file-name-directory (expand-file-name first-file)))
             (repo-root (greger-tools--find-git-repo-root file-dir)))
        (unless repo-root
          (error "File %s is not in a git repository" first-file))

        ;; Set default-directory to the repository root for git operations
        (let ((default-directory repo-root)
              (all-files files))

          ;; Add chat buffer file if provided, it has a file, and it's already tracked in git
          (when (and chat-buffer (buffer-file-name chat-buffer))
            (let ((chat-file (buffer-file-name chat-buffer)))
              ;; Only proceed if the chat file is already tracked by git
              (when (greger-tools--is-file-tracked-by-git chat-file repo-root)
                ;; Save the chat buffer first if it has unsaved changes
                (with-current-buffer chat-buffer
                  (when (buffer-modified-p)
                    (save-buffer)))
                ;; Add chat file to the list of files to stage
                (push chat-file all-files))))

          ;; Stage the files
          (dolist (file all-files)
            (let ((relative-path (file-relative-name (expand-file-name file) repo-root)))
              (unless (= 0 (call-process "git" nil nil nil "add" relative-path))
                (error "Failed to stage file: %s" file))))

          ;; Create the commit
          (unless (= 0 (call-process "git" nil nil nil "commit" "-m" commit-message))
            (error "Failed to create commit"))

          (format "Successfully staged %d file(s) and committed with message: %s"
                  (length all-files) commit-message)))
    (error
     (format "Git operation failed: %s" (error-message-string err)))))

(defun greger-tools--read-file (path &optional include-line-numbers start-line end-line)
  "Read file at PATH. If INCLUDE-LINE-NUMBERS is non-nil, prepend line numbers.
If START-LINE is specified, start reading from that line (1-based).
If END-LINE is specified, stop reading at that line (inclusive, 1-based)."
  (unless (stringp path)
    (error "Path must be a string"))

  (when (and start-line (not (integerp start-line)))
    (error "start-line must be an integer"))

  (when (and end-line (not (integerp end-line)))
    (error "end-line must be an integer"))

  (when (and start-line (< start-line 1))
    (error "start-line must be >= 1"))

  (when (and end-line (< end-line 1))
    (error "end-line must be >= 1"))

  (when (and start-line end-line (> start-line end-line))
    (error "start-line must be <= end-line"))

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
          (let* ((all-lines (split-string (buffer-string) "\n"))
                 (total-lines (length all-lines))
                 (actual-start (or start-line 1))
                 (actual-end (or end-line total-lines))
                 (selected-lines (greger-tools--extract-line-range all-lines actual-start actual-end))
                 (contents (mapconcat 'identity selected-lines "\n")))
            (if include-line-numbers
                (greger-tools--add-line-numbers-with-offset contents actual-start)
              contents)))
      (error (format "Failed to read file: %s" (error-message-string err))))))

(defun greger-tools--extract-line-range (lines start-line end-line)
  "Extract lines from LINES between START-LINE and END-LINE (1-based, inclusive)."
  (let ((start-index (1- start-line))  ; Convert to 0-based index
        (end-index (1- end-line)))     ; Convert to 0-based index
    ;; Ensure indices are within bounds
    (setq start-index (max 0 start-index))
    (setq end-index (min (1- (length lines)) end-index))
    ;; Extract the range
    (cl-subseq lines start-index (1+ end-index))))

(defun greger-tools--add-line-numbers-with-offset (content start-line-num)
  "Add line numbers to CONTENT string starting from START-LINE-NUM."
  (let ((lines (split-string content "\n"))
        (line-num start-line-num)
        (max-width 0)
        result)

    ;; Calculate the width needed for line numbers (based on the highest line number)
    (setq max-width (length (number-to-string (+ start-line-num (length lines) -1))))

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

(defun greger-tools--ripgrep (pattern path callback &optional case-sensitive file-type context-lines max-results)
  "Search for PATTERN in PATH using the rg command line tool directly."
  (cond
   ((not (stringp pattern))
    (funcall callback nil "Pattern must be a string"))

   ((not (stringp path))
    (funcall callback nil "Path must be a string"))

   ((not (executable-find "rg"))
    (funcall callback nil "ripgrep (rg) command not found. Please install ripgrep"))

   (t
    (let ((expanded-path (expand-file-name path)))
      (if (not (file-exists-p expanded-path))
          (funcall callback nil (format "Path does not exist: %s" expanded-path))

        ;; Build the rg command arguments
        (let ((args '()))

          ;; Add case sensitivity flag
          (if case-sensitive
              (setq args (append args '("--case-sensitive")))
            (setq args (append args '("--smart-case"))))

          ;; Add context lines if specified
          (when (and context-lines (> context-lines 0))
            (setq args (append args (list "--context" (number-to-string context-lines)))))

          ;; Add max results limit if specified (using --max-count)
          (when (and max-results (> max-results 0))
            (setq args (append args (list "--max-count" (number-to-string max-results)))))

          ;; Add file type if specified
          (when (and file-type (not (string-empty-p file-type)))
            (setq args (append args (list "--type" file-type))))

          ;; Add line numbers and no heading for better output format
          (setq args (append args '("--line-number" "--no-heading")))

          ;; Add the pattern and path
          (setq args (append args (list pattern expanded-path)))

          ;; Execute the command asynchronously
          (greger-tools--run-async-subprocess
           "rg" args nil
           (lambda (output error)
             (if error
                 (funcall callback nil (format "Failed to execute ripgrep search: %s" error))
               (funcall callback
                       (if (string-empty-p (string-trim output))
                           "No matches found"
                         output)
                       nil)))))))))

(defun greger-tools--write-new-file (file-path contents git-commit-message &optional buffer)
  "Write CONTENTS to a new file at FILE-PATH. Fails if file already exists.
If BUFFER is provided, it will be staged and committed along with the new file."
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
    (let ((git-result (greger-tools--git-stage-and-commit (list expanded-path) git-commit-message buffer)))
      (format "Successfully wrote new file %s with %d characters. %s"
              expanded-path (length contents) git-result))))

(defun greger-tools--make-directory (path git-commit-message &optional buffer)
  "Recursively create directory at PATH.
If BUFFER is provided, it will be staged and committed along with the directory."
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
            (let ((git-result (greger-tools--git-stage-and-commit (list expanded-path) git-commit-message buffer)))
              (format "Successfully created directory: %s. %s" expanded-path git-result)))
        (error (format "Failed to create directory: %s" (error-message-string err)))))))

(defun greger-tools--rename-file (old-path new-path git-commit-message &optional buffer)
  "Rename file from OLD-PATH to NEW-PATH.
If BUFFER is provided, it will be staged and committed along with the renamed file."
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
                             git-commit-message buffer)))
            (format "Successfully renamed %s to %s. %s" expanded-old-path expanded-new-path git-result)))
      (error (format "Failed to rename file: %s" (error-message-string err))))))

(defun greger-tools--replace-function (file-path function-name contents line-number commit-message &optional buffer)
  "Replace FUNCTION-NAME in FILE-PATH with new CONTENTS at LINE-NUMBER.
If BUFFER is provided, it will be staged and committed along with the modified file."
  (unless (stringp file-path)
    (error "file_path must be a string"))

  (unless (stringp function-name)
    (error "function_name must be a string"))

  (unless (stringp contents)
    (error "contents must be a string"))

  (unless (integerp line-number)
    (error "line_number must be an integer"))

  (unless (stringp commit-message)
    (error "commit_message must be a string"))

  (let ((expanded-path (expand-file-name file-path)))

    ;; Check if file exists
    (unless (file-exists-p expanded-path)
      (error "File does not exist: %s" expanded-path))

    ;; Check file extension for supported languages
    (unless (or (string-suffix-p ".py" expanded-path)
                (string-suffix-p ".el" expanded-path))
      (error "Only Python (.py) and Elisp (.el) files are supported"))

    (with-current-buffer (find-file-noselect expanded-path)
     ;; Go to the specified line number
     (goto-line line-number)

     ;; Verify function name is at this line
     (beginning-of-line)
     (let ((line-content (buffer-substring-no-properties
                          (line-beginning-position)
                          (line-end-position))))

       ;; Check if function is defined on this line based on file type
       (let ((function-pattern
              (cond
               ((string-suffix-p ".py" expanded-path)
                (format "^\\s-*def\\s-+%s\\s-*(" (regexp-quote function-name)))
               ((string-suffix-p ".el" expanded-path)
                (format "^\\s-*(defun\\s-+%s\\s-*(" (regexp-quote function-name)))
               (t (error "Unsupported file type")))))

         (unless (string-match-p function-pattern line-content)
           (error "Function '%s' not found at line %d in %s. Line content: %s"
                  function-name line-number expanded-path line-content))))

     ;; Delete the existing function using end-of-defun then beginning-of-defun
     (let (start-pos end-pos)
       ;; First go to end of defun to get the end position
       (end-of-defun)
       (setq end-pos (point))

       ;; Then go to beginning of defun to get start position
       (beginning-of-defun)
       (setq start-pos (point))

       ;; Delete the function
       (delete-region start-pos end-pos)

       ;; Insert new contents
       (insert contents)

       ;; Ensure there's a newline at the end if not present
       (unless (string-suffix-p "\n" contents)
         (insert "\n")))

     ;; Save the file
     (save-buffer))

    ;; Stage and commit the file
    (let ((git-result (greger-tools--git-stage-and-commit (list expanded-path) commit-message buffer)))
      (format "Successfully replaced function '%s' in %s. %s"
              function-name expanded-path git-result))))

(defun greger-tools--replace-file (file-path contents git-commit-message &optional buffer)
  "Replace the entire contents of FILE-PATH with CONTENTS.
If BUFFER is provided, it will be staged and committed along with the modified file."
  (unless (stringp file-path)
    (error "file_path must be a string"))

  (unless (stringp contents)
    (error "contents must be a string"))

  (unless (stringp git-commit-message)
    (error "git_commit_message must be a string"))

  (let ((expanded-path (expand-file-name file-path)))

    ;; Check if file exists
    (unless (file-exists-p expanded-path)
      (error "File does not exist: %s" expanded-path))

    ;; Check if it's actually a file and not a directory
    (when (file-directory-p expanded-path)
      (error "Path is a directory, not a file: %s" expanded-path))

    (with-current-buffer (find-file-noselect expanded-path)
     ;; Select all content and replace it
     (erase-buffer)
     (insert contents)

     ;; Save the file
     (save-buffer))

    ;; Stage and commit the file
    (let ((git-result (greger-tools--git-stage-and-commit (list expanded-path) git-commit-message buffer)))
      (format "Successfully replaced contents of %s with %d characters. %s"
              expanded-path (length contents) git-result))))

(defun greger-tools--str-replace (file-path original-content new-content git-commit-message &optional buffer)
  "Replace ORIGINAL-CONTENT with NEW-CONTENT in FILE-PATH.
If BUFFER is provided, it will be staged and committed along with the modified file."
  (unless (stringp file-path)
    (error "file_path must be a string"))

  (unless (stringp original-content)
    (error "original_content must be a string"))

  (unless (stringp new-content)
    (error "new_content must be a string"))

  (unless (stringp git-commit-message)
    (error "git_commit_message must be a string"))

  (let ((expanded-path (expand-file-name file-path)))

    ;; Check if file exists
    (unless (file-exists-p expanded-path)
      (error "File does not exist: %s" expanded-path))

    ;; Check if it's actually a file and not a directory
    (when (file-directory-p expanded-path)
      (error "Path is a directory, not a file: %s" expanded-path))

    (with-current-buffer (find-file-noselect expanded-path)
     ;; Use isearch to find the original content
     (goto-char (point-min))
     (let ((case-fold-search nil)) ; Make search case-sensitive
       (if (search-forward original-content nil t)
           (progn
             ;; Replace the found content
             (replace-match new-content nil t)
             ;; Save the file
             (save-buffer))
         (error "Original content not found in file: %s -- Try again!" expanded-path))))

    ;; Stage and commit the file
    (let ((git-result (greger-tools--git-stage-and-commit (list expanded-path) git-commit-message buffer)))
      (format "Successfully replaced content in %s. %s" expanded-path git-result))))

(defun greger-tools--insert (file-path line-number content git-commit-message &optional buffer)
  "Insert CONTENT at LINE-NUMBER in FILE-PATH.
If BUFFER is provided, it will be staged and committed along with the modified file."
  (unless (stringp file-path)
    (error "file_path must be a string"))

  (unless (integerp line-number)
    (error "line_number must be an integer"))

  (unless (>= line-number 0)
    (error "line_number must be >= 0"))

  (unless (stringp content)
    (error "content must be a string"))

  (unless (stringp git-commit-message)
    (error "git_commit_message must be a string"))

  (let ((expanded-path (expand-file-name file-path)))

    ;; Check if file exists
    (unless (file-exists-p expanded-path)
      (error "File does not exist: %s" expanded-path))

    ;; Check if it's actually a file and not a directory
    (when (file-directory-p expanded-path)
      (error "Path is a directory, not a file: %s" expanded-path))

    (with-current-buffer (find-file-noselect expanded-path)
     ;; Navigate to the insertion point
     (goto-char (point-min))
     (if (= line-number 0)
         ;; Insert at beginning of file
         (goto-char (point-min))
       ;; Go to the specified line - this is where the fix is needed
       (goto-line line-number)
       ;; Move to beginning of line to insert before it, not after it
       (beginning-of-line))

     ;; Insert the content
     (if (= line-number 0)
         ;; At beginning of file, insert content and newline
         (progn
           (insert content)
           (unless (string-suffix-p "\n" content)
             (insert "\n")))
       ;; Before a line, insert content then newline
       (progn
         (insert content)
         (unless (string-suffix-p "\n" content)
           (insert "\n"))))

     ;; Save the file
     (save-buffer))

    ;; Stage and commit the file
    (let ((git-result (greger-tools--git-stage-and-commit (list expanded-path) git-commit-message buffer)))
      (format "Successfully inserted %d characters at line %d in %s. %s"
              (length content) line-number expanded-path git-result))))

(defun greger-tools--git-log (path &optional max-rows)
  "View git commit logs using git command line for PATH."
  (unless (stringp path)
    (error "path must be a string"))

  (let ((expanded-path (expand-file-name path))
        (max-count (or max-rows 100)))

    (unless (file-exists-p expanded-path)
      (error "Path does not exist: %s" expanded-path))

    ;; Get the directory to search for git repo (if path is a file, use its directory)
    (let* ((search-dir (if (file-directory-p expanded-path)
                          expanded-path
                        (file-name-directory expanded-path)))
           (repo-root (greger-tools--find-git-repo-root search-dir)))
      (unless repo-root
        (error "Path %s is not in a git repository" expanded-path))

      (condition-case err
          (let ((default-directory repo-root))
            (with-temp-buffer
              (let ((exit-code (call-process "git" nil t nil "log"
                                           "--oneline" "--decorate" "--graph"
                                           (format "--max-count=%d" max-count))))
                (if (= exit-code 0)
                    (let ((results (buffer-string)))
                      (if (string-empty-p (string-trim results))
                          "No git log available"
                        results))
                  (error "Git log command failed with exit code %d" exit-code)))))
        (error (format "Failed to retrieve git log: %s" (error-message-string err)))))))

(defun greger-tools--git-show-commit (commit-hash path)
  "View git commit using git command line for PATH."
  (unless (stringp commit-hash)
    (error "commit_hash must be a string"))

  (unless (stringp path)
    (error "path must be a string"))

  (let ((expanded-path (expand-file-name path)))

    (unless (file-exists-p expanded-path)
      (error "Path does not exist: %s" expanded-path))

    ;; Get the directory to search for git repo (if path is a file, use its directory)
    (let* ((search-dir (if (file-directory-p expanded-path)
                          expanded-path
                        (file-name-directory expanded-path)))
           (repo-root (greger-tools--find-git-repo-root search-dir)))
      (unless repo-root
        (error "Path %s is not in a git repository" expanded-path))

      (condition-case err
          (let ((default-directory repo-root))
            (with-temp-buffer
              (let ((exit-code (call-process "git" nil t nil "show" commit-hash)))
                (if (= exit-code 0)
                    (let ((results (buffer-string)))
                      (if (string-empty-p (string-trim results))
                          "No git commit available"
                        results))
                  (error "Git show command failed with exit code %d" exit-code)))))
        (error (format "Failed to show git commit: %s" (error-message-string err)))))))

(defun greger-tools--eval-elisp-defuns (file-path function-names)
  (unless (stringp file-path)
    (error "File path must be a string"))

  (unless (vectorp function-names)
    (error "Function names must be a vector"))

  (let ((expanded-path (expand-file-name file-path)))
    ;; Check if file exists
    (unless (file-exists-p expanded-path)
      (error "File does not exist: %s" expanded-path))

    ;; Check if it's actually a file
    (when (file-directory-p expanded-path)
      (error "Path is a directory, not a file: %s" expanded-path))

    (with-current-buffer (find-file-noselect expanded-path)
     ;; Navigate to and evaluate each function
     (dotimes (i (length function-names))
       (let ((function-name (aref function-names i)))
         (goto-char (point-min))

         ;; Search for the function definition
         (let ((function-pattern (format "^\\s-*(\\(ert-deftest\\|defun\\)\\s-+%s\\s-*("
                                         (regexp-quote function-name))))
           (unless (re-search-forward function-pattern nil t)
             (error "Function '%s' not found in %s" function-name expanded-path))

           ;; Move to beginning of defun and evaluate it
           (beginning-of-defun)
           (eval-defun nil)))))
    "Eval successful"))

(defun greger-tools--ert-test (test-file-path function-names)
  "Execute ERT tests by evaluating test functions and running them with ert.
TEST-FILE-PATH is the path to the test file.
FUNCTION-NAMES is a vector of test function names to evaluate and run."

  ;; First eval the test function names
  (greger-tools--eval-elisp-defuns test-file-path function-names)

  (condition-case err
      (progn
        ;; Now run the tests using ert-run-tests with a custom listener
        (let* ((function-names-list (append function-names nil)) ; Convert vector to list for processing
               (test-selector `(member ,@(mapcar #'intern function-names-list)))
               (output-lines '())
               (stats nil))

          ;; Define a custom listener to capture test output
          (let ((listener
                 (lambda (event-type &rest event-args)
                   (cl-case event-type
                     (run-started
                      (cl-destructuring-bind (stats-obj) event-args
                        (setq stats stats-obj)
                        (push (format "Running %d tests (%s)"
                                      (length (ert--stats-tests stats-obj))
                                      (current-time-string))
                              output-lines)))
                     (test-started
                      (cl-destructuring-bind (stats-obj test) event-args
                        (push (format "Test %S started" (ert-test-name test))
                              output-lines)))
                     (test-ended
                      (cl-destructuring-bind (stats-obj test result) event-args
                        (let ((test-name (ert-test-name test))
                              (expected-p (ert-test-result-expected-p test result)))
                          (push (format "Test %S: %s%s%s"
                                        test-name
                                        (ert-string-for-test-result result expected-p)
                                        (if (ert-test-result-duration result)
                                            (format " (%.3fs)" (ert-test-result-duration result))
                                          "")
                                        (let ((reason (ert-reason-for-test-result result)))
                                          (if (string-empty-p reason) "" reason)))
                                output-lines)
                          ;; Add condition details for failures
                          (when (and (ert-test-result-with-condition-p result)
                                     (not expected-p))
                            (let ((condition (ert-test-result-with-condition-condition result)))
                              (push (format "  Condition: %S" condition) output-lines))))))
                     (run-ended
                      (cl-destructuring-bind (stats-obj aborted-p) event-args
                        (let ((completed (ert-stats-completed stats-obj))
                              (expected (ert-stats-completed-expected stats-obj))
                              (unexpected (ert-stats-completed-unexpected stats-obj))
                              (skipped (ert-stats-skipped stats-obj)))
                          (push (format "\n%sRan %d tests, %d results as expected, %d unexpected%s"
                                        (if aborted-p "Aborted: " "")
                                        completed
                                        expected
                                        unexpected
                                        (if (zerop skipped) "" (format ", %d skipped" skipped)))
                                output-lines))))))))

            ;; Run the tests
            (setq stats (ert-run-tests test-selector listener))

            ;; Format the results
            (let ((result-text (string-join (nreverse output-lines) "\n")))
              (format "Successfully evaluated %d test function(s) from %s and executed them with ert.\n\nTest Results:\n%s"
                      (length function-names) test-file-path result-text)))))

    (error (format "Failed to execute ERT tests: %s" (error-message-string err)))))

(defun greger-tools--shell-command (command callback &optional working-directory)
  "Execute COMMAND in WORKING-DIRECTORY and call CALLBACK with (result error).
Prompts for permission before running the command for security."
  (let ((work-dir (or working-directory ".")))
    (cond
     ((not (stringp command))
      (funcall callback nil "Command must be a string"))

     ((string-empty-p (string-trim command))
      (funcall callback nil "Command cannot be empty"))

     ((not (stringp work-dir))
      (funcall callback nil "Working directory must be a string"))

     (t
      (let ((expanded-work-dir (expand-file-name work-dir)))
        (cond
         ((not (file-exists-p expanded-work-dir))
          (funcall callback nil (format "Working directory does not exist: %s" expanded-work-dir)))

         ((not (file-directory-p expanded-work-dir))
          (funcall callback nil (format "Working directory path is not a directory: %s" expanded-work-dir)))

         ((not (y-or-n-p (format "Execute shell command: '%s' in directory '%s'? "
                                command expanded-work-dir)))
          (funcall callback nil "Shell command execution cancelled by user"))

         (t
          ;; Check if command contains shell operators (pipes, redirections, etc.)
          (if (string-match-p "[|><&;]" command)
              ;; Use shell to execute commands with shell operators
              (greger-tools--run-async-subprocess
               "sh" (list "-c" command) expanded-work-dir
               (lambda (output error)
                 (if error
                     (funcall callback nil error)
                   (funcall callback
                           (format "Command executed successfully:\n%s" output)
                           nil))))
            ;; For simple commands, parse into program and arguments
            (let* ((command-parts (split-string-and-unquote command))
                   (program (car command-parts))
                   (args (cdr command-parts)))

              ;; Execute the command asynchronously
              (greger-tools--run-async-subprocess
               program args expanded-work-dir
               (lambda (output error)
                 (if error
                     (funcall callback nil error)
                   (funcall callback
                           (format "Command executed successfully:\n%s" output)
                           nil))))))))))))))

(defun greger-tools--read-webpage (url &optional extract-text use-highest-readability)
  "Read webpage content from URL.
If EXTRACT-TEXT is non-nil (default t), extract and return text content.
If EXTRACT-TEXT is nil, return raw HTML.
If USE-HIGHEST-READABILITY is non-nil, use eww's aggressive readability setting."
  (unless (stringp url)
    (error "URL must be a string"))

  (when (string-empty-p (string-trim url))
    (error "URL cannot be empty"))

  (unless (greger-web-is-web-url-p url)
    (error "Invalid URL format: %s (must start with http:// or https://)" url))

  (condition-case err
      (greger-web-download-page url extract-text use-highest-readability)
    (error (format "Failed to read webpage: %s" (error-message-string err)))))

(provide 'greger-stdlib)

;;; greger-stdlib.el ends here
