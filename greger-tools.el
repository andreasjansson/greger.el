;;; greger-tools.el --- Tool definitions for greger agent -*- lexical-binding: t -*-

;;; Commentary:
;; Defines tools available to the greger agent

;;; Code:

(require 'json)
(require 'magit)
(require 'rg)
(require 'cl-lib)

(setq greger-tools-registry
      '(
        (read-file . ((name . "read-file")
                      (description . "Read the contents of a file from the filesystem")
                      (input_schema . ((type . "object")
                                       (properties . ((path . ((type . "string")
                                                               (description . "Path to the file to read")))
                                                      (include_line_numbers . ((type . "boolean")
                                                                               (description . "Whether to include line numbers in the output. If you plan to modify the file, you should include line numbers here so you know which lines to edit.")
                                                                               (default . nil)))
                                                      (start_line . ((type . "integer")
                                                                     (description . "Starting line number (1-based) to begin reading from. If not specified, reads from the beginning of the file.")
                                                                     (default . nil)))
                                                      (end_line . ((type . "integer")
                                                                   (description . "Ending line number (1-based) to stop reading at (inclusive). If not specified, reads to the end of the file.")
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

        (patch . ((name . "patch")
                  (description . "Apply a patch to one or more files using unified diff format. Fast and efficient for specific changes, especially in large files, but requires careful crafting of patch contents. Use replace-file for complete file replacement or replace-function for Python/Elisp functions.")
                  (input_schema . ((type . "object")
                                   (properties . ((patch_content . ((type . "string")
                                                                    (description . "Patch content in unified diff format (without timestamps). Multiple files can be patched in a single operation by including multiple file diffs in the patch content. Use standard unified diff format: '--- filename' and '+++ filename' headers, followed by hunks with '@@' markers.")))
                                                  (git_commit_message . ((type . "string")
                                                                         (description . "Git commit message for this change")))))
                                   (required . ["patch_content" "git_commit_message"])))))

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

        (replace-function . ((name . "replace-function")
                             (description . "Replace a function in a Python or Elisp file with new contents. Fast and reliable - only supports 'def function_name' and '(defun function_name ())' forms. Use replace-file for complete file replacement or patch for other specific changes.")
                             (input_schema . ((type . "object")
                                              (properties . ((file_path . ((type . "string")
                                                                           (description . "Path to the file containing the function")))
                                                             (function_name . ((type . "string")
                                                                               (description . "Name of the function to replace")))
                                                             (contents . ((type . "string")
                                                                          (description . "New function contents to replace the existing function")))
                                                             (line_number . ((type . "integer")
                                                                             (description . "Line number where the function is defined")))
                                                             (commit_message . ((type . "string")
                                                                                (description . "Git commit message for this change")))))
                                              (required . ["file_path" "function_name" "contents" "line_number" "commit_message"])))))

        (replace-file . ((name . "replace-file")
                         (description . "Replace the entire contents of an existing file. Slow but reliable - replaces the complete file contents. Use patch for specific changes in large files, or replace-function for Python/Elisp functions.")
                         (input_schema . ((type . "object")
                                          (properties . ((file_path . ((type . "string")
                                                                       (description . "Path to the file to replace")))
                                                         (contents . ((type . "string")
                                                                      (description . "New contents to replace the entire file")))
                                                         (git_commit_message . ((type . "string")
                                                                                (description . "Git commit message for this change")))))
                                          (required . ["file_path" "contents" "git_commit_message"])))))

        (str-replace . ((name . "str-replace")
                        (description . "Replace a specific string or content block in a file with new content. Finds the exact original content and replaces it with new content. Be extra careful to format the original_content exactly correctly, taking extra care with whitespace.")
                        (input_schema . ((type . "object")
                                         (properties . ((file_path . ((type . "string")
                                                                      (description . "Path to the file to modify")))
                                                        (original_content . ((type . "string")
                                                                             (description . "The exact content to find and replace")))
                                                        (new_content . ((type . "string")
                                                                        (description . "The new content to replace the original content with")))
                                                        (git_commit_message . ((type . "string")
                                                                               (description . "Git commit message for this change")))))
                                         (required . ["file_path" "original_content" "new_content" "git_commit_message"])))))

        (insert . ((name . "insert")
                   (description . "Insert text at a specific line number in a file. The text will be inserted before the specified line number (use 0 to insert at the beginning of the file, 1 to insert before the first line, etc.). Useful for adding new content, comments, or code blocks at precise locations without replacing existing content.")
                   (input_schema . ((type . "object")
                                    (properties . ((file_path . ((type . "string")
                                                                 (description . "Path to the file to modify")))
                                                   (line_number . ((type . "integer")
                                                                   (description . "Line number before which to insert the content (0 for beginning of file, 1 to insert before first line, etc.)")))
                                                   (content . ((type . "string")
                                                               (description . "Content to insert at the specified location")))
                                                   (git_commit_message . ((type . "string")
                                                                          (description . "Git commit message for this change")))))
                                    (required . ["file_path" "line_number" "content" "git_commit_message"])))))

        (git-log . ((name . "git-log")
                    (description . "View git commit logs using magit in a split screen")
                    (input_schema . ((type . "object")
                                     (properties . ((path . ((type . "string")
                                                             (description . "Path to the git repository or file to view logs for")
                                                             (default . ".")))))
                                     (required . [])))))

        (git-show-commit . ((name . "git-show-commit")
                            (description . "View a specific git commit using magit in a split screen")
                            (input_schema . ((type . "object")
                                             (properties . ((commit_hash . ((type . "string")
                                                                            (description . "The commit hash to view")))
                                                            (path . ((type . "string")
                                                                     (description . "Path to the git repository")
                                                                     (default . ".")))))
                                             (required . ["commit_hash"])))))
        (ert-test . ((name . "ert-test")
                     (description . "Execute ERT tests by evaluating test functions and running them with ert")
                     (input_schema . ((type . "object")
                                      (properties . ((test_file_path . ((type . "string")
                                                                        (description . "Path to the test file containing ERT tests")))
                                                     (function_names . ((type . "array")
                                                                        (items . ((type . "string")))
                                                                        (description . "List of ERT test function names to evaluate and run")))))
                                      (required . ["test_file_path" "function_names"])))))
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
       (alist-get 'include_line_numbers args)
       (alist-get 'start_line args)
       (alist-get 'end_line args)))

     ((eq tool-symbol 'list-directory)
      (greger-tools--list-directory
       (or (alist-get 'path args) ".")
       (alist-get 'show-hidden args)
       (alist-get 'recursive args)))

     ((eq tool-symbol 'patch)
      (greger-tools--patch
       (alist-get 'patch_content args)
       (alist-get 'git_commit_message args)))

     ((eq tool-symbol 'ripgrep)
      (greger-tools--ripgrep
       (alist-get 'pattern args)
       (or (alist-get 'path args) ".")
       (alist-get 'case-sensitive args)
       (alist-get 'file-type args)
       (or (alist-get 'context-lines args) 0)
       (or (alist-get 'max-results args) 50)))

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

     ((eq tool-symbol 'replace-function)
      (greger-tools--replace-function
       (alist-get 'file_path args)
       (alist-get 'function_name args)
       (alist-get 'contents args)
       (alist-get 'line_number args)
       (alist-get 'commit_message args)))

     ((eq tool-symbol 'replace-file)
      (greger-tools--replace-file
       (alist-get 'file_path args)
       (alist-get 'contents args)
       (alist-get 'git_commit_message args)))

     ((eq tool-symbol 'str-replace)
      (greger-tools--str-replace
       (alist-get 'file_path args)
       (alist-get 'original_content args)
       (alist-get 'new_content args)
       (alist-get 'git_commit_message args)))

     ((eq tool-symbol 'insert)
      (greger-tools--insert
       (alist-get 'file_path args)
       (alist-get 'line_number args)
       (alist-get 'content args)
       (alist-get 'git_commit_message args)))

     ((eq tool-symbol 'git-log)
      (greger-tools--git-log
       (or (alist-get 'path args) ".")))

     ((eq tool-symbol 'git-show-commit)
      (greger-tools--git-show-commit
       (alist-get 'commit_hash args)
       (or (alist-get 'path args) ".")))

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
        (let ((default-directory repo-root)
              (magit-save-repository-buffers nil))
          ;; Stage the files
          (magit-stage-files files)

          ;; Create the commit
          (magit-commit-create (list "-m" commit-message))

          (format "Successfully staged %d file(s) and committed with message: %s"
                  (length files) commit-message)))
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

(defun greger-tools--add-line-numbers (content)
  "Add line numbers to CONTENT string."
  (greger-tools--add-line-numbers-with-offset content 1))

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

(defun greger-tools--ripgrep (pattern path &optional case-sensitive file-type context-lines max-results)
  "Search for PATTERN in PATH using the rg.el package."
  (unless (stringp pattern)
    (error "Pattern must be a string"))

  (unless (stringp path)
    (error "Path must be a string"))

  ;; Check if rg executable is available
  (unless (executable-find "rg")
    (error "ripgrep (rg) command not found. Please install ripgrep"))

  (let ((expanded-path (expand-file-name path)))
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

      (condition-case err
          (let ((default-directory search-dir))
            ;; Use rg-run to perform the search
            (rg-run pattern files-pattern search-dir literal nil flags)

            ;; Wait a moment for the search to complete and collect results
            (sit-for 0.5)

            ;; Get the buffer contents
            (let ((results (with-current-buffer (rg-buffer-name)
                             (buffer-substring-no-properties
                              (line-beginning-position)
                              (line-end-position)))))

              ;; Return results or indicate no matches
              (if (or (string-empty-p (string-trim results))
                      (string-match-p "No matches found" results)
                      (string-match-p "0 matches" results))
                  "No matches found"
                results)))
        (error (format "Failed to execute ripgrep search: %s" (error-message-string err)))))))

(defmacro greger-tools--with-split-window (&rest body)
  "Execute BODY in a split window context, returning focus to original window.
If not split, split horizontally and select other window.
If already split, select next window.
Always returns focus to the original window after executing BODY."
  `(let ((original-window (selected-window)))
     (unwind-protect
         (progn
           (if (= (length (window-list)) 1)
               ;; Not split, split horizontally and select other window
               (progn
                 (split-window-horizontally)
                 (other-window 1))
             ;; Already split, select next window
             (other-window 1))
           ,@body)
       ;; Always return to original window
       (select-window original-window))))

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

(defun greger-tools--patch (patch-content git-commit-message)
  "Apply PATCH-CONTENT using the patch command and commit changes."
  (unless (stringp patch-content)
    (error "patch_content must be a string"))

  (unless (stringp git-commit-message)
    (error "git_commit_message must be a string"))

  ;; Check if patch command is available
  (unless (executable-find "patch")
    (error "patch command not found. Please install patch"))

  (let* ((affected-files (greger-tools--extract-files-from-patch patch-content))
         (working-dir (greger-tools--find-common-directory affected-files))
         (normalized-patch (greger-tools--normalize-patch-paths patch-content working-dir))
         (patch-file (make-temp-file "greger-patch-" nil ".patch")))

    (unwind-protect
        (progn
          ;; Write normalized patch content to temporary file
          (with-temp-file patch-file
            (insert normalized-patch)
            (unless (string-suffix-p "\n" normalized-patch)
              (insert "\n")))

          ;; Change to the working directory and apply the patch
          (let ((default-directory working-dir))
            (let* ((patch-command (format "patch --ignore-whitespace -p0 --no-backup-if-mismatch < %s"
                                          (shell-quote-argument patch-file)))
                   (result (shell-command-to-string patch-command))
                   (exit-code (shell-command patch-command)))

              (if (= exit-code 0)
                  (progn
                    ;; Patch applied successfully, now stage and commit
                    (let ((git-result (greger-tools--git-stage-and-commit affected-files git-commit-message)))
                      (format "Successfully applied patch to %d file(s): %s. %s"
                              (length affected-files)
                              (mapconcat #'identity affected-files ", ")
                              git-result)))
                ;; Patch failed
                (error "Failed to apply patch. Error: %s" result)))))

      ;; Cleanup: delete temporary patch file
      (when (file-exists-p patch-file)
        (delete-file patch-file)))))

(defun greger-tools--find-common-directory (file-paths)
  "Find the deepest common directory that contains all FILE-PATHS."
  (if (null file-paths)
      default-directory
    (let ((expanded-paths (mapcar #'expand-file-name file-paths)))
      (if (= (length expanded-paths) 1)
          ;; Single file - use its directory
          (file-name-directory (car expanded-paths))
        ;; Multiple files - find common parent
        (let ((common-dir (file-name-directory (car expanded-paths))))
          (dolist (path (cdr expanded-paths))
            (let ((dir (file-name-directory path)))
              (while (and (not (string-empty-p common-dir))
                          (not (string-prefix-p common-dir dir)))
                (setq common-dir (file-name-directory (directory-file-name common-dir))))))
          (or common-dir "/"))))))

(defun greger-tools--normalize-patch-paths (patch-content working-dir)
  "Normalize file paths in PATCH-CONTENT to be relative to WORKING-DIR."
  (let ((lines (split-string patch-content "\n"))
        (normalized-lines '()))

    (dolist (line lines)
      (cond
       ;; Handle --- lines (old file)
       ((string-match "^--- \\(.+\\)$" line)
        (let* ((file-path (match-string 1 line))
               (relative-path (greger-tools--make-relative-to-dir file-path working-dir)))
          (push (format "--- %s" relative-path) normalized-lines)))

       ;; Handle +++ lines (new file)
       ((string-match "^\\+\\+\\+ \\(.+\\)$" line)
        (let* ((file-path (match-string 1 line))
               (relative-path (greger-tools--make-relative-to-dir file-path working-dir)))
          (push (format "+++ %s" relative-path) normalized-lines)))

       ;; Keep all other lines as-is
       (t
        (push line normalized-lines))))

    (mapconcat #'identity (reverse normalized-lines) "\n")))

(defun greger-tools--make-relative-to-dir (file-path base-dir)
  "Make FILE-PATH relative to BASE-DIR."
  (let ((expanded-file (expand-file-name file-path))
        (expanded-base (expand-file-name base-dir)))
    (file-relative-name expanded-file expanded-base)))

(defun greger-tools--extract-files-from-patch (patch-content)
  "Extract list of affected files from PATCH-CONTENT."
  (let ((files '())
        (lines (split-string patch-content "\n")))
    (dolist (line lines)
      (when (string-match "^\\+\\+\\+ \\(.+\\)$" line)
        (let ((file (match-string 1 line)))
          ;; Remove any timestamp or tab characters from the filename
          (setq file (car (split-string file "[\t]")))
          (unless (member file files)
            (push file files)))))
    (reverse files)))

(defun greger-tools--replace-function (file-path function-name contents line-number commit-message)
  "Replace FUNCTION-NAME in FILE-PATH with new CONTENTS at LINE-NUMBER."
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

    (greger-tools--with-split-window
     (find-file expanded-path)

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
    (let ((git-result (greger-tools--git-stage-and-commit (list expanded-path) commit-message)))
      (format "Successfully replaced function '%s' in %s. %s"
              function-name expanded-path git-result))))

(defun greger-tools--replace-file (file-path contents git-commit-message)
  "Replace the entire contents of FILE-PATH with CONTENTS."
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

    (greger-tools--with-split-window
     (find-file expanded-path)

     ;; Select all content and replace it
     (erase-buffer)
     (insert contents)

     ;; Save the file
     (save-buffer))

    ;; Stage and commit the file
    (let ((git-result (greger-tools--git-stage-and-commit (list expanded-path) git-commit-message)))
      (format "Successfully replaced contents of %s with %d characters. %s"
              expanded-path (length contents) git-result))))

(defun greger-tools--str-replace (file-path original-content new-content git-commit-message)
  "Replace ORIGINAL-CONTENT with NEW-CONTENT in FILE-PATH."
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

    (greger-tools--with-split-window
     (find-file expanded-path)

     ;; Use isearch to find the original content
     (goto-char (point-min))
     (let ((case-fold-search nil)) ; Make search case-sensitive
       (if (search-forward original-content nil t)
           (progn
             ;; Replace the found content
             (replace-match new-content nil t)
             ;; Save the file
             (save-buffer))
         (error "Original content not found in file: %s" expanded-path))))

    ;; Stage and commit the file
    (let ((git-result (greger-tools--git-stage-and-commit (list expanded-path) git-commit-message)))
      (format "Successfully replaced content in %s. %s" expanded-path git-result))))

(defun greger-tools--insert (file-path line-number content git-commit-message)
  "Insert CONTENT at LINE-NUMBER in FILE-PATH."
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

    (greger-tools--with-split-window
     (find-file expanded-path)

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
    (let ((git-result (greger-tools--git-stage-and-commit (list expanded-path) git-commit-message)))
      (format "Successfully inserted %d characters at line %d in %s. %s"
              (length content) line-number expanded-path git-result))))

(defun greger-tools--git-log (path)
  "View git commit logs using magit in a split screen for PATH."
  (unless (stringp path)
    (error "path must be a string"))

  (let ((expanded-path (expand-file-name path)))

    (unless (file-exists-p expanded-path)
      (error "Path does not exist: %s" expanded-path))

    ;; Find git repository root
    (let ((repo-root (magit-toplevel expanded-path)))
      (unless repo-root
        (error "Path %s is not in a git repository" expanded-path))

      (condition-case err
          (let ((default-directory repo-root)
                (magit-save-repository-buffers nil)
                (results))
            (with-current-buffer (magit-log-all '())
              (setq results (buffer-substring-no-properties (point-min) (point-max)))
              (magit-log-bury-buffer 0))

            (if (string-empty-p (string-trim results))
                "No git log available"
              results))
        (error (format "Failed to retrieve git log: %s" (error-message-string err)))))))

(defun greger-tools--git-show-commit (commit-hash path)
  "View git commit using magit in a split screen for PATH."
  (unless (stringp commit-hash)
    (error "commit_hash must be a string"))

  (unless (stringp path)
    (error "path must be a string"))

  (let ((expanded-path (expand-file-name path)))

    (unless (file-exists-p expanded-path)
      (error "Path does not exist: %s" expanded-path))

    ;; Find git repository root
    (let ((repo-root (magit-toplevel expanded-path)))
      (unless repo-root
        (error "Path %s is not in a git repository" expanded-path))

      (condition-case err
          (let ((default-directory repo-root)
                (magit-save-repository-buffers nil)
                (results))
            (with-current-buffer (magit-revision-setup-buffer commit-hash nil nil)
              (setq results (buffer-substring-no-properties (point-min) (point-max)))
              (magit-log-bury-buffer 0))

            (if (string-empty-p (string-trim results))
                "No git commit available"
              results))
        (error (format "Failed to show git commit: %s" (error-message-string err)))))))

(provide 'greger-tools)

;;; greger-tools.el ends here
