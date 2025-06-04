;;; greger-stdlib.el --- Tool definitions for greger agent -*- lexical-binding: t -*-

;; Copyright (C) 2023 Andreas Jansson

;; Author: Andreas Jansson <andreas@jansson.me.uk>
;; Version: 0.1.0
;; URL: https://github.com/andreasjansson/greger.el
;; SPDX-License-Identifier: MIT

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:
;; Defines tools available to the greger agent

;;; Code:

(require 'greger-tools)
(require 'greger-web)

;; Assertion helper functions
(defun greger-stdlib--assert-arg-string (name value)
  "Assert that VALUE is a string, error with NAME if not."
  (unless (stringp value)
    (error "Invalid argument: %s must be a string" name)))

(defun greger-stdlib--assert-arg-string-vector (name value)
  "Assert that VALUE is a vector of strings, error with NAME if not."
  (unless (vectorp value)
    (error "Invalid argument: %s must be a vector" name))
  (seq-doseq (item value)
    (unless (stringp item)
      (error "Invalid argument: each element in %s must be a string" name))))

(defun greger-stdlib--assert-arg-int-between (name value greater-or-equal less-or-equal)
  "Assert that VALUE is an integer between GREATER-OR-EQUAL and LESS-OR-EQUAL.
Error with NAME if not. Either bound can be nil to skip that check."
  (unless (integerp value)
    (error "Invalid argument: %s must be an integer" name))
  (when (and greater-or-equal (< value greater-or-equal))
    (error "Invalid argument: %s must be >= %d" name greater-or-equal))
  (when (and less-or-equal (> value less-or-equal))
    (error "Invalid argument: %s must be <= %d" name less-or-equal)))

(defun greger-stdlib--assert-arg-string-web-url (name value)
  "Assert that VALUE is a valid web URL string, error with NAME if not."
  (greger-stdlib--assert-arg-string name value)
  (when (string-empty-p (string-trim value))
    (error "Invalid argument: %s cannot be empty" name))
  (unless (greger-web-is-web-url-p value)
    (error "Invalid argument: %s must be a valid URL (starting with http:// or https://)" name)))

;; Register all tools using the macro
(greger-register-tool "read-file"
  :description "Read the contents of a file from the filesystem"
  :properties '((path . ((type . "string")
                         (description . "Path to the file to read")))
                (include-line-numbers . ((type . "boolean")
                                         (description . "Whether to include line numbers in the output. If you plan to modify the file, you should include line numbers here so you know which lines to edit.")
                                         (default . nil)))
                (start-line . ((type . "integer")
                               (description . "Starting line number (1-based) to begin reading from. If not specified, reads from the beginning of the file.")
                               (default . nil)))
                (end-line . ((type . "integer")
                             (description . "Ending line number (1-based) to stop reading at (inclusive). If not specified, reads to the end of the file.")
                             (default . nil))))
  :required '("path")
  :function 'greger-stdlib--read-file)

(greger-register-tool "list-directory"
  :description "List files and directories in a given directory"
  :properties '((path . ((type . "string")
                         (description . "Path to the directory to list. Defaults to current directory.")
                         (default . ".")))
                (exclude-directories-recursive . ((type . "array")
                                                   (items . ((type . "string")))
                                                   (description . "List of directory names to exclude when recursively listing files. If you wish to exclude no files, pass in a list with an empty string, e.g. [\"\"].")
                                                   (default . (".git" "__pycache__"))))
                (recursive . ((type . "boolean")
                              (description . "Whether to list files recursively")
                              (default . nil))))
  :required '()
  :function 'greger-stdlib--list-directory)

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
  :function 'greger-stdlib--ripgrep
  :pass-callback t)

(greger-register-tool "write-new-file"
  :description "Write a new file with the given contents. Fails if the file already exists."
  :properties '((file-path . ((type . "string")
                              (description . "Absolute path to the new file")))
                (contents . ((type . "string")
                             (description . "Contents to write to the new file")))
                (git-commit-message . ((type . "string")
                                       (description . "Git commit message for this change"))))
  :required '("file-path" "contents" "git-commit-message")
  :function 'greger-stdlib--write-new-file
  :pass-buffer t)

(greger-register-tool "make-directory"
  :description "Recursively create a directory and all parent directories if they don't exist"
  :properties '((path . ((type . "string")
                         (description . "Path to the directory to create")))
                (git-commit-message . ((type . "string")
                                       (description . "Git commit message for this change"))))
  :required '("path" "git-commit-message")
  :function 'greger-stdlib--make-directory
  :pass-buffer t)

(greger-register-tool "rename-file"
  :description "Rename or move a file from one path to another"
  :properties '((old-path . ((type . "string")
                             (description . "Current path of the file")))
                (new-path . ((type . "string")
                             (description . "New path for the file")))
                (git-commit-message . ((type . "string")
                                       (description . "Git commit message for this change"))))
  :required '("old-path" "new-path" "git-commit-message")
  :function 'greger-stdlib--rename-file
  :pass-buffer t)

(greger-register-tool "delete-files"
  :description "Delete the files and if they're tracked in git it should stage the deletion and commit"
  :properties '((file-paths . ((type . "array")
                               (items . ((type . "string")))
                               (description . "List of file paths to delete")))
                (git-commit-message . ((type . "string")
                                       (description . "Git commit message for this change"))))
  :required '("file-paths" "git-commit-message")
  :function 'greger-stdlib--delete-files
  :pass-buffer t)

(greger-register-tool "replace-file"
  :description "Replace the entire contents of an existing file. Slow but reliable - replaces the complete file contents. Use str-replace for targeted changes in larger files."
  :properties '((file-path . ((type . "string")
                              (description . "Path to the file to replace")))
                (contents . ((type . "string")
                             (description . "New contents to replace the entire file")))
                (git-commit-message . ((type . "string")
                                       (description . "Git commit message for this change"))))
  :required '("file-path" "contents" "git-commit-message")
  :function 'greger-stdlib--replace-file
  :pass-buffer t)

(greger-register-tool "str-replace"
  :description "Replace a specific string or content block in a file with new content. Finds the exact original content and replaces it with new content. Be extra careful to format the original-content exactly correctly, taking extra care with whitespace and newlines. If you're making large swaths of changes, consider using replace-file instead"
  :properties '((file-path . ((type . "string")
                              (description . "Path to the file to modify")))
                (original-content . ((type . "string")
                                     (description . "The exact content to find and replace")))
                (new-content . ((type . "string")
                                (description . "The new content to replace the original content with")))
                (git-commit-message . ((type . "string")
                                       (description . "Git commit message for this change"))))
  :required '("file-path" "original-content" "new-content" "git-commit-message")
  :function 'greger-stdlib--str-replace
  :pass-buffer t)

(greger-register-tool "insert"
  :description "Insert text at a specific line number in a file. The text will be inserted before the specified line number (use 0 to insert at the beginning of the file, 1 to insert before the first line, etc.). Useful for adding new content, comments, or code blocks at precise locations without replacing existing content."
  :properties '((file-path . ((type . "string")
                              (description . "Path to the file to modify")))
                (line-number . ((type . "integer")
                                (description . "Line number before which to insert the content (0 for beginning of file, 1 to insert before first line, etc.)")))
                (content . ((type . "string")
                            (description . "Content to insert at the specified location")))
                (git-commit-message . ((type . "string")
                                       (description . "Git commit message for this change"))))
  :required '("file-path" "line-number" "content" "git-commit-message")
  :function 'greger-stdlib--insert
  :pass-buffer t)

(greger-register-tool "git-log"
  :description "View git commit logs."
  :properties '((path . ((type . "string")
                         (description . "Path to the git repository or any file in the repository view logs for")
                         (default . ".")))
                (max-rows . ((type . "integer")
                            (description . "Maximum number of log entries to return")
                            (default . 100))))
  :required '()
  :function 'greger-stdlib--git-log)

(greger-register-tool "git-show-commit"
  :description "View a specific git commit."
  :properties '((commit-hash . ((type . "string")
                                (description . "The commit hash to view")))
                (path . ((type . "string")
                         (description . "Path to the git repository or any file in the repository")
                         (default . "."))))
  :required '("commit-hash")
  :function 'greger-stdlib--git-show-commit)

(greger-register-tool "eval-elisp-defuns"
  :description "Evaluate Emacs lisp defuns in a specific file. Useful when the code has changed and you want to use the updated code."
  :properties '((file-path . ((type . "string")
                              (description . "Path to the file containing functions/defuns to evaluate")))
                (function-names . ((type . "array")
                                   (items . ((type . "string")))
                                   (description . "List of function names to evaluate and run"))))
  :required '("file-path" "function-names")
  :function 'greger-stdlib--eval-elisp-defuns)

(greger-register-tool "shell-command"
  :description "Execute an arbitrary shell command and return the output. Prompts for permission before running the command for security."
  :properties '((command . ((type . "string")
                            (description . "The shell command to execute")))
                (working-directory . ((type . "string")
                                      (description . "Directory to run the command in")
                                      (default . "."))))
  :required '("command")
  :function 'greger-stdlib--shell-command
  :pass-callback t
  :pass-metadata t)

(greger-register-tool "read-webpage"
  :description "Read webpage content from a URL. Can return either extracted text or raw HTML."
  :properties '((url . ((type . "string")
                        (description . "The URL to read content from")))
                (extract-text . ((type . "boolean")
                                 (description . "Whether to extract text content or return raw HTML")
                                 (default . t)))
                (use-highest-readability . ((type . "boolean")
                                            (description . "Whether to use eww's aggressive highest readability setting for better text extraction")
                                            (default . nil))))
  :required '("url")
  :function 'greger-stdlib--read-webpage)

;; Helper functions

(defun greger-stdlib--run-async-subprocess (command args working-directory callback)
  "Run COMMAND with ARGS in WORKING-DIRECTORY and call CALLBACK.
CALLBACK will be called with (output nil) on success or (nil error-message) on
failure."
  (let* ((process-name (format "greger-subprocess-%s" (make-temp-name "")))
         (process-buffer (generate-new-buffer (format " *%s*" process-name)))
         (default-directory (expand-file-name (or working-directory "."))))

    (condition-case err
        (let ((process (apply #'start-process process-name process-buffer command args)))
          (set-process-query-on-exit-flag process nil)

          (set-process-sentinel
           process
           (lambda (proc _event)
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

(defun greger-stdlib--find-git-repo-root (start-dir)
  "Find the git repository root starting from START-DIR."
  (let ((dir (expand-file-name start-dir)))
    (while (and dir
                (not (file-exists-p (expand-file-name ".git" dir)))
                (not (string= dir (directory-file-name dir))))
      (setq dir (file-name-directory (directory-file-name dir))))
    (when (and dir (file-exists-p (expand-file-name ".git" dir)))
      dir)))

(defun greger-stdlib--is-file-tracked-by-git (file-path repo-root)
  "Check if FILE-PATH is tracked by git in REPO-ROOT.
Returns t if the file is tracked, nil otherwise."
  (let ((default-directory repo-root)
        (relative-path (file-relative-name (expand-file-name file-path) repo-root)))
    (= 0 (call-process "git" nil nil nil "ls-files" "--error-unmatch" relative-path))))

;; Tools below

(defun greger-stdlib--git-stage-and-commit (files commit-message &optional chat-buffer)
  "Stage FILES and commit with COMMIT-MESSAGE using git command line.
If CHAT-BUFFER is provided, also stage and commit the chat buffer file."
  (condition-case err
      (let* ((first-file (car files))
             (file-dir (file-name-directory (expand-file-name first-file)))
             (repo-root (greger-stdlib--find-git-repo-root file-dir)))
        (unless repo-root
          (error "File %s is not in a git repository" first-file))

        ;; Set default-directory to the repository root for git operations
        (let ((default-directory repo-root)
              (all-files files))

          ;; Add chat buffer file if provided, it has a file, and it's already tracked in git
          (when (and chat-buffer (buffer-file-name chat-buffer))
            (let ((chat-file (buffer-file-name chat-buffer)))
              ;; Only proceed if the chat file is already tracked by git
              (when (greger-stdlib--is-file-tracked-by-git chat-file repo-root)
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
    (error "Git operation failed: %s" (error-message-string err))))

(defun greger-stdlib--read-file (path &optional include-line-numbers start-line end-line)
  "Read file at PATH. If INCLUDE-LINE-NUMBERS is non-nil, prepend line numbers.
If START-LINE is specified, start reading from that line (1-based).
If END-LINE is specified, stop reading at that line (inclusive, 1-based)."
  (greger-stdlib--assert-arg-string "path" path)
  (when start-line
    (greger-stdlib--assert-arg-int-between "start-line" start-line 1 nil))
  (when end-line
    (greger-stdlib--assert-arg-int-between "end-line" end-line 1 nil))

  (when (and start-line end-line (> start-line end-line))
    (error "Invalid value: start-line must be <= end-line"))

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
                 ;; Remove trailing empty line if it exists (from trailing newline)
                 (all-lines (if (and (> (length all-lines) 0)
                                    (string-empty-p (car (last all-lines))))
                               (butlast all-lines)
                             all-lines))
                 (total-lines (length all-lines))
                 (actual-start (or start-line 1))
                 (actual-end (or end-line total-lines))
                 (selected-lines (greger-stdlib--extract-line-range all-lines actual-start actual-end))
                 (contents (mapconcat #'identity selected-lines "\n")))
            (if include-line-numbers
                (greger-stdlib--add-line-numbers-with-offset contents actual-start)
              contents)))
      (error "Failed to read file: %s" (error-message-string err)))))

(defun greger-stdlib--extract-line-range (lines start-line end-line)
  "Extract lines from LINES between START-LINE and END-LINE (1-based, inclusive)."
  (let ((start-index (1- start-line))  ; Convert to 0-based index
        (end-index (1- end-line)))     ; Convert to 0-based index
    ;; Ensure indices are within bounds
    (setq start-index (max 0 start-index))
    (setq end-index (min (1- (length lines)) end-index))
    ;; Extract the range
    (cl-subseq lines start-index (1+ end-index))))

(defun greger-stdlib--add-line-numbers-with-offset (content start-line-num)
  "Add line numbers to CONTENT string starting from START-LINE-NUM."
  (let* ((lines (split-string content "\n"))
         (line-num start-line-num)
         (max-width (length (number-to-string (+ start-line-num (length lines) -1))))
         (number-format (concat "%" (number-to-string max-width) "d: %s"))
         result)

    ;; Add line numbers to each line
    (dolist (line lines)
      (push (format number-format line-num line) result)
      (setq line-num (1+ line-num)))

    ;; Join back with newlines
    (mapconcat #'identity (reverse result) "\n")))

(defun greger-stdlib--list-directory (path &optional exclude-directories-recursive recursive)
  "List directory contents at PATH with detailed file information.
Similar to \\='ls -Rla\\='.
EXCLUDE-DIRECTORIES-RECURSIVE is a vector of directory names to exclude when
recursively listing (defaults to [\".git\" \"__pycache__\"]).
If RECURSIVE is non-nil, list files recursively."
  (greger-stdlib--assert-arg-string "path" path)

  (let ((expanded-path (expand-file-name path))
        (original-path path))
    (unless (file-exists-p expanded-path)
      (error "Directory does not exist: %s" expanded-path))

    (unless (file-directory-p expanded-path)
      (error "Path is not a directory: %s" expanded-path))

    (unless (file-readable-p expanded-path)
      (error "Directory is not readable: %s" expanded-path))

    (condition-case err
        (greger-stdlib--list-directory-recursive expanded-path exclude-directories-recursive original-path recursive)
      (error "Failed to list directory: %s" (error-message-string err)))))

(defun greger-stdlib--list-directory-recursive (path exclude-directories-recursive original-path recursive &optional prefix)
  "Recursively list directory contents with detailed information.
PATH is the actual directory path to list.
EXCLUDE-DIRECTORIES-RECURSIVE is vector of directory names to exclude when
recursively listing.
ORIGINAL-PATH is used for display purposes at the root level.
RECURSIVE determines if we should recurse into subdirectories.
PREFIX is used internally for nested directory structure."
  (let ((all-results '())
        (subdirs '())
        (display-path (cond
                       ;; Root level: use original path or relative notation
                       ((string= (or prefix "") "")
                        (let ((display-path-base (or original-path path)))
                          (if (string= display-path-base ".")
                              "./"
                            (file-name-as-directory display-path-base))))
                       ;; Nested level with absolute original path: build absolute path
                       ((and original-path (file-name-absolute-p original-path))
                        (file-name-as-directory
                         (expand-file-name prefix (directory-file-name original-path))))
                       ;; Nested level with relative path: use relative notation
                       (t (concat "./" prefix)))))

    ;; Build current directory listing
    (let ((current-listing '()))
      ;; Add directory header
      (push (format "%s:" display-path) current-listing)

      ;; Add current and parent directory entries
      (push (greger-stdlib--format-file-info path "." exclude-directories-recursive) current-listing)
      (unless (string= (expand-file-name path) (expand-file-name "/"))
        (push (greger-stdlib--format-file-info (file-name-directory (directory-file-name path)) ".." exclude-directories-recursive) current-listing))

      ;; Process files and directories
      (let ((files (directory-files path t)))
        (dolist (file (sort files #'string<))
          (let* ((basename (file-name-nondirectory file)))
            (when (and (not (string= basename "."))
                       (not (string= basename "..")))
              (let ((formatted (greger-stdlib--format-file-info file basename exclude-directories-recursive)))
                (when formatted
                  (push formatted current-listing)))
              ;; Collect subdirectories for recursive processing, excluding based on pattern
              (when (and recursive
                         (file-directory-p file)
                         (greger-stdlib--should-include-directory-in-recursive-listing-p basename exclude-directories-recursive))
                (push file subdirs))))))

      ;; Add current directory to results (reverse to get correct order)
      (setq all-results (reverse current-listing)))

    ;; Process subdirectories recursively if requested
    (when recursive
      (dolist (subdir (reverse subdirs)) ; Reverse to maintain alphabetical order
        (let* ((basename (file-name-nondirectory subdir))
               (subdir-results (greger-stdlib--list-directory-recursive
                               subdir exclude-directories-recursive original-path recursive
                               (concat (or prefix "") basename "/"))))
          (setq all-results (append all-results (list "" subdir-results))))))

    ;; Return results
    (if (> (length all-results) 1) ; More than just the header
        (mapconcat #'identity all-results "\n")
      (format "%s:\nDirectory is empty" display-path))))

(defun greger-stdlib--format-file-info (filepath displayname _exclude-directories-recursive)
  "Format file information similar to \\='ls -la\\=' output.
FILEPATH is the full path to the file.
DISPLAYNAME is the name to display in the output.
_EXCLUDE-DIRECTORIES-RECURSIVE is unused in this function."
  (when (file-exists-p filepath)
    (let* ((attrs (file-attributes filepath))
           (file-type (nth 0 attrs))
           (size (nth 7 attrs))
           (mode-string (greger-stdlib--file-mode-string attrs))
           (size-or-dir (if (eq file-type t) "(dir)" (format "%8d" size))))

      (format "%s  %s  %s"
              mode-string
              size-or-dir
              displayname))))

(defun greger-stdlib--file-mode-string (attrs)
  "Convert file attributes to mode string like \\='drwxr-xr-x\\='.
ATTRS should be the result of `file-attributes'."
  (let* ((file-type (nth 0 attrs))
         (mode (nth 8 attrs))
         (type-char (cond
                     ((eq file-type t) "d")          ; directory
                     ((stringp file-type) "l")       ; symbolic link
                     (t "-")))                       ; regular file
         (perms (if (stringp mode)
                    (substring mode 1)  ; Skip the type character from mode string
                  "rwxrwxrwx")))       ; Default fallback
    (concat type-char perms)))

(defun greger-stdlib--should-include-directory-in-recursive-listing-p (directory-name exclude-directories-recursive)
  "Return t if directory with DIRECTORY-NAME should be included in listing.
EXCLUDE-DIRECTORIES-RECURSIVE is a vector of directory names to exclude.
If EXCLUDE-DIRECTORIES-RECURSIVE is nil, use default excludes.
If EXCLUDE-DIRECTORIES-RECURSIVE is an empty vector, exclude nothing."
  (let ((actual-exclude-list (if (null exclude-directories-recursive)
                                 [".git" "__pycache__"]
                               exclude-directories-recursive)))
    (not (seq-contains-p actual-exclude-list directory-name))))

(defun greger-stdlib--ripgrep (pattern path callback &optional case-sensitive file-type context-lines max-results)
  "Search for PATTERN in PATH using the rg command line tool directly.
CALLBACK is called with (result error) when search completes.
CASE-SENSITIVE, FILE-TYPE, CONTEXT-LINES and MAX-RESULTS are optional."
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
          (greger-stdlib--run-async-subprocess
           "rg" args nil
           (lambda (output error)
             (if error
                 (funcall callback nil (format "Failed to execute ripgrep search: %s" error))
               (funcall callback
                       (if (string-empty-p (string-trim output))
                           "No matches found"
                         output)
                       nil))))))))))

(defun greger-stdlib--write-new-file (file-path contents git-commit-message &optional buffer)
  "Write CONTENTS to a new file at FILE-PATH. Fails if file already exists.
GIT-COMMIT-MESSAGE will be used for the git commit.
If BUFFER is provided, it will be staged and committed along with the new file."
  (greger-stdlib--assert-arg-string "file-path" file-path)
  (greger-stdlib--assert-arg-string "contents" contents)

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
      (error "Failed to write file: %s" (error-message-string err)))

    ;; Stage and commit changes - infer the file to stage
    (let ((git-result (greger-stdlib--git-stage-and-commit (list expanded-path) git-commit-message buffer)))
      (format "Successfully wrote new file %s with %d characters. %s"
              expanded-path (length contents) git-result))))

(defun greger-stdlib--make-directory (path git-commit-message &optional buffer)
  "Recursively create directory at PATH.
GIT-COMMIT-MESSAGE will be used for the git commit.
If BUFFER is provided, it will be staged and committed along with the directory."
  (unless (stringp path)
    (error "Invalid type: path must be a string"))

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
            (let ((git-result (greger-stdlib--git-stage-and-commit (list expanded-path) git-commit-message buffer)))
              (format "Successfully created directory: %s. %s" expanded-path git-result)))
        (error "Failed to create directory: %s" (error-message-string err))))))

(defun greger-stdlib--rename-file (old-path new-path git-commit-message &optional buffer)
  "Rename file from OLD-PATH to NEW-PATH.
GIT-COMMIT-MESSAGE will be used for the git commit.
If BUFFER is provided, it will be staged and committed with the renamed file."
  (unless (stringp old-path)
    (error "Invalid type: old-path must be a string"))

  (unless (stringp new-path)
    (error "Invalid type: new-path must be a string"))

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
          (let ((git-result (greger-stdlib--git-stage-and-commit
                             (list expanded-old-path expanded-new-path)
                             git-commit-message buffer)))
            (format "Successfully renamed %s to %s. %s" expanded-old-path expanded-new-path git-result)))
      (error "Failed to rename file: %s" (error-message-string err)))))

(defun greger-stdlib--delete-files (file-paths git-commit-message &optional buffer)
  "Delete files at FILE-PATHS and stage the deletion in git if tracked.
GIT-COMMIT-MESSAGE will be used for the git commit.
If BUFFER is provided, it will be staged and committed with deleted files."
  (unless (vectorp file-paths)
    (error "Invalid type: file-paths must be a vector"))

  (unless (stringp git-commit-message)
    (error "Invalid type: git-commit-message must be a string"))

  (let ((expanded-paths '())
        (deleted-files '())
        (git-tracked-files '()))

    ;; Validate all files exist first
    (seq-doseq (file-path file-paths)
      (unless (stringp file-path)
        (error "Each file path must be a string"))
      (let ((expanded-path (expand-file-name file-path)))
        (unless (file-exists-p expanded-path)
          (error "File does not exist: %s" expanded-path))
        (when (file-directory-p expanded-path)
          (error "Cannot delete directories: %s (only files are supported)" expanded-path))
        (push expanded-path expanded-paths)))

    ;; Check which files are tracked by git before deletion
    (dolist (expanded-path (reverse expanded-paths))
      (let* ((file-dir (file-name-directory expanded-path))
             (repo-root (greger-stdlib--find-git-repo-root file-dir)))
        (when (and repo-root
                   (greger-stdlib--is-file-tracked-by-git expanded-path repo-root))
          (push expanded-path git-tracked-files))))

    ;; Delete the files
    (condition-case err
        (dolist (expanded-path (reverse expanded-paths))
          (delete-file expanded-path)
          (push expanded-path deleted-files))
      (error "Failed to delete file: %s" (error-message-string err)))

    ;; Stage and commit the deletions for git-tracked files
    (let ((git-result
           (if git-tracked-files
               (greger-stdlib--git-stage-and-commit
                (reverse git-tracked-files)
                git-commit-message
                buffer)
             "No files were tracked by git")))

      (format "Successfully deleted %d file(s): %s. Git status: %s"
              (length deleted-files)
              (mapconcat #'identity (reverse deleted-files) ", ")
              git-result))))

(defun greger-stdlib--replace-file (file-path contents git-commit-message &optional buffer)
  "Replace the entire contents of FILE-PATH with CONTENTS.
GIT-COMMIT-MESSAGE will be used for the git commit.
If BUFFER is provided, it will be staged and committed along with the file."
  (unless (stringp file-path)
    (error "Invalid type: file-path must be a string"))

  (unless (stringp contents)
    (error "Invalid type: contents must be a string"))

  (unless (stringp git-commit-message)
    (error "Invalid type: git-commit-message must be a string"))

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
    (let ((git-result (greger-stdlib--git-stage-and-commit (list expanded-path) git-commit-message buffer)))
      (format "Successfully replaced contents of %s with %d characters. %s"
              expanded-path (length contents) git-result))))

(defun greger-stdlib--count-paren-balance (content)
  "Count paren balance in CONTENT, ignoring parens in strings and comments.
Returns the difference between left and right parens (left - right).
Uses `parse-partial-sexp' to properly handle strings and comments."
  (with-temp-buffer
    ;; Set up the buffer with lisp-mode syntax table for proper parsing
    (with-syntax-table lisp-data-mode-syntax-table
      (insert content)
      (goto-char (point-min))
      (let ((balance 0)
            (state nil)
            (pos (point-min)))
        (while (< pos (point-max))
          ;; Parse to the next character
          (setq state (parse-partial-sexp pos (1+ pos) nil nil state))
          (let ((char (char-after pos)))
            ;; Only count parens if we're not in a string or comment
            (unless (or (nth 3 state)  ; in string
                        (nth 4 state)) ; in comment
              (cond
               ((eq char ?\() (setq balance (1+ balance)))
               ((eq char ?\)) (setq balance (1- balance))))))
          (setq pos (1+ pos)))
        balance))))

(defun greger-stdlib--str-replace (file-path original-content new-content git-commit-message &optional buffer)
  "Replace ORIGINAL-CONTENT with NEW-CONTENT in FILE-PATH.
GIT-COMMIT-MESSAGE will be used for the git commit.
If BUFFER is provided, it will be staged and committed along with the file.
For Emacs Lisp files (.el), checks that parentheses balance is maintained."
  (unless (stringp file-path)
    (error "Invalid type: file-path must be a string"))

  (unless (stringp original-content)
    (error "Invalid type: original-content must be a string"))

  (unless (stringp new-content)
    (error "Invalid type: new-content must be a string"))

  (unless (stringp git-commit-message)
    (error "Invalid type: git-commit-message must be a string"))

  (let ((expanded-path (expand-file-name file-path)))

    ;; Check if file exists
    (unless (file-exists-p expanded-path)
      (error "File does not exist: %s" expanded-path))

    ;; Check if it's actually a file and not a directory
    (when (file-directory-p expanded-path)
      (error "Path is a directory, not a file: %s" expanded-path))

    ;; Check paren balance for Emacs Lisp files
    (when (string-suffix-p ".el" expanded-path)
      (let ((orig-balance (greger-stdlib--count-paren-balance original-content))
            (new-balance (greger-stdlib--count-paren-balance new-content)))
        (unless (= orig-balance new-balance)
          (error "Parentheses balance mismatch in Emacs Lisp file: original has balance %d, new has balance %d. They must be equal"
                 orig-balance new-balance))))

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
    (let ((git-result (greger-stdlib--git-stage-and-commit (list expanded-path) git-commit-message buffer)))
      (format "Successfully replaced content in %s. %s" expanded-path git-result))))

(defun greger-stdlib--insert (file-path line-number content git-commit-message &optional buffer)
  "Insert CONTENT at LINE-NUMBER in FILE-PATH.
GIT-COMMIT-MESSAGE will be used for the git commit.
If BUFFER is provided, it will be staged and committed along with the file."
  (unless (stringp file-path)
    (error "Invalid type: file-path must be a string"))

  (unless (integerp line-number)
    (error "Invalid type: line-number must be an integer"))

  (unless (>= line-number 0)
    (error "Invalid type: line-number must be >= 0"))

  (unless (stringp content)
    (error "Invalid type: content must be a string"))

  (unless (stringp git-commit-message)
    (error "Invalid type: git-commit-message must be a string"))

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
       (goto-char (point-min))
        (forward-line (1- line-number))
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
    (let ((git-result (greger-stdlib--git-stage-and-commit (list expanded-path) git-commit-message buffer)))
      (format "Successfully inserted %d characters at line %d in %s. %s"
              (length content) line-number expanded-path git-result))))

(defun greger-stdlib--git-log (path &optional max-rows)
  "View git commit logs using git command line for PATH.
MAX-ROWS limits the number of log entries returned (default 100)."
  (unless (stringp path)
    (error "Path must be a string"))

  (let ((expanded-path (expand-file-name path))
        (max-count (or max-rows 100)))

    (unless (file-exists-p expanded-path)
      (error "Path does not exist: %s" expanded-path))

    ;; Get the directory to search for git repo (if path is a file, use its directory)
    (let* ((search-dir (if (file-directory-p expanded-path)
                          expanded-path
                        (file-name-directory expanded-path)))
           (repo-root (greger-stdlib--find-git-repo-root search-dir)))
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
        (error "Failed to retrieve git log: %s" (error-message-string err))))))

(defun greger-stdlib--git-show-commit (commit-hash path)
  "View git commit using git command line for PATH.
COMMIT-HASH specifies which commit to show."
  (unless (stringp commit-hash)
    (error "Invalid type: commit-hash must be a string"))

  (unless (stringp path)
    (error "Invalid type: path must be a string"))

  (let ((expanded-path (expand-file-name path)))

    (unless (file-exists-p expanded-path)
      (error "Path does not exist: %s" expanded-path))

    ;; Get the directory to search for git repo (if path is a file, use its directory)
    (let* ((search-dir (if (file-directory-p expanded-path)
                          expanded-path
                        (file-name-directory expanded-path)))
           (repo-root (greger-stdlib--find-git-repo-root search-dir)))
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
        (error "Failed to show git commit: %s" (error-message-string err))))))

(defun greger-stdlib--eval-elisp-defuns (file-path function-names)
  "Evaluate Emacs Lisp function definitions from FILE-PATH.
FUNCTION-NAMES specifies which functions to evaluate."
  (unless (stringp file-path)
    (error "Invalid type: file-path must be a string"))

  (unless (vectorp function-names)
    (error "Invalid type: function-names must be a vector"))

  (let ((expanded-path (expand-file-name file-path)))
    ;; Check if file exists
    (unless (file-exists-p expanded-path)
      (error "File does not exist: %s" expanded-path))

    ;; Check if it's actually a file
    (when (file-directory-p expanded-path)
      (error "Path is a directory, not a file: %s" expanded-path))

    (with-current-buffer (find-file-noselect expanded-path)
     ;; Navigate to and evaluate each function
     (seq-doseq (function-name function-names)
       (goto-char (point-min))

       ;; Search for the function definition
       (let ((function-pattern (format "^\\s-*(\\(ert-deftest\\|defun\\)\\s-+%s\\s-*("
                                       (regexp-quote function-name))))
         (unless (re-search-forward function-pattern nil t)
           (error "Function '%s' not found in %s" function-name expanded-path))

         ;; Move to beginning of defun and evaluate it
         (beginning-of-defun)
         (eval-defun nil))))
    "Eval successful"))

(defun greger-stdlib--shell-command (command callback &optional working-directory metadata)
  "Execute COMMAND in WORKING-DIRECTORY and call CALLBACK with (result error).
Prompts for permission before running the command for security.
If METADATA contains safe-shell-commands and COMMAND is in that list, skips
permission prompt."
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

         ((let ((safe-commands (plist-get metadata :safe-shell-commands)))
            (and (not (member command safe-commands))
                 (not (y-or-n-p (format "Execute shell command: '%s' in directory '%s'? "
                                       command expanded-work-dir)))))
          (funcall callback nil "Shell command execution cancelled by user"))

         (t
          ;; Check if command contains shell operators (pipes, redirections, etc.)
          (if (string-match-p "[|><&;]" command)
              ;; Use shell to execute commands with shell operators
              (greger-stdlib--run-async-subprocess
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
              (greger-stdlib--run-async-subprocess
               program args expanded-work-dir
               (lambda (output error)
                 (if error
                     (funcall callback nil error)
                   (funcall callback
                           (format "Command executed successfully:\n%s" output)
                           nil)))))))))))))

(defun greger-stdlib--read-webpage (url &optional extract-text use-highest-readability)
  "Read webpage content from URL.
If EXTRACT-TEXT is non-nil (default t), extract and return text content.
If EXTRACT-TEXT is nil, return raw HTML.
If USE-HIGHEST-READABILITY is non-nil, use eww's aggressive readability setting."
  (unless (stringp url)
    (error "Invalid type: url must be a string"))

  (when (string-empty-p (string-trim url))
    (error "Invalid type: url cannot be empty"))

  (unless (greger-web-is-web-url-p url)
    (error "Invalid URL format: %s (must start with http:// or https://)" url))

  (condition-case err
      (greger-web-download-page url extract-text use-highest-readability)
    (error "Failed to read webpage: %s" (error-message-string err))))

(provide 'greger-stdlib)

;;; greger-stdlib.el ends here
