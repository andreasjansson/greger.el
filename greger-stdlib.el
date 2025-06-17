;;; greger-stdlib.el --- Tool definitions for greger agent -*- lexical-binding: t -*-

;; Copyright (C) 2025 Andreas Jansson

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
(require 'cl-lib)

;; Server tool registrations

(greger-register-server-tool "web_search"
                             :type "web_search_20250305"
                             :max_uses 5)

;; Tool registrations

;; Editing tools

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

(greger-register-tool "write-new-file"
                      :description "Write a new file with the given contents. Fails if the file already exists."
                      :properties '((path . ((type . "string")
                                                  (description . "Absolute path to the new file")))
                                    (contents . ((type . "string")
                                                 (description . "Contents to write to the new file")))
                                    (git-commit-message . ((type . "string")
                                                           (description . "Git commit message for this change"))))
                      :required '("path" "contents" "git-commit-message")
                      :function 'greger-stdlib--write-new-file
                      :pass-buffer t)

(greger-register-tool "replace-file"
                      :description "Replace the entire contents of an existing file. Slow but reliable - replaces the complete file contents. Use str-replace for targeted changes in larger files."
                      :properties '((path . ((type . "string")
                                                  (description . "Path to the file to replace")))
                                    (contents . ((type . "string")
                                                 (description . "New contents to replace the entire file")))
                                    (git-commit-message . ((type . "string")
                                                           (description . "Git commit message for this change"))))
                      :required '("path" "contents" "git-commit-message")
                      :function 'greger-stdlib--replace-file
                      :pass-buffer t)

(greger-register-tool "str-replace"
                      :description "Replace a specific string or content block in a file with new content. Finds the exact original content and replaces it with new content. Be extra careful to format the original-content exactly correctly, taking extra care with whitespace and newlines. In addition to replacing strings, str-replace can also be used to prepend, append, or delete contents from a file. If you're making large swaths of changes, consider using replace-file instead."
                      :properties '((path . ((type . "string")
                                                  (description . "Path to the file to modify")))
                                    (original-content . ((type . "string")
                                                         (description . "The exact content to find and replace")))
                                    (new-content . ((type . "string")
                                                    (description . "The new content to replace the original content with")))
                                    (git-commit-message . ((type . "string")
                                                           (description . "Git commit message for this change")))
                                    (replace-all . ((type . "boolean")
                                                    (description . "If true, replace all instances of original-content. If false (default), replace only the first instance")
                                                    (default . nil))))
                      :required '("path" "original-content" "new-content" "git-commit-message")
                      :function 'greger-stdlib--str-replace
                      :pass-buffer t)

;; File tools

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
                      :properties '((paths . ((type . "array")
                                                   (items . ((type . "string")))
                                                   (description . "List of file paths to delete")))
                                    (git-commit-message . ((type . "string")
                                                           (description . "Git commit message for this change"))))
                      :required '("paths" "git-commit-message")
                      :function 'greger-stdlib--delete-files
                      :pass-buffer t)

(greger-register-tool "list-directory"
                      :description "List files and directories in a given directory"
                      :properties '((path . ((type . "string")
                                             (description . "Path to the directory to list. Defaults to current directory.")
                                             (default . ".")))
                                    (exclude-directories-recursive . ((type . "array")
                                                                      (items . ((type . "string")))
                                                                      (description . "List of directory names to exclude when recursively listing files.")
                                                                      (default . (".git" "__pycache__"))))
                                    (recursive . ((type . "boolean")
                                                  (description . "Whether to list files recursively")
                                                  (default . nil))))
                      :required '()
                      :function 'greger-stdlib--list-directory)

;; Ripgrep tool

(greger-register-tool "ripgrep"
                      :description "Search for patterns in files using ripgrep (rg) command line tool. Note that ripgrep only matches on single lines, so you can't search across multiple lines."
                      :properties '((pattern . ((type . "string")
                                                (description . "The search pattern (regex or literal string). Uses regular expression syntax by default. Meta characters like .(){}*+?[]^$|\\  have special meaning and should be escaped with backslash if you want to match them literally. Use the fixed-strings parameter for literal string matching. Supports Unicode by default. You have a tendency to add a trailing double quote `\"` to the end of `pattern` -- don't to that since it will break matching!")))
                                    (path . ((type . "string")
                                             (description . "Directory or file path to search in. Directories are searched recursively. Supports glob patterns and respects .gitignore rules by default. Use '.' for current directory, or specify multiple paths separated by spaces")
                                             (default . ".")))
                                    (case-sensitive . ((type . "boolean")
                                                       (description . "Whether the search should be case-sensitive")
                                                       (default . nil)))
                                    (file-type . ((type . "string")
                                                  (description . "Restrict search to specific file types using predefined type names. Examples: 'py' (Python), 'js' (JavaScript), 'md' (Markdown), 'cpp' (C++), 'elisp' (Emacs Lisp), 'java', 'rust', 'go', 'html', 'css', 'json', 'xml', 'yaml', 'sh' (shell scripts), 'sql', 'tex', 'dockerfile'. Use 'rg --type-list' to see all available types and their file extensions")))
                                    (context-lines . ((type . "integer")
                                                      (description . "Number of context lines to show around matches")
                                                      (default . 0)))
                                    (fixed-strings . ((type . "boolean")
                                                      (description . "Treat the pattern as a literal string instead of a regular expression")
                                                      (default . nil)))
                                    (word-regexp . ((type . "boolean")
                                                    (description . "Only show matches surrounded by word boundaries")
                                                    (default . nil)))
                                    (line-regexp . ((type . "boolean")
                                                    (description . "Only show matches where the entire line participates in the match")
                                                    (default . nil)))
                                    (max-results . ((type . "integer")
                                                    (description . "Maximum number of results to return")
                                                    (default . 50))))
                      :required '("pattern")
                      :function 'greger-stdlib--ripgrep
                      :pass-callback t)

;; Shell commands

(greger-register-tool "shell-command"
                      :description "Execute an arbitrary shell command and return the output. Prompts for permission before running the command for security."
                      :properties '((command . ((type . "string")
                                                (description . "The shell command to execute")))
                                    (working-directory . ((type . "string")
                                                          (description . "Directory to run the command in")
                                                          (default . ".")))
                                    (timeout . ((type . "integer")
                                                (description . "Timeout in seconds for command execution")
                                                (default . 600))))
                      :required '("command")
                      :function 'greger-stdlib--shell-command
                      :pass-callback t
                      :pass-metadata t)

;; Web tools

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

(cl-defun greger-stdlib--assert-arg-string (name value &key min-length)
  "Assert that VALUE is a string, error with NAME if not."
  (unless (stringp value)
    (error "Invalid argument: %s must be a string" name))
  (when (and min-length (< (length value) min-length))
    (error "Invalid argument: %s must have a length of at least %d" name min-length)))

(defun greger-stdlib--assert-arg-bool (name value)
  "Assert that VALUE is a boolean, error with NAME if not."
  (unless (booleanp value)
    (error "Invalid argument: %s must be a boolean" name)))

(defun greger-stdlib--assert-arg-string-vector (name value)
  "Assert that VALUE is a vector of strings, error with NAME if not."
  (unless (vectorp value)
    (error "Invalid argument: %s must be a vector" name))
  (seq-doseq (item value)
    (unless (stringp item)
      (error "Invalid argument: each element in %s must be a string" name))))

(cl-defun greger-stdlib--assert-arg-int (name value &key ge le)
  "Assert that VALUE is an integer within the specified bounds.
Error with NAME if not. Either bound can be nil to skip that check."
  (unless (integerp value)
    (error "Invalid argument: %s must be an integer" name))
  (when (and ge (< value ge))
    (error "Invalid argument: %s must be >= %d" name ge))
  (when (and le (> value le))
    (error "Invalid argument: %s must be <= %d" name le)))

(defun greger-stdlib--assert-arg-string-web-url (name value)
  "Assert that VALUE is a valid web URL string, error with NAME if not."
  (greger-stdlib--assert-arg-string name value)
  (when (string-empty-p (string-trim value))
    (error "Invalid argument: %s cannot be empty" name))
  (unless (greger-web-is-web-url-p value)
    (error "Invalid argument: %s must be a valid URL (starting with http:// or https://)" name)))

(cl-defun greger-stdlib--run-async-subprocess (&key command args working-directory callback timeout env)
  "Run COMMAND with ARGS in WORKING-DIRECTORY and call CALLBACK.
CALLBACK will be called with (output nil) on success or (nil error-message) on
failure.
TIMEOUT specifies the maximum time in seconds to wait for completion,
the default is no timeout.
ENV is an optional alist of environment variables to set.
Returns a cancel function that can be called to interrupt the process."
  (let* ((process-name (format "greger-subprocess-%s" (make-temp-name "")))
         (process-buffer (generate-new-buffer (format " *%s*" process-name)))
         (default-directory (expand-file-name (or working-directory ".")))
         (process-environment (if env
                                  (append (mapcar (lambda (pair) (format "%s=%s" (car pair) (cdr pair))) env)
                                          process-environment)
                                process-environment))
         (process nil)
         (callback-called nil)
         (timeout-timer nil))

    (condition-case err
        (progn
          (setq process (apply #'start-process process-name process-buffer command args))
          (set-process-query-on-exit-flag process nil)

          ;; Set up timeout timer if specified
          (when timeout
            (setq timeout-timer
                  (run-at-time timeout nil
                               (lambda ()
                                 (when (and process (process-live-p process))
                                   (unless callback-called
                                     (setq callback-called t)
                                     (when timeout-timer
                                       (cancel-timer timeout-timer))
                                     (interrupt-process process)
                                     (sit-for 0.1)
                                     (when (process-live-p process)
                                       (delete-process process))
                                     (when (buffer-live-p process-buffer)
                                       (kill-buffer process-buffer))
                                     (funcall callback nil (format "Command timed out after %d seconds" timeout))))))))

          (set-process-sentinel
           process
           (lambda (proc _event)
             (unless callback-called
               (setq callback-called t)
               ;; Cancel timeout timer if it's running
               (when timeout-timer
                 (cancel-timer timeout-timer))
               (let ((exit-status (process-exit-status proc))
                     (output (with-current-buffer process-buffer
                               (buffer-string))))
                 ;; TODO: remove debug
                 (when (buffer-live-p process-buffer)
                   (kill-buffer process-buffer))
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
                                      output)))))))))

          ;; Return cancel function
          (lambda ()
            ;; Cancel timeout timer if it's running
            (when timeout-timer
              (cancel-timer timeout-timer))
            (when (and process (process-live-p process))
              (interrupt-process process)
              (sit-for 0.1)
              (when (process-live-p process)
                (delete-process process)))
            (unless callback-called
              (setq callback-called t)
              (when (buffer-live-p process-buffer)
                (kill-buffer process-buffer))
              ;; Call callback with cancellation error
              (funcall callback nil "Command execution was cancelled"))))
      (error
       (when (buffer-live-p process-buffer)
         (kill-buffer process-buffer))
       (funcall callback nil (format "Failed to start process: %s" (error-message-string err)))
       ;; Return no-op cancel function if process failed to start
       (lambda () nil)))))

(defun greger-stdlib--find-git-repo-root (start-dir)
  "Find the git repository root starting from START-DIR."
  (let ((dir (expand-file-name start-dir)))
    (while (and dir
                (not (file-exists-p (expand-file-name ".git" dir)))
                (not (string= dir (directory-file-name dir))))
      (setq dir (file-name-directory (directory-file-name dir))))
    (when (and dir (file-exists-p (expand-file-name ".git" dir)))
      dir)))

(defun greger-stdlib--is-file-tracked-by-git (path repo-root)
  "Check if PATH is tracked by git in REPO-ROOT.
Returns t if the file is tracked, nil otherwise."
  (let ((default-directory repo-root)
        (relative-path (file-relative-name (expand-file-name path) repo-root)))
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

(defun greger-stdlib--read-file (path include-line-numbers start-line end-line)
  "Read file at PATH.  If INCLUDE-LINE-NUMBERS is non-nil, prepend line numbers.
If START-LINE is specified, start reading from that line (1-based).
If END-LINE is specified, stop reading at that line (inclusive, 1-based)."
  (greger-stdlib--assert-arg-string "path" path)
  (greger-stdlib--assert-arg-bool "include-line-numbers" include-line-numbers)
  (when start-line
    (greger-stdlib--assert-arg-int "start-line" start-line :ge 1))
  (when end-line
    (greger-stdlib--assert-arg-int "end-line" end-line :ge 1))

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

(defun greger-stdlib--write-new-file (path contents git-commit-message &optional buffer)
  "Write CONTENTS to a new file at PATH.  Fails if file already exists.
GIT-COMMIT-MESSAGE will be used for the git commit.
If BUFFER is provided, it will be staged and committed along with the new file."
  (greger-stdlib--assert-arg-string "path" path)
  (greger-stdlib--assert-arg-string "contents" contents)

  (let ((expanded-path (expand-file-name path)))

    ;; Check if file already exists
    (when (file-exists-p expanded-path)
      (error "File already exists: %s" expanded-path))

    (when (string-suffix-p ".el" expanded-path)
      (let ((balance (greger-stdlib--count-paren-balance contents)))
        (unless (= balance 0)
          (error "Unbalanced parentheses in Emacs Lisp content: contents has balance %d. Must be 0. Try again!" balance))))

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
  (greger-stdlib--assert-arg-string "path" path)

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
  (greger-stdlib--assert-arg-string "old-path" old-path)
  (greger-stdlib--assert-arg-string "new-path" new-path)

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

(defun greger-stdlib--delete-files (paths git-commit-message &optional buffer)
  "Delete files at PATHS and stage the deletion in git if tracked.
GIT-COMMIT-MESSAGE will be used for the git commit.
If BUFFER is provided, it will be staged and committed with deleted files."
  (greger-stdlib--assert-arg-string-vector "paths" paths)
  (greger-stdlib--assert-arg-string "git-commit-message" git-commit-message)

  (let ((expanded-paths '())
        (deleted-files '())
        (git-tracked-files '()))

    ;; Validate all files exist first
    (seq-doseq (path paths)
      (let ((expanded-path (expand-file-name path)))
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

(defun greger-stdlib--replace-file (path contents git-commit-message &optional buffer)
  "Replace the entire contents of PATH with CONTENTS.
GIT-COMMIT-MESSAGE will be used for the git commit.
If BUFFER is provided, it will be staged and committed along with the file."
  (greger-stdlib--assert-arg-string "path" path)
  (greger-stdlib--assert-arg-string "contents" contents)
  (greger-stdlib--assert-arg-string "git-commit-message" git-commit-message)

  (let ((expanded-path (expand-file-name path)))

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

(defun greger-stdlib--str-replace (path original-content new-content
                                             git-commit-message &optional
                                             replace-all buffer)
  "Replace ORIGINAL-CONTENT with NEW-CONTENT in FILE-PATH.
GIT-COMMIT-MESSAGE will be used for the git commit.
If REPLACE-ALL is non-nil, replace all instances; otherwise replace only the
first instance.
If BUFFER is provided, it will be staged and committed along with the file.
For Emacs Lisp files (.el), checks that parentheses balance is maintained."
  (greger-stdlib--assert-arg-string "path" path)
  (greger-stdlib--assert-arg-string "original-content" original-content)
  (greger-stdlib--assert-arg-string "new-content" new-content)
  (greger-stdlib--assert-arg-string "git-commit-message" git-commit-message)

  (let ((expanded-path (expand-file-name path)))

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
          (error "Parentheses balance mismatch in Emacs Lisp content: original has balance %d, new has balance %d.  They must be equal.  Try again!"
                 orig-balance new-balance))))

    (let ((replacements-made 0))
      (with-current-buffer (find-file-noselect expanded-path)
        (let ((case-fold-search nil)) ; Make search case-sensitive
          (goto-char (point-min))
          (if replace-all
              ;; Replace all instances
              (progn
                (while (search-forward original-content nil t)
                  (replace-match new-content nil t)
                  (setq replacements-made (1+ replacements-made)))
                (when (= replacements-made 0)
                  (error "Original content not found in file: %s -- Try again!" expanded-path)))
            ;; Replace only first instance (original behavior)
            ;; But first check if there are multiple occurrences
            (let ((first-occurrence-pos nil)
                  (occurrence-count 0))
              (while (search-forward original-content nil t)
                (setq occurrence-count (1+ occurrence-count))
                (when (= occurrence-count 1)
                  (setq first-occurrence-pos (match-beginning 0))))
              (cond
               ((= occurrence-count 0)
                (error "Original content not found in file: %s -- Try again!" expanded-path))
               ((> occurrence-count 1)
                (error "Found %d occurrences of original content in file: %s. Use replace-all=t to replace all instances, or make the original content more specific -- Try again!" occurrence-count expanded-path))
               (t
                ;; Exactly one occurrence, replace it
                (goto-char first-occurrence-pos)
                (search-forward original-content nil t)
                (replace-match new-content nil t)
                (setq replacements-made 1)))))

          ;; Save the file
          (save-buffer)))

      ;; Stage and commit the file
      (let ((git-result (greger-stdlib--git-stage-and-commit (list expanded-path) git-commit-message buffer))
            (count-msg (if (> replacements-made 1)
                           (format " (made %d replacements)" replacements-made)
                         "")))
        (format "Successfully replaced content in %s%s. %s" expanded-path count-msg git-result)))))

(defun greger-stdlib--shell-command (command callback &optional working-directory timeout metadata)
  "Execute COMMAND in WORKING-DIRECTORY and call CALLBACK with (result error).
Prompts for permission before running the command for security.
If METADATA contains safe-shell-commands and COMMAND is in that list, skips
permission prompt.
TIMEOUT specifies the maximum time in seconds to wait for command completion (default 600).
Returns a cancel function that can interrupt the command execution."
  (greger-stdlib--assert-arg-string "command" command :min-length 1)
  (when working-directory
    (greger-stdlib--assert-arg-string "working-directory" working-directory))

  (let* ((work-dir (or working-directory "."))
         (expanded-work-dir (expand-file-name work-dir))
         (safe-commands (plist-get metadata :safe-shell-commands))
         (allow-all-shell-commands (plist-get metadata :allow-all-shell-commands)))

    (unless (file-exists-p expanded-work-dir)
      (error "Working directory does not exist: %s" expanded-work-dir))
    (unless (file-directory-p expanded-work-dir)
      (error "Working directory path is not a directory: %s" expanded-work-dir))
    (when (and (not allow-all-shell-commands)
               (not (member command safe-commands))
               (not (y-or-n-p (format "Execute shell command: '%s' in directory '%s'? "
                                      command expanded-work-dir))))
      (error "Shell command execution cancelled by user"))

    (greger-stdlib--run-async-subprocess
     :command "bash"
     :args (list "-c" command)
     :working-directory expanded-work-dir
     :callback (lambda (output error)
                 (if error
                     (funcall callback nil error)
                   (funcall callback
                            (format "Command executed successfully:\n%s" output)
                            nil)))
     :timeout timeout
     :env '(("PAGER" . "cat")))))

(defun greger-stdlib--ripgrep (pattern path callback case-sensitive file-type
                                       context-lines fixed-strings word-regexp
                                       line-regexp max-results)
  "Search for PATTERN in PATH using the rg command line tool directly.
CALLBACK is called with (result error) when search completes.
CASE-SENSITIVE, FILE-TYPE, CONTEXT-LINES, FIXED-STRINGS, WORD-REGEXP,
LINE-REGEXP and MAX-RESULTS are optional."

  (unless (executable-find "rg")
    (error "Command not found: ripgrep (rg).  Please install ripgrep"))

  (greger-stdlib--assert-arg-string "pattern" pattern)
  (greger-stdlib--assert-arg-string "path" path)
  (greger-stdlib--assert-arg-bool "case-sensitive" case-sensitive)
  (greger-stdlib--assert-arg-bool "fixed-strings" fixed-strings)
  (greger-stdlib--assert-arg-bool "word-regexp" word-regexp)
  (greger-stdlib--assert-arg-bool "line-regexp" line-regexp)
  (greger-stdlib--assert-arg-int "context-lines" context-lines :ge 0)
  (greger-stdlib--assert-arg-int "max-results" max-results :ge 1)

  (let ((expanded-path (expand-file-name path)))

    (if (not (file-exists-p expanded-path))
        (funcall callback nil (format "Path does not exist: %s" expanded-path))

      (let ((args '()))
        (if case-sensitive
            (setq args (append args '("--case-sensitive")))
          (setq args (append args '("--smart-case"))))

        (when (and context-lines (> context-lines 0))
          (setq args (append args (list "--context" (number-to-string context-lines)))))

        (when (and max-results (> max-results 0))
          (setq args (append args (list "--max-count" (number-to-string max-results)))))

        (when (and file-type (not (string-empty-p file-type)))
          (setq args (append args (list "--type" file-type))))

        (when fixed-strings
          (setq args (append args '("--fixed-strings"))))

        (when word-regexp
          (setq args (append args '("--word-regexp"))))

        (when line-regexp
          (setq args (append args '("--line-regexp"))))

        (setq args (append args '("--line-number" "--no-heading")))

        (setq args (append args (list pattern expanded-path)))

        (greger-stdlib--run-async-subprocess
         :command "rg"
         :args args
         :working-directory nil
         :callback (lambda (output error)
                     (if error
                         ;; Check if it's a "no matches" error (exit code 1 with no output)
                         (if (string-match-p "failed with exit code 1" error)
                             (funcall callback "No matches found" nil)
                           (funcall callback nil (format "Failed to execute ripgrep search: %s" error)))
                       (funcall callback
                                (if (string-empty-p (string-trim output))
                                    "No matches found"
                                  output)
                                nil))))))))

(defun greger-stdlib--read-webpage (url &optional extract-text use-highest-readability)
  "Read webpage content from URL.
If EXTRACT-TEXT is non-nil (default t), extract and return text content.
If EXTRACT-TEXT is nil, return raw HTML.
If USE-HIGHEST-READABILITY is non-nil, use eww's aggressive readability setting."
  (greger-stdlib--assert-arg-string-web-url "url" url)

  (condition-case err
      (greger-web-download-page url extract-text use-highest-readability)
    (error "Failed to read webpage: %s" (error-message-string err))))

(provide 'greger-stdlib)

;;; greger-stdlib.el ends here
