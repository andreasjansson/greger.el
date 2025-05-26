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

        (patch . ((name . "patch")
                  (description . "Apply a patch to one or more files using unified diff format. The patch will be applied, then the affected files will be staged and committed to git.")
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

     ((eq tool-symbol 'patch)
      (greger-tools--patch
       (alist-get 'patch_content args)
       (alist-get 'git_commit_message args)))

     ((eq tool-symbol 'replace-function)
      (greger-tools--replace-function
       (alist-get 'file_path args)
       (alist-get 'line_number args)
       (alist-get 'name args)
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

          ;; TODO: remove debug
          (message (format "normalized-patch: %s" normalized-patch))

          ;; Change to the working directory and apply the patch
          (let ((default-directory working-dir))
            (let* ((patch-command (format "patch -p0 --no-backup-if-mismatch < %s"
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

(provide 'greger-tools)

;;; greger-tools.el ends here
