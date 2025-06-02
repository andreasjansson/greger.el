;;; greger-lib-lsp.el --- LSP-based tools for greger agent -*- lexical-binding: t -*-

;;; Commentary:
;; Provides LSP-powered refactoring and code analysis tools for greger

;;; Code:

(require 'greger-tools)
(require 'lsp-mode)
(require 'cl-lib)

;;; Tool registrations

(greger-register-tool "lsp-rename"
  :description "Rename a symbol across the entire codebase using LSP"
  :properties '((new_name . ((type . "string")
                            (description . "New name for the symbol")))
                (file_path . ((type . "string")
                             (description . "Path to file containing the symbol")))
                (line . ((type . "integer")
                        (description . "Line number (1-based) where symbol is located")))
                (column . ((type . "integer")
                          (description . "Column number (0-based) where symbol starts"))))
  :required '("new_name" "file_path" "line" "column")
  :function 'greger-lib-lsp--rename)

(greger-register-tool "lsp-format"
  :description "Format code according to language standards using LSP"
  :properties '((file_path . ((type . "string")
                             (description . "Path to the file to format")))
                (start_line . ((type . "integer")
                              (description . "Start line for range formatting (1-based). If not provided, formats entire file")
                              (default . nil)))
                (end_line . ((type . "integer")
                            (description . "End line for range formatting (1-based). If not provided, formats entire file")
                            (default . nil))))
  :required '("file_path")
  :function 'greger-lib-lsp--format)

(greger-register-tool "lsp-find-definition"
  :description "Find the definition(s) of a symbol at a specific location"
  :properties '((file_path . ((type . "string")
                             (description . "Path to the file")))
                (line . ((type . "integer")
                        (description . "Line number (1-based)")))
                (column . ((type . "integer")
                          (description . "Column number (0-based)")))
                (include_declaration . ((type . "boolean")
                                       (description . "Also include declarations")
                                       (default . :json-false))))
  :required '("file_path" "line" "column")
  :function 'greger-lib-lsp--find-definition)

(greger-register-tool "lsp-find-references"
  :description "Find all references to a symbol at a specific location"
  :properties '((file_path . ((type . "string")
                             (description . "Path to the file")))
                (line . ((type . "integer")
                        (description . "Line number (1-based)")))
                (column . ((type . "integer")
                          (description . "Column number (0-based)")))
                (include_declaration . ((type . "boolean")
                                       (description . "Include the symbol declaration in results")
                                       (default . :json-true)))
                (max_results . ((type . "integer")
                               (description . "Maximum number of references to return")
                               (default . 100))))
  :required '("file_path" "line" "column")
  :function 'greger-lib-lsp--find-references)

(greger-register-tool "lsp-document-symbols"
  :description "Get document symbols for one or more files"
  :properties '((file_paths . ((type . "array")
                              (items . ((type . "string")))
                              (description . "List of file paths to get symbols for")))
                (detailed . ((type . "boolean")
                            (description . "Include detailed symbols like variables, constants, etc.")
                            (default . :json-false))))
  :required '("file_paths")
  :function 'greger-lib-lsp--document-symbols)

;;; Helper functions

(defun greger-lsp--ensure-server (file-path)
  "Ensure LSP server is running for FILE-PATH.
Returns the buffer visiting the file, or signals an error if LSP is not available."
  (let ((buffer (or (find-buffer-visiting file-path)
                    (find-file-noselect file-path))))
    (with-current-buffer buffer
      (unless (and (bound-and-true-p lsp-mode) lsp--buffer-workspaces)
        (if (fboundp 'lsp)
            (progn
              (lsp)
              (unless lsp--buffer-workspaces
                (error "Failed to start LSP server for %s" file-path)))
          (error "LSP mode not available. Please install and configure lsp-mode"))))
    buffer))

(defun greger-lsp--with-buffer-at-position (file-path line column func)
  "Execute FUNC in buffer visiting FILE-PATH at LINE and COLUMN.
LINE is 1-based, COLUMN is 0-based."
  (let ((buffer (greger-lsp--ensure-server file-path)))
    (with-current-buffer buffer
      (save-excursion
        (condition-case err
            (progn
              ;; Ensure line is within buffer bounds
              (let ((max-line (line-number-at-pos (point-max))))
                (when (> line max-line)
                  (error "Line %d exceeds file length (%d lines)" line max-line)))

              ;; Go to the specified line
              (goto-char (point-min))
              (forward-line (1- line))

              ;; Ensure column is within line bounds
              (let ((line-length (- (line-end-position) (line-beginning-position))))
                (forward-char (min column line-length)))

              (funcall func))
          (error
           (error "Position error in %s at line %d, column %d: %s"
                  file-path line column (error-message-string err))))))))

(defun greger-lsp--feature-supported-p (method)
  "Check if the current LSP server supports METHOD."
  (lsp-feature? method))

(defun greger-lsp--position-params (file-path line column)
  "Create LSP position parameters for FILE-PATH at LINE and COLUMN."
  (greger-lsp--with-buffer-at-position file-path line column
    (lambda ()
      (lsp--text-document-position-params))))

(defun greger-lsp--format-location (location)
  "Format a single LSP LOCATION for display."
  (let* ((uri (lsp:location-uri location))
         (range (lsp:location-range location))
         (start (lsp:range-start range))
         (line (1+ (lsp:position-line start)))
         (character (lsp:position-character start))
         (file-path (lsp--uri-to-path uri)))
    (format "%s:%d:%d" (substring-no-properties (file-relative-name file-path)) line character)))

(defun greger-lsp--format-locations (locations)
  "Format a list of LSP LOCATIONS for display."
  (if (null locations)
      "No locations found"
    (mapconcat #'greger-lsp--format-location locations "\n")))

(defun greger-lsp--format-symbol (symbol)
  "Format a single LSP SYMBOL for display."
  (let* ((name (lsp:symbol-information-name symbol))
         (kind (lsp:symbol-information-kind symbol))
         (location (lsp:symbol-information-location symbol))
         (container (lsp:symbol-information-container-name symbol))
         (kind-name (alist-get kind lsp-symbol-kinds "Unknown"))
         (formatted-location (greger-lsp--format-location location)))
    (format "%s [%s] %s%s"
            name
            kind-name
            formatted-location
            (if container (format " (in %s)" container) ""))))

(defun greger-lsp--detailed-symbol-kind-p (kind)
  "Check if symbol KIND is a detailed type that should be filtered in non-detailed mode."
  (member kind '(13   ; Variable
                 14   ; Constant
                 15   ; String
                 16   ; Number
                 17   ; Boolean
                 18   ; Array
                 19   ; Object
                 20   ; Key
                 21   ; Null
                 22   ; EnumMember
                 23   ; Struct
                 24   ; Event
                 25   ; Operator
                 26   ; TypeParameter
                 )))

(defun greger-lsp--filter-detailed-symbols (symbols detailed)
  "Filter SYMBOLS based on DETAILED flag. If DETAILED is nil, remove detailed symbol types."
  (if detailed
      symbols
    (cl-remove-if (lambda (symbol)
                    (let ((kind (gethash "kind" symbol)))
                      (greger-lsp--detailed-symbol-kind-p kind)))
                  symbols)))

(defun greger-lsp--format-document-symbol (symbol &optional indent-level detailed)
  "Format a document symbol SYMBOL for display with optional INDENT-LEVEL and DETAILED flag."
  (let* ((indent-level (or indent-level 0))
         (indent (make-string (* indent-level 2) ?\ ))
         (name (gethash "name" symbol))
         (kind (gethash "kind" symbol))
         (kind-name (alist-get kind lsp-symbol-kinds "Unknown"))
         (range (gethash "range" symbol))
         (start (gethash "start" range))
         (line (1+ (gethash "line" start)))
         (character (gethash "character" start))
         (children (gethash "children" symbol))
         (filtered-children (when children (greger-lsp--filter-detailed-symbols children detailed)))
         (result (format "%s%s [%s] (line %d, col %d)"
                        indent name kind-name line character)))

    ;; Add children if they exist after filtering
    (when (and filtered-children (> (length filtered-children) 0))
      (setq result (concat result "\n"
                          (mapconcat (lambda (child)
                                      (greger-lsp--format-document-symbol child (1+ indent-level) detailed))
                                    filtered-children "\n"))))
    result))

(defun greger-lsp--format-document-symbols (symbols file-path &optional detailed)
  "Format document SYMBOLS for FILE-PATH as a readable string with optional DETAILED flag."
  (let ((relative-path (if (and (bound-and-true-p lsp-mode) (lsp-workspace-root))
                           (file-relative-name file-path (lsp-workspace-root))
                         (file-relative-name file-path)))
        (filtered-symbols (greger-lsp--filter-detailed-symbols symbols detailed)))
    (if (or (null filtered-symbols) (= (length filtered-symbols) 0))
        (format "No symbols found in %s" relative-path)
      (format "Symbols in %s:\n%s"
              relative-path
              (mapconcat (lambda (symbol)
                          (greger-lsp--format-document-symbol symbol 0 detailed))
                        filtered-symbols "\n")))))

;;; Tool implementations

(defun greger-lib-lsp--rename (new-name file-path line column)
  "Rename symbol at FILE-PATH:LINE:COLUMN to NEW-NAME using LSP."
  (greger-lsp--with-buffer-at-position
   file-path line column
   (lambda ()
     (unless (greger-lsp--feature-supported-p "textDocument/rename")
       (error "LSP server does not support rename"))

     ;; Get symbol info first to show what we're renaming
     (let* ((symbol-info (condition-case nil
                             (substring-no-properties (or (thing-at-point 'symbol) "unknown"))
                           (error "unknown")))
            (edits (let ((lsp-response-timeout 10)) ; Shorter timeout for tests
                     (lsp-request "textDocument/rename"
                                  `(:textDocument ,(lsp--text-document-identifier)
                                                  :position ,(lsp--cur-position)
                                                  :newName ,new-name)))))

       (if edits
           (progn
             (lsp--apply-workspace-edit edits 'rename)

             ;; Save all modified buffers to ensure changes are persisted
             (-let (((&WorkspaceEdit :document-changes? :changes?) edits))
               ;; Save buffers modified by documentChanges
               (when document-changes?
                 (seq-do (lambda (change)
                           (when-let ((text-doc (lsp-get change :textDocument))
                                     (uri (lsp-get text-doc :uri))
                                     (file-path (lsp--uri-to-path uri)))
                             (when-let ((buffer (get-file-buffer file-path)))
                               (with-current-buffer buffer
                                 (save-buffer)))))
                         document-changes?))

               ;; Save buffers modified by changes
               (when changes?
                 (lsp-map (lambda (uri _text-edits)
                            (when-let ((file-path (lsp--uri-to-path uri)))
                              (when-let ((buffer (get-file-buffer file-path)))
                                (with-current-buffer buffer
                                  (save-buffer)))))
                          changes?)))

             ;; Count the changes - WorkspaceEdit can have either changes or documentChanges
             (let ((change-count
                    (-let (((&WorkspaceEdit :document-changes? :changes?) edits))
                      (+ (if document-changes?
                             ;; Count edits in each document change
                             (seq-reduce (lambda (total change)
                                          (+ total (length (lsp-get change :edits))))
                                        document-changes? 0)
                           0)
                         (if changes?
                             (let ((total 0))
                               (lsp-map (lambda (_uri text-edits)
                                          (setq total (+ total (length text-edits))))
                                        changes?)
                               total)
                           0)))))
               (substring-no-properties
                (format "Successfully renamed '%s' to '%s' in %d location(s)"
                        symbol-info
                        new-name
                        change-count))))
         "No changes made - symbol may not exist or rename not applicable")))))

(defun greger-lib-lsp--format (file-path &optional start-line end-line)
  "Format FILE-PATH using LSP. If START-LINE and END-LINE provided, format only that range."
  (let ((buffer (greger-lsp--ensure-server file-path)))
    (with-current-buffer buffer
      (let ((edits (if (and start-line end-line)
                       ;; Range formatting
                       (progn
                         (unless (greger-lsp--feature-supported-p "textDocument/rangeFormatting")
                           (error "LSP server does not support range formatting"))
                         (save-excursion
                           (goto-char (point-min))
                           (forward-line (1- start-line))
                           (let ((start-pos (point)))
                             (forward-line (- end-line start-line))
                             (let ((lsp-response-timeout 10))
                               (lsp-request "textDocument/rangeFormatting"
                                            `(:textDocument ,(lsp--text-document-identifier)
                                                            :range (:start ,(lsp--point-to-position start-pos)
                                                                           :end ,(lsp--point-to-position (point)))
                                                            :options (:tabSize 4 :insertSpaces t)))))))
                     ;; Full document formatting
                     (progn
                       (unless (greger-lsp--feature-supported-p "textDocument/formatting")
                         (error "LSP server does not support formatting"))
                       (let ((lsp-response-timeout 10))
                         (lsp-request "textDocument/formatting"
                                      `(:textDocument ,(lsp--text-document-identifier)
                                                      :options (:tabSize 4 :insertSpaces t))))))))
        (if (and edits (not (seq-empty-p edits)))
            (progn
              (lsp--apply-text-edits edits 'format)
              (save-buffer)
              (substring-no-properties
               (format "Successfully formatted %s (%d edit(s) applied)"
                       (file-relative-name file-path)
                       (length edits))))
          "No formatting changes needed")))))

(defun greger-lib-lsp--find-definition (file-path line column &optional include-declaration)
  "Find definition(s) of symbol at FILE-PATH:LINE:COLUMN using LSP."
  (greger-lsp--with-buffer-at-position
   file-path line column
   (lambda ()
     (unless (greger-lsp--feature-supported-p "textDocument/definition")
       (error "LSP server does not support go-to-definition"))

     (let* ((symbol-info (condition-case nil
                             (substring-no-properties (or (thing-at-point 'symbol) "unknown"))
                           (error "unknown")))
            (locations (let ((lsp-response-timeout 10)) ; Shorter timeout for tests
                         (lsp-request "textDocument/definition"
                                      `(:textDocument ,(lsp--text-document-identifier)
                                                      :position ,(lsp--cur-position)))))
            (result-text (greger-lsp--format-locations locations)))

       ;; Also get declarations if requested and supported
       (when (and include-declaration
                  (greger-lsp--feature-supported-p "textDocument/declaration"))
         (let ((declarations (let ((lsp-response-timeout 10))
                               (lsp-request "textDocument/declaration"
                                            `(:textDocument ,(lsp--text-document-identifier)
                                                            :position ,(lsp--cur-position))))))
           (when declarations
             (setq result-text (concat result-text "\n\nDeclarations:\n"
                                       (greger-lsp--format-locations declarations))))))

       (substring-no-properties
        (format "Definition(s) for '%s':\n%s" symbol-info result-text))))))

(defun greger-lib-lsp--find-references (file-path line column &optional include-declaration max-results)
  "Find references to symbol at FILE-PATH:LINE:COLUMN using LSP."
  (greger-lsp--with-buffer-at-position
   file-path line column
   (lambda ()
     (unless (greger-lsp--feature-supported-p "textDocument/references")
       (error "LSP server does not support find-references"))

     (let* ((symbol-info (condition-case nil
                             (substring-no-properties (or (thing-at-point 'symbol) "unknown"))
                           (error "unknown")))
            (params `(:textDocument ,(lsp--text-document-identifier)
                                    :position ,(lsp--cur-position)
                                    :context (:includeDeclaration ,(if include-declaration t :json-false))))
            (locations (let ((lsp-response-timeout 10)) ; Shorter timeout for tests
                         (lsp-request "textDocument/references" params)))
            (limited-locations (if max-results
                                   (seq-take locations max-results)
                                 locations))
            (result-text (greger-lsp--format-locations limited-locations)))

       (substring-no-properties
        (format "References for '%s' (%d found%s):\n%s"
                symbol-info
                (length locations)
                (if (and max-results (> (length locations) max-results))
                    (format ", showing first %d" max-results)
                  "")
                result-text))))))

(defun greger-lib-lsp--document-symbols (file-paths)
  "Get document symbols for FILE-PATHS using LSP."
  (let ((results '()))
    (dolist (file-path file-paths)
      (let ((buffer (greger-lsp--ensure-server file-path)))
        (with-current-buffer buffer
          (unless (greger-lsp--feature-supported-p "textDocument/documentSymbol")
            (error "LSP server does not support document symbols"))

          (let* ((symbols (let ((lsp-response-timeout 10))
                            (lsp--get-document-symbols)))
                 (formatted (greger-lsp--format-document-symbols symbols file-path)))
            (push formatted results)))))

    (substring-no-properties
     (mapconcat #'identity (reverse results) "\n\n"))))


(provide 'greger-lib-lsp)

;;; greger-lib-lsp.el ends here
