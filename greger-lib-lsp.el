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
  :function 'greger-tools--lsp-rename)

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
  :function 'greger-tools--lsp-format)

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
  :function 'greger-tools--lsp-find-definition)

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
  :function 'greger-tools--lsp-find-references)

(greger-register-tool "lsp-document-symbols"
  :description "Get all symbols (functions, classes, variables, etc.) in a document"
  :properties '((file_path . ((type . "string")
                             (description . "Path to the file")))
                (symbol_type . ((type . "string")
                               (description . "Filter by symbol type (Function, Class, Variable, etc.)")
                               (default . nil)))
                (hierarchical . ((type . "boolean")
                                (description . "Return symbols in hierarchical structure")
                                (default . :json-true))))
  :required '("file_path")
  :function 'greger-tools--lsp-document-symbols)

(greger-register-tool "lsp-workspace-symbols"
  :description "Search for symbols across the entire workspace"
  :properties '((query . ((type . "string")
                         (description . "Search query for symbol names")))
                (max_results . ((type . "integer")
                               (description . "Maximum number of results")
                               (default . 50)))
                (symbol_type . ((type . "string")
                               (description . "Filter by symbol type (Function, Class, Variable, etc.)")
                               (default . nil))))
  :required '("query")
  :function 'greger-tools--lsp-workspace-symbols)

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
        (goto-char (point-min))
        (condition-case nil
            (progn
              (forward-line (1- line))
              (forward-char (min column (- (line-end-position) (line-beginning-position))))
              (funcall func))
          (error
           ;; If we can't reach the position, go to end of buffer and try
           (goto-char (point-max))
           (funcall func)))))))

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
    (format "%s:%d:%d" (file-relative-name file-path) line character)))

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

(defun greger-lsp--format-document-symbol (symbol &optional indent)
  "Format a single LSP document SYMBOL for display with optional INDENT."
  (condition-case err
      (let* ((name (cond
                    ((lsp:document-symbol-name? symbol) (lsp:document-symbol-name symbol))
                    ((lsp:symbol-information-name? symbol) (lsp:symbol-information-name symbol))
                    (t (format "unknown-%s" symbol))))
             (kind (cond
                    ((lsp:document-symbol-kind? symbol) (lsp:document-symbol-kind symbol))
                    ((lsp:symbol-information-kind? symbol) (lsp:symbol-information-kind symbol))
                    (t 1))) ; Default to File kind
             (kind-name (alist-get kind lsp-symbol-kinds "Unknown"))
             (range (cond
                     ((lsp:document-symbol-range? symbol) (lsp:document-symbol-range symbol))
                     ((lsp:symbol-information-location? symbol)
                      (lsp:location-range (lsp:symbol-information-location symbol)))
                     (t nil)))
             (line (if range
                       (1+ (lsp:position-line (lsp:range-start range)))
                     0))
             (indent-str (make-string (or indent 0) ?\s)))
        (format "%s%s [%s] (line %d)" indent-str name kind-name line))
    (error (format "Error formatting symbol %s: %s" symbol (error-message-string err)))))

(defun greger-lsp--format-document-symbols (symbols &optional hierarchical indent)
  "Format a list of LSP document SYMBOLS for display.
If HIERARCHICAL is true, format with indentation to show structure."
  (if (null symbols)
      "No symbols found"
    (mapconcat
     (lambda (symbol)
       (let ((formatted (greger-lsp--format-document-symbol symbol indent)))
         (if (and hierarchical (lsp:document-symbol-children? symbol))
             (concat formatted "\n"
                     (greger-lsp--format-document-symbols
                      (append (lsp:document-symbol-children? symbol) nil)
                      hierarchical
                      (+ (or indent 0) 2)))
           formatted)))
     symbols "\n")))

;;; Tool implementations

(defun greger-tools--lsp-rename (new-name file-path line column)
  "Rename symbol at FILE-PATH:LINE:COLUMN to NEW-NAME using LSP."
  (condition-case err
      (greger-lsp--with-buffer-at-position file-path line column
        (lambda ()
          (unless (greger-lsp--feature-supported-p "textDocument/rename")
            (error "LSP server does not support rename"))

          ;; Get symbol info first to show what we're renaming
          (let* ((symbol-info (condition-case nil
                                  (thing-at-point 'symbol)
                                (error "unknown")))
                 (edits (lsp-request "textDocument/rename"
                                   (lsp-make-rename-params
                                    :text-document (lsp--text-document-identifier)
                                    :position (lsp--cur-position)
                                    :new-name new-name))))
            (if edits
                (progn
                  (lsp--apply-workspace-edit edits 'rename)
                  (format "Successfully renamed '%s' to '%s' in %d location(s)"
                          symbol-info
                          new-name
                          (length (lsp:workspace-edit-changes edits))))
              "No changes made - symbol may not exist or rename not applicable"))))
    (error (format "LSP rename failed: %s" (error-message-string err)))))

(defun greger-tools--lsp-format (file-path &optional start-line end-line)
  "Format FILE-PATH using LSP. If START-LINE and END-LINE provided, format only that range."
  (condition-case err
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
                                (lsp-request "textDocument/rangeFormatting"
                                           (lsp--make-document-range-formatting-params start-pos (point))))))
                        ;; Full document formatting
                        (progn
                          (unless (greger-lsp--feature-supported-p "textDocument/formatting")
                            (error "LSP server does not support formatting"))
                          (lsp-request "textDocument/formatting"
                                     (lsp--make-document-formatting-params))))))
            (if (and edits (not (seq-empty-p edits)))
                (progn
                  (lsp--apply-text-edits edits 'format)
                  (save-buffer)
                  (format "Successfully formatted %s (%d edit(s) applied)"
                          (file-relative-name file-path)
                          (length edits)))
              "No formatting changes needed"))))
    (error (format "LSP format failed: %s" (error-message-string err)))))

(defun greger-tools--lsp-find-definition (file-path line column &optional include-declaration)
  "Find definition(s) of symbol at FILE-PATH:LINE:COLUMN using LSP."
  (condition-case err
      (greger-lsp--with-buffer-at-position file-path line column
        (lambda ()
          (unless (greger-lsp--feature-supported-p "textDocument/definition")
            (error "LSP server does not support go-to-definition"))

          (let* ((symbol-info (condition-case nil
                                  (thing-at-point 'symbol)
                                (error "unknown")))
                 (locations (lsp-request "textDocument/definition"
                                       (lsp--text-document-position-params)))
                 (result-text (greger-lsp--format-locations locations)))

            ;; Also get declarations if requested and supported
            (when (and include-declaration
                       (greger-lsp--feature-supported-p "textDocument/declaration"))
              (let ((declarations (lsp-request "textDocument/declaration"
                                             (lsp--text-document-position-params))))
                (when declarations
                  (setq result-text (concat result-text "\n\nDeclarations:\n"
                                          (greger-lsp--format-locations declarations))))))

            (format "Definition(s) for '%s':\n%s" symbol-info result-text))))
    (error (format "LSP find-definition failed: %s" (error-message-string err)))))

(defun greger-tools--lsp-find-references (file-path line column &optional include-declaration max-results)
  "Find references to symbol at FILE-PATH:LINE:COLUMN using LSP."
  (condition-case err
      (greger-lsp--with-buffer-at-position file-path line column
        (lambda ()
          (unless (greger-lsp--feature-supported-p "textDocument/references")
            (error "LSP server does not support find-references"))

          (let* ((symbol-info (condition-case nil
                                  (thing-at-point 'symbol)
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

            (format "References for '%s' (%d found%s):\n%s"
                    symbol-info
                    (length locations)
                    (if (and max-results (> (length locations) max-results))
                        (format ", showing first %d" max-results)
                      "")
                    result-text))))
    (error (format "LSP find-references failed: %s" (error-message-string err)))))

(defun greger-tools--lsp-document-symbols (file-path &optional symbol-type hierarchical)
  "Get document symbols for FILE-PATH using LSP."
  (condition-case err
      (let ((buffer (greger-lsp--ensure-server file-path)))
        (with-current-buffer buffer
          (unless (greger-lsp--feature-supported-p "textDocument/documentSymbol")
            (error "LSP server does not support document symbols"))

          (let* ((symbols (let ((lsp-response-timeout 10)) ; Shorter timeout for tests
                                  (lsp-request "textDocument/documentSymbol"
                                               `(:textDocument ,(lsp--text-document-identifier)))))
                 ;; Filter by symbol type if specified
                 (filtered-symbols (if symbol-type
                                     (let ((target-kind (cl-position symbol-type lsp-symbol-kinds :test #'string-equal-ignore-case)))
                                       (if target-kind
                                           (seq-filter (lambda (sym)
                                                        (= (if (lsp:document-symbol-kind sym)
                                                               (lsp:document-symbol-kind sym)
                                                             (lsp:symbol-information-kind sym))
                                                           target-kind))
                                                      symbols)
                                         symbols))
                                   symbols))
                 (result-text (greger-lsp--format-document-symbols filtered-symbols hierarchical)))

            (format "Document symbols for %s%s:\n%s"
                    (file-relative-name file-path)
                    (if symbol-type (format " (type: %s)" symbol-type) "")
                    result-text))))
    (error (format "LSP document-symbols failed: %s" (error-message-string err)))))

(defun greger-tools--lsp-workspace-symbols (query &optional max-results symbol-type)
  "Search for symbols across workspace using LSP."
  (condition-case err
      (progn
        ;; Ensure we have at least one LSP workspace
        (unless lsp--session
          (error "No LSP session found. Please open a file with LSP support first"))

        (unless (cl-some (lambda (ws) (lsp-feature? "workspace/symbol" ws))
                         (lsp--session-workspaces lsp--session))
          (error "No LSP server supports workspace symbols"))

        (let* ((symbols (let ((lsp-response-timeout 10)) ; Shorter timeout for tests
                                  (lsp-request "workspace/symbol" `(:query ,query))))
               ;; Filter by symbol type if specified
               (filtered-symbols (if symbol-type
                                   (let ((target-kind (cl-position symbol-type lsp-symbol-kinds :test #'string-equal-ignore-case)))
                                     (if target-kind
                                         (seq-filter (lambda (sym)
                                                      (= (lsp:symbol-information-kind sym) target-kind))
                                                    symbols)
                                       symbols))
                                 symbols))
               ;; Limit results if specified
               (limited-symbols (if max-results
                                  (seq-take filtered-symbols max-results)
                                filtered-symbols))
               (result-text (if (null limited-symbols)
                              "No symbols found"
                            (mapconcat #'greger-lsp--format-symbol limited-symbols "\n"))))

          (format "Workspace symbols for query '%s'%s (%d found%s):\n%s"
                  query
                  (if symbol-type (format " (type: %s)" symbol-type) "")
                  (length filtered-symbols)
                  (if (and max-results (> (length filtered-symbols) max-results))
                      (format ", showing first %d" max-results)
                    "")
                  result-text)))
    (error (format "LSP workspace-symbols failed: %s" (error-message-string err)))))


(provide 'greger-lib-lsp)

;;; greger-lib-lsp.el ends here
