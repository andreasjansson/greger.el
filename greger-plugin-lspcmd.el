;;; greger-plugin-lspcmd.el --- LSP code navigation plugin -*- lexical-binding: t -*-

;; Copyright (C) 2025 Andreas Jansson

;; Author: Andreas Jansson <andreas@jansson.me.uk>
;; Version: 0.1.0
;; URL: https://github.com/andreasjansson/greger.el
;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Provides LSP-powered code navigation tools via the lspcmd CLI tool.
;; Requires lspcmd to be installed: pip install lspcmd
;;
;; Enable in a Greger session with: <plugin>lspcmd</plugin>

;;; Code:

(require 'greger-plugin)

(defun greger-lspcmd--run (&rest args)
  "Run lspcmd with ARGS, flattening nested lists and removing nils."
  (with-temp-buffer
    (let ((flat-args (flatten-list (seq-remove #'null args)))
          (exit-code nil))
      (setq exit-code (apply #'call-process "lspcmd" nil t nil flat-args))
      (if (zerop exit-code)
          (string-trim (buffer-string))
        (error "lspcmd: %s" (buffer-string))))))

(defun greger-lspcmd--grep (pattern &optional path kind exclude docs case-sensitive root)
  "Search for symbols matching PATTERN."
  (greger-lspcmd--run "grep" pattern path
                      (when kind (list "-k" kind))
                      (when docs "--docs")
                      (when case-sensitive "--case-sensitive")
                      (when root (list "--root" root))
                      (mapcar (lambda (x) (list "-x" x)) (or exclude '()))))

(defun greger-lspcmd--files (&optional path exclude include root)
  "Show file tree with symbol counts."
  (greger-lspcmd--run "files" path
                      (when root (list "--root" root))
                      (mapcar (lambda (x) (list "-x" x)) (or exclude '()))
                      (mapcar (lambda (x) (list "-i" x)) (or include '()))))

(defun greger-lspcmd--show (symbol &optional context head root)
  "Show definition of SYMBOL."
  (greger-lspcmd--run "show" symbol
                      (when context (list "-n" (number-to-string context)))
                      (when head (list "--head" (number-to-string head)))
                      (when root (list "--root" root))))

(defun greger-lspcmd--refs (symbol &optional context root)
  "Find references to SYMBOL."
  (greger-lspcmd--run "refs" symbol
                      (when context (list "-n" (number-to-string context)))
                      (when root (list "--root" root))))

(defun greger-lspcmd--calls (&optional from to max-depth include-non-workspace root)
  "Show call hierarchy."
  (unless (or from to) (error "Requires 'from' or 'to'"))
  (greger-lspcmd--run "calls"
                      (when from (list "--from" from))
                      (when to (list "--to" to))
                      (when max-depth (list "--max-depth" (number-to-string max-depth)))
                      (when include-non-workspace "--include-non-workspace")
                      (when root (list "--root" root))))

(defun greger-lspcmd--implementations (symbol &optional context root)
  "Find implementations of SYMBOL."
  (greger-lspcmd--run "implementations" symbol
                      (when context (list "-n" (number-to-string context)))
                      (when root (list "--root" root))))

(defun greger-lspcmd--supertypes (symbol &optional context root)
  "Find supertypes of SYMBOL."
  (greger-lspcmd--run "supertypes" symbol
                      (when context (list "-n" (number-to-string context)))
                      (when root (list "--root" root))))

(defun greger-lspcmd--subtypes (symbol &optional context root)
  "Find subtypes of SYMBOL."
  (greger-lspcmd--run "subtypes" symbol
                      (when context (list "-n" (number-to-string context)))
                      (when root (list "--root" root))))

(defun greger-lspcmd--declaration (symbol &optional context root)
  "Find declaration of SYMBOL."
  (greger-lspcmd--run "declaration" symbol
                      (when context (list "-n" (number-to-string context)))
                      (when root (list "--root" root))))

(defun greger-lspcmd--rename (symbol new-name &optional root)
  "Rename SYMBOL to NEW-NAME."
  (greger-lspcmd--run "rename" symbol new-name
                      (when root (list "--root" root))))

(defun greger-lspcmd--mv (old-path new-path &optional root)
  "Move OLD-PATH to NEW-PATH and update imports."
  (greger-lspcmd--run "mv" old-path new-path
                      (when root (list "--root" root))))

(defun greger-lspcmd--workspace-add (root)
  "Add ROOT as a workspace."
  (greger-lspcmd--run "workspace" "add" "--root" root))


;; Plugin definition

(greger-plugin 'lspcmd
  :tools
  ((greger-plugin-tool "lspcmd-grep"
     :description "Search for symbols matching a regex pattern using LSP. PREFERRED over ripgrep for finding function/class/method definitions. Only searches symbol names (not file contents). Use ripgrep for string literals, comments, or multi-word text search."
     :properties ((pattern . ((type . "string") (description . "Regex pattern to match symbol names (case-insensitive by default)")))
                  (path . ((type . "string") (description . "File path or directory (supports wildcards). Directories search recursively.") (default . nil)))
                  (kind . ((type . "string") (description . "Filter by symbol kind (comma-separated). Valid: class, function, method, variable, constant, interface, struct, enum, property, field, constructor, module, namespace, package, typeparameter") (default . nil)))
                  (exclude . ((type . "array") (items . ((type . "string"))) (description . "Exclude files matching glob pattern or directory") (default . nil)))
                  (docs . ((type . "boolean") (description . "Include documentation/docstrings for each symbol") (default . nil)))
                  (case-sensitive . ((type . "boolean") (description . "Case-sensitive pattern matching") (default . nil)))
                  (root . ((type . "string") (description . "Workspace root directory. Defaults to current working directory.") (default . nil))))
     :required ("pattern")
     :function greger-lspcmd--grep)

   (greger-plugin-tool "lspcmd-files"
     :description "Show source file tree with symbol and line counts. Good starting point for exploring a project."
     :properties ((path . ((type . "string") (description . "Directory path to list (defaults to workspace root)") (default . nil)))
                  (exclude . ((type . "array") (items . ((type . "string"))) (description . "Exclude files matching glob pattern or directory") (default . nil)))
                  (include . ((type . "array") (items . ((type . "string"))) (description . "Include default-excluded directories (e.g., .git, node_modules)") (default . nil)))
                  (root . ((type . "string") (description . "Workspace root directory. Defaults to current working directory.") (default . nil))))
     :required ()
     :function greger-lspcmd--files)

   (greger-plugin-tool "lspcmd-show"
     :description "Print the full definition/body of a symbol. PREFERRED over read-file for viewing function/method bodies."
     :properties ((symbol . ((type . "string") (description . "Symbol to show. Formats: SymbolName, Parent.Symbol, path:Symbol, path:Parent.Symbol, path:line:Symbol")))
                  (context . ((type . "integer") (description . "Lines of context around definition") (default . nil)))
                  (head . ((type . "integer") (description . "Maximum lines to show (default: 200)") (default . nil)))
                  (root . ((type . "string") (description . "Workspace root directory. Defaults to current working directory.") (default . nil))))
     :required ("symbol")
     :function greger-lspcmd--show)

   (greger-plugin-tool "lspcmd-refs"
     :description "Find all references to a symbol across the workspace. PREFERRED over ripgrep for finding symbol usages."
     :properties ((symbol . ((type . "string") (description . "Symbol to find references for. Formats: SymbolName, Parent.Symbol, path:Symbol")))
                  (context . ((type . "integer") (description . "Lines of context around each reference") (default . nil)))
                  (root . ((type . "string") (description . "Workspace root directory. Defaults to current working directory.") (default . nil))))
     :required ("symbol")
     :function greger-lspcmd--refs)

   (greger-plugin-tool "lspcmd-calls"
     :description "Show call hierarchy for a symbol. Find what a function calls (--from) or what calls it (--to)."
     :properties ((from . ((type . "string") (description . "Starting symbol to show outgoing calls from") (default . nil)))
                  (to . ((type . "string") (description . "Target symbol to show incoming calls to") (default . nil)))
                  (max-depth . ((type . "integer") (description . "Maximum recursion depth (default: 3)") (default . nil)))
                  (include-non-workspace . ((type . "boolean") (description . "Include calls to symbols outside workspace (stdlib, dependencies)") (default . nil)))
                  (root . ((type . "string") (description . "Workspace root directory. Defaults to current working directory.") (default . nil))))
     :required ()
     :function greger-lspcmd--calls)

   (greger-plugin-tool "lspcmd-implementations"
     :description "Find implementations of an interface or abstract method."
     :properties ((symbol . ((type . "string") (description . "Interface or abstract method to find implementations of")))
                  (context . ((type . "integer") (description . "Lines of context around each implementation") (default . nil)))
                  (root . ((type . "string") (description . "Workspace root directory. Defaults to current working directory.") (default . nil))))
     :required ("symbol")
     :function greger-lspcmd--implementations)

   (greger-plugin-tool "lspcmd-supertypes"
     :description "Find direct supertypes (parent classes/interfaces) of a type."
     :properties ((symbol . ((type . "string") (description . "Type to find supertypes of")))
                  (context . ((type . "integer") (description . "Lines of context") (default . nil)))
                  (root . ((type . "string") (description . "Workspace root directory. Defaults to current working directory.") (default . nil))))
     :required ("symbol")
     :function greger-lspcmd--supertypes)

   (greger-plugin-tool "lspcmd-subtypes"
     :description "Find direct subtypes (child classes) of a type."
     :properties ((symbol . ((type . "string") (description . "Type to find subtypes of")))
                  (context . ((type . "integer") (description . "Lines of context") (default . nil)))
                  (root . ((type . "string") (description . "Workspace root directory. Defaults to current working directory.") (default . nil))))
     :required ("symbol")
     :function greger-lspcmd--subtypes)

   (greger-plugin-tool "lspcmd-declaration"
     :description "Find the declaration of a symbol."
     :properties ((symbol . ((type . "string") (description . "Symbol to find declaration of")))
                  (context . ((type . "integer") (description . "Lines of context") (default . nil)))
                  (root . ((type . "string") (description . "Workspace root directory. Defaults to current working directory.") (default . nil))))
     :required ("symbol")
     :function greger-lspcmd--declaration)

   (greger-plugin-tool "lspcmd-rename"
     :description "Rename a symbol across the entire workspace."
     :properties ((symbol . ((type . "string") (description . "Symbol to rename. Formats: SymbolName, Parent.Symbol, path:Symbol")))
                  (new-name . ((type . "string") (description . "New name for the symbol")))
                  (root . ((type . "string") (description . "Workspace root directory. Defaults to current working directory.") (default . nil))))
     :required ("symbol" "new-name")
     :function greger-lspcmd--rename)

   (greger-plugin-tool "lspcmd-mv"
     :description "Move/rename a file and update all imports across the workspace."
     :properties ((old-path . ((type . "string") (description . "Current file path")))
                  (new-path . ((type . "string") (description . "New file path")))
                  (root . ((type . "string") (description . "Workspace root directory. Defaults to current working directory.") (default . nil))))
     :required ("old-path" "new-path")
     :function greger-lspcmd--mv)

   (greger-plugin-tool "lspcmd-workspace-add"
     :description "Add a workspace root directory for LSP operations. Required before using other lspcmd tools on a new project."
     :properties ((root . ((type . "string") (description . "Workspace root directory path"))))
     :required ("root")
     :function greger-lspcmd--workspace-add)))

(provide 'greger-plugin-lspcmd)

;;; greger-plugin-lspcmd.el ends here
