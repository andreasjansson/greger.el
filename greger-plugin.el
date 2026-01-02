;;; greger-plugin.el --- Plugin system for greger -*- lexical-binding: t -*-

;; Copyright (C) 2025 Andreas Jansson

;; Author: Andreas Jansson <andreas@jansson.me.uk>
;; Version: 0.1.0
;; URL: https://github.com/andreasjansson/greger.el
;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Provides a plugin system for Greger that allows grouping tools and skills.
;; Plugins can be enabled per-session using <plugin>name</plugin> in the buffer.

;;; Code:

(require 'greger-tools)

(defvar greger-plugin-registry (make-hash-table :test 'equal)
  "Registry mapping plugin names to their definitions.")

(defun greger-plugin-tool (name &rest args)
  "Define a tool NAME with ARGS.  Returns a tool definition plist.
Used inside `greger-plugin' :tools list.

ARGS is a plist with:
  :description - Tool description string
  :properties  - Alist of parameter definitions
  :required    - List of required parameter names
  :function    - Function to call when tool is executed

Additional args supported by `greger-register-tool' are also accepted:
  :pass-buffer, :pass-callback, :streaming, :pass-metadata"
  (append (list :name name) args))

(defmacro greger-plugin (name &rest args)
  "Define plugin NAME with :skills and :tools.

NAME is a symbol (quoted or unquoted).
ARGS is a plist with:
  :skills - Path to a SKILL.md file (optional)
  :tools  - List of tool definitions created with `greger-plugin-tool'

Example:
  (greger-plugin \\='lspcmd
    :skills \"~/.config/greger/skills/lspcmd/SKILL.md\"
    :tools
    ((greger-plugin-tool \"lspcmd-grep\"
       :description \"Search symbols...\"
       :properties ((pattern . ((type . \"string\"))))
       :required (\"pattern\")
       :function my-grep-fn)))"
  (declare (indent 1))
  (let* ((name-str (cond
                    ((and (listp name) (eq (car name) 'quote))
                     (symbol-name (cadr name)))
                    ((symbolp name)
                     (symbol-name name))
                    (t name)))
         (skills (plist-get args :skills))
         (tools (plist-get args :tools))
         (tool-defs (mapcar #'greger-plugin--expand-tool-def tools))
         (tool-names (mapcar (lambda (def) (plist-get def :name)) tool-defs)))
    `(progn
       ,@(mapcar (lambda (def) `(greger-plugin--register-tool-from-def ',def))
                 tool-defs)
       (puthash ,name-str
                (list :tools ',tool-names
                      :skills ,skills)
                greger-plugin-registry))))

(defun greger-plugin--expand-tool-def (tool-form)
  "Expand TOOL-FORM (a greger-plugin-tool call) to a plist at macro-expansion time."
  (if (and (listp tool-form)
           (eq (car tool-form) 'greger-plugin-tool))
      (apply #'greger-plugin-tool (cdr tool-form))
    (error "Invalid tool definition: %S" tool-form)))

(defun greger-plugin--register-tool-from-def (tool-def)
  "Register a tool from TOOL-DEF plist."
  (let ((name (plist-get tool-def :name))
        (description (plist-get tool-def :description))
        (properties (plist-get tool-def :properties))
        (required (plist-get tool-def :required))
        (function (plist-get tool-def :function))
        (pass-buffer (plist-get tool-def :pass-buffer))
        (pass-callback (plist-get tool-def :pass-callback))
        (streaming (plist-get tool-def :streaming))
        (pass-metadata (plist-get tool-def :pass-metadata)))
    (greger-register-tool name
                          :description description
                          :properties properties
                          :required required
                          :function function
                          :pass-buffer pass-buffer
                          :pass-callback pass-callback
                          :streaming streaming
                          :pass-metadata pass-metadata)))

(defun greger-plugin-tools (name)
  "Get list of tool names for plugin NAME."
  (plist-get (gethash name greger-plugin-registry) :tools))

(defun greger-plugin-skill (name)
  "Get skill content for plugin NAME, or nil if not found."
  (when-let* ((path (plist-get (gethash name greger-plugin-registry) :skills))
              ((file-exists-p path)))
    (with-temp-buffer
      (insert-file-contents path)
      (buffer-string))))

(defun greger-plugin-exists-p (name)
  "Return non-nil if plugin NAME exists in the registry."
  (gethash name greger-plugin-registry))

(defun greger-plugin-list ()
  "Return list of all registered plugin names."
  (let ((names '()))
    (maphash (lambda (name _) (push name names)) greger-plugin-registry)
    (nreverse names)))

;; Buffer parsing for <plugin> tags

(defun greger-plugin-parse-buffer-plugins (buffer)
  "Parse BUFFER for <plugin> tags and return tools to enable.
Returns a plist with :session-tools (from SYSTEM) and :turn-tools (from last USER).
Tools from SYSTEM apply to the whole session.
Tools from the last USER section apply only to that turn."
  (with-current-buffer buffer
    (let* ((parser (treesit-parser-create 'greger))
           (root-node (treesit-parser-root-node parser))
           (session-plugins '())
           (turn-plugins '()))

      ;; Walk all nodes to find system and user sections
      (dolist (child (treesit-node-children root-node))
        (let ((node-type (treesit-node-type child)))
          (cond
           ;; System section: plugins apply to whole session
           ((string= node-type "system")
            (let ((plugins (greger-plugin--extract-plugins-from-node child)))
              (setq session-plugins (append session-plugins plugins))))

           ;; User section: only keep plugins from the LAST user section
           ((string= node-type "user")
            (setq turn-plugins (greger-plugin--extract-plugins-from-node child))))))

      ;; Collect all tools from session and turn plugins
      (let ((session-tools '())
            (turn-tools '()))
        (dolist (plugin-name session-plugins)
          (when-let* ((tools (greger-plugin-tools plugin-name)))
            (setq session-tools (append session-tools tools))))
        (dolist (plugin-name turn-plugins)
          (when-let* ((tools (greger-plugin-tools plugin-name)))
            (setq turn-tools (append turn-tools tools))))

        (list :session-tools (delete-dups session-tools)
              :turn-tools (delete-dups turn-tools)
              :session-plugins session-plugins
              :turn-plugins turn-plugins)))))

(defun greger-plugin--extract-plugins-from-node (node)
  "Extract plugin names from <plugin>name</plugin> tags in NODE.
Uses regex to parse the text content of the node."
  (let ((text (treesit-node-text node t))
        (plugins '()))
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (while (re-search-forward "<plugin>\\([^<]+\\)</plugin>" nil t)
        (let ((plugin-name (string-trim (match-string 1))))
          (when (greger-plugin-exists-p plugin-name)
            (push plugin-name plugins)))))
    (nreverse plugins)))

(defun greger-plugin-get-buffer-tools (buffer base-tools)
  "Get tools for BUFFER: BASE-TOOLS plus enabled plugin tools.
Session plugins (from SYSTEM) and turn plugins (from last USER) are combined."
  (let* ((parsed (greger-plugin-parse-buffer-plugins buffer))
         (session-tools (plist-get parsed :session-tools))
         (turn-tools (plist-get parsed :turn-tools)))
    (delete-dups (append base-tools session-tools turn-tools))))

(provide 'greger-plugin)

;;; greger-plugin.el ends here
