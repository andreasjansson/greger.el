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
  (let ((name-str (cond
                   ((and (listp name) (eq (car name) 'quote))
                    (symbol-name (cadr name)))
                   ((symbolp name)
                    (symbol-name name))
                   (t name)))
        (skills (plist-get args :skills))
        (tools (plist-get args :tools)))
    `(let ((tool-names '()))
       (dolist (tool-def (list ,@tools))
         (let ((tool-name (plist-get tool-def :name)))
           (push tool-name tool-names)
           (greger-plugin--register-tool-from-def tool-def)))
       (puthash ,name-str
                (list :tools (nreverse tool-names)
                      :skills ,skills)
                greger-plugin-registry))))

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

(provide 'greger-plugin)

;;; greger-plugin.el ends here
