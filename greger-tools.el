;;; greger-tools.el --- Tool registry for greger agent -*- lexical-binding: t -*-

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
;; Defines tools registration functions

;;; Code:

(require 'json)
(require 'cl-lib)

;; Tool structure for tracking executing tools
(cl-defstruct greger-tool
  cancel-fn)

;; Registry to hold tool definitions
(defvar greger-tools-registry (make-hash-table :test 'equal)
  "Hash table mapping tool names to their definitions.")

;; Registry to hold server tool definitions
(defvar greger-server-tools-registry (make-hash-table :test 'equal)
  "Hash table mapping server tool names to their definitions.")

;; greger-register-tool is the main public API of this package, so it uses the package prefix "greger-"
;; rather than the file prefix "greger-tools-"
;; package-lint: disable=wrong-prefix
(defmacro greger-register-tool (name &rest args)
  "Register a tool with NAME and properties specified in ARGS.
ARGS should be a plist containing :description, :properties, :required,
:function, and optionally :pass-buffer, :pass-callback, and :pass-metadata.

Example:
  (greger-register-tool \\='rename-file\\='
    :description \\='Rename or move a file from one path to another\\='
    :properties \\='((old-path . ((type . \\='string\\=')
                              (description . \\='Current path of the file\\=')))
                  (new-path . ((type . \\='string\\=')
                              (description . \\='New path for the file\\=')))
                  (git-commit-message . ((type . \\='string\\=')
                                        (description . \\='Git commit message\\='))))
    :required \\='(\\='old-path\\=' \\='new-path\\=' \\='git-commit-message\\=')
    :function \\='greger-tools--rename-file
    :pass-buffer t
    :pass-callback t
    :pass-metadata t)

  When :pass-callback is set to t, the callback function will be passed to the
  tool function as a \\='callback\\=' parameter instead of `greger-tools-execute\\='
  calling the callback with the result.
  When :pass-metadata is set to t, the metadata from the parser will be passed
  as a \\='metadata\\=' parameter."
  (let ((description (plist-get args :description))
        (properties (plist-get args :properties))
        (required (plist-get args :required))
        (function (plist-get args :function))
        (pass-buffer (plist-get args :pass-buffer))
        (pass-callback (plist-get args :pass-callback))
        (pass-metadata (plist-get args :pass-metadata)))
    `(puthash ,name
              (list :schema (list (cons 'name ,name)
                                  (cons 'description ,description)
                                  (cons 'input_schema
                                        (list (cons 'type "object")
                                              (cons 'properties ,properties)
                                              (cons 'required ,required))))
                    :function ,function
                    :pass-buffer ,pass-buffer
                    :pass-callback ,pass-callback
                    :pass-metadata ,pass-metadata)
              greger-tools-registry)))

;; greger-register-server-tool is the main public API for server tools, so it uses the package prefix "greger-"
;; rather than the file prefix "greger-tools-"
;; package-lint: disable=wrong-prefix
(defmacro greger-register-server-tool (name &rest args)
  "Register a server tool with NAME and properties specified in ARGS.
Server tools are processed by the server (e.g., Anthropic's web search tool).
ARGS should be a plist containing at least :type and any other
named parameters specific to the server tool.

Example:
  (greger-register-server-tool \\='web_search\\='
    :type \\='web_search_20250305\\='
    :max_uses 5
    :allowed_domains \\='[\\='example.com\\=' \\='trusteddomain.org\\=']
    :user_location \\='((type . \\='approximate\\=')
                     (city . \\='San Francisco\\=')
                     (region . \\='California\\=')
                     (country . \\='US\\=')
                     (timezone . \\='America/Los_Angeles\\=')))

The raw JSON string will be displayed for the server tool definition."
  (let ((type (plist-get args :type))
        (remaining-args (copy-sequence args)))
    ;; Remove :type from remaining-args
    (setq remaining-args (cl-copy-list remaining-args))
    (cl-remf remaining-args :type)

    ;; Build the tool definition alist - extract symbol name from quoted form
    (let ((tool-def (list (cons 'type type)
                          (cons 'name (if (and (listp name) (eq (car name) 'quote))
                                         (symbol-name (cadr name))
                                       (if (symbolp name)
                                           (symbol-name name)
                                         name))))))
      ;; Add remaining parameters
      (while remaining-args
        (let ((key (pop remaining-args))
              (value (pop remaining-args)))
          (when key
            (push (cons (intern (substring (symbol-name key) 1)) value) tool-def))))

      `(puthash ,name (nreverse ',tool-def) greger-server-tools-registry))))

(defun greger-tools-get-schemas (tool-names)
  "Get tool schemas for TOOL-NAMES."
  (mapcar (lambda (tool-name)
            (let ((tool-def (gethash tool-name greger-tools-registry)))
              (if tool-def
                  (plist-get tool-def :schema)
                (error "Unknown tool: %s" tool-name))))
          tool-names))

(defun greger-server-tools-get-schemas (tool-names)
  "Get server tool schemas for TOOL-NAMES as JSON strings."
  (mapcar (lambda (tool-name)
            ;; Convert string to symbol if needed for lookup (server tools are stored with symbol keys)
            (let* ((lookup-key (if (stringp tool-name) (intern tool-name) tool-name))
                   (tool-def (gethash lookup-key greger-server-tools-registry)))
              (if tool-def
                  (json-encode tool-def)
                (error "Unknown server tool: %s (looked up as %s)" tool-name lookup-key))))
          tool-names))

(defun greger-server-tools-get-all-schemas ()
  "Get all registered server tool schemas as JSON strings."
  (let ((tools '()))
    (maphash (lambda (name def)
               (push (json-encode def) tools))
             greger-server-tools-registry)
    tools))

(cl-defun greger-tools-execute (&key tool-name args callback buffer metadata)
  "Execute TOOL-NAME with ARGS and call CALLBACK with (result error).
Returns a greger-tool struct for tracking execution and cancellation.
If the tool has :pass-buffer set, BUFFER will be passed to the tool function.
If the tool has :pass-callback set, CALLBACK will be passed to the tool
function instead of `greger-tools-execute' calling the callback with result.
If the tool has :pass-metadata set, METADATA will be passed to the tool
function."

  (let ((tool-def (gethash tool-name greger-tools-registry))
        (cancel-fn nil))
    (if tool-def
        (let ((func (plist-get tool-def :function))
              (pass-buffer (plist-get tool-def :pass-buffer))
              (pass-callback (plist-get tool-def :pass-callback))
              (pass-metadata (plist-get tool-def :pass-metadata)))
          ;; Add buffer parameter if pass-buffer is set and buffer is provided
          (when (and pass-buffer buffer)
            (setq args (cons (cons 'buffer buffer) args)))
          ;; Add callback parameter if pass-callback is set
          (when pass-callback
            (setq args (cons (cons 'callback callback) args)))
          ;; Add metadata parameter if pass-metadata is set and metadata is provided
          (when (and pass-metadata metadata)
            (setq args (cons (cons 'metadata metadata) args)))
          (condition-case err
              (if pass-callback
                  ;; When pass-callback is set, the function handles calling the callback
                  (let ((result (greger-tools--call-function-with-args func args tool-def)))
                    (when (functionp result)
                      (setq cancel-fn result)))
                ;; Normal case: call callback with result
                (let ((result (greger-tools--call-function-with-args func args tool-def)))
                  (when (functionp result)
                    (setq cancel-fn result))
                  (funcall callback result nil)))
            (error
             (funcall callback nil err))))
      (funcall callback nil (format "Unknown tool: %s" tool-name)))
    ;; Return greger-tool struct
    (make-greger-tool :cancel-fn cancel-fn)))

(defun greger-tools--call-function-with-args (func args tool-def)
  "Call FUNC with arguments extracted from ARGS alist based on function signature.
Returns the result of calling the function.
TOOL-DEF provides the tool definition for accessing defaults."
  (let ((func-args (greger-tools--extract-function-args func args tool-def)))
    (apply func func-args)))

(defun greger-tools--extract-function-args (func args tool-def)
  "Extract arguments for FUNC from ARGS alist based on function signature.
TOOL-DEF provides the tool definition for accessing defaults and required
parameters.  Returns a list of arguments in the correct order for the function."

  (let ((arg-list (help-function-arglist func))
        (result '())
        (required-params (when tool-def
                          (let* ((schema (plist-get tool-def :schema))
                                 (input-schema (alist-get 'input_schema schema)))
                            (alist-get 'required input-schema)))))
    (dolist (arg-name arg-list)
      (cond
       ;; Handle &optional marker
       ((eq arg-name '&optional)
        t)
       ;; Handle &rest marker (stop processing)
       ((eq arg-name '&rest)
        (cl-return))
       ;; Handle regular arguments
       (t
        (let* ((arg-symbol (if (symbolp arg-name) arg-name (intern (symbol-name arg-name))))
               (arg-key (intern (symbol-name arg-symbol)))
               (arg-provided-p (assoc arg-key args))
               (is-required (member (symbol-name arg-key) required-params)))

          (cond
           ;; Required parameter not provided
           ((and is-required (not arg-provided-p))
            (error "Required parameter missing: %s" arg-key))

           ;; Parameter provided (required or optional)
           (arg-provided-p
            (let ((raw-value (alist-get arg-key args)))
              ;; Check if this parameter should be parsed as JSON array
              (push (greger-tools--maybe-parse-json-value raw-value arg-key tool-def) result)))

           ;; Optional parameter not provided - use default or nil
           ((not is-required)
            (let ((default-value (greger-tools--get-default-from-schema arg-key tool-def)))
              (push default-value result)))

           ;; Optional parameter not provided and no more optional args
           (t
            (cl-return)))))))
    (nreverse result)))

(defun greger-tools--arg-provided-p (arg-name args)
  "Check if ARG-NAME was provided in ARGS."
  (let* ((arg-symbol (if (symbolp arg-name) arg-name (intern (symbol-name arg-name))))
         (arg-key (intern (symbol-name arg-symbol))))
    (assoc arg-key args)))

(defun greger-tools--get-arg-value (arg-name args tool-def)
  "Get value for ARG-NAME from ARGS alist, handling defaults from TOOL-DEF schema."
  (let* ((arg-symbol (if (symbolp arg-name) arg-name (intern (symbol-name arg-name))))
         (arg-key (intern (symbol-name arg-symbol)))
         (value (alist-get arg-key args)))
    ;; If value is provided, use it
    (if value
        value
      ;; Otherwise, check for default in tool schema, then hardcoded defaults
      (or (greger-tools--get-default-from-schema arg-key tool-def)
          ;; Keep existing hardcoded defaults for backward compatibility
          (cond
           ((eq arg-key 'path) ".")
           ((eq arg-key 'context-lines) 0)
           ((eq arg-key 'max-results) 50)
           (t nil))))))

(defun greger-tools--get-default-from-schema (arg-key tool-def)
  "Get default value for ARG-KEY from TOOL-DEF schema."
  (when tool-def
    (let* ((schema (plist-get tool-def :schema))
           (input-schema (alist-get 'input_schema schema))
           (properties (alist-get 'properties input-schema))
           (arg-property (alist-get arg-key properties))
           (default-value (alist-get 'default arg-property)))
      default-value)))

(defun greger-tools--maybe-parse-json-value (value arg-key tool-def)
  "Parse VALUE as JSON if ARG-KEY requires JSON parsing based on TOOL-DEF schema.
Handles arrays, booleans, and numbers."
  (let* ((schema (plist-get tool-def :schema))
         (input-schema (alist-get 'input_schema schema))
         (properties (alist-get 'properties input-schema))
         (arg-property (alist-get arg-key properties))
         (param-type (alist-get 'type arg-property)))
    (cond
     ;; Parse JSON array string
     ((and (stringp value) (string= param-type "array"))
      (condition-case nil
          (json-parse-string value :array-type 'list)
        (error value))) ; Return original value if parsing fails

     ;; Parse boolean strings
     ((and (stringp value) (string= param-type "boolean"))
      (cond
       ((string= value "true") t)
       ((string= value "false") nil)
       ((string= value ":json-true") t)
       ((string= value ":json-false") nil)
       (t nil))) ; Return nil if not a recognized boolean

     ((and (symbolp value) (string= param-type "boolean"))
      (cond
       ((eq value :json-true) t)
       ((eq value :json-false) nil)
       ((eq value t) t)        ; Handle regular Emacs Lisp t
       ((eq value nil) nil)    ; Handle regular Emacs Lisp nil
       (t nil))) ; Return nil if not a recognized symbol

     ;; Parse number strings
     ((or (string= param-type "integer") (string= param-type "number"))
      (condition-case nil
          (if (string-match-p "^-?[0-9]+$" value)
              (string-to-number value)
            (if (string-match-p "^-?[0-9]*\\.[0-9]+$" value)
                (string-to-number value)
              value)) ; Return original if not a number
        (error value)))

     ;; For other types (string, object), return as-is
     (t value))))

(provide 'greger-tools)

;;; greger-tools.el ends here
