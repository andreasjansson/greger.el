;;; greger-tools.el --- Tool registry for greger agent -*- lexical-binding: t -*-

;;; Commentary:
;; Defines tools registration functions

;;; Code:

(require 'json)
(require 'magit)
(require 'rg)
(require 'cl-lib)

;; Registry to hold tool definitions
(defvar greger-tools-registry (make-hash-table :test 'equal)
  "Hash table mapping tool names to their definitions.")

(defmacro greger-register-tool (name &rest args)
  "Register a tool with NAME and properties specified in ARGS.
ARGS should be a plist containing :description, :properties, :required, :function, and optionally :pass-buffer and :pass-callback.

Example:
  (greger-register-tool \"rename-file\"
    :description \"Rename or move a file from one path to another\"
    :properties '((old_path . ((type . \"string\")
                              (description . \"Current path of the file\")))
                  (new_path . ((type . \"string\")
                              (description . \"New path for the file\")))
                  (git_commit_message . ((type . \"string\")
                                        (description . \"Git commit message for this change\"))))
    :required '(\"old_path\" \"new_path\" \"git_commit_message\")
    :function 'greger-tools--rename-file
    :pass-buffer t)

  When :pass-callback is set to a symbol, the callback function will be passed to the tool
  function instead of greger-tools-execute calling the callback with the result."
  (let ((description (plist-get args :description))
        (properties (plist-get args :properties))
        (required (plist-get args :required))
        (function (plist-get args :function))
        (pass-buffer (plist-get args :pass-buffer))
        (pass-callback (plist-get args :pass-callback)))
    `(puthash ,name
              (list :schema (list (cons 'name ,name)
                                  (cons 'description ,description)
                                  (cons 'input_schema
                                        (list (cons 'type "object")
                                              (cons 'properties ,properties)
                                              (cons 'required ,required))))
                    :function ,function
                    :pass-buffer ,pass-buffer
                    :pass-callback ,pass-callback)
              greger-tools-registry)))

(defun greger-tools-get-schemas (tool-names)
  "Get tool schemas for TOOL-NAMES."
  (mapcar (lambda (tool-name)
            (let ((tool-def (gethash tool-name greger-tools-registry)))
              (if tool-def
                  (plist-get tool-def :schema)
                (error "Unknown tool: %s" tool-name))))
          tool-names))

(defun greger-tools-execute (tool-name args callback buffer)
  "Execute TOOL-NAME with ARGS and call CALLBACK with (result error).
If the tool has :pass-buffer set, BUFFER will be passed to the tool function."
  (let ((tool-def (gethash tool-name greger-tools-registry)))
    (if tool-def
        (let ((func (plist-get tool-def :function))
              (pass-buffer (plist-get tool-def :pass-buffer)))
          ;; Add buffer parameter if pass-buffer is set and buffer is provided
          (when (and pass-buffer buffer)
            (setq args (cons (cons 'buffer buffer) args)))
          (condition-case err
              (let ((result (greger-tools--call-function-with-args func args tool-def)))
                (funcall callback result nil))
            (error
             (funcall callback nil err))))
      (funcall callback nil (format "Unknown tool: %s" tool-name)))))

(defun greger-tools--call-function-with-args (func args tool-def)
  "Call FUNC with arguments extracted from ARGS alist based on function signature.
TOOL-DEF provides the tool definition for accessing defaults."
  (let ((func-args (greger-tools--extract-function-args func args tool-def)))
    (apply func func-args)))

(defun greger-tools--extract-function-args (func args tool-def)
  "Extract arguments for FUNC from ARGS alist based on function signature.
TOOL-DEF provides the tool definition for accessing defaults and required parameters.
Returns a list of arguments in the correct order for the function."
  (let ((arg-list (help-function-arglist func))
        (result '())
        (optional-started nil)
        (required-params (when tool-def
                          (let* ((schema (plist-get tool-def :schema))
                                 (input-schema (alist-get 'input_schema schema)))
                            (alist-get 'required input-schema)))))
    (dolist (arg-name arg-list)
      (cond
       ;; Handle &optional marker
       ((eq arg-name '&optional)
        (setq optional-started t))
       ;; Handle &rest marker (stop processing)
       ((eq arg-name '&rest)
        (cl-return))
       ;; Handle regular arguments
       (t
        (let* ((arg-symbol (if (symbolp arg-name) arg-name (intern (symbol-name arg-name))))
               (arg-key (intern (replace-regexp-in-string "-" "_" (symbol-name arg-symbol))))
               (arg-provided-p (assoc arg-key args))
               (is-required (member (symbol-name arg-key) required-params)))

          (cond
           ;; Required parameter not provided
           ((and is-required (not arg-provided-p))
            (error "Required parameter missing: %s" arg-key))

           ;; Parameter provided (required or optional)
           (arg-provided-p
            (push (alist-get arg-key args) result))

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
         (arg-key (intern (replace-regexp-in-string "-" "_" (symbol-name arg-symbol)))))
    (assoc arg-key args)))

(defun greger-tools--get-arg-value (arg-name args tool-def)
  "Get value for ARG-NAME from ARGS alist, handling defaults from TOOL-DEF schema."
  (let* ((arg-symbol (if (symbolp arg-name) arg-name (intern (symbol-name arg-name))))
         (arg-key (intern (replace-regexp-in-string "-" "_" (symbol-name arg-symbol))))
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

(provide 'greger-tools)

;;; greger-tools.el ends here
