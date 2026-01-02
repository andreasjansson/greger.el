;;; greger-plugin-lspcmd-test.el --- Tests for greger-plugin-lspcmd -*- lexical-binding: t -*-

(require 'ert)
(require 'greger-plugin-lspcmd)

(ert-deftest greger-plugin-lspcmd-test-plugin-registered ()
  "Test that the lspcmd plugin is registered."
  (should (greger-plugin-exists-p "lspcmd")))

(ert-deftest greger-plugin-lspcmd-test-all-tools-registered ()
  "Test that all lspcmd tools are registered."
  (let ((expected-tools '("lspcmd-grep"
                          "lspcmd-files"
                          "lspcmd-show"
                          "lspcmd-refs"
                          "lspcmd-calls"
                          "lspcmd-implementations"
                          "lspcmd-supertypes"
                          "lspcmd-subtypes"
                          "lspcmd-declaration"
                          "lspcmd-rename"
                          "lspcmd-mv"
                          "lspcmd-workspace-add")))
    (dolist (tool expected-tools)
      (should (gethash tool greger-tools-registry)))
    (should (equal expected-tools (greger-plugin-tools "lspcmd")))))

(ert-deftest greger-plugin-lspcmd-test-grep-tool-schema ()
  "Test that lspcmd-grep has correct schema."
  (let* ((schemas (greger-tools-get-schemas '("lspcmd-grep")))
         (schema (car schemas))
         (input-schema (alist-get 'input_schema schema))
         (properties (alist-get 'properties input-schema))
         (required (alist-get 'required input-schema)))
    (should (equal "lspcmd-grep" (alist-get 'name schema)))
    (should (string-match-p "Search for symbols" (alist-get 'description schema)))
    (should (assq 'pattern properties))
    (should (assq 'path properties))
    (should (assq 'kind properties))
    (should (assq 'exclude properties))
    (should (assq 'docs properties))
    (should (assq 'case-sensitive properties))
    (should (assq 'root properties))
    (should (equal '("pattern") required))))

(ert-deftest greger-plugin-lspcmd-test-show-tool-schema ()
  "Test that lspcmd-show has correct schema."
  (let* ((schemas (greger-tools-get-schemas '("lspcmd-show")))
         (schema (car schemas))
         (input-schema (alist-get 'input_schema schema))
         (properties (alist-get 'properties input-schema))
         (required (alist-get 'required input-schema)))
    (should (equal "lspcmd-show" (alist-get 'name schema)))
    (should (string-match-p "full definition" (alist-get 'description schema)))
    (should (assq 'symbol properties))
    (should (assq 'context properties))
    (should (assq 'head properties))
    (should (assq 'root properties))
    (should (equal '("symbol") required))))

(ert-deftest greger-plugin-lspcmd-test-calls-tool-schema ()
  "Test that lspcmd-calls has correct schema with no required params."
  (let* ((schemas (greger-tools-get-schemas '("lspcmd-calls")))
         (schema (car schemas))
         (input-schema (alist-get 'input_schema schema))
         (properties (alist-get 'properties input-schema))
         (required (alist-get 'required input-schema)))
    (should (equal "lspcmd-calls" (alist-get 'name schema)))
    (should (assq 'from properties))
    (should (assq 'to properties))
    (should (assq 'max-depth properties))
    (should (assq 'include-non-workspace properties))
    ;; No required params - at least one of from/to must be provided at runtime
    (should (equal '() required))))

(ert-deftest greger-plugin-lspcmd-test-rename-tool-schema ()
  "Test that lspcmd-rename has correct schema."
  (let* ((schemas (greger-tools-get-schemas '("lspcmd-rename")))
         (schema (car schemas))
         (input-schema (alist-get 'input_schema schema))
         (required (alist-get 'required input-schema)))
    (should (equal "lspcmd-rename" (alist-get 'name schema)))
    (should (member "symbol" required))
    (should (member "new-name" required))))

(ert-deftest greger-plugin-lspcmd-test-mv-tool-schema ()
  "Test that lspcmd-mv has correct schema."
  (let* ((schemas (greger-tools-get-schemas '("lspcmd-mv")))
         (schema (car schemas))
         (input-schema (alist-get 'input_schema schema))
         (required (alist-get 'required input-schema)))
    (should (equal "lspcmd-mv" (alist-get 'name schema)))
    (should (member "old-path" required))
    (should (member "new-path" required))))

;; Mock tests - test the functions with a mock lspcmd

(defvar greger-plugin-lspcmd-test--mock-responses nil
  "Alist of (args . response) for mocking lspcmd.")

(defvar greger-plugin-lspcmd-test--mock-calls nil
  "List of args passed to mock lspcmd.")

(defun greger-plugin-lspcmd-test--mock-run (&rest args)
  "Mock implementation of greger-lspcmd--run."
  (let ((flat-args (flatten-list (seq-remove #'null args))))
    (push flat-args greger-plugin-lspcmd-test--mock-calls)
    (or (cdr (assoc flat-args greger-plugin-lspcmd-test--mock-responses))
        (format "mock response for: %S" flat-args))))

(defmacro greger-plugin-lspcmd-test--with-mock (&rest body)
  "Execute BODY with mocked lspcmd."
  `(let ((greger-plugin-lspcmd-test--mock-calls nil))
     (cl-letf (((symbol-function 'greger-lspcmd--run) #'greger-plugin-lspcmd-test--mock-run))
       ,@body)))

(ert-deftest greger-plugin-lspcmd-test-grep-builds-correct-args ()
  "Test that lspcmd-grep builds correct command arguments."
  (greger-plugin-lspcmd-test--with-mock
   ;; Basic call
   (greger-lspcmd--grep "test-pattern")
   (should (equal '(("grep" "test-pattern"))
                  greger-plugin-lspcmd-test--mock-calls))

   (setq greger-plugin-lspcmd-test--mock-calls nil)

   ;; With all options
   (greger-lspcmd--grep "pattern" "src/" "function" '("test" "vendor") t t "/root")
   (let ((call (car greger-plugin-lspcmd-test--mock-calls)))
     (should (member "grep" call))
     (should (member "pattern" call))
     (should (member "src/" call))
     (should (member "-k" call))
     (should (member "function" call))
     (should (member "--docs" call))
     (should (member "--case-sensitive" call))
     (should (member "--root" call))
     (should (member "/root" call))
     (should (member "-x" call))
     (should (member "test" call))
     (should (member "vendor" call)))))

(ert-deftest greger-plugin-lspcmd-test-files-builds-correct-args ()
  "Test that lspcmd-files builds correct command arguments."
  (greger-plugin-lspcmd-test--with-mock
   ;; Basic call
   (greger-lspcmd--files)
   (should (equal '(("files"))
                  greger-plugin-lspcmd-test--mock-calls))

   (setq greger-plugin-lspcmd-test--mock-calls nil)

   ;; With options
   (greger-lspcmd--files "src/" '("node_modules") '(".git") "/root")
   (let ((call (car greger-plugin-lspcmd-test--mock-calls)))
     (should (member "files" call))
     (should (member "src/" call))
     (should (member "-x" call))
     (should (member "node_modules" call))
     (should (member "-i" call))
     (should (member ".git" call))
     (should (member "--root" call)))))

(ert-deftest greger-plugin-lspcmd-test-show-builds-correct-args ()
  "Test that lspcmd-show builds correct command arguments."
  (greger-plugin-lspcmd-test--with-mock
   ;; Basic call
   (greger-lspcmd--show "MyClass.method")
   (should (equal '(("show" "MyClass.method"))
                  greger-plugin-lspcmd-test--mock-calls))

   (setq greger-plugin-lspcmd-test--mock-calls nil)

   ;; With options
   (greger-lspcmd--show "MyClass" 5 100 "/root")
   (let ((call (car greger-plugin-lspcmd-test--mock-calls)))
     (should (member "show" call))
     (should (member "MyClass" call))
     (should (member "-n" call))
     (should (member "5" call))
     (should (member "--head" call))
     (should (member "100" call)))))

(ert-deftest greger-plugin-lspcmd-test-calls-requires-from-or-to ()
  "Test that lspcmd-calls requires at least from or to."
  (should-error (greger-lspcmd--calls) :type 'error))

(ert-deftest greger-plugin-lspcmd-test-calls-builds-correct-args ()
  "Test that lspcmd-calls builds correct command arguments."
  (greger-plugin-lspcmd-test--with-mock
   ;; With --from
   (greger-lspcmd--calls "main" nil nil nil nil)
   (let ((call (car greger-plugin-lspcmd-test--mock-calls)))
     (should (member "calls" call))
     (should (member "--from" call))
     (should (member "main" call)))

   (setq greger-plugin-lspcmd-test--mock-calls nil)

   ;; With --to
   (greger-lspcmd--calls nil "helper" nil nil nil)
   (let ((call (car greger-plugin-lspcmd-test--mock-calls)))
     (should (member "--to" call))
     (should (member "helper" call)))

   (setq greger-plugin-lspcmd-test--mock-calls nil)

   ;; With all options
   (greger-lspcmd--calls "main" "helper" 5 t "/root")
   (let ((call (car greger-plugin-lspcmd-test--mock-calls)))
     (should (member "--from" call))
     (should (member "--to" call))
     (should (member "--max-depth" call))
     (should (member "5" call))
     (should (member "--include-non-workspace" call)))))

(ert-deftest greger-plugin-lspcmd-test-rename-builds-correct-args ()
  "Test that lspcmd-rename builds correct command arguments."
  (greger-plugin-lspcmd-test--with-mock
   (greger-lspcmd--rename "OldName" "NewName" "/root")
   (let ((call (car greger-plugin-lspcmd-test--mock-calls)))
     (should (member "rename" call))
     (should (member "OldName" call))
     (should (member "NewName" call))
     (should (member "--root" call)))))

(ert-deftest greger-plugin-lspcmd-test-mv-builds-correct-args ()
  "Test that lspcmd-mv builds correct command arguments."
  (greger-plugin-lspcmd-test--with-mock
   (greger-lspcmd--mv "old/path.py" "new/path.py" "/root")
   (let ((call (car greger-plugin-lspcmd-test--mock-calls)))
     (should (member "mv" call))
     (should (member "old/path.py" call))
     (should (member "new/path.py" call)))))

(ert-deftest greger-plugin-lspcmd-test-workspace-add-builds-correct-args ()
  "Test that lspcmd-workspace-add builds correct command arguments."
  (greger-plugin-lspcmd-test--with-mock
   (greger-lspcmd--workspace-add "/my/project")
   (let ((call (car greger-plugin-lspcmd-test--mock-calls)))
     (should (member "workspace" call))
     (should (member "add" call))
     (should (member "--root" call))
     (should (member "/my/project" call)))))

(provide 'greger-plugin-lspcmd-test)

;;; greger-plugin-lspcmd-test.el ends here
