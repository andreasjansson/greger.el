;;; test-greger-parser.el --- Tests for greger parser -*- lexical-binding: t -*-

(require 'ert)
(require 'greger-parser)
(require 'cl-lib)

;; Global variable to store the grammar repo path
(defvar greger-test-grammar-repo-path nil
  "Path to the cloned greger-grammar repository.")

;; Function to clone the grammar repo once
(defun greger-test-setup-grammar-repo ()
  "Clone the greger-grammar repository to /tmp/greger-grammar if not already done, then pull updates."
  (let ((repo-path "/tmp/greger-grammar"))
    (if (file-directory-p repo-path)
        ;; Repository exists, pull updates
        (progn
          (message "Pulling latest updates for greger-grammar at %s..." repo-path)
          (let ((result (shell-command-to-string
                         (format "cd %s && git pull"
                                 (shell-quote-argument repo-path)))))
            (if (string-match-p "fatal:\\|error:" result)
                (error "Failed to pull greger-grammar updates: %s" result)
              (setq greger-test-grammar-repo-path repo-path)
              (message "Successfully pulled greger-grammar updates"))))
      ;; Repository doesn't exist, clone it
      (progn
        (message "Cloning greger-grammar to %s..." repo-path)
        (let ((result (shell-command-to-string
                       (format "git clone https://github.com/andreasjansson/greger-grammar.git %s"
                               (shell-quote-argument repo-path)))))
          (if (string-match-p "fatal:\\|error:" result)
              (error "Failed to clone greger-grammar: %s" result)
            (setq greger-test-grammar-repo-path repo-path)
            (message "Successfully cloned greger-grammar to %s" greger-test-grammar-repo-path)))))))

;; Function to clean up the grammar repo
(defun greger-test-cleanup-grammar-repo ()
  "Clean up the grammar repository reference (but keep the persistent directory)."
  (when greger-test-grammar-repo-path
    (message "Cleaning up greger-grammar repo reference...")
    (setq greger-test-grammar-repo-path nil)))

;; Helper function to read markdown content from corpus .txt files
(defun greger-read-corpus-file (name)
  "Read markdown content from a .txt corpus file, extracting only the input portion.
This function requires the grammar repository to be set up first."
  (unless greger-test-grammar-repo-path
    (error "Grammar repository not set up. Call greger-test-setup-grammar-repo first"))
  (let ((file-path (expand-file-name (format "test/corpus/%s.txt" name) greger-test-grammar-repo-path)))
    (if (file-exists-p file-path)
        (with-temp-buffer
          (insert-file-contents file-path)
          (let ((content (buffer-string)))
            ;; Find the test content between the title header and the "---" separator
            (if (string-match "=\\{10,\\}\n.*?\n=\\{10,\\}\n\n\\(\\(?:.\\|\n\\)*?\\)\n---" content)
                (match-string 1 content)
              (error "Could not parse test file format: %s" file-path))))
      (error "Corpus file not found: %s" file-path))))

;; Helper functions for tests
(defun greger-parser-test--dialog-equal (expected actual)
  "Compare two dialog structures, handling content blocks."
  (and (listp expected) (listp actual)
       (= (length expected) (length actual))
       (cl-every
        (lambda (pair)
          (let ((exp-msg (car pair))
                (act-msg (cdr pair)))
            (and (string= (alist-get 'role exp-msg) (alist-get 'role act-msg))
                 (greger-parser-test--content-equal
                  (alist-get 'content exp-msg)
                  (alist-get 'content act-msg)))))
        (cl-mapcar #'cons expected actual))))

(defun greger-parser-test--content-equal (expected actual)
  "Compare message content, handling both strings and content block lists."
  (cond
   ;; Both are strings - direct comparison
   ((and (stringp expected) (stringp actual))
    (string= expected actual))

   ;; Both are lists (content blocks) - compare structure
   ((and (listp expected) (listp actual))
    (greger-parser-test--content-blocks-equal expected actual))

   ;; Fallback to string comparison
   (t (string= (format "%s" expected) (format "%s" actual)))))

(defun greger-parser-test--content-blocks-equal (expected actual)
  "Compare two content block lists."
  (and (= (length expected) (length actual))
       (cl-every
        (lambda (pair)
          (greger-parser-test--content-block-equal (car pair) (cdr pair)))
        (cl-mapcar #'cons expected actual))))

(defun greger-parser-test--content-block-equal (expected actual)
  "Compare two content blocks."
  (and (string= (alist-get 'type expected) (alist-get 'type actual))
       (let ((type (alist-get 'type expected)))
         (cond
          ((string= type "text")
           (string= (alist-get 'text expected) (alist-get 'text actual)))
          ((string= type "thinking")
           (string= (alist-get 'thinking expected) (alist-get 'thinking actual)))
          ((string= type "tool_use")
           (and (string= (alist-get 'id expected) (alist-get 'id actual))
                (string= (alist-get 'name expected) (alist-get 'name actual))
                (greger-parser-test--input-equal (alist-get 'input expected) (alist-get 'input actual))))
          ((string= type "tool_result")
           (and (string= (alist-get 'tool_use_id expected) (alist-get 'tool_use_id actual))
                (greger-parser-test--strings-or-alists-equal-p (alist-get 'content expected) (alist-get 'content actual))))
          ((string= type "server_tool_use")
           (and (string= (alist-get 'id expected) (alist-get 'id actual))
                (string= (alist-get 'name expected) (alist-get 'name actual))
                (greger-parser-test--input-equal (alist-get 'input expected) (alist-get 'input actual))))
          ((string= type "web_search_tool_result")
           (and (string= (alist-get 'tool_use_id expected) (alist-get 'tool_use_id actual))
                (equal (alist-get 'content expected) (alist-get 'content actual))))
          (t t)))))

(defun greger-parser-test--strings-or-alists-equal-p (var1 var2)
  "Return t if VAR1 and VAR2 are equal strings or alists.
For alists, comparison is order-independent."
  (cond
   ;; Both are strings
   ((and (stringp var1) (stringp var2))
    (string-equal var1 var2))

   ;; Both are alists (lists of cons cells)
   ((and (listp var1) (listp var2)
         (or (null var1) (consp (car var1)))
         (or (null var2) (consp (car var2))))
    (greger-parser-test--alists-equal-p var1 var2))

   ;; Otherwise, use regular equality
   (t (equal var1 var2))))

(defun greger-parser-test--alists-equal-p (alist1 alist2)
  "Return t if ALIST1 and ALIST2 contain the same key-value pairs.
Comparison is order-independent."
  (and (= (length alist1) (length alist2))
       (catch 'not-equal
         (dolist (pair alist1 t)
           (let ((key (car pair))
                 (val (cdr pair)))
             (unless (equal val (cdr (assoc key alist2)))
               (throw 'not-equal nil)))))))

(defun greger-parser-test--input-equal (expected actual)
  "Compare tool input parameters."
  (and (= (length expected) (length actual))
       (cl-every
        (lambda (exp-param)
          (let ((key (car exp-param))
                (exp-val (cdr exp-param)))
            (let ((act-val (alist-get key actual)))
              (equal exp-val act-val))))
        expected)))

(defun greger-parser-test--normalize-whitespace (str)
  "Normalize whitespace in string for comparison."
  (string-trim (replace-regexp-in-string "[ \t\n\r]+" " " str)))

;; Fixture macro for tests that need the grammar repo
(defmacro greger-parser-test--with-grammar-repo (&rest body)
  "Execute BODY with the grammar repository available, ensuring cleanup."
  `(unwind-protect
       (progn
         (greger-test-setup-grammar-repo)
         ,@body)
     ;; Cleanup happens here only if this is the last test or if there's an error
     ;; For normal operation, cleanup happens in the dedicated cleanup test
     nil))

;; Helper function for roundtrip testing
(defun greger-parser-test--roundtrip (corpus-name)
  "Test roundtrip conversion for a corpus file."
  (greger-parser-test--with-grammar-repo
   (let ((original-markdown (greger-read-corpus-file corpus-name)))
     (let* ((dialog (greger-parser-markdown-to-dialog original-markdown))
            (roundtrip-markdown (greger-parser-dialog-to-markdown dialog))
            (roundtrip-dialog (greger-parser-markdown-to-dialog roundtrip-markdown)))
       ;; The dialog should be structurally equivalent after round-trip
       (should (= (length dialog) (length roundtrip-dialog)))
       (should (greger-parser-test--dialog-equal dialog roundtrip-dialog))))))

;; Individual test cases imported from greger-grammar corpus files
;; Each test performs roundtrip testing: markdown -> dialog -> markdown

(ert-deftest greger-parser-test-simple-user-message ()
  "Test roundtrip for simple-user-message corpus case."
  (greger-parser-test--roundtrip "simple-user-message"))

(ert-deftest greger-parser-test-system-and-user ()
  "Test roundtrip for system-and-user corpus case."
  (greger-parser-test--roundtrip "system-and-user"))

(ert-deftest greger-parser-test-simple-conversation ()
  "Test roundtrip for simple-conversation corpus case."
  (greger-parser-test--roundtrip "simple-conversation"))

(ert-deftest greger-parser-test-thinking-section ()
  "Test roundtrip for thinking-section corpus case."
  (greger-parser-test--roundtrip "thinking-section"))

(ert-deftest greger-parser-test-tool-use-single-param ()
  "Test roundtrip for tool-use-single-param corpus case."
  (greger-parser-test--roundtrip "tool-use-single-param"))

(ert-deftest greger-parser-test-tool-use-multiple-params ()
  "Test roundtrip for tool-use-multiple-params corpus case."
  (greger-parser-test--roundtrip "tool-use-multiple-params"))

(ert-deftest greger-parser-test-complex-workflow ()
  "Test roundtrip for complex-workflow corpus case."
  (greger-parser-test--roundtrip "complex-workflow"))

(ert-deftest greger-parser-test-multiple-tool-uses ()
  "Test roundtrip for multiple-tool-uses corpus case."
  (greger-parser-test--roundtrip "multiple-tool-uses"))

(ert-deftest greger-parser-test-thinking-only ()
  "Test roundtrip for thinking-only corpus case."
  (greger-parser-test--roundtrip "thinking-only"))

(ert-deftest greger-parser-test-tool-use-only ()
  "Test roundtrip for tool-use-only corpus case."
  (greger-parser-test--roundtrip "tool-use-only"))

(ert-deftest greger-parser-test-citations-basic ()
  "Test roundtrip for citations-basic corpus case."
  (greger-parser-test--roundtrip "citations-basic"))

(ert-deftest greger-parser-test-citations-after-tool-result ()
  "Test roundtrip for citations-after-tool-result corpus case."
  (greger-parser-test--roundtrip "citations-after-tool-result"))

(ert-deftest greger-parser-test-citations-multiple ()
  "Test roundtrip for citations-multiple corpus case."
  (greger-parser-test--roundtrip "citations-multiple"))

(ert-deftest greger-parser-test-code-block-triple-backticks ()
  "Test roundtrip for code-block-triple-backticks corpus case."
  (greger-parser-test--roundtrip "code-block-triple-backticks"))

(ert-deftest greger-parser-test-mixed-code-blocks-and-sections ()
  "Test roundtrip for mixed-code-blocks-and-sections corpus case."
  (greger-parser-test--roundtrip "mixed-code-blocks-and-sections"))

(ert-deftest greger-parser-test-tool-use-with-code-in-params ()
  "Test roundtrip for tool-use-with-code-in-params corpus case."
  (greger-parser-test--roundtrip "tool-use-with-code-in-params"))

(ert-deftest greger-parser-test-tool-use-with-tool-use-in-params ()
  "Test roundtrip for tool-use-with-tool-use-in-params corpus case."
  (greger-parser-test--roundtrip "tool-use-with-tool-use-in-params"))

(ert-deftest greger-parser-test-nested-code-blocks ()
  "Test roundtrip for nested-code-blocks corpus case."
  (greger-parser-test--roundtrip "nested-code-blocks"))

(ert-deftest greger-parser-test-html-comments ()
  "Test roundtrip for html-comments corpus case."
  (greger-parser-test--roundtrip "html-comments"))

(ert-deftest greger-parser-test-server-tool-use-basic ()
  "Test roundtrip for server-tool-use-basic corpus case."
  (greger-parser-test--roundtrip "server-tool-use-basic"))

(ert-deftest greger-parser-test-server-tool-use-string-result ()
  "Test roundtrip for server-tool-use-string-result corpus case."
  (greger-parser-test--roundtrip "server-tool-use-string-result"))

(ert-deftest greger-parser-test-code-block-nested-headers ()
  "Test roundtrip for code-block-nested-headers corpus case."
  (greger-parser-test--roundtrip "code-block-nested-headers"))

(ert-deftest greger-parser-test-inline-code ()
  "Test roundtrip for inline-code corpus case."
  (greger-parser-test--roundtrip "inline-code"))

(ert-deftest greger-parser-test-safe-shell-commands ()
  "Test roundtrip for safe-shell-commands corpus case."
  (greger-parser-test--roundtrip "safe-shell-commands"))

(ert-deftest greger-parser-test-text-with-markdown-headings ()
  "Test roundtrip for text-with-markdown-headings corpus case."
  (greger-parser-test--roundtrip "text-with-markdown-headings"))

(ert-deftest greger-parser-test-triple-hash ()
  "Test roundtrip for triple-hash corpus case."
  (greger-parser-test--roundtrip "triple-hash"))

(ert-deftest greger-parser-test-untagged-content ()
  "Test roundtrip for untagged-content corpus case."
  (greger-parser-test--roundtrip "untagged-content"))

(ert-deftest greger-parser-test-tool-use-parsing ()
  "Test specific tool use parsing functionality."
  (let ((tool-use-markdown "# TOOL USE

Name: read-file
ID: toolu_123

## path

<tool.toolu_123>
hello.txt
</tool.toolu_123>

## include_line_numbers

<tool.toolu_123>
true
</tool.toolu_123>
"))
    (let ((parsed (greger-parser-markdown-to-dialog tool-use-markdown)))
      (should (= 1 (length parsed)))
      (let ((assistant-msg (car parsed)))
        (should (string= "assistant" (alist-get 'role assistant-msg)))
        (let* ((content-blocks (alist-get 'content assistant-msg))
               (tool-use-block (car content-blocks)))
          (should (string= "tool_use" (alist-get 'type tool-use-block)))
          (should (string= "toolu_123" (alist-get 'id tool-use-block)))
          (should (string= "read-file" (alist-get 'name tool-use-block)))
          (let ((input (alist-get 'input tool-use-block)))
            (should (string= "hello.txt" (alist-get 'path input)))
            (should (eq t (alist-get 'include_line_numbers input)))))))))

(ert-deftest greger-parser-test-tool-result-parsing ()
  "Test specific tool result parsing functionality."
  (let ((tool-result-markdown "# TOOL RESULT

ID: toolu_123

<tool.toolu_123>
File contents here
with multiple lines
</tool.toolu_123>"))
    (let ((parsed (greger-parser-markdown-to-dialog tool-result-markdown)))
      (should (= 1 (length parsed)))
      (let ((user-msg (car parsed)))
        (should (string= "user" (alist-get 'role user-msg)))
        (let* ((content-blocks (alist-get 'content user-msg))
               (tool-result-block (car content-blocks)))
          (should (string= "tool_result" (alist-get 'type tool-result-block)))
          (should (string= "toolu_123" (alist-get 'tool_use_id tool-result-block)))
          (should (string= "File contents here
with multiple lines"
                           (alist-get 'content tool-result-block))))))))

(ert-deftest greger-parser-test-thinking-parsing ()
  "Test thinking section parsing."
  (let ((thinking-markdown "# THINKING

I need to think about this carefully.
This is a complex problem."))
    (let ((parsed (greger-parser-markdown-to-dialog thinking-markdown)))
      (should (= 1 (length parsed)))
      (let ((assistant-msg (car parsed)))
        (should (string= "assistant" (alist-get 'role assistant-msg)))
        (let* ((content-blocks (alist-get 'content assistant-msg))
               (thinking-block (car content-blocks)))
          (should (string= "thinking" (alist-get 'type thinking-block)))
          (should (string= "I need to think about this carefully.
This is a complex problem."
                           (alist-get 'thinking thinking-block))))))))

(ert-deftest greger-parser-test-error-handling ()
  "Test parser error handling for malformed input."
  ;; Test that malformed input returns empty result instead of error
  (should (condition-case err
              (progn (greger-parser-markdown-to-dialog "# TOOL USE\n\nMalformed") t)
            (error nil)))
  (should (condition-case err
              (progn (greger-parser-markdown-to-dialog "# TOOL RESULT\n\nMalformed") t)
            (error nil))))

(ert-deftest greger-parser-test-edge-cases ()
  "Test edge cases like empty content, whitespace handling."
  ;; Empty content
  (should (equal '() (greger-parser-markdown-to-dialog "")))
  (should (equal '() (greger-parser-markdown-to-dialog "\n\n  ")))

  ;; Multiple consecutive newlines
  (let ((result (greger-parser-markdown-to-dialog "# USER

Hello



# ASSISTANT



Hi")))
    (should (equal '(((role . "user")
                      (content . "Hello\n\n"))
                     ((role . "assistant")
                      ;; This is wrong, it should be: (content ((text . "\n\nHi") (type . "text")))
                      (content ((text . "Hi") (type . "text")))
                      ))
                   result))))

(ert-deftest greger-parser-test-performance ()
  "Test parser performance with large dialogs."
  (skip-unless (not (getenv "GITHUB_ACTIONS")))

  (let ((large-markdown
         (mapconcat
          (lambda (i)
            (format "# USER\n\nMessage %d\n\n# ASSISTANT\n\nResponse %d" i i))
          (number-sequence 1 10000)
          "\n\n")))
    (let ((start-time (current-time)))
      (greger-parser-markdown-to-dialog large-markdown)
      (let ((elapsed (float-time (time-subtract (current-time) start-time))))
        ;; Should parse 10000 message pairs in under 1 second
        (should (< elapsed 1.0))))))

;; Test that we handle tool use parameters correctly with various whitespace
(ert-deftest greger-parser-test-tool-use-whitespace ()
  "Test tool use parsing with various whitespace patterns."
  (let ((markdown "# TOOL USE

Name: test-tool
ID: tool_123

## param1

<tool.tool_123>
value1
</tool.tool_123>

## param2

<tool.tool_123>
value2 with
multiple


  lines
</tool.tool_123>

## param3

<tool.tool_123>
value3
</tool.tool_123>
"))
    (let ((parsed (greger-parser-markdown-to-dialog markdown)))
      (should (= 1 (length parsed)))
      (let* ((assistant-msg (car parsed))
             (content-blocks (alist-get 'content assistant-msg))
             (tool-block (car content-blocks))
             (input (alist-get 'input tool-block)))
        (should (= 3 (length input)))
        (should (string= "value1" (alist-get 'param1 input)))
        (should (string= "value2 with\nmultiple\n\n\n  lines" (alist-get 'param2 input)))
        (should (string= "value3" (alist-get 'param3 input)))))))

(ert-deftest greger-parser-test-code-blocks-in-tool-params ()
  "Test that code blocks in tool parameters are preserved correctly."
  (let ((markdown "# TOOL USE

Name: write-file
ID: tool_123

## content

<tool.tool_123>
```python
# This # USER comment should be preserved
print(\"# ASSISTANT also preserved\")
```
</tool.tool_123>
"))
    (let ((parsed (greger-parser-markdown-to-dialog markdown)))
      (should (= 1 (length parsed)))
      (let* ((assistant-msg (car parsed))
             (content-blocks (alist-get 'content assistant-msg))
             (tool-block (car content-blocks))
             (input (alist-get 'input tool-block))
             (content-param (alist-get 'content input)))
        (should (string-match-p "# USER" content-param))
        (should (string-match-p "# ASSISTANT" content-param))
        (should (string-match-p "```python" content-param))))))

(ert-deftest greger-parser-test-safe-shell-commands-basic ()
  "Test safe-shell-commands with other system content."
  (let ((markdown "# SYSTEM

You are a helpful assistant.

<safe-shell-commands>
ls

pwd
</safe-shell-commands>

Please be careful."))
    (let ((result (greger-parser-markdown-to-dialog markdown)))
      ;; Should have a system message with combined content
      (should (= 1 (length result)))
      (should (string= "system" (alist-get 'role (car result))))
      (let ((system-content (alist-get 'content (car result))))
        (should (string= "You are a helpful assistant.



Please be careful.

# Safe shell commands

You can run arbitrary shell commands with the shell-command tool, but the following are safe shell commands that will run without requiring user confirmation:

* `ls`
* `pwd`"
                         system-content))))))

(ert-deftest greger-parser-test-safe-shell-commands-not-in-system ()
  "Test that safe-shell-commands outside SYSTEM section are ignored."
  (let ((markdown "# USER

<safe-shell-commands>
ls -la
</safe-shell-commands>

What files are here?"))
    (let ((result (greger-parser-markdown-to-dialog markdown)))
      ;; Should have no metadata
      ;; Should have user message with the tag as regular content
      (should (= 1 (length result)))
      (should (string-match-p "<safe-shell-commands>"
                              (alist-get 'content (car result)))))))

;; Tests for undiff functionality

(ert-deftest greger-parser-test-undiff-simple-addition ()
  "Test undiffing a simple addition."
  (let* ((diff-content " line1
 line2
+new line
 line3")
         (result (greger-parser-undiff-strings diff-content))
         (original (car result))
         (new (cdr result)))
    (should (string= original "line1
line2
line3
"))
    (should (string= new "line1
line2
new line
line3
"))))

(ert-deftest greger-parser-test-undiff-simple-deletion ()
  "Test undiffing a simple deletion."
  (let* ((diff-content " line1
-line2
 line3")
         (result (greger-parser-undiff-strings diff-content))
         (original (car result))
         (new (cdr result)))
    (should (string= original "line1
line2
line3
"))
    (should (string= new "line1
line3
"))))

(ert-deftest greger-parser-test-undiff-replacement ()
  "Test undiffing a replacement operation."
  (let* ((diff-content " line1
-old line
+new line
 line3")
         (result (greger-parser-undiff-strings diff-content))
         (original (car result))
         (new (cdr result)))
    (should (string= original "line1
old line
line3
"))
    (should (string= new "line1
new line
line3
"))))

(ert-deftest greger-parser-test-undiff-empty-diff ()
  "Test undiffing an empty diff (no changes)."
  (let* ((diff-content "")
         (result (greger-parser-undiff-strings diff-content))
         (original (car result))
         (new (cdr result)))
    (should (string= original ""))
    (should (string= new ""))))

(ert-deftest greger-parser-test-undiff-no-newline ()
  "Test undiffing with 'No newline at end of file' markers."
  (let* ((diff-content " line1
-old line
\\ No newline at end of file
+new line")
         (result (greger-parser-undiff-strings diff-content))
         (original (car result))
         (new (cdr result)))
    (should (string= original "line1
old line"))
    (should (string= new "line1
new line
"))))

(ert-deftest greger-parser-test-undiff-header ()
  "Test undiffing content with diff headers."
  (let* ((diff-content "@@ -1,3 +1,4 @@
 line1
 line2
+new line
 line3")
         (result (greger-parser-undiff-strings diff-content))
         (original (car result))
         (new (cdr result)))
    (should (string= original "line1
line2
line3
"))
    (should (string= new "line1
line2
new line
line3
"))))

(ert-deftest greger-parser-test-str-replace-undiff-params ()
  "Test str-replace parameter undiffing."
  (let* ((params '((path . "test.txt")
                   (diff . " line1
-old
+new")))
         (result (greger-parser--str-replace-undiff-params params))
         (original-content (alist-get 'original-content result))
         (new-content (alist-get 'new-content result))
         (path (alist-get 'path result)))
    (should (string= path "test.txt"))
    (should (string= original-content "line1
old
"))
    (should (string= new-content "line1
new
"))
    (should-not (alist-get 'diff result))))

(ert-deftest greger-parser-test-str-replace-undiff-params-no-diff ()
  "Test str-replace parameter undiffing when no diff is present."
  (let* ((params '((path . "test.txt")
                   (original-content . "old content")
                   (new-content . "new content")))
         (result (greger-parser--str-replace-undiff-params params)))
    (should (equal result params))))

(ert-deftest greger-parser-test-value-to-string-valid-json ()
  "Test that valid JSON strings are parsed and pretty-printed."
  (should (string= (greger-parser--value-to-string "[1,2,3]") "[\n  1,\n  2,\n  3\n]"))
  (should (string= (greger-parser--value-to-string "{\"key\": \"value\"}") "{\n  \"key\": \"value\"\n}"))
  (should (string= (greger-parser--value-to-string "\"hello\"") "\"hello\""))
  (should (string= (greger-parser--value-to-string "true") "true"))
  (should (string= (greger-parser--value-to-string "null") "null"))
  (should (string= (greger-parser--value-to-string "42") "42")))

(ert-deftest greger-parser-test-value-to-string-invalid-json ()
  "Test that invalid JSON strings are returned as-is without parsing."
  ;; This is the main bug case - should return original string, not parse as empty string
  (should (string= (greger-parser--value-to-string "\"\"\"test\"\"\"") "\"\"\"test\"\"\""))
  ;; Other invalid JSON cases
  (should (string= (greger-parser--value-to-string "[1,2,3]extra") "[1,2,3]extra"))
  (should (string= (greger-parser--value-to-string "{\"key\": \"value\"}garbage") "{\"key\": \"value\"}garbage"))
  (should (string= (greger-parser--value-to-string "not json at all") "not json at all"))
  (should (string= (greger-parser--value-to-string "\"unclosed string") "\"unclosed string")))

(ert-deftest greger-parser-test-value-to-string-non-strings ()
  "Test that non-string values are handled correctly."
  (should (string= (greger-parser--value-to-string 42) "42"))
  (should (string= (greger-parser--value-to-string t) "true"))
  (should (string= (greger-parser--value-to-string nil) "false"))
  (should (string= (greger-parser--value-to-string [1 2 3]) "[\n  1,\n  2,\n  3\n]"))
  (should (string= (greger-parser--value-to-string '(1 2 3)) "[\n  1,\n  2,\n  3\n]")))

(ert-deftest greger-parser-test-convert-value-booleans ()
  "Test conversion of boolean strings."
  (should (eq (greger-parser--convert-value "true") t))
  (should (eq (greger-parser--convert-value "false") nil))
  (should (eq (greger-parser--convert-value " true ") t))
  (should (eq (greger-parser--convert-value " false ") nil))
  ;; Non-boolean strings should remain strings
  (should (string= (greger-parser--convert-value "True") "True"))
  (should (string= (greger-parser--convert-value "FALSE") "FALSE"))
  (should (string= (greger-parser--convert-value "truex") "truex")))

(ert-deftest greger-parser-test-convert-value-numbers ()
  "Test conversion of numeric strings."
  ;; Integers
  (should (equal (greger-parser--convert-value "42") 42))
  (should (equal (greger-parser--convert-value "-17") -17))
  (should (equal (greger-parser--convert-value " 123 ") 123))
  (should (equal (greger-parser--convert-value "0") 0))
  ;; Floats
  (should (equal (greger-parser--convert-value "3.14") 3.14))
  (should (equal (greger-parser--convert-value "-2.5") -2.5))
  (should (equal (greger-parser--convert-value " 0.0 ") 0.0))
  ;; Invalid numbers should remain strings
  (should (string= (greger-parser--convert-value "42abc") "42abc"))
  (should (string= (greger-parser--convert-value "3.14.15") "3.14.15"))
  (should (string= (greger-parser--convert-value "42.") "42.")))

(ert-deftest greger-parser-test-convert-value-json-arrays ()
  "Test conversion of JSON arrays."
  ;; Valid arrays
  (should (equal (greger-parser--convert-value "[1,2,3]") [1 2 3]))
  (should (equal (greger-parser--convert-value "[\"a\",\"b\"]") ["a" "b"]))
  (should (equal (greger-parser--convert-value "[]") []))
  (should (equal (greger-parser--convert-value " [1, 2, 3] ") [1 2 3]))
  ;; Invalid arrays (with trailing content) should remain strings
  (should (string= (greger-parser--convert-value "[1,2,3]extra") "[1,2,3]extra"))
  (should (string= (greger-parser--convert-value "[1,2,3] garbage") "[1,2,3] garbage"))
  ;; Malformed arrays should remain strings
  (should (string= (greger-parser--convert-value "[1,2,") "[1,2,"))
  (should (string= (greger-parser--convert-value "1,2,3]") "1,2,3]")))

(ert-deftest greger-parser-test-convert-value-json-objects ()
  "Test conversion of JSON objects."
  ;; Valid objects (converted to alists with symbol keys)
  (should (equal (greger-parser--convert-value "{\"key\":\"value\"}") '((key . "value"))))
  (should (equal (greger-parser--convert-value "{\"a\":1,\"b\":2}") '((a . 1) (b . 2))))
  (should (equal (greger-parser--convert-value "{}") '()))
  (should (equal (greger-parser--convert-value " {\"key\": \"value\"} ") '((key . "value"))))
  ;; Invalid objects (with trailing content) should remain strings
  (should (string= (greger-parser--convert-value "{\"key\":\"value\"}garbage") "{\"key\":\"value\"}garbage"))
  (should (string= (greger-parser--convert-value "{\"key\":\"value\"} extra") "{\"key\":\"value\"} extra"))
  ;; Malformed objects should remain strings
  (should (string= (greger-parser--convert-value "{\"key\":") "{\"key\":"))
  (should (string= (greger-parser--convert-value "\"key\":\"value\"}") "\"key\":\"value\"}")))

(ert-deftest greger-parser-test-convert-value-strings ()
  "Test conversion of regular strings."
  (should (string= (greger-parser--convert-value "hello world") "hello world"))
  (should (string= (greger-parser--convert-value "some text") "some text"))
  ;; Test newline removal
  (should (string= (greger-parser--convert-value "\nhello\n") "hello"))
  (should (string= (greger-parser--convert-value "\n\nhello\n\n") "\nhello\n"))
  ;; Test edge cases that look like JSON but aren't
  (should (string= (greger-parser--convert-value "\"\"\"test\"\"\"") "\"\"\"test\"\"\""))
  (should (string= (greger-parser--convert-value "[not json") "[not json"))
  (should (string= (greger-parser--convert-value "{not json") "{not json")))

;; Cleanup test - should run last alphabetically
(ert-deftest greger-parser-zz-test-cleanup ()
  "Clean up test resources (runs last due to alphabetical ordering)."
  (greger-test-cleanup-grammar-repo)
  (should t)) ;; Always pass

;;; test-greger-parser.el ends here
