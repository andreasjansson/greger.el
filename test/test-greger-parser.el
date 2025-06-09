;;; test-greger-parser.el --- Tests for greger parser -*- lexical-binding: t -*-

(require 'ert)
(require 'greger-parser)
(require 'cl-lib)

;; Helper function to read markdown content from corpus .txt files
(defun greger-read-corpus-file (name)
  "Read markdown content from a .txt corpus file, extracting only the input portion."
  (let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
         (file-path (expand-file-name (format "greger-grammar/test/corpus/%s.txt" name) test-dir)))
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
                (string= (alist-get 'content expected) (alist-get 'content actual))))
          ((string= type "server_tool_use")
           (and (string= (alist-get 'id expected) (alist-get 'id actual))
                (string= (alist-get 'name expected) (alist-get 'name actual))
                (greger-parser-test--input-equal (alist-get 'input expected) (alist-get 'input actual))))
          ((string= type "server_tool_result")
           (and (string= (alist-get 'tool_use_id expected) (alist-get 'tool_use_id actual))
                (equal (alist-get 'content expected) (alist-get 'content actual))))
          (t t)))))

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

;; Helper function for roundtrip testing
(defun greger-parser-test--roundtrip (corpus-name)
  "Test roundtrip conversion for a corpus file."
  (let ((original-markdown (greger-read-corpus-file corpus-name)))
    (let* ((dialog (greger-parser-markdown-to-dialog original-markdown))
           (roundtrip-markdown (greger-parser-dialog-to-markdown dialog))
           (roundtrip-dialog (greger-parser-markdown-to-dialog roundtrip-markdown)))
      ;; The dialog should be structurally equivalent after round-trip
      (should (= (length dialog) (length roundtrip-dialog)))
      (should (greger-parser-test--dialog-equal dialog roundtrip-dialog)))))

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

(ert-deftest greger-parser-test-tool-use-parsing ()
  "Test specific tool use parsing functionality."
  (let ((tool-use-markdown "## TOOL USE:

Name: read-file
ID: toolu_123

### path

<tool.toolu_123>
hello.txt
</tool.toolu_123>

### include_line_numbers

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
  (let ((tool-result-markdown "## TOOL RESULT:

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
  (let ((thinking-markdown "## THINKING:

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
              (progn (greger-parser-markdown-to-dialog "## TOOL USE:\n\nMalformed") t)
            (error nil)))
  (should (condition-case err
              (progn (greger-parser-markdown-to-dialog "## TOOL RESULT:\n\nMalformed") t)
            (error nil))))

(ert-deftest greger-parser-test-edge-cases ()
  "Test edge cases like empty content, whitespace handling."
  ;; Empty content
  (should (equal '() (greger-parser-markdown-to-dialog "")))
  (should (equal '() (greger-parser-markdown-to-dialog "\n\n  ")))

  ;; Only whitespace in sections - should return empty list
  (should (equal '() (greger-parser-markdown-to-dialog "## USER:\n\n\n\n")))

  ;; Multiple consecutive newlines
  (let ((result (greger-parser-markdown-to-dialog "## USER:\n\n\n\nHello\n\n\n\n## ASSISTANT:\n\n\n\nHi")))
    (should (= 2 (length result)))
    (should (string= "Hello" (alist-get 'content (car result))))
    (should (string= "Hi" (alist-get 'content (cadr result))))))

(ert-deftest greger-parser-test-performance ()
  "Test parser performance with large dialogs."
  (let ((large-markdown
         (mapconcat
          (lambda (i)
            (format "## USER:\n\nMessage %d\n\n## ASSISTANT:\n\nResponse %d" i i))
          (number-sequence 1 100)
          "\n\n")))
    (let ((start-time (current-time)))
      (greger-parser-markdown-to-dialog large-markdown)
      (let ((elapsed (float-time (time-subtract (current-time) start-time))))
        ;; Should parse 100 message pairs in under 1 second
        (should (< elapsed 1.0))))))

(ert-deftest greger-parser-test-complex-mixed-content ()
  "Test parsing of complex mixed content with thinking, tools, and text."
  (let ((complex-markdown "## USER:

Help me with a file

## THINKING:

The user wants help with a file. I should ask what they need.

## ASSISTANT:

What kind of help do you need with the file?

## TOOL USE:

Name: list-directory
ID: toolu_abc

### path

<tool.toolu_abc>
.
</tool.toolu_abc>"))
    (let ((parsed (greger-parser-markdown-to-dialog complex-markdown)))
      (should (= 2 (length parsed)))
      ;; First message should be user
      (should (string= "user" (alist-get 'role (car parsed))))
      ;; Second message should be assistant with mixed content
      (let ((assistant-msg (cadr parsed)))
        (should (string= "assistant" (alist-get 'role assistant-msg)))
        (let ((content-blocks (alist-get 'content assistant-msg)))
          (should (= 3 (length content-blocks)))
          ;; Should have thinking, text, and tool_use blocks
          (should (string= "thinking" (alist-get 'type (car content-blocks))))
          (should (string= "text" (alist-get 'type (cadr content-blocks))))
          (should (string= "tool_use" (alist-get 'type (caddr content-blocks)))))))))

(ert-deftest greger-parser-test-markdown-generation ()
  "Test that generated markdown follows expected format."
  (let ((dialog '(((role . "user") (content . "Test message"))
                  ((role . "assistant") (content . (((type . "thinking") (thinking . "Let me think")) ((type . "text") (text . "Here's my response")) ((type . "tool_use") (id . "tool_123") (name . "test-tool") (input . ((param . "value")))))))
                  ((role . "user") (content . (((type . "tool_result") (tool_use_id . "tool_123") (content . "Tool output")))))
                  ((role . "assistant") (content . "Final response")))))
    (let ((markdown (greger-parser-dialog-to-markdown dialog)))
      ;; Should contain all expected sections
      (should (string-match-p "## USER:" markdown))
      (should (string-match-p "## THINKING:" markdown))
      (should (string-match-p "## ASSISTANT:" markdown))
      (should (string-match-p "## TOOL USE:" markdown))
      (should (string-match-p "## TOOL RESULT:" markdown))
      (should (string-match-p "Name: test-tool" markdown))
      (should (string-match-p "ID: tool_123" markdown))
      (should (string-match-p "### param" markdown))
      (should (string-match-p "value" markdown)))))

;; Test untagged content at the beginning
(ert-deftest greger-parser-test-untagged-content ()
  "Test that untagged content at the beginning is treated as user message."
  (let ((markdown "Hello, this is untagged content

## ASSISTANT:

I understand you have untagged content."))
    (let ((parsed (greger-parser-markdown-to-dialog markdown)))
      (should (= 2 (length parsed)))
      (should (string= "user" (alist-get 'role (car parsed))))
      (should (string= "Hello, this is untagged content" (alist-get 'content (car parsed))))
      (should (string= "assistant" (alist-get 'role (cadr parsed))))
      (should (string= "I understand you have untagged content." (alist-get 'content (cadr parsed)))))))

;; Test that we handle tool use parameters correctly with various whitespace
(ert-deftest greger-parser-test-tool-use-whitespace ()
  "Test tool use parsing with various whitespace patterns."
  (let ((markdown "## TOOL USE:

Name: test-tool
ID: tool_123

### param1

<tool.tool_123>
value1
</tool.tool_123>

### param2

<tool.tool_123>
value2 with
multiple


  lines
</tool.tool_123>

### param3

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

(ert-deftest greger-parser-test-code-block-parsing ()
  "Test that section headers inside code blocks are not parsed."
  (let ((markdown "## USER:

Here's code with fake headers:

```
## ASSISTANT:
This looks like a header but isn't
## TOOL USE:
Same with this
```

Real content continues.

## ASSISTANT:

I see your code."))
    (let ((parsed (greger-parser-markdown-to-dialog markdown)))
      (should (= 2 (length parsed)))
      ;; First message should contain the entire user content including code block
      (let ((user-content (alist-get 'content (car parsed))))
        (should (string-match-p "## ASSISTANT:" user-content))
        (should (string-match-p "## TOOL USE:" user-content))
        (should (string-match-p "Real content continues" user-content)))
      ;; Second message should be the real assistant response
      (should (string= "assistant" (alist-get 'role (cadr parsed))))
      (should (string= "I see your code." (alist-get 'content (cadr parsed)))))))

(ert-deftest greger-parser-test-inline-code-blocks ()
  "Test that section headers inside inline code are not parsed."
  (let ((markdown "## USER:

Use ``## ASSISTANT: response`` to format.

## ASSISTANT:

Got it!"))
    (let ((parsed (greger-parser-markdown-to-dialog markdown)))
      (should (= 2 (length parsed)))
      (should (string-match-p "## ASSISTANT: response" (alist-get 'content (car parsed))))
      (should (string= "Got it!" (alist-get 'content (cadr parsed)))))))

(ert-deftest greger-parser-test-code-blocks-in-tool-params ()
  "Test that code blocks in tool parameters are preserved correctly."
  (let ((markdown "## TOOL USE:

Name: write-file
ID: tool_123

### content

<tool.tool_123>
```python
# This ## USER: comment should be preserved
print(\"## ASSISTANT: also preserved\")
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
        (should (string-match-p "## USER:" content-param))
        (should (string-match-p "## ASSISTANT:" content-param))
        (should (string-match-p "```python" content-param))))))

;; Include tag tests
(ert-deftest greger-parser-test-include-tag-basic ()
  "Test basic include tag functionality."
  (let ((test-file (make-temp-file "greger-test-include" nil ".txt" "Hello from included file!"))
        (markdown nil)
        (expected nil))
    (unwind-protect
        (progn
          (setq markdown (format "## USER:

Here's the content:

<include>%s</include>

What do you think?" test-file))

          (setq expected "## USER:

Here's the content:

Hello from included file!

What do you think?")

          (let ((parsed (greger-parser-markdown-to-dialog markdown)))
            (should (= 1 (length parsed)))
            (let ((generated-markdown (greger-parser-dialog-to-markdown parsed)))
              (should (string= expected generated-markdown)))))
      (when (file-exists-p test-file)
        (delete-file test-file)))))

(ert-deftest greger-parser-test-include-tag-with-code ()
  "Test include tag with code attribute."
  (let ((test-file (make-temp-file "greger-test-include" nil ".py" "def hello():\n    print('Hello, world!')"))
        (markdown nil)
        (expected nil))
    (unwind-protect
        (progn
          (setq markdown (format "## USER:

Here's the Python code:

<include code>%s</include>

Review this code." test-file))

          (setq expected (format "## USER:

Here's the Python code:

%s:
```
def hello():
    print('Hello, world!')
```

Review this code." test-file))

          (let ((parsed (greger-parser-markdown-to-dialog markdown)))
            (should (= 1 (length parsed)))
            (let ((generated-markdown (greger-parser-dialog-to-markdown parsed)))
              (should (string= expected generated-markdown)))))
      (when (file-exists-p test-file)
        (delete-file test-file)))))

(ert-deftest greger-parser-test-include-tag-nonexistent-file ()
  "Test include tag with nonexistent file."
  (let ((markdown "## USER:

Try to include: <include>/nonexistent/file.txt</include>

This should handle errors gracefully.")
        (expected "## USER:

Try to include: [Error reading file: /nonexistent/file.txt]

This should handle errors gracefully."))
    (let ((parsed (greger-parser-markdown-to-dialog markdown)))
      (should (= 1 (length parsed)))
      (let ((generated-markdown (greger-parser-dialog-to-markdown parsed)))
        (should (string= expected generated-markdown))))))

(ert-deftest greger-parser-test-include-tag-multiline-content ()
  "Test include tag with multiline file content."
  (let ((test-file (make-temp-file "greger-test-include" nil ".txt" "Line 1\nLine 2\n\nLine 4 after empty line"))
        (markdown nil)
        (expected nil))
    (unwind-protect
        (progn
          (setq markdown (format "## USER:

Multiline content:

<include>%s</include>

End of message." test-file))

          (setq expected "## USER:

Multiline content:

Line 1
Line 2

Line 4 after empty line

End of message.")

          (let ((parsed (greger-parser-markdown-to-dialog markdown)))
            (should (= 1 (length parsed)))
            (let ((generated-markdown (greger-parser-dialog-to-markdown parsed)))
              (should (string= expected generated-markdown)))))
      (when (file-exists-p test-file)
        (delete-file test-file)))))


(ert-deftest greger-parser-test-include-tag-recursive ()
  "Test include tag with file that contains another include tag."
  (let ((inner-file (make-temp-file "greger-test-inner" nil ".txt" "Inner file content"))
        (outer-file nil)
        (markdown nil)
        (expected nil))
    (unwind-protect
        (progn
          (setq outer-file (make-temp-file "greger-test-outer" nil ".txt"
                                          (format "Before include\n<include>%s</include>\nAfter include" inner-file)))
          (setq markdown (format "## USER:

Recursive include:

<include>%s</include>

Done." outer-file))

          (setq expected "## USER:

Recursive include:

Before include
Inner file content
After include

Done.")

          (let ((parsed (greger-parser-markdown-to-dialog markdown)))
            (should (= 1 (length parsed)))
            (let ((generated-markdown (greger-parser-dialog-to-markdown parsed)))
              (should (string= expected generated-markdown)))))
      (when (and inner-file (file-exists-p inner-file))
        (delete-file inner-file))
      (when (and outer-file (file-exists-p outer-file))
        (delete-file outer-file)))))

(ert-deftest greger-parser-test-include-tag-in-assistant-section ()
  "Test include tag in assistant section."
  (let ((test-file (make-temp-file "greger-test-include" nil ".txt" "Assistant included content"))
        (markdown nil)
        (expected nil))
    (unwind-protect
        (progn
          (setq markdown (format "## USER:

Show me the file.

## ASSISTANT:

Here's the content:

<include>%s</include>

Hope this helps!" test-file))

          (setq expected "## USER:

Show me the file.

## ASSISTANT:

Here's the content:

Assistant included content

Hope this helps!")

          (let ((parsed (greger-parser-markdown-to-dialog markdown)))
            (should (= 2 (length parsed)))
            (let ((generated-markdown (greger-parser-dialog-to-markdown parsed)))
              (should (string= expected generated-markdown)))))
      (when (file-exists-p test-file)
        (delete-file test-file)))))

(ert-deftest greger-parser-test-include-tag-with-code-in-code-block ()
  "Test include tag with code attribute where content has code blocks."
  (let ((test-file (make-temp-file "greger-test-include" nil ".py" "def example():\n    pass\n"))
        (markdown nil)
        (expected nil))
    (unwind-protect
        (progn
          (setq markdown (format "## USER:

<include code>%s</include>" test-file))

          (setq expected (format "## USER:

%s:
```
def example():
    pass
```" test-file))

          (let ((parsed (greger-parser-markdown-to-dialog markdown)))
            (should (= 1 (length parsed)))
            (let ((generated-markdown (greger-parser-dialog-to-markdown parsed)))
              (should (string= expected generated-markdown)))))
      (when (file-exists-p test-file)
        (delete-file test-file)))))

;; Tests to ensure include tags are NOT processed in code blocks or tool content
(ert-deftest greger-parser-test-include-tag-not-processed-in-code-blocks ()
  "Test that include tags inside code blocks are not processed."
  (let ((test-file (make-temp-file "greger-test-include" nil ".txt" "This should not be included"))
        (markdown nil)
        (expected nil))
    (unwind-protect
        (progn
          (setq markdown (format "## USER:

Here's some code with an include tag:

```
<include>%s</include>
```

The include should not be processed." test-file))

          (setq expected (format "## USER:

Here's some code with an include tag:

```
<include>%s</include>
```

The include should not be processed." test-file))

          (let ((parsed (greger-parser-markdown-to-dialog markdown)))
            (should (= 1 (length parsed)))
            (let ((generated-markdown (greger-parser-dialog-to-markdown parsed)))
              (should (string= expected generated-markdown)))))
      (when (file-exists-p test-file)
        (delete-file test-file)))))

(ert-deftest greger-parser-test-include-tag-not-processed-in-inline-code ()
  "Test that include tags inside inline code are not processed."
  (let ((test-file (make-temp-file "greger-test-include" nil ".txt" "This should not be included"))
        (markdown nil)
        (expected nil))
    (unwind-protect
        (progn
          (setq markdown (format "## USER:

Use `<include>%s</include>` to include files.

The include in backticks should not be processed." test-file))

          (setq expected (format "## USER:

Use `<include>%s</include>` to include files.

The include in backticks should not be processed." test-file))

          (let ((parsed (greger-parser-markdown-to-dialog markdown)))
            (should (= 1 (length parsed)))
            (let ((generated-markdown (greger-parser-dialog-to-markdown parsed)))
              (should (string= expected generated-markdown)))))
      (when (file-exists-p test-file)
        (delete-file test-file)))))

(ert-deftest greger-parser-test-include-tag-not-processed-in-tool-params ()
  "Test that include tags inside tool parameters are not processed."
  (let ((test-file (make-temp-file "greger-test-include" nil ".txt" "This should not be included"))
        (markdown nil)
        (expected nil))
    (unwind-protect
        (progn
          (setq markdown (format "## TOOL USE:

Name: write-file
ID: tool_123

### content

<tool.tool_123>
<include>%s</include>
</tool.tool_123>" test-file))

          (setq expected (format "## TOOL USE:

Name: write-file
ID: tool_123

### content

<tool.tool_123>
<include>%s</include>
</tool.tool_123>" test-file))

          (let ((parsed (greger-parser-markdown-to-dialog markdown)))
            (should (= 1 (length parsed)))
            (let ((generated-markdown (greger-parser-dialog-to-markdown parsed)))
              (should (string= expected generated-markdown)))))
      (when (file-exists-p test-file)
        (delete-file test-file)))))

(ert-deftest greger-parser-test-include-tag-web-url ()
  "Test include tag with web URL functionality."
  (let ((markdown "## USER:

Check this out:

<include>https://pub-b88c9764a4fc46baa90b9e8e1544f59e.r2.dev/hello.html</include>

What do you think?")
        (expected "## USER:

Check this out:

Hello world!

What do you think?"))
    ;; This test just verifies that URL handling doesn't crash
    ;; The exact content will vary based on the response
    (let ((parsed (greger-parser-markdown-to-dialog markdown)))
      (should (= 1 (length parsed)))
      (let ((generated-markdown (greger-parser-dialog-to-markdown parsed)))
        (should (string= expected generated-markdown))))))

(ert-deftest greger-parser-test-include-tag-web-url-with-code ()
  "Test include tag with web URL and code attribute."
  (let ((markdown "## USER:

<include code>https://pub-b88c9764a4fc46baa90b9e8e1544f59e.r2.dev/hello.html</include>

Pretty cool!")
        (expected "## USER:

https://pub-b88c9764a4fc46baa90b9e8e1544f59e.r2.dev/hello.html:
```
Hello world!
```

Pretty cool!"))
    ;; This test verifies URL handling with code formatting
    (let ((parsed (greger-parser-markdown-to-dialog markdown)))
      (should (= 1 (length parsed)))
      (let ((generated-markdown (greger-parser-dialog-to-markdown parsed)))
        (should (string= expected generated-markdown))))))

(ert-deftest greger-parser-test-include-tag-invalid-url ()
  "Test include tag with invalid web URL."
  (let ((markdown "## USER:

This should fail:

<include>https://invalid-url-that-does-not-exist-12345.com</include>

Error handling test")
        (expected "## USER:

This should fail:

[Error reading URL: https://invalid-url-that-does-not-exist-12345.com]

Error handling test"))
    (let ((parsed (greger-parser-markdown-to-dialog markdown)))
      (should (= 1 (length parsed)))
      (let ((generated-markdown (greger-parser-dialog-to-markdown parsed)))
        (should (string= expected generated-markdown))))))

;;; test-greger-parser.el ends here
