;;; test-greger-parser.el --- Tests for greger parser -*- lexical-binding: t -*-

(require 'ert)
(require 'greger-parser)
(require 'json)
(require 'cl-lib)

;; Test cases with tool use, thinking, and complex scenarios
(defconst greger-parser-test-cases
  '(
    ;; Simple user message
    (:name "simple-user-message"
     :markdown "## USER:

Hello, how are you?"
     :dialog ((user . "Hello, how are you?")))

    ;; System and user message
    (:name "system-and-user"
     :markdown "## SYSTEM:

You are a helpful assistant.

## USER:

What's the weather like?"
     :dialog ((system . "You are a helpful assistant.")
              (user . "What's the weather like?")))

    ;; Simple conversation
    (:name "simple-conversation"
     :markdown "## USER:

Hello

## ASSISTANT:

Hi there! How can I help you today?"
     :dialog ((user . "Hello")
              (assistant . "Hi there! How can I help you today?")))

    ;; Thinking section (becomes part of assistant message)
    (:name "thinking-section"
     :markdown "## USER:

What's 2+2?

## THINKING:

This is a simple arithmetic question. I can answer this directly without needing any tools.

## ASSISTANT:

2 + 2 = 4"
     :dialog ((user . "What's 2+2?")
              (assistant . "[{\"type\":\"thinking\",\"thinking\":\"This is a simple arithmetic question. I can answer this directly without needing any tools.\"},{\"type\":\"text\",\"text\":\"2 + 2 = 4\"}]")))

    ;; Tool use with single parameter
    (:name "tool-use-single-param"
     :markdown "## USER:

Read the file hello.txt

## TOOL USE:

Name: read-file
ID: toolu_123

### path

hello.txt

## TOOL RESULT:

ID: toolu_123

Hello, world!

## ASSISTANT:

The file contains: Hello, world!"
     :dialog ((user . "Read the file hello.txt")
              (assistant . "[{\"type\":\"tool_use\",\"id\":\"toolu_123\",\"name\":\"read-file\",\"input\":{\"path\":\"hello.txt\"}}]")
              (user . "[{\"type\":\"tool_result\",\"tool_use_id\":\"toolu_123\",\"content\":\"Hello, world!\"}]")
              (assistant . "The file contains: Hello, world!")))

    ;; Tool use with multiple parameters
    (:name "tool-use-multiple-params"
     :markdown "## USER:

Search for python files containing 'def main'

## TOOL USE:

Name: ripgrep
ID: toolu_456

### pattern

def main

### file-type

py

### context-lines

2

## TOOL RESULT:

ID: toolu_456

src/main.py:10:def main():
src/utils.py:25:def main_helper():

## ASSISTANT:

I found 2 matches for 'def main' in Python files."
     :dialog ((user . "Search for python files containing 'def main'")
              (assistant . "[{\"type\":\"tool_use\",\"id\":\"toolu_456\",\"name\":\"ripgrep\",\"input\":{\"pattern\":\"def main\",\"file-type\":\"py\",\"context-lines\":\"2\"}}]")
              (user . "[{\"type\":\"tool_result\",\"tool_use_id\":\"toolu_456\",\"content\":\"src/main.py:10:def main():\\nsrc/utils.py:25:def main_helper():\"}]")
              (assistant . "I found 2 matches for 'def main' in Python files.")))

    ;; Complex workflow with thinking, tool use, and multiple responses
    (:name "complex-workflow"
     :markdown "## USER:

who's the current king of sweden?

## THINKING:

The user is asking about the current king of Sweden. This is a factual question that I can search for to get the most up-to-date information. I'll use the search function to find this information.

## TOOL USE:

Name: search-286d2fd3
ID: toolu_01Kf8avk1cBqH5ZHoXL92Duc

### query

current king of Sweden 2024

### include_answer

basic

### max_results

3

## TOOL RESULT:

ID: toolu_01Kf8avk1cBqH5ZHoXL92Duc

{\"query\": \"current king of Sweden 2024\", \"answer\": \"Carl XVI Gustaf\", \"response_time\": 2.38}

## ASSISTANT:

The current King of Sweden is **Carl XVI Gustaf**. He has been reigning since 1973 and is the longest-reigning monarch in Swedish history."
     :dialog ((user . "who's the current king of sweden?")
              (assistant . "[{\"type\":\"thinking\",\"thinking\":\"The user is asking about the current king of Sweden. This is a factual question that I can search for to get the most up-to-date information. I'll use the search function to find this information.\"},{\"type\":\"tool_use\",\"id\":\"toolu_01Kf8avk1cBqH5ZHoXL92Duc\",\"name\":\"search-286d2fd3\",\"input\":{\"query\":\"current king of Sweden 2024\",\"include_answer\":\"basic\",\"max_results\":\"3\"}}]")
              (user . "[{\"type\":\"tool_result\",\"tool_use_id\":\"toolu_01Kf8avk1cBqH5ZHoXL92Duc\",\"content\":\"{\\\"query\\\": \\\"current king of Sweden 2024\\\", \\\"answer\\\": \\\"Carl XVI Gustaf\\\", \\\"response_time\\\": 2.38}\"}]")
              (assistant . "The current King of Sweden is **Carl XVI Gustaf**. He has been reigning since 1973 and is the longest-reigning monarch in Swedish history.")))

    ;; Multiple tool uses in sequence
    (:name "multiple-tool-uses"
     :markdown "## USER:

List files and read the first one

## TOOL USE:

Name: list-directory
ID: toolu_111

### path

.

## TOOL RESULT:

ID: toolu_111

file1.txt
file2.txt
README.md

## TOOL USE:

Name: read-file
ID: toolu_222

### path

file1.txt

## TOOL RESULT:

ID: toolu_222

This is the content of file1.

## ASSISTANT:

I found 3 files in the directory. The first file (file1.txt) contains: \"This is the content of file1.\""
     :dialog ((user . "List files and read the first one")
              (assistant . "[{\"type\":\"tool_use\",\"id\":\"toolu_111\",\"name\":\"list-directory\",\"input\":{\"path\":\".\"}}]")
              (user . "[{\"type\":\"tool_result\",\"tool_use_id\":\"toolu_111\",\"content\":\"file1.txt\\nfile2.txt\\nREADME.md\"}]")
              (assistant . "[{\"type\":\"tool_use\",\"id\":\"toolu_222\",\"name\":\"read-file\",\"input\":{\"path\":\"file1.txt\"}}]")
              (user . "[{\"type\":\"tool_result\",\"tool_use_id\":\"toolu_222\",\"content\":\"This is the content of file1.\"}]")
              (assistant . "I found 3 files in the directory. The first file (file1.txt) contains: \"This is the content of file1.\"")))

    ;; Tool use with multiline parameter values
    (:name "tool-use-multiline-params"
     :markdown "## USER:

Write a new Python file

## TOOL USE:

Name: write-new-file
ID: toolu_789

### file_path

script.py

### contents

#!/usr/bin/env python3

def main():
    print(\"Hello, world!\")

if __name__ == \"__main__\":
    main()

### git_commit_message

Add new Python script

## TOOL RESULT:

ID: toolu_789

Successfully wrote new file script.py with 85 characters.

## ASSISTANT:

I've created a new Python script file with a basic Hello World program."
     :dialog ((user . "Write a new Python file")
              (assistant . "[{\"type\":\"tool_use\",\"id\":\"toolu_789\",\"name\":\"write-new-file\",\"input\":{\"file_path\":\"script.py\",\"contents\":\"#!/usr/bin/env python3\\n\\ndef main():\\n    print(\\\"Hello, world!\\\")\\n\\nif __name__ == \\\"__main__\\\":\\n    main()\",\"git_commit_message\":\"Add new Python script\"}}]")
              (user . "[{\"type\":\"tool_result\",\"tool_use_id\":\"toolu_789\",\"content\":\"Successfully wrote new file script.py with 85 characters.\"}]")
              (assistant . "I've created a new Python script file with a basic Hello World program.")))

    ;; Just thinking without any other content
    (:name "thinking-only"
     :markdown "## USER:

Let me think about this

## THINKING:

I need to consider all the options carefully before responding."
     :dialog ((user . "Let me think about this")
              (assistant . "[{\"type\":\"thinking\",\"thinking\":\"I need to consider all the options carefully before responding.\"}]")))

    ;; Tool use without any following content
    (:name "tool-use-only"
     :markdown "## USER:

Read a file

## TOOL USE:

Name: read-file
ID: toolu_999

### path

test.txt"
     :dialog ((user . "Read a file")
              (assistant . "[{\"type\":\"tool_use\",\"id\":\"toolu_999\",\"name\":\"read-file\",\"input\":{\"path\":\"test.txt\"}}]")))

(:name "code-block-triple-backticks"
     :markdown "## USER:

Here's some code:

```
## ASSISTANT:
This should not be parsed as a section header
## TOOL USE:
Neither should this
```

What do you think?"
     :dialog ((user . "Here's some code:\n\n```\n## ASSISTANT:\nThis should not be parsed as a section header\n## TOOL USE:\nNeither should this\n```\n\nWhat do you think?")))

    ;; Code blocks with section headers inside (double backticks)
    (:name "code-block-double-backticks"
     :markdown "## USER:

Inline code: ``## ASSISTANT: not a header`` and more text.

## ASSISTANT:

I see the inline code."
     :dialog ((user . "Inline code: ``## ASSISTANT: not a header`` and more text.")
              (assistant . "I see the inline code.")))

    ;; Mixed code blocks and real sections
    (:name "mixed-code-blocks-and-sections"
     :markdown "## USER:

Here's a code example:

```python
def example():
    # This has ## USER: in a comment
    print(\"## ASSISTANT: not a real header\")
```

Now please analyze it.

## ASSISTANT:

I can see your code example."
     :dialog ((user . "Here's a code example:\n\n```python\ndef example():\n    # This has ## USER: in a comment\n    print(\"## ASSISTANT: not a real header\")\n```\n\nNow please analyze it.")
              (assistant . "I can see your code example.")))

    ;; Tool use with code blocks in parameters
    (:name "tool-use-with-code-in-params"
     :markdown "## USER:

Write some Python code

## TOOL USE:

Name: write-file
ID: toolu_999

### filename

example.py

### content

```python
def main():
    # This ## USER: comment should not break parsing
    print(\"Hello world\")

if __name__ == \"__main__\":
    main()
```

## TOOL RESULT:

ID: toolu_999

File written successfully

## ASSISTANT:

I've written the Python file."
     :dialog ((user . "Write some Python code")
              (assistant . "[{\"type\":\"tool_use\",\"id\":\"toolu_999\",\"name\":\"write-file\",\"input\":{\"filename\":\"example.py\",\"content\":\"```python\\ndef main():\\n    # This ## USER: comment should not break parsing\\n    print(\\\"Hello world\\\")\\n\\nif __name__ == \\\"__main__\\\":\\n    main()\\n```\"}}]")
              (user . "[{\"type\":\"tool_result\",\"tool_use_id\":\"toolu_999\",\"content\":\"File written successfully\"}]")
              (assistant . "I've written the Python file.")))

    ;; Nested code blocks (backticks inside code blocks)
    (:name "nested-code-blocks"
     :markdown "## USER:

How do I use backticks in markdown?

## ASSISTANT:

You can use triple backticks:

```
Here's how to show `inline code` in a code block:
Use single backticks around `your code`.
```

Does that help?"
     :dialog ((user . "How do I use backticks in markdown?")
              (assistant . "You can use triple backticks:\n\n```\nHere's how to show `inline code` in a code block:\nUse single backticks around `your code`.\n```\n\nDoes that help?")))

    ))

;; Helper functions for tests
(defun greger-parser-test--dialog-equal (expected actual)
  "Compare two dialog structures, handling JSON content blocks."
  (and (listp expected) (listp actual)
       (= (length expected) (length actual))
       (cl-every
        (lambda (pair)
          (let ((exp-msg (car pair))
                (act-msg (cdr pair)))
            (and (eq (car exp-msg) (car act-msg))
                 (greger-parser-test--content-equal (cdr exp-msg) (cdr act-msg)))))
        (cl-mapcar #'cons expected actual))))

(defun greger-parser-test--content-equal (expected actual)
  "Compare message content, handling JSON strings."
  (cond
   ;; Both are strings - direct comparison
   ((and (stringp expected) (stringp actual))
    (string= expected actual))

   ;; Expected is JSON string, parse and compare
   ((and (stringp expected) (string-prefix-p "[" expected))
    (condition-case nil
        (let ((expected-parsed (json-read-from-string expected))
              (actual-parsed (if (stringp actual)
                                (json-read-from-string actual)
                              actual)))
          (greger-parser-test--json-equal expected-parsed actual-parsed))
      (error nil)))

   ;; Fallback to string comparison
   (t (string= (format "%s" expected) (format "%s" actual)))))

(defun greger-parser-test--json-equal (expected actual)
  "Compare two parsed JSON structures."
  (cond
   ;; Both are vectors (arrays)
   ((and (vectorp expected) (vectorp actual))
    (and (= (length expected) (length actual))
         (cl-every (lambda (pair)
                     (greger-parser-test--json-equal (car pair) (cdr pair)))
                   (cl-mapcar #'cons (append expected nil) (append actual nil)))))

   ;; Both are alists (objects)
   ((and (listp expected) (listp actual))
    (and (= (length expected) (length actual))
         (cl-every (lambda (exp-pair)
                     (let ((key (car exp-pair))
                           (exp-val (cdr exp-pair)))
                       (let ((act-val (alist-get key actual)))
                         (greger-parser-test--json-equal exp-val act-val))))
                   expected)))

   ;; Direct comparison for primitives
   (t (equal expected actual))))

(defun greger-parser-test--normalize-whitespace (str)
  "Normalize whitespace in string for comparison."
  (string-trim (replace-regexp-in-string "[ \t\n\r]+" " " str)))

;; Main test suite
(ert-deftest greger-parser-test-markdown-to-dialog ()
  "Test converting markdown to dialog format."
  (dolist (test-case greger-parser-test-cases)
    (let ((name (plist-get test-case :name))
          (markdown (plist-get test-case :markdown))
          (expected-dialog (plist-get test-case :dialog)))
      (message "Testing markdown-to-dialog for: %s" name)
      (let ((actual-dialog (greger-parser-parse-dialog markdown)))
        (should (greger-parser-test--dialog-equal expected-dialog actual-dialog))))))

(ert-deftest greger-parser-test-roundtrip ()
  "Test that markdown -> dialog -> markdown preserves structure."
  (dolist (test-case greger-parser-test-cases)
    (let ((name (plist-get test-case :name))
          (original-markdown (plist-get test-case :markdown)))
      (message "Testing roundtrip for: %s" name)
      (let* ((dialog (greger-parser-parse-dialog original-markdown))
             (roundtrip-markdown (greger-parser-dialog-to-markdown dialog))
             (roundtrip-dialog (greger-parser-parse-dialog roundtrip-markdown)))
        ;; The dialog should be structurally equivalent after round-trip
        (should (= (length dialog) (length roundtrip-dialog)))
        (should (greger-parser-test--dialog-equal dialog roundtrip-dialog))))))

(ert-deftest greger-parser-test-tool-use-parsing ()
  "Test specific tool use parsing functionality."
  (let ((tool-use-markdown "## TOOL USE:

Name: read-file
ID: toolu_123

### path

hello.txt

### include_line_numbers

true"))
    (let ((parsed (greger-parser-parse-dialog tool-use-markdown)))
      (should (= 1 (length parsed)))
      (let ((assistant-msg (car parsed)))
        (should (eq 'assistant (car assistant-msg)))
        (let* ((content-json (cdr assistant-msg))
               (content-blocks (json-read-from-string content-json))
               (tool-use-block (aref content-blocks 0)))
          (should (string= "tool_use" (alist-get 'type tool-use-block)))
          (should (string= "toolu_123" (alist-get 'id tool-use-block)))
          (should (string= "read-file" (alist-get 'name tool-use-block)))
          (let ((input (alist-get 'input tool-use-block)))
            (should (string= "hello.txt" (alist-get 'path input)))
            (should (string= "true" (alist-get 'include_line_numbers input)))))))))

(ert-deftest greger-parser-test-tool-result-parsing ()
  "Test specific tool result parsing functionality."
  (let ((tool-result-markdown "## TOOL RESULT:

ID: toolu_123

File contents here
with multiple lines"))
    (let ((parsed (greger-parser-parse-dialog tool-result-markdown)))
      (should (= 1 (length parsed)))
      (let ((user-msg (car parsed)))
        (should (eq 'user (car user-msg)))
        (let* ((content-json (cdr user-msg))
               (content-blocks (json-read-from-string content-json))
               (tool-result-block (aref content-blocks 0)))
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
    (let ((parsed (greger-parser-parse-dialog thinking-markdown)))
      (should (= 1 (length parsed)))
      (let ((assistant-msg (car parsed)))
        (should (eq 'assistant (car assistant-msg)))
        (let* ((content-json (cdr assistant-msg))
               (content-blocks (json-read-from-string content-json))
               (thinking-block (aref content-blocks 0)))
          (should (string= "thinking" (alist-get 'type thinking-block)))
          (should (string= "I need to think about this carefully.
This is a complex problem."
                          (alist-get 'thinking thinking-block))))))))

(ert-deftest greger-parser-test-error-handling ()
  "Test parser error handling for malformed input."
  ;; Test that malformed input returns empty result instead of error
  (should (condition-case err
              (progn (greger-parser-parse-dialog "## TOOL USE:\n\nMalformed") t)
            (error nil)))
  (should (condition-case err
              (progn (greger-parser-parse-dialog "## TOOL RESULT:\n\nMalformed") t)
            (error nil))))

(ert-deftest greger-parser-test-edge-cases ()
  "Test edge cases like empty content, whitespace handling."
  ;; Empty content
  (should (equal '() (greger-parser-parse-dialog "")))
  (should (equal '() (greger-parser-parse-dialog "\n\n  ")))

  ;; Only whitespace in sections - should return empty list
  (should (equal '() (greger-parser-parse-dialog "## USER:\n\n\n\n")))

  ;; Multiple consecutive newlines
  (let ((result (greger-parser-parse-dialog "## USER:\n\n\n\nHello\n\n\n\n## ASSISTANT:\n\n\n\nHi")))
    (should (= 2 (length result)))
    (should (string= "Hello" (cdr (car result))))
    (should (string= "Hi" (cdr (cadr result))))))

(ert-deftest greger-parser-test-performance ()
  "Test parser performance with large dialogs."
  (let ((large-markdown
         (mapconcat
          (lambda (i)
            (format "## USER:\n\nMessage %d\n\n## ASSISTANT:\n\nResponse %d" i i))
          (number-sequence 1 100)
          "\n\n")))
    (let ((start-time (current-time)))
      (greger-parser-parse-dialog large-markdown)
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

."))
    (let ((parsed (greger-parser-parse-dialog complex-markdown)))
      (should (= 2 (length parsed)))
      ;; First message should be user
      (should (eq 'user (car (car parsed))))
      ;; Second message should be assistant with mixed content
      (let ((assistant-msg (cadr parsed)))
        (should (eq 'assistant (car assistant-msg)))
        (let* ((content-json (cdr assistant-msg))
               (content-blocks (json-read-from-string content-json)))
          (should (= 3 (length content-blocks)))
          ;; Should have thinking, text, and tool_use blocks
          (should (string= "thinking" (alist-get 'type (aref content-blocks 0))))
          (should (string= "text" (alist-get 'type (aref content-blocks 1))))
          (should (string= "tool_use" (alist-get 'type (aref content-blocks 2)))))))))

(ert-deftest greger-parser-test-markdown-generation ()
  "Test that generated markdown follows expected format."
  (let ((dialog '((user . "Test message")
                  (assistant . "[{\"type\":\"thinking\",\"thinking\":\"Let me think\"},{\"type\":\"text\",\"text\":\"Here's my response\"},{\"type\":\"tool_use\",\"id\":\"tool_123\",\"name\":\"test-tool\",\"input\":{\"param\":\"value\"}}]")
                  (user . "[{\"type\":\"tool_result\",\"tool_use_id\":\"tool_123\",\"content\":\"Tool output\"}]")
                  (assistant . "Final response"))))
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
    (let ((parsed (greger-parser-parse-dialog markdown)))
      (should (= 2 (length parsed)))
      (should (eq 'user (car (car parsed))))
      (should (string= "Hello, this is untagged content" (cdr (car parsed))))
      (should (eq 'assistant (car (cadr parsed))))
      (should (string= "I understand you have untagged content." (cdr (cadr parsed)))))))

;; Test that we handle tool use parameters correctly with various whitespace
(ert-deftest greger-parser-test-tool-use-whitespace ()
  "Test tool use parsing with various whitespace patterns."
  (let ((markdown "## TOOL USE:

Name: test-tool
ID: tool_123

### param1

value1

### param2

value2 with
multiple


  lines

### param3

value3"))
    (let ((parsed (greger-parser-parse-dialog markdown)))
      (should (= 1 (length parsed)))
      (let* ((assistant-msg (car parsed))
             (content-json (cdr assistant-msg))
             (content-blocks (json-read-from-string content-json))
             (tool-block (aref content-blocks 0))
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
    (let ((parsed (greger-parser-parse-dialog markdown)))
      (should (= 2 (length parsed)))
      ;; First message should contain the entire user content including code block
      (let ((user-content (cdr (car parsed))))
        (should (string-match-p "## ASSISTANT:" user-content))
        (should (string-match-p "## TOOL USE:" user-content))
        (should (string-match-p "Real content continues" user-content)))
      ;; Second message should be the real assistant response
      (should (eq 'assistant (car (cadr parsed))))
      (should (string= "I see your code." (cdr (cadr parsed)))))))

(ert-deftest greger-parser-test-inline-code-blocks ()
  "Test that section headers inside inline code are not parsed."
  (let ((markdown "## USER:

Use ``## ASSISTANT: response`` to format.

## ASSISTANT:

Got it!"))
    (let ((parsed (greger-parser-parse-dialog markdown)))
      (should (= 2 (length parsed)))
      (should (string-match-p "## ASSISTANT: response" (cdr (car parsed))))
      (should (string= "Got it!" (cdr (cadr parsed)))))))

(ert-deftest greger-parser-test-code-blocks-in-tool-params ()
  "Test that code blocks in tool parameters are preserved correctly."
  (let ((markdown "## TOOL USE:

Name: write-file
ID: tool_123

### content

```python
# This ## USER: comment should be preserved
print(\"## ASSISTANT: also preserved\")
```"))
    (let ((parsed (greger-parser-parse-dialog markdown)))
      (should (= 1 (length parsed)))
      (let* ((assistant-msg (car parsed))
             (content-json (cdr assistant-msg))
             (content-blocks (json-read-from-string content-json))
             (tool-block (aref content-blocks 0))
             (input (alist-get 'input tool-block))
             (content-param (alist-get 'content input)))
        (should (string-match-p "## USER:" content-param))
        (should (string-match-p "## ASSISTANT:" content-param))
        (should (string-match-p "```python" content-param))))))

(provide 'test-greger-parser)

;;; test-greger-parser.el ends here
