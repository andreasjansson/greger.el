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
  (unless greger-test-grammar-repo-path
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
              (message "Successfully cloned greger-grammar to %s" greger-test-grammar-repo-path))))))))

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
  (let* ((markdown "# USER

Hello, how are you?")
       (dialog (greger-parser-markdown-to-dialog markdown))
       (roundtrip-markdown (greger-parser-dialog-to-markdown dialog))
       (expected-dialog '(((role . "user")
                           (content . "Hello, how are you?")))))
  (should (equal expected-dialog dialog))
  (should (string= markdown roundtrip-markdown))))

(ert-deftest greger-parser-system-and-user ()
  "Test roundtrip for simple-user-message corpus case."
  (let* ((markdown "# SYSTEM

You are a helpful assistant.

# USER

What's the weather like?")
       (dialog (greger-parser-markdown-to-dialog markdown))
       (roundtrip-markdown (greger-parser-dialog-to-markdown dialog))
       (expected-dialog '(((role . "system")
                           (content . "You are a helpful assistant."))
                          ((role . "user")
                           (content . "What's the weather like?")))))
  (should (equal expected-dialog dialog))
  (should (string= markdown roundtrip-markdown))))

(ert-deftest greger-parser-test-simple-conversation ()
  "Test roundtrip for simple-conversation corpus case."
  (let* ((markdown "# USER

Hello

# ASSISTANT

Hi there! How can I help you today?")
         (dialog (greger-parser-markdown-to-dialog markdown))
         (roundtrip-markdown (greger-parser-dialog-to-markdown dialog))
         (expected-dialog '(((role . "user")
                             (content . "Hello"))
                            ((role . "assistant")
                             (content ((text . "Hi there! How can I help you today?")
                                       (type . "text")))))))
    (should (equal expected-dialog dialog))
    (should (string= markdown roundtrip-markdown))))

(ert-deftest greger-parser-test-thinking-section ()
  "Test roundtrip for thinking-section corpus case."
  (let* ((markdown "# USER

What's 2+2?

# THINKING

Signature: sig123

This is a simple arithmetic question. I can answer this directly without needing any tools.

# ASSISTANT

2 + 2 = 4

# USER

What's 1+1?

# THINKING

Another simple question. But I haven't finished generating yet.")
         (dialog (greger-parser-markdown-to-dialog markdown))
         (roundtrip-markdown (greger-parser-dialog-to-markdown dialog))
         (expected-dialog '(((role . "user")
                             (content . "What's 2+2?"))
                            ((role . "assistant")
                             (content ((type . "thinking")
                                       (signature . "sig123")
                                       (thinking . "This is a simple arithmetic question. I can answer this directly without needing any tools."))))
                            ((role . "assistant")
                             (content ((text . "2 + 2 = 4")
                                       (type . "text"))))
                            ((role . "user")
                             (content . "What's 1+1?"))
                            ((role . "assistant")
                             (content ((type . "thinking")
                                       (thinking . "Another simple question. But I haven't finished generating yet.")))))))
    (should (equal expected-dialog dialog))
    (should (string= markdown roundtrip-markdown))))

(ert-deftest greger-parser-test-tool-use-single-param ()
  "Test roundtrip for tool-use-single-param corpus case."
  (let* ((markdown "# USER

Read the file hello.txt

# TOOL USE

Name: read-file
ID: toolu_123

## path

<tool.toolu_123>
hello.txt
</tool.toolu_123>

# TOOL RESULT

ID: toolu_123

<tool.toolu_123>
Hello, world!
</tool.toolu_123>

# ASSISTANT

The file contains: Hello, world!")
         (dialog (greger-parser-markdown-to-dialog markdown))
         (roundtrip-markdown (greger-parser-dialog-to-markdown dialog))
         (expected-dialog '(((role . "user")
                             (content . "Read the file hello.txt"))
                            ((role . "assistant")
                             (content ((type . "tool_use")
                                       (id . "toolu_123")
                                       (name . "read-file")
                                       (input ((path . "hello.txt"))))))
                            ((role . "user")
                             (content ((type . "tool_result")
                                       (tool_use_id . "toolu_123")
                                       (content . "Hello, world!"))))
                            ((role . "assistant")
                             (content ((text . "The file contains: Hello, world!")
                                       (type . "text")))))))
    (should (equal expected-dialog dialog))
    (should (string= markdown roundtrip-markdown))))

(ert-deftest greger-parser-test-tool-use-multiple-params ()
  "Test roundtrip for tool-use-multiple-params corpus case."
  (let* ((markdown "# USER

Search for python files containing 'def main'

# TOOL USE

Name: ripgrep
ID: toolu_456

## pattern

<tool.toolu_456>
def main
</tool.toolu_456>

## file-type

<tool.toolu_456>
py
</tool.toolu_456>

## context-lines

<tool.toolu_456>
2
</tool.toolu_456>

# TOOL RESULT

ID: toolu_456

<tool.toolu_456>
src/main.py:10:def main():
src/utils.py:25:def main_helper():
</tool.toolu_456>

# ASSISTANT

I found 2 matches for 'def main' in Python files.")
         (dialog (greger-parser-markdown-to-dialog markdown))
         (roundtrip-markdown (greger-parser-dialog-to-markdown dialog))
         (expected-dialog '(((role . "user")
                             (content . "Search for python files containing 'def main'"))
                            ((role . "assistant")
                             (content ((type . "tool_use")
                                       (id . "toolu_456")
                                       (name . "ripgrep")
                                       (input ((pattern . "def main")
                                               (file-type . "py")
                                               (context-lines . 2))))))
                            ((role . "user")
                             (content ((type . "tool_result")
                                       (tool_use_id . "toolu_456")
                                       (content . "src/main.py:10:def main():
src/utils.py:25:def main_helper():"))))
                            ((role . "assistant")
                             (content ((text . "I found 2 matches for 'def main' in Python files.")
                                       (type . "text")))))))
    (should (equal expected-dialog dialog))
    (should (string= markdown roundtrip-markdown))))

(ert-deftest greger-parser-test-complex-workflow ()
  "Test roundtrip for complex-workflow corpus case."
  (let* ((markdown "# USER

who's the current king of sweden?

# THINKING

The user is asking about the current king of Sweden. This is a factual question that I can search for to get the most up-to-date information. I'll use the search function to find this information.

# TOOL USE

Name: search-286d2fd3
ID: toolu_01Kf8avk1cBqH5ZHoXL92Duc

## query

<tool.toolu_01Kf8avk1cBqH5ZHoXL92Duc>
current king of Sweden 2024
</tool.toolu_01Kf8avk1cBqH5ZHoXL92Duc>

## include_answer

<tool.toolu_01Kf8avk1cBqH5ZHoXL92Duc>
basic
</tool.toolu_01Kf8avk1cBqH5ZHoXL92Duc>

## max_results

<tool.toolu_01Kf8avk1cBqH5ZHoXL92Duc>
3
</tool.toolu_01Kf8avk1cBqH5ZHoXL92Duc>

# TOOL RESULT

ID: toolu_01Kf8avk1cBqH5ZHoXL92Duc

<tool.toolu_01Kf8avk1cBqH5ZHoXL92Duc>
{
  \"query\": \"current king of Sweden 2024\",
  \"answer\": \"Carl XVI Gustaf\",
  \"response_time\": 2.38
}
</tool.toolu_01Kf8avk1cBqH5ZHoXL92Duc>

# ASSISTANT

The current King of Sweden is **Carl XVI Gustaf**. He has been reigning since 1973 and is the longest-reigning monarch in Swedish history.")
         (dialog (greger-parser-markdown-to-dialog markdown))
         (roundtrip-markdown (greger-parser-dialog-to-markdown dialog))
         (expected-dialog '(((role . "user")
                             (content . "who's the current king of sweden?"))
                            ((role . "assistant")
                             (content ((type . "thinking")
                                       (thinking . "The user is asking about the current king of Sweden. This is a factual question that I can search for to get the most up-to-date information. I'll use the search function to find this information."))))
                            ((role . "assistant")
                             (content ((type . "tool_use")
                                       (id . "toolu_01Kf8avk1cBqH5ZHoXL92Duc")
                                       (name . "search-286d2fd3")
                                       (input ((query . "current king of Sweden 2024")
                                               (include_answer . "basic")
                                               (max_results . 3))))))
                            ((role . "user")
                             (content ((type . "tool_result")
                                       (tool_use_id . "toolu_01Kf8avk1cBqH5ZHoXL92Duc")
                                       (content . "{
  \"query\": \"current king of Sweden 2024\",
  \"answer\": \"Carl XVI Gustaf\",
  \"response_time\": 2.38
}"))))
                            ((role . "assistant")
                             (content ((text . "The current King of Sweden is **Carl XVI Gustaf**. He has been reigning since 1973 and is the longest-reigning monarch in Swedish history.")
                                       (type . "text")))))))
    (should (equal expected-dialog dialog))
    (should (string= markdown roundtrip-markdown))))

(ert-deftest greger-parser-test-multiple-tool-uses ()
  "Test roundtrip for multiple-tool-uses corpus case."
  (let* ((markdown "# USER

List files and read the first one

# TOOL USE

Name: list-directory
ID: toolu_111

## path

<tool.toolu_111>
.
</tool.toolu_111>

# TOOL RESULT

ID: toolu_111

<tool.toolu_111>
file1.txt
file2.txt
README.md
</tool.toolu_111>

# TOOL USE

Name: read-file
ID: toolu_222

## path

<tool.toolu_222>
file1.txt
</tool.toolu_222>

# TOOL RESULT

ID: toolu_222

<tool.toolu_222>
This is the content of file1.
</tool.toolu_222>

# ASSISTANT

I found 3 files in the directory. The first file (file1.txt) contains: \"This is the content of file1.\"")
         (dialog (greger-parser-markdown-to-dialog markdown))
         (roundtrip-markdown (greger-parser-dialog-to-markdown dialog))
         (expected-dialog '(((role . "user")
                             (content . "List files and read the first one"))
                            ((role . "assistant")
                             (content ((type . "tool_use")
                                       (id . "toolu_111")
                                       (name . "list-directory")
                                       (input ((path . "."))))))
                            ((role . "user")
                             (content ((type . "tool_result")
                                       (tool_use_id . "toolu_111")
                                       (content . "file1.txt
file2.txt
README.md"))))
                            ((role . "assistant")
                             (content ((type . "tool_use")
                                       (id . "toolu_222")
                                       (name . "read-file")
                                       (input ((path . "file1.txt"))))))
                            ((role . "user")
                             (content ((type . "tool_result")
                                       (tool_use_id . "toolu_222")
                                       (content . "This is the content of file1."))))
                            ((role . "assistant")
                             (content ((text . "I found 3 files in the directory. The first file (file1.txt) contains: \"This is the content of file1.\"")
                                       (type . "text")))))))
    (should (equal expected-dialog dialog))
    (should (string= markdown roundtrip-markdown))))

(ert-deftest greger-parser-test-thinking-only ()
  "Test roundtrip for thinking-only corpus case."
  (let* ((markdown "# USER

Let me think about this

# THINKING

I need to consider all the options carefully before responding.")
         (dialog (greger-parser-markdown-to-dialog markdown))
         (roundtrip-markdown (greger-parser-dialog-to-markdown dialog))
         (expected-dialog '(((role . "user")
                             (content . "Let me think about this"))
                            ((role . "assistant")
                             (content ((type . "thinking")
                                       (thinking . "I need to consider all the options carefully before responding.")))))))
    (should (equal expected-dialog dialog))
    (should (string= markdown roundtrip-markdown))))

(ert-deftest greger-parser-test-tool-use-only ()
  "Test roundtrip for tool-use-only corpus case."
  (let* ((markdown "# USER

Read a file

# TOOL USE

Name: read-file
ID: toolu_999

## path

<tool.toolu_999>
test.txt
</tool.toolu_999>")
         (dialog (greger-parser-markdown-to-dialog markdown))
         (roundtrip-markdown (greger-parser-dialog-to-markdown dialog))
         (expected-dialog '(((role . "user")
                             (content . "Read a file"))
                            ((role . "assistant")
                             (content ((type . "tool_use")
                                       (id . "toolu_999")
                                       (name . "read-file")
                                       (input ((path . "test.txt")))))))))
    (should (equal expected-dialog dialog))
    (should (string= markdown roundtrip-markdown))))

(ert-deftest greger-parser-test-citations-basic ()
  "Test roundtrip for citations-basic corpus case."
  (let* ((markdown "# USER

When was Claude Shannon born?

# SERVER TOOL USE

Name: web_search
ID: srvtoolu_01WYG3ziw53XMcoyKL4XcZmE

## query

<tool.srvtoolu_01WYG3ziw53XMcoyKL4XcZmE>
claude shannon birth date
</tool.srvtoolu_01WYG3ziw53XMcoyKL4XcZmE>

# WEB SEARCH TOOL RESULT

ID: srvtoolu_01WYG3ziw53XMcoyKL4XcZmE

<tool.srvtoolu_01WYG3ziw53XMcoyKL4XcZmE>
[
  {
    \"type\": \"web_search_result\",
    \"url\": \"https://en.wikipedia.org/wiki/Claude_Shannon\",
    \"title\": \"Claude Shannon - Wikipedia\",
    \"encrypted_content\": \"EqgfCioIARgBIiQ3YTAwMjY1Mi1mZjM5LTQ1NGUtODgxNC1kNjNjNTk1ZWI3Y...\",
    \"page_age\": \"April 30, 2025\"
  }
]
</tool.srvtoolu_01WYG3ziw53XMcoyKL4XcZmE>

# ASSISTANT

Based on the search results,

# ASSISTANT

Claude Shannon was born on April 30, 1916, in Petoskey, Michigan

## https://en.wikipedia.org/wiki/Claude_Shannon

Title: Claude Shannon - Wikipedia
Cited text: Claude Elwood Shannon (April 30, 1916 – February 24, 2001) was an American mathematician, electrical engineer, computer scientist, cryptographer and i...
Encrypted index: Eo8BCioIAhgBIiQyYjQ0OWJmZi1lNm..")
         (dialog (greger-parser-markdown-to-dialog markdown))
         (roundtrip-markdown (greger-parser-dialog-to-markdown dialog))
         (expected-dialog '(((role . "user")
                             (content . "When was Claude Shannon born?"))
                            ((role . "assistant")
                             (content ((type . "server_tool_use")
                                       (id . "srvtoolu_01WYG3ziw53XMcoyKL4XcZmE")
                                       (name . "web_search")
                                       (input ((query . "claude shannon birth date"))))))
                            ((role . "assistant")
                             (content ((type . "web_search_tool_result")
                                       (tool_use_id . "srvtoolu_01WYG3ziw53XMcoyKL4XcZmE")
                                       (content . [((type . "web_search_result")
                                                    (url . "https://en.wikipedia.org/wiki/Claude_Shannon")
                                                    (title . "Claude Shannon - Wikipedia")
                                                    (encrypted_content . "EqgfCioIARgBIiQ3YTAwMjY1Mi1mZjM5LTQ1NGUtODgxNC1kNjNjNTk1ZWI3Y...")
                                                    (page_age . "April 30, 2025"))]))))
                            ((role . "assistant")
                             (content ((text . "Based on the search results,")
                                       (type . "text"))))
                            ((role . "assistant")
                             (content ((text . "Claude Shannon was born on April 30, 1916, in Petoskey, Michigan")
                                       (type . "text")
                                       (citations . ((url . "https://en.wikipedia.org/wiki/Claude_Shannon")
                                                     (title . "Claude Shannon - Wikipedia")
                                                     (cited_text . "Claude Elwood Shannon (April 30, 1916 – February 24, 2001) was an American mathematician, electrical engineer, computer scientist, cryptographer and i...")
                                                     (encrypted_index . "Eo8BCioIAhgBIiQyYjQ0OWJmZi1lNm..")))))))))
    (should (equal expected-dialog dialog))
    (should (string= markdown roundtrip-markdown))))

(ert-deftest greger-parser-test-citations-after-tool-result ()
  "Test roundtrip for citations-after-tool-result corpus case."
  (let* ((markdown "# USER

What's the current weather?

# SERVER TOOL USE

Name: web_search
ID: srvtoolu_456

## query

<tool.srvtoolu_456>
current weather
</tool.srvtoolu_456>

# WEB SEARCH TOOL RESULT

ID: srvtoolu_456

<tool.srvtoolu_456>
Weather search results
</tool.srvtoolu_456>

# ASSISTANT

It's currently sunny and 75°F

## https://weather.com

Title: Weather.com
Cited text: Currently sunny with a temperature of 75 degrees Fahrenheit...
Encrypted index: xyz789")
         (dialog (greger-parser-markdown-to-dialog markdown))
         (roundtrip-markdown (greger-parser-dialog-to-markdown dialog))
         (expected-dialog '(((role . "user")
                             (content . "What's the current weather?"))
                            ((role . "assistant")
                             (content ((type . "server_tool_use")
                                       (id . "srvtoolu_456")
                                       (name . "web_search")
                                       (input ((query . "current weather"))))))
                            ((role . "assistant")
                             (content ((type . "web_search_tool_result")
                                       (tool_use_id . "srvtoolu_456")
                                       (content . "Weather search results"))))
                            ((role . "assistant")
                             (content ((text . "It's currently sunny and 75°F")
                                       (type . "text")
                                       (citations . ((url . "https://weather.com")
                                                     (title . "Weather.com")
                                                     (cited_text . "Currently sunny with a temperature of 75 degrees Fahrenheit...")
                                                     (encrypted_index . "xyz789")))))))))
    (should (equal expected-dialog dialog))
    (should (string= markdown roundtrip-markdown))))

(ert-deftest greger-parser-test-citations-multiple ()
  "Test roundtrip for citations-multiple corpus case."
  (let* ((markdown "# USER

Tell me about Einstein and Newton

# SERVER TOOL USE

Name: web_search
ID: srvtoolu_789

## query

<tool.srvtoolu_789>
Einstein Newton physics
</tool.srvtoolu_789>

# WEB SEARCH TOOL RESULT

ID: srvtoolu_789

<tool.srvtoolu_789>
Physics search results
</tool.srvtoolu_789>

# ASSISTANT

Einstein developed the theory of relativity

## https://physics.com/einstein

Title: 
Cited text: Albert Einstein developed the theory of relativity in the early 20th century...
Encrypted index: def456

# ASSISTANT

while

# ASSISTANT

Newton formulated the laws of motion

## https://physics.com/newton

Title: Newton Biography
Cited text: 
Encrypted index: ghi789")
         (dialog (greger-parser-markdown-to-dialog markdown))
         (roundtrip-markdown (greger-parser-dialog-to-markdown dialog))
         (expected-dialog '(((role . "user")
                             (content . "Tell me about Einstein and Newton"))
                            ((role . "assistant")
                             (content ((type . "server_tool_use")
                                       (id . "srvtoolu_789")
                                       (name . "web_search")
                                       (input ((query . "Einstein Newton physics"))))))
                            ((role . "assistant")
                             (content ((type . "web_search_tool_result")
                                       (tool_use_id . "srvtoolu_789")
                                       (content . "Physics search results"))))
                            ((role . "assistant")
                             (content ((text . "Einstein developed the theory of relativity")
                                       (type . "text")
                                       (citations . ((url . "https://physics.com/einstein")
                                                     (title . "")
                                                     (cited_text . "Albert Einstein developed the theory of relativity in the early 20th century...")
                                                     (encrypted_index . "def456"))))))
                            ((role . "assistant")
                             (content ((text . "while")
                                       (type . "text"))))
                            ((role . "assistant")
                             (content ((text . "Newton formulated the laws of motion")
                                       (type . "text")
                                       (citations . ((url . "https://physics.com/newton")
                                                     (title . "Newton Biography")
                                                     (cited_text . "")
                                                     (encrypted_index . "ghi789")))))))))
    (should (equal expected-dialog dialog))
    (should (string= markdown roundtrip-markdown))))

(ert-deftest greger-parser-test-code-block-triple-backticks ()
  "Test roundtrip for code-block-triple-backticks corpus case."
  (let* ((markdown "# USER

Here's some code:

```
# ASSISTANT
This should not be parsed as a section header
# TOOL USE
Neither should this
```

What do you think?")
         (dialog (greger-parser-markdown-to-dialog markdown))
         (roundtrip-markdown (greger-parser-dialog-to-markdown dialog))
         (expected-dialog '(((role . "user")
                             (content . "Here's some code:

```
# ASSISTANT
This should not be parsed as a section header
# TOOL USE
Neither should this
```

What do you think?")))))
    (should (equal expected-dialog dialog))
    (should (string= markdown roundtrip-markdown))))

(ert-deftest greger-parser-test-mixed-code-blocks-and-sections ()
  "Test roundtrip for mixed-code-blocks-and-sections corpus case."
  (let* ((markdown "# USER

Here's a code example:

```python
def example():
    # This has # USER in a comment
    print(\"# ASSISTANT not a real header\")
```

Now please analyze it.

# ASSISTANT

I can see your code example.")
         (dialog (greger-parser-markdown-to-dialog markdown))
         (roundtrip-markdown (greger-parser-dialog-to-markdown dialog))
         (expected-dialog '(((role . "user")
                             (content . "Here's a code example:

```python
def example():
    # This has # USER in a comment
    print(\"# ASSISTANT not a real header\")
```

Now please analyze it."))
                            ((role . "assistant")
                             (content ((text . "I can see your code example.")
                                       (type . "text")))))))
    (should (equal expected-dialog dialog))
    (should (string= markdown roundtrip-markdown))))

(ert-deftest greger-parser-test-tool-use-with-code-in-params ()
  "Test roundtrip for tool-use-with-code-in-params corpus case."
  (let* ((markdown "# USER

Write some Python code

# TOOL USE

Name: write-file
ID: toolu_999

## filename

<tool.toolu_999>
example.py
</tool.toolu_999>

## content

<tool.toolu_999>
```python
def main():
    # This # USER comment should not break parsing
    print(\"Hello world\")

if __name__ == \"__main__\":
    main()
```
</tool.toolu_999>

# TOOL RESULT

ID: toolu_999

<tool.toolu_999>
File written successfully
</tool.toolu_999>

# ASSISTANT

I've written the Python file.")
         (dialog (greger-parser-markdown-to-dialog markdown))
         (roundtrip-markdown (greger-parser-dialog-to-markdown dialog))
         (expected-dialog '(((role . "user")
                             (content . "Write some Python code"))
                            ((role . "assistant")
                             (content ((type . "tool_use")
                                       (id . "toolu_999")
                                       (name . "write-file")
                                       (input ((filename . "example.py")
                                               (content . "```python
def main():
    # This # USER comment should not break parsing
    print(\"Hello world\")

if __name__ == \"__main__\":
    main()
```"))))))
                            ((role . "user")
                             (content ((type . "tool_result")
                                       (tool_use_id . "toolu_999")
                                       (content . "File written successfully"))))
                            ((role . "assistant")
                             (content ((text . "I've written the Python file.")
                                       (type . "text")))))))
    (should (equal expected-dialog dialog))
    (should (string= markdown roundtrip-markdown))))

(ert-deftest greger-parser-test-tool-use-with-tool-use-in-params ()
  "Test roundtrip for tool-use-with-tool-use-in-params corpus case."
  (let* ((markdown "# USER

Write some Python code

# TOOL USE

Name: write-file
ID: toolu_999

## filename

<tool.toolu_999>
example.py
</tool.toolu_999>

## content

<tool.toolu_999>
foo
<tool.toolu_123>
bar
</tool.toolu_123>
</tool.toolu_999>

# TOOL RESULT

ID: toolu_999

<tool.toolu_999>
File written successfully
</tool.toolu_999>

# ASSISTANT

I've written the Python file.")
         (dialog (greger-parser-markdown-to-dialog markdown))
         (roundtrip-markdown (greger-parser-dialog-to-markdown dialog))
         (expected-dialog '(((role . "user")
                             (content . "Write some Python code"))
                            ((role . "assistant")
                             (content ((type . "tool_use")
                                       (id . "toolu_999")
                                       (name . "write-file")
                                       (input ((filename . "example.py")
                                               (content . "foo
<tool.toolu_123>
bar
</tool.toolu_123>"))))))
                            ((role . "user")
                             (content ((type . "tool_result")
                                       (tool_use_id . "toolu_999")
                                       (content . "File written successfully"))))
                            ((role . "assistant")
                             (content ((text . "I've written the Python file.")
                                       (type . "text")))))))
    (should (equal expected-dialog dialog))
    (should (string= markdown roundtrip-markdown))))

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
