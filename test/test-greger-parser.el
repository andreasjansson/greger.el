;;; test-greger-parser.el --- Tests for greger parser -*- lexical-binding: t -*-

(require 'ert)
(require 'greger-parser)
(require 'cl-lib)

;; Test cases with tool use, thinking, and complex scenarios
(defconst greger-parser-test-cases
  '(
    ;; Simple user message
    (:name "simple-user-message"
           :markdown "## USER:

Hello, how are you?"
           :dialog (((role . "user") (content . "Hello, how are you?"))))

    ;; System and user message
    (:name "system-and-user"
           :markdown "## SYSTEM:

You are a helpful assistant.

## USER:

What's the weather like?"
           :dialog (((role . "system") (content . "You are a helpful assistant."))
                    ((role . "user") (content . "What's the weather like?"))))

    ;; Simple conversation
    (:name "simple-conversation"
           :markdown "## USER:

Hello

## ASSISTANT:

Hi there! How can I help you today?"
           :dialog (((role . "user") (content . "Hello"))
                    ((role . "assistant") (content . "Hi there! How can I help you today?"))))

    ;; Thinking section (becomes part of assistant message)
    (:name "thinking-section"
           :markdown "## USER:

What's 2+2?

## THINKING:

This is a simple arithmetic question. I can answer this directly without needing any tools.

## ASSISTANT:

2 + 2 = 4"
           :dialog (((role . "user") (content . "What's 2+2?"))
                    ((role . "assistant") (content . (((type . "thinking") (thinking . "This is a simple arithmetic question. I can answer this directly without needing any tools.")) ((type . "text") (text . "2 + 2 = 4")))))))

    ;; Tool use with single parameter
    (:name "tool-use-single-param"
           :markdown "## USER:

Read the file hello.txt

## TOOL USE:

Name: read-file
ID: toolu_123

### path

<tool.toolu_123>
hello.txt
</tool.toolu_123>

## TOOL RESULT:

ID: toolu_123

<tool.toolu_123>
Hello, world!
</tool.toolu_123>

## ASSISTANT:

The file contains: Hello, world!"
           :dialog (((role . "user") (content . "Read the file hello.txt"))
                    ((role . "assistant") (content . (((type . "tool_use") (id . "toolu_123") (name . "read-file") (input . ((path . "hello.txt")))))))
                    ((role . "user") (content . (((type . "tool_result") (tool_use_id . "toolu_123") (content . "Hello, world!")))))
                    ((role . "assistant") (content . "The file contains: Hello, world!"))))

    ;; Tool use with multiple parameters
    (:name "tool-use-multiple-params"
           :markdown "## USER:

Search for python files containing 'def main'

## TOOL USE:

Name: ripgrep
ID: toolu_456

### pattern

<tool.toolu_456>
def main
</tool.toolu_456>

### file-type

<tool.toolu_456>
py
</tool.toolu_456>

### context-lines

<tool.toolu_456>
2
</tool.toolu_456>

## TOOL RESULT:

ID: toolu_456

<tool.toolu_456>
src/main.py:10:def main():
src/utils.py:25:def main_helper():
</tool.toolu_456>

## ASSISTANT:

I found 2 matches for 'def main' in Python files."
           :dialog (((role . "user") (content . "Search for python files containing 'def main'"))
                    ((role . "assistant") (content . (((type . "tool_use") (id . "toolu_456") (name . "ripgrep") (input . ((pattern . "def main") (file-type . "py") (context-lines . 2)))))))
                    ((role . "user") (content . (((type . "tool_result") (tool_use_id . "toolu_456") (content . "src/main.py:10:def main():\nsrc/utils.py:25:def main_helper():")))))
                    ((role . "assistant") (content . "I found 2 matches for 'def main' in Python files."))))

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

<tool.toolu_01Kf8avk1cBqH5ZHoXL92Duc>
current king of Sweden 2024
</tool.toolu_01Kf8avk1cBqH5ZHoXL92Duc>

### include_answer

<tool.toolu_01Kf8avk1cBqH5ZHoXL92Duc>
basic
</tool.toolu_01Kf8avk1cBqH5ZHoXL92Duc>

### max_results

<tool.toolu_01Kf8avk1cBqH5ZHoXL92Duc>
3
</tool.toolu_01Kf8avk1cBqH5ZHoXL92Duc>

## TOOL RESULT:

ID: toolu_01Kf8avk1cBqH5ZHoXL92Duc

<tool.toolu_01Kf8avk1cBqH5ZHoXL92Duc>
{\"query\": \"current king of Sweden 2024\", \"answer\": \"Carl XVI Gustaf\", \"response_time\": 2.38}
</tool.toolu_01Kf8avk1cBqH5ZHoXL92Duc>

## ASSISTANT:

The current King of Sweden is **Carl XVI Gustaf**. He has been reigning since 1973 and is the longest-reigning monarch in Swedish history."
           :dialog (((role . "user") (content . "who's the current king of sweden?"))
                    ((role . "assistant") (content . (((type . "thinking") (thinking . "The user is asking about the current king of Sweden. This is a factual question that I can search for to get the most up-to-date information. I'll use the search function to find this information.")) ((type . "tool_use") (id . "toolu_01Kf8avk1cBqH5ZHoXL92Duc") (name . "search-286d2fd3") (input . ((query . "current king of Sweden 2024") (include_answer . "basic") (max_results . 3)))))))
                    ((role . "user") (content . (((type . "tool_result") (tool_use_id . "toolu_01Kf8avk1cBqH5ZHoXL92Duc") (content . "{\"query\": \"current king of Sweden 2024\", \"answer\": \"Carl XVI Gustaf\", \"response_time\": 2.38}")))))
                    ((role . "assistant") (content . "The current King of Sweden is **Carl XVI Gustaf**. He has been reigning since 1973 and is the longest-reigning monarch in Swedish history."))))

    ;; Multiple tool uses in sequence
    (:name "multiple-tool-uses"
           :markdown "## USER:

List files and read the first one

## TOOL USE:

Name: list-directory
ID: toolu_111

### path

<tool.toolu_111>
.
</tool.toolu_111>

## TOOL RESULT:

ID: toolu_111

<tool.toolu_111>
file1.txt
file2.txt
README.md
</tool.toolu_111>

## TOOL USE:

Name: read-file
ID: toolu_222

### path

<tool.toolu_222>
file1.txt
</tool.toolu_222>

## TOOL RESULT:

ID: toolu_222

<tool.toolu_222>
This is the content of file1.
</tool.toolu_222>

## ASSISTANT:

I found 3 files in the directory. The first file (file1.txt) contains: \"This is the content of file1.\""
           :dialog (((role . "user") (content . "List files and read the first one"))
                    ((role . "assistant") (content . (((type . "tool_use") (id . "toolu_111") (name . "list-directory") (input . ((path . ".")))))))
                    ((role . "user") (content . (((type . "tool_result") (tool_use_id . "toolu_111") (content . "file1.txt\nfile2.txt\nREADME.md")))))
                    ((role . "assistant") (content . (((type . "tool_use") (id . "toolu_222") (name . "read-file") (input . ((path . "file1.txt")))))))
                    ((role . "user") (content . (((type . "tool_result") (tool_use_id . "toolu_222") (content . "This is the content of file1.")))))
                    ((role . "assistant") (content . "I found 3 files in the directory. The first file (file1.txt) contains: \"This is the content of file1.\""))))

    ;; Tool use with multiline parameter values
    (:name "tool-use-multiline-params"
           :markdown "## USER:

Write a new Python file

## TOOL USE:

Name: write-new-file
ID: toolu_789

### file_path

<tool.toolu_789>
script.py
</tool.toolu_789>

### contents

<tool.toolu_789>
#!/usr/bin/env python3

def main():
    print(\"Hello, world!\")

if __name__ == \"__main__\":
    main()
</tool.toolu_789>

### git_commit_message

<tool.toolu_789>
Add new Python script
</tool.toolu_789>

## TOOL RESULT:

ID: toolu_789

<tool.toolu_789>
Successfully wrote new file script.py with 85 characters.
</tool.toolu_789>

## ASSISTANT:

I've created a new Python script file with a basic Hello World program."
           :dialog (((role . "user") (content . "Write a new Python file"))
                    ((role . "assistant") (content . (((type . "tool_use") (id . "toolu_789") (name . "write-new-file") (input . ((file_path . "script.py") (contents . "#!/usr/bin/env python3\n\ndef main():\n    print(\"Hello, world!\")\n\nif __name__ == \"__main__\":\n    main()") (git_commit_message . "Add new Python script")))))))
                    ((role . "user") (content . (((type . "tool_result") (tool_use_id . "toolu_789") (content . "Successfully wrote new file script.py with 85 characters.")))))
                    ((role . "assistant") (content . "I've created a new Python script file with a basic Hello World program."))))

    ;; Just thinking without any other content
    (:name "thinking-only"
           :markdown "## USER:

Let me think about this

## THINKING:

I need to consider all the options carefully before responding."
           :dialog (((role . "user") (content . "Let me think about this"))
                    ((role . "assistant") (content . (((type . "thinking") (thinking . "I need to consider all the options carefully before responding.")))))))

    ;; Tool use without any following content
    (:name "tool-use-only"
           :markdown "## USER:

Read a file

## TOOL USE:

Name: read-file
ID: toolu_999

### path

<tool.toolu_999>
test.txt
</tool.toolu_999>
"
           :dialog (((role . "user") (content . "Read a file"))
                    ((role . "assistant") (content . (((type . "tool_use") (id . "toolu_999") (name . "read-file") (input . ((path . "test.txt")))))))))

    (:name "tool-use-with-tags"
           :markdown "## USER:

Read a file

## TOOL USE:

Name: read-file
ID: toolu_999

### path

<tool.toolu_999>
test.txt

## USER:

foo
</tool.toolu_999>
"
           :dialog (((role . "user") (content . "Read a file"))
                    ((role . "assistant") (content . (((type . "tool_use") (id . "toolu_999") (name . "read-file") (input . ((path . "test.txt\n\n## USER:\n\nfoo")))))))))

    ;; Tool result with empty lines preserved
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
           :dialog (((role . "user") (content . "Here's some code:\n\n```\n## ASSISTANT:\nThis should not be parsed as a section header\n## TOOL USE:\nNeither should this\n```\n\nWhat do you think?"))))

    ;; Code blocks with section headers inside (double backticks)
    (:name "code-block-double-backticks"
           :markdown "## USER:

Inline code: ``## ASSISTANT: not a header`` and more text.

## ASSISTANT:

I see the inline code."
           :dialog (((role . "user") (content . "Inline code: ``## ASSISTANT: not a header`` and more text."))
                    ((role . "assistant") (content . "I see the inline code."))))

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
           :dialog (((role . "user") (content . "Here's a code example:\n\n```python\ndef example():\n    # This has ## USER: in a comment\n    print(\"## ASSISTANT: not a real header\")\n```\n\nNow please analyze it."))
                    ((role . "assistant") (content . "I can see your code example."))))

    ;; Tool use with code blocks in parameters
    (:name "tool-use-with-code-in-params"
           :markdown "## USER:

Write some Python code

## TOOL USE:

Name: write-file
ID: toolu_999

### filename

<tool.toolu_999>
example.py
</tool.toolu_999>

### content

<tool.toolu_999>
```python
def main():
    # This ## USER: comment should not break parsing
    print(\"Hello world\")

if __name__ == \"__main__\":
    main()
```
</tool.toolu_999>

## TOOL RESULT:

ID: toolu_999

<tool.toolu_999>
File written successfully
</tool.toolu_999>

## ASSISTANT:

I've written the Python file."
           :dialog (((role . "user") (content . "Write some Python code"))
                    ((role . "assistant") (content . (((type . "tool_use") (id . "toolu_999") (name . "write-file") (input . ((filename . "example.py") (content . "```python\ndef main():\n    # This ## USER: comment should not break parsing\n    print(\"Hello world\")\n\nif __name__ == \"__main__\":\n    main()\n```")))))))
                    ((role . "user") (content . (((type . "tool_result") (tool_use_id . "toolu_999") (content . "File written successfully")))))
                    ((role . "assistant") (content . "I've written the Python file."))))

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
           :dialog (((role . "user") (content . "How do I use backticks in markdown?"))
                    ((role . "assistant") (content . "You can use triple backticks:\n\n```\nHere's how to show `inline code` in a code block:\nUse single backticks around `your code`.\n```\n\nDoes that help?"))))

    (:name "tool-use-complex-params"
           :markdown "## USER:

Test complex parameters

## TOOL USE:

Name: complex-tool
ID: toolu_complex

### string_param

<tool.toolu_complex>
hello world
</tool.toolu_complex>

### number_param

<tool.toolu_complex>
42
</tool.toolu_complex>

### float_param

<tool.toolu_complex>
3.14
</tool.toolu_complex>

### bool_true

<tool.toolu_complex>
true
</tool.toolu_complex>

### bool_false

<tool.toolu_complex>
false
</tool.toolu_complex>

### list_param

<tool.toolu_complex>
[\"item1\", \"item2\", 3]
</tool.toolu_complex>

### dict_param

<tool.toolu_complex>
{\"key\": \"value\", \"count\": 5}
</tool.toolu_complex>

## TOOL RESULT:

ID: toolu_complex

<tool.toolu_complex>
Success
</tool.toolu_complex>

## ASSISTANT:

Tool executed with complex parameters."
           :dialog (((role . "user") (content . "Test complex parameters"))
                    ((role . "assistant") (content . (((type . "tool_use") (id . "toolu_complex") (name . "complex-tool") (input . ((string_param . "hello world") (number_param . 42) (float_param . 3.14) (bool_true . t) (bool_false) (list_param . ["item1" "item2" 3]) (dict_param . ((key . "value") (count . 5)))))))))
                    ((role . "user") (content . (((type . "tool_result") (tool_use_id . "toolu_complex") (content . "Success")))))
                    ((role . "assistant") (content . "Tool executed with complex parameters."))))

    (:name "tool-result-empty-lines"
           :markdown "## TOOL USE:

Name: write-file
ID: tool_123

### content

<tool.tool_123>
foo


bar
</tool.tool_123>

## TOOL RESULT:

ID: tool_123

<tool.tool_123>
foo


bar
</tool.tool_123>

## ASSISTANT:

File written successfully."
           :dialog (((role . "assistant") (content . (((type . "tool_use") (id . "tool_123") (name . "write-file") (input . ((content . "foo\n\n\nbar")))))))
                    ((role . "user") (content . (((type . "tool_result") (tool_use_id . "tool_123") (content . "foo\n\n\nbar")))))
                    ((role . "assistant") (content . "File written successfully."))))

    (:name "html-comments"
           :markdown "## USER:

Here's some code:

<!-- comment -->
<!-- multi
line

comment -->

```
<!-- comment should be included -->
## ASSISTANT:
This should not be parsed as a section header
## TOOL USE:
Neither should this
```

What do you think?"
           :dialog (((role . "user") (content . "Here's some code:\n\n\n\n\n```\n<!-- comment should be included -->\n## ASSISTANT:\nThis should not be parsed as a section header\n## TOOL USE:\nNeither should this\n```\n\nWhat do you think?"))))

    ;; Server tool use and result test cases
    (:name "server-tool-use-basic"
           :markdown "## USER:

Search for current weather in San Francisco

## SERVER TOOL USE:

Name: web_search
ID: srvtoolu_123

### query

<tool.srvtoolu_123>
current weather San Francisco
</tool.srvtoolu_123>

## SERVER TOOL RESULT:

ID: srvtoolu_123

<tool.srvtoolu_123>
[
  {
    \"title\": \"Weather in San Francisco\",
    \"url\": \"https://weather.com/sf\",
    \"content\": \"Sunny, 72°F\"
  }
]
</tool.srvtoolu_123>

## ASSISTANT:

The current weather in San Francisco is sunny and 72°F."
           :dialog (((role . "user") (content . "Search for current weather in San Francisco"))
                    ((role . "assistant") (content . (((type . "server_tool_use")
                                                       (id . "srvtoolu_123")
                                                       (name . "web_search")
                                                       (input . ((query . "current weather San Francisco"))))
                                                      ((type . "server_tool_result")
                                                       (tool_use_id . "srvtoolu_123")
                                                       (content . (((title . "Weather in San Francisco")
                                                                    (url . "https://weather.com/sf")
                                                                    (content . "Sunny, 72°F")))))
                                                      ((type . "text") (text . "The current weather in San Francisco is sunny and 72°F.")))))))

    (:name "server-tool-use-string-result"
           :markdown "## USER:

What's the weather like?

## SERVER TOOL USE:

Name: web_search
ID: srvtoolu_456

### query

<tool.srvtoolu_456>
weather
</tool.srvtoolu_456>

## SERVER TOOL RESULT:

ID: srvtoolu_456

<tool.srvtoolu_456>
Sunny and warm today
</tool.srvtoolu_456>

## ASSISTANT:

It looks like it's sunny and warm today!"
           :dialog (((role . "user") (content . "What's the weather like?"))
                    ((role . "assistant") (content . (((type . "server_tool_use")
                                                       (id . "srvtoolu_456")
                                                       (name . "web_search")
                                                       (input . ((query . "weather"))))
                                                      ((type . "server_tool_result")
                                                       (tool_use_id . "srvtoolu_456")
                                                       (content . "Sunny and warm today"))
                                                      ((type . "text")
                                                       (text . "It looks like it's sunny and warm today!")))))))

    ;; Citation parsing test cases
    (:name "citations-basic"
           :markdown "## USER:

When was Claude Shannon born?

## SERVER TOOL USE:

Name: web_search
ID: srvtoolu_01WYG3ziw53XMcoyKL4XcZmE

### query

<tool.srvtoolu_01WYG3ziw53XMcoyKL4XcZmE>
claude shannon birth date
</tool.srvtoolu_01WYG3ziw53XMcoyKL4XcZmE>

## SERVER TOOL RESULT:

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

## ASSISTANT:

Based on the search results, <cite>Claude Shannon was born on April 30, 1916, in Petoskey, Michigan</cite>

## CITATIONS:

### https://en.wikipedia.org/wiki/Claude_Shannon

Title: Claude Shannon - Wikipedia
Cited text: Claude Elwood Shannon (April 30, 1916 – February 24, 2001) was an American mathematician, electrical engineer, computer scientist, cryptographer and i...
Encrypted index: Eo8BCioIAhgBIiQyYjQ0OWJmZi1lNm.."
           :dialog (((role . "user") (content . "When was Claude Shannon born?"))
                    ((role . "assistant") (content . (((type . "server_tool_use")
                                                       (id . "srvtoolu_01WYG3ziw53XMcoyKL4XcZmE")
                                                       (name . "web_search")
                                                       (input . ((query . "claude shannon birth date"))))
                                                      ((type . "web_search_tool_result")
                                                       (tool_use_id . "srvtoolu_01WYG3ziw53XMcoyKL4XcZmE")
                                                       (content . (((type . "web_search_result")
                                                                    (url . "https://en.wikipedia.org/wiki/Claude_Shannon")
                                                                    (title . "Claude Shannon - Wikipedia")
                                                                    (encrypted_content . "EqgfCioIARgBIiQ3YTAwMjY1Mi1mZjM5LTQ1NGUtODgxNC1kNjNjNTk1ZWI3Y...")
                                                                    (page_age . "April 30, 2025")))))
                                                      ((type . "text") (text . "Based on the search results, "))
                                                      ((type . "text") (text . "Claude Shannon was born on April 30, 1916, in Petoskey, Michigan")
                                                       (citations . (((type . "web_search_result_location")
                                                                      (url . "https://en.wikipedia.org/wiki/Claude_Shannon")
                                                                      (title . "Claude Shannon - Wikipedia")
                                                                      (cited_text . "Claude Elwood Shannon (April 30, 1916 – February 24, 2001) was an American mathematician, electrical engineer, computer scientist, cryptographer and i...")
                                                                      (encrypted_index . "Eo8BCioIAhgBIiQyYjQ0OWJmZi1lNm.."))))))))))

    ;; Citations immediately after tool result
    (:name "citations-after-tool-result"
           :markdown "## USER:

What's the current weather?

## SERVER TOOL USE:

Name: web_search
ID: srvtoolu_456

### query

<tool.srvtoolu_456>
current weather
</tool.srvtoolu_456>

## SERVER TOOL RESULT:

ID: srvtoolu_456

<tool.srvtoolu_456>
[
  {
    \"type\": \"web_search_result\",
    \"url\": \"https://weather.com\",
    \"title\": \"Weather.com\",
    \"encrypted_content\": \"abc123...\",
    \"page_age\": \"Today\"
  }
]
</tool.srvtoolu_456>

## ASSISTANT:

<cite>It's currently sunny and 75°F</cite>

## CITATIONS:

### https://weather.com

Title: Weather.com
Cited text: Currently sunny with a temperature of 75 degrees Fahrenheit...
Encrypted index: xyz789"
           :dialog (((role . "user") (content . "What's the current weather?"))
                    ((role . "assistant") (content . (((type . "server_tool_use")
                                                       (id . "srvtoolu_456")
                                                       (name . "web_search")
                                                       (input . ((query . "current weather"))))
                                                      ((type . "web_search_tool_result")
                                                       (tool_use_id . "srvtoolu_456")
                                                       (content . (((type . "web_search_result")
                                                                    (url . "https://weather.com")
                                                                    (title . "Weather.com")
                                                                    (encrypted_content . "abc123...")
                                                                    (page_age . "Today")))))
                                                      ((type . "text")
                                                       (text . "It's currently sunny and 75°F")
                                                       (citations . (((type . "web_search_result_location")
                                                                      (url . "https://weather.com")
                                                                      (title . "Weather.com")
                                                                      (cited_text . "Currently sunny with a temperature of 75 degrees Fahrenheit...")
                                                                      (encrypted_index . "xyz789"))))))))))

    ;; Multiple citations in same text
    (:name "citations-multiple"
           :markdown "## USER:

Tell me about Einstein and Newton

## SERVER TOOL USE:

Name: web_search
ID: srvtoolu_789

### query

<tool.srvtoolu_789>
Einstein Newton physics
</tool.srvtoolu_789>

## SERVER TOOL RESULT:

ID: srvtoolu_789

<tool.srvtoolu_789>
[
  {
    \"type\": \"web_search_result\",
    \"url\": \"https://physics.com/einstein\",
    \"title\": \"Einstein Biography\",
    \"encrypted_content\": \"def456...\",
    \"page_age\": \"Recent\"
  },
  {
    \"type\": \"web_search_result\",
    \"url\": \"https://physics.com/newton\",
    \"title\": \"Newton Biography\",
    \"encrypted_content\": \"ghi789...\",
    \"page_age\": \"Recent\"
  }
]
</tool.srvtoolu_789>

## ASSISTANT:

<cite>Einstein developed the theory of relativity</cite>

## CITATIONS:

### https://physics.com/einstein

Title: Einstein Biography
Cited text: Albert Einstein developed the theory of relativity in the early 20th century...
Encrypted index: def456

## ASSISTANT:

while <cite>Newton formulated the laws of motion</cite>.

## CITATIONS:

### https://physics.com/newton

Title: Newton Biography
Cited text: Isaac Newton formulated the three laws of motion...
Encrypted index: ghi789"
           :dialog (((role . "user") (content . "Tell me about Einstein and Newton"))
                    ((role . "assistant") (content . (((type . "server_tool_use")
                                                       (id . "srvtoolu_789")
                                                       (name . "web_search")
                                                       (input . ((query . "Einstein Newton physics"))))
                                                      ((type . "web_search_tool_result")
                                                       (tool_use_id . "srvtoolu_789")
                                                       (content . (((type . "web_search_result")
                                                                    (url . "https://physics.com/einstein")
                                                                    (title . "Einstein Biography")
                                                                    (encrypted_content . "def456...")
                                                                    (page_age . "Recent"))
                                                                   ((type . "web_search_result")
                                                                    (url . "https://physics.com/newton")
                                                                    (title . "Newton Biography")
                                                                    (encrypted_content . "ghi789...")
                                                                    (page_age . "Recent")))))
                                                      ((type . "text")
                                                       (text . "Einstein developed the theory of relativity")
                                                       (citations . ((
                                                                      (encrypted_index . "def456")
                                                                      (cited_text . "Albert Einstein developed the theory of relativity in the early 20th century...")
                                                                      (title . "Einstein Biography")
                                                                      (type . "web_search_result_location")
                                                                      (url . "https://physics.com/einstein")
                                                                      ))))
                                                      ((type . "text") (text . "while "))
                                                      ((type . "text")
                                                       (text . "Newton formulated the laws of motion")
                                                       (citations . ((
                                                                      (encrypted_index . "ghi789")
                                                                      (cited_text . "Isaac Newton formulated the three laws of motion...")
                                                                      (title . "Newton Biography")
                                                                      (type . "web_search_result_location")
                                                                      (url . "https://physics.com/newton")
                                                                      ))))
                                                      ((type . "text") (text . ".")))))))
    ))

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

;; Main test suite
(ert-deftest greger-parser-test-markdown-to-dialog ()
  "Test converting markdown to dialog format."
  (dolist (test-case greger-parser-test-cases)
    (let ((name (plist-get test-case :name))
          (markdown (plist-get test-case :markdown))
          (expected-dialog (plist-get test-case :dialog)))
      (message "Testing markdown-to-dialog for: %s" name)
      (let ((actual-dialog (greger-parser-parse-dialog-messages-only markdown)))
        (should (greger-parser-test--dialog-equal expected-dialog actual-dialog))))))

(ert-deftest greger-parser-test-roundtrip ()
  "Test that markdown -> dialog -> markdown preserves structure."
  (dolist (test-case greger-parser-test-cases)
    (let ((name (plist-get test-case :name))
          (original-markdown (plist-get test-case :markdown)))
      (message "Testing roundtrip for: %s" name)
      (let* ((dialog (greger-parser-parse-dialog-messages-only original-markdown))
             (roundtrip-markdown (greger-parser-dialog-to-markdown dialog))
             (roundtrip-dialog (greger-parser-parse-dialog-messages-only roundtrip-markdown)))
        ;; The dialog should be structurally equivalent after round-trip
        (should (= (length dialog) (length roundtrip-dialog)))
        (should (greger-parser-test--dialog-equal dialog roundtrip-dialog))))))

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
    (let ((parsed (greger-parser-parse-dialog-messages-only tool-use-markdown)))
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
    (let ((parsed (greger-parser-parse-dialog-messages-only tool-result-markdown)))
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
    (let ((parsed (greger-parser-parse-dialog-messages-only thinking-markdown)))
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
              (progn (greger-parser-parse-dialog-messages-only "## TOOL USE:\n\nMalformed") t)
            (error nil)))
  (should (condition-case err
              (progn (greger-parser-parse-dialog-messages-only "## TOOL RESULT:\n\nMalformed") t)
            (error nil))))

(ert-deftest greger-parser-test-edge-cases ()
  "Test edge cases like empty content, whitespace handling."
  ;; Empty content
  (should (equal '() (greger-parser-parse-dialog-messages-only "")))
  (should (equal '() (greger-parser-parse-dialog-messages-only "\n\n  ")))

  ;; Only whitespace in sections - should return empty list
  (should (equal '() (greger-parser-parse-dialog-messages-only "## USER:\n\n\n\n")))

  ;; Multiple consecutive newlines
  (let ((result (greger-parser-parse-dialog-messages-only "## USER:\n\n\n\nHello\n\n\n\n## ASSISTANT:\n\n\n\nHi")))
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
      (greger-parser-parse-dialog-messages-only large-markdown)
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
    (let ((parsed (greger-parser-parse-dialog-messages-only complex-markdown)))
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
    (let ((parsed (greger-parser-parse-dialog-messages-only markdown)))
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
    (let ((parsed (greger-parser-parse-dialog-messages-only markdown)))
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
    (let ((parsed (greger-parser-parse-dialog-messages-only markdown)))
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
    (let ((parsed (greger-parser-parse-dialog-messages-only markdown)))
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
    (let ((parsed (greger-parser-parse-dialog-messages-only markdown)))
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

          (let ((parsed (greger-parser-parse-dialog-messages-only markdown)))
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

          (let ((parsed (greger-parser-parse-dialog-messages-only markdown)))
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
    (let ((parsed (greger-parser-parse-dialog-messages-only markdown)))
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

          (let ((parsed (greger-parser-parse-dialog-messages-only markdown)))
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

          (let ((parsed (greger-parser-parse-dialog-messages-only markdown)))
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

          (let ((parsed (greger-parser-parse-dialog-messages-only markdown)))
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

          (let ((parsed (greger-parser-parse-dialog-messages-only markdown)))
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

          (let ((parsed (greger-parser-parse-dialog-messages-only markdown)))
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

          (let ((parsed (greger-parser-parse-dialog-messages-only markdown)))
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

          (let ((parsed (greger-parser-parse-dialog-messages-only markdown)))
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
    (let ((parsed (greger-parser-parse-dialog-messages-only markdown)))
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
    (let ((parsed (greger-parser-parse-dialog-messages-only markdown)))
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
    (let ((parsed (greger-parser-parse-dialog-messages-only markdown)))
      (should (= 1 (length parsed)))
      (let ((generated-markdown (greger-parser-dialog-to-markdown parsed)))
        (should (string= expected generated-markdown))))))

;; Tests for safe-shell-commands metadata
(ert-deftest greger-parser-test-safe-shell-commands-basic ()
  "Test basic safe-shell-commands parsing."
  (let ((markdown "## SYSTEM:

<safe-shell-commands>
ls -la
pwd
echo hello
</safe-shell-commands>")
        (expected-metadata '(:safe-shell-commands ("ls -la" "pwd" "echo hello"))))
    (let ((result (greger-parser-parse-dialog markdown)))
      ;; Should have one system message with the auto-generated descriptive text
      (should (= 1 (length (plist-get result :messages))))
      (let ((system-msg (car (plist-get result :messages))))
        (should (string= "system" (alist-get 'role system-msg)))
        (let ((content (alist-get 'content system-msg)))
          (should (string= "You can run arbitrary shell commands with the shell-command tool, but the following are safe shell commands that will run without requiring user confirmation:

* `ls -la`
* `pwd`
* `echo hello`"
                           content))))
      (should (equal expected-metadata (plist-get result :metadata))))))

(ert-deftest greger-parser-test-safe-shell-commands-with-system-content ()
  "Test safe-shell-commands with other system content."
  (let ((markdown "## SYSTEM:

You are a helpful assistant.

<safe-shell-commands>
ls
pwd
</safe-shell-commands>

Please be careful."))
    (let ((result (greger-parser-parse-dialog markdown)))
      ;; Should have a system message with combined content
      (should (= 1 (length (plist-get result :messages))))
      (should (string= "system" (alist-get 'role (car (plist-get result :messages)))))
      ;; Should also have metadata since safe-shell-commands can coexist with content
      (should (equal '(:safe-shell-commands ("ls" "pwd")) (plist-get result :metadata)))
      ;; System message should contain the original content and the auto-generated safe commands text
      (let ((system-content (alist-get 'content (car (plist-get result :messages)))))
        (should (string= "You are a helpful assistant.



Please be careful.

You can run arbitrary shell commands with the shell-command tool, but the following are safe shell commands that will run without requiring user confirmation:

* `ls`
* `pwd`"
                         system-content))))))

(ert-deftest greger-parser-test-safe-shell-commands-only-once ()
  "Test that only one safe-shell-commands block is allowed."
  (let ((markdown "## SYSTEM:

<safe-shell-commands>
ls
pwd
</safe-shell-commands>

<safe-shell-commands>
echo hello
</safe-shell-commands>"))
    (let ((result (greger-parser-parse-dialog markdown)))
      ;; Should extract the first one found
      (should (equal '(:safe-shell-commands ("ls" "pwd")) (plist-get result :metadata))))))

(ert-deftest greger-parser-test-safe-shell-commands-empty-lines ()
  "Test safe-shell-commands with empty lines and whitespace."
  (let ((markdown "## SYSTEM:

<safe-shell-commands>

ls -la

pwd

echo hello

</safe-shell-commands>"))
    (let ((result (greger-parser-parse-dialog markdown)))
      ;; Should have one system message with the auto-generated descriptive text
      (should (= 1 (length (plist-get result :messages))))
      (let ((system-msg (car (plist-get result :messages))))
        (should (string= "system" (alist-get 'role system-msg)))
        (let ((content (alist-get 'content system-msg)))
          (should (string= "You can run arbitrary shell commands with the shell-command tool, but the following are safe shell commands that will run without requiring user confirmation:

* `ls -la`
* `pwd`
* `echo hello`"
                           content))))
      (should (equal '(:safe-shell-commands ("ls -la" "pwd" "echo hello"))
                     (plist-get result :metadata))))))

(ert-deftest greger-parser-test-safe-shell-commands-not-in-system ()
  "Test that safe-shell-commands outside SYSTEM section are ignored."
  (let ((markdown "## USER:

<safe-shell-commands>
ls -la
</safe-shell-commands>

What files are here?"))
    (let ((result (greger-parser-parse-dialog markdown)))
      ;; Should have no metadata
      (should (equal '() (plist-get result :metadata)))
      ;; Should have user message with the tag as regular content
      (should (= 1 (length (plist-get result :messages))))
      (should (string-match-p "<safe-shell-commands>"
                             (alist-get 'content (car (plist-get result :messages))))))))

(ert-deftest greger-parser-test-safe-shell-commands-in-code-block ()
  "Test that safe-shell-commands inside code blocks are not processed."
  (let ((markdown "## SYSTEM:

Here's an example:

```
<safe-shell-commands>
ls -la
</safe-shell-commands>
```

Don't process that."))
    (let ((result (greger-parser-parse-dialog markdown)))
      ;; Should have no metadata
      (should (equal '() (plist-get result :metadata)))
      ;; Should have system message with code block
      (should (= 1 (length (plist-get result :messages))))
      (should (string-match-p "<safe-shell-commands>"
                             (alist-get 'content (car (plist-get result :messages))))))))

(ert-deftest greger-parser-test-system-content-with-safe-commands-example ()
  "Test the exact example from the user: system content with safe-shell-commands."
  (let ((markdown "## SYSTEM:

you are a friendly assistant

<safe-shell-commands>
command1
command2
</safe-shell-commands>

## USER:

Hello"))
    (let ((result (greger-parser-parse-dialog markdown)))
      ;; Should have both system and user messages
      (should (= 2 (length (plist-get result :messages))))

      ;; Check system message - should contain both original content and auto-generated safe commands text
      (let ((system-msg (car (plist-get result :messages))))
        (should (string= "system" (alist-get 'role system-msg)))
        (let ((content (alist-get 'content system-msg)))
          (should (string= "you are a friendly assistant

You can run arbitrary shell commands with the shell-command tool, but the following are safe shell commands that will run without requiring user confirmation:

* `command1`
* `command2`"
                           content))))

      ;; Check user message
      (let ((user-msg (cadr (plist-get result :messages))))
        (should (string= "user" (alist-get 'role user-msg)))
        (should (string= "Hello" (alist-get 'content user-msg))))

      ;; Should have metadata with safe shell commands
      (should (equal '(:safe-shell-commands ("command1" "command2")) (plist-get result :metadata))))))

(provide 'test-greger-parser)

;;; test-greger-parser.el ends here
