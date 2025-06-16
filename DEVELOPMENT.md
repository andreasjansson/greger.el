# Greger.el development

## Architecture

### Dialog representation

The user facing representation is markdown, which is parsed by [greger-grammar](https://github.com/andreasjansson/greger-grammar) into a [tree-sitter](https://tree-sitter.github.io/tree-sitter/) parse tree.

For example, this markdown

```markdown
# SYSTEM

You are a helpful assistant.

# USER

Hello

# ASSISTANT

Hi there! How can I help you today?
```

has this parse tree

```lisp
(source_file
  (system
    (system_header)
    (text))
  (user
    (user_header)
    (text))
  (assistant
    (assistant_header)
    (text)))
```

The [test/corpus](https://github.com/andreasjansson/greger-grammar/tree/main/test/corpus) directory has plenty of examples of how markdown is transformed into parse trees.

The parse tree is then transformed into an internal "dialog" representation by `(greger-parser-markdown-to-dialog (markdown-str))`. The above markdown becomes the following dialog:

```emacs-lisp
(((role . "system")
  (content . "You are a helpful assistant."))
 ((role . "user")
  (content . "Hello"))
 ((role . "assistant")
  (content
   ((text . "Hi there! How can I help you today?")
    (type . "text")))))
```

The dialog representation is almost identical to the Claude [messages format](https://docs.anthropic.com/en/api/messages), except the system message is embedded in the dialog. Before sending the request to Claude, we therefore have to extract the system message from the dialog.

### Claude streaming

All requests to Claude use streaming, but the output is not always streamed back to the user. Streaming is enabled for assistant text and thinking messages, but other types of messages, e.g. tool use, are first accumulated and then outputted to the Greger buffer.

Streaming requests are sent, and data is accumulted and handled with callbacks in `greger-client.el`.

## Local development

Greger.el uses [Eldev](https://emacs-eldev.github.io/eldev/). You need to install Eldev before running tests.

### Run tests locally

```
$ eldev test
```

### Run [Melpazoid](https://github.com/riscy/melpazoid) locally

```
$ ./script/melpazoid
```

