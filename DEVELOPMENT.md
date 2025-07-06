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

### Grammar Development with Git Branches

When developing changes to the greger grammar, you can configure greger.el to install from a specific git branch instead of the default 'main' branch:

#### Method 1: Using the helper function

1. Run `M-x greger-set-grammar-branch` and enter your development branch name (e.g., `feature-eval-tags`)

2. Run `M-x greger-install-grammar` to install the grammar from your branch

#### Method 2: Manual configuration

Add this to your init.el or evaluate interactively:

```elisp
(setq greger-local-grammar-path "your-branch-name")
```

#### Reverting to Main Branch

To go back to using the main branch:

- Run `M-x greger-set-grammar-branch` and leave the branch name empty
- Or set `greger-local-grammar-path` to `nil`

#### Development Workflow

1. Create a feature branch in the greger-grammar repository
2. Push your changes to that branch
3. Set greger.el to use your branch with `M-x greger-set-grammar-branch`
4. Run `M-x greger-install-grammar` to install from your branch
5. Test your changes in greger.el

This approach ensures you're always working with properly compiled grammars and can easily switch between different development branches.

