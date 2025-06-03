# Greger.el - Agentic coding in Emacs

Greger is a Claude chat interface with tool use. It can read and edit code, navigate code with LSP, download web pages, run shell commands, etc.

## Usage

Start a new Greger session with

```
M-x greger
```

### Keybindings

In greger buffers:

- `M-RET` - Send message with tool support
- `C-M-RET` - Send message without tools
- `C-; a` - Insert assistant tag
- `C-; u` - Insert user tag
- `C-; s` - Insert system tag
- `C-; i` - Insert include directive
- `C-; f` - Insert file include
- `C-; c` - Copy code block at point
- `C-; m` - Change AI model
- `TAB` - Toggle tool section visibility

## Installation

```emacs-lisp
(use-package greger
  :commands (greger)
  :config
  ;; Set your preferred model
  (setq greger-model 'claude-sonnet-4-20250514))
```

## Configuration

Configure your Claude API key:

```bash
export ANTHROPIC_API_KEY="your-claude-api-key"
```

Or set it in your Emacs configuration:

```elisp
(setenv "ANTHROPIC_API_KEY" "your-claude-api-key")
```

## Features

The chat buffer is a complete representation of all the messages in the conversation with LLM, and it can be edited as a normal markdown buffer.

Token caching is automatically enabled for cost efficiency.

### Available tools:

greger.el includes a set of tool out of the box:

- File operations (read, write, edit files)
- Web browsing and content extraction
- LSP integration (rename symbols, find definitions/references)
- Git operations
- Shell command execution
- Directory operations

Tools can be easily extended by defining new functions and registering them with the tool system.

### Including Files and Code

You can include file contents in your conversations:

```markdown
<include>/path/to/file.txt</include>
<include code>/path/to/source.py</include>
```

Note that the included files will be updated on each run, so it's often better to write out a file path and let Greger read the file itself.

### Safe Shell Commands

Greger automatically generates descriptive text for safe shell commands that don't require user confirmation. When you define safe commands in a system section:

```markdown
## SYSTEM:

<safe-shell-commands>
bash script/package-lint
bash script/test --verbose
bash script/test --verbose --file test/test-parser.el
</safe-shell-commands>
```

The parser automatically generates this system prompt:

```
You can run arbitrary shell commands with the shell-command tool, but the following are safe shell commands that will run without requiring user confirmation:

* `bash script/package-lint`
* `bash script/test --verbose`
* `bash script/test --verbose --file test/test-parser.el`
```

You can also combine safe shell commands with other system content:

```markdown
## SYSTEM:

You are a helpful assistant for an Emacs Lisp project.

<safe-shell-commands>
bash script/package-lint
bash script/test --verbose
</safe-shell-commands>
```

This will append the safe commands description to your existing system prompt. The commands listed in `<safe-shell-commands>` are extracted as metadata and used by the shell-command tool to skip permission prompts.

### Available models

Greger supports the latest Claude models:

- **claude-sonnet-4-20250514** - Fast, high-quality model for most tasks
- **claude-opus-4-20250514** - Most capable model for complex reasoning

## Development

### Running Tests

The project includes comprehensive ERT tests for all major components. Use the test script to run them:

```bash
# Run all tests
./script/test

# Run tests for a specific file
./script/test -f test-greger-parser.el

# Run tests with verbose output
./script/test --verbose

# Show help
./script/test --help
```

### Package Linting

Check code quality with package-lint:

```bash
bash script/package-lint
```

## License

MIT
