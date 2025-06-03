# Greger - Chat with Language Models

Greger is an Emacs package that provides an interface for interacting with AI language models including Claude, OpenAI GPT, Google Gemini, and more. It features a rich chat interface with support for tool use, code execution, and streaming responses.

## Features

- **Multiple AI Providers**: Support for Claude, OpenAI, Google Gemini, Groq, Ollama, and Replicate models
- **Tool Integration**: Extensible tool system with built-in tools for file operations, web browsing, LSP integration, and more
- **Rich Chat Interface**: Markdown-based chat format with syntax highlighting and collapsible tool sections
- **Streaming Responses**: Real-time streaming of AI responses
- **Code Integration**: Easy inclusion of files, code blocks, and buffer contents in conversations
- **Customizable**: Extensive customization options for models, prompts, and behavior

## Installation

Install from MELPA:

```elisp
M-x package-install RET greger RET
```

Or manually:

```elisp
(use-package greger
  :commands (greger)
  :config
  ;; Set your preferred model
  (setq greger-model 'claude/claude-sonnet-4-20250514))
```

## Configuration

Configure API keys for your preferred providers:

```elisp
;; For Claude
(setq claude-api-key "your-claude-api-key")

;; For OpenAI
(setq openai-api-key "your-openai-api-key")

;; For Google Gemini
(setq google-api-key "your-google-api-key")
```

## Usage

Start a new chat session:

```
M-x greger
```

### Key Bindings

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

### Including Files and Code

You can include file contents in your conversations:

```markdown
<include>/path/to/file.txt</include>
<include code>/path/to/source.py</include>
```

## Available Models

Greger supports many models across different providers:

- **Claude**: claude-3-haiku, claude-3-opus, claude-3-5-sonnet, claude-sonnet-4, claude-opus-4
- **OpenAI**: gpt-4o, gpt-4o-mini, gpt-4-turbo, o1-preview
- **Google**: gemini-pro
- **Groq**: llama3-8b-8192, llama3-70b-8192, mixtral-8x7b-32768
- **Ollama**: Various local models
- **Replicate**: Meta Llama models, Snowflake Arctic

## Tool System

Greger includes a powerful tool system that allows AI models to perform actions:

- File operations (read, write, edit files)
- Web browsing and content extraction
- LSP integration (rename symbols, find definitions/references)
- Git operations
- Shell command execution
- Directory operations

Tools can be easily extended by defining new functions and registering them with the tool system.

## Development

### Running Tests

The project includes comprehensive ERT tests for all major components. Use the test script to run them:

```bash
# Run all tests
bash script/test

# Run tests for a specific file
bash script/test -f test-greger-agent.el

# Run tests with verbose output
bash script/test --verbose

# Show help
bash script/test --help
```

Test files include:
- `test-greger-agent.el` - Agent and tool execution tests
- `test-greger-parser.el` - Markdown parsing and dialog conversion tests
- `test-greger-stdlib.el` - Standard library function tests
- `test-greger-lib-lsp.el` - LSP integration tests
- `test-greger-tools.el` - Tool registration and parameter handling tests

### Package Linting

Check code quality with package-lint:

```bash
bash script/package-lint
```

## License

Copyright (C) 2023 Andreas Jansson

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.
