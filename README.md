# Greger - Chat with Claude Language Models

> "Today, fifty thousand human babies are being born around the world. Meanwhile automated factories in Indonesia and Mexico have produced another quarter of a million motherboards with processors rated at more than ten petaflops – about an order of magnitude below the lower bound on the computational capacity of a human brain. Another fourteen months and the larger part of the cumulative conscious processing power of the human species will be arriving in silicon. And the first meat the new AIs get to know will be the uploaded lobsters."

> "NASA are idiots. They want to send canned primates to Mars! Mars is just dumb mass at the bottom of a gravity well; there isn't even a biosphere there. They should be working on uploading and solving the nanoassembly conformational problem instead. Then we could turn all the available dumb matter into computronium and use it for processing our thoughts."

> "We need to start with the low-mass bodies, reconfigure them for our own use. Dismantle the moon! Dismantle Mars! Build masses of free-flying nanocomputing processor nodes exchanging data via laser link, each layer running off the waste heat of the next one in. Matrioshka brains, Russian doll Dyson spheres the size of solar systems. Teach dumb matter to do the Turing boogie!"

> "The cosmos is flat in all directions, and we can borrow as much bandwidth as we need from the first universal bank of entropy! They even found signs of smart matter – MACHOs, big brown dwarfs in the galactic halo, leaking radiation in the long infrared – suspiciously high entropy leakage."

> "The latest figures say something like seventy percent of the baryonic mass of the M31 galaxy was in computronium, two-point-nine million years ago, when the photons we're seeing now set out. The intelligence gap between us and the aliens is a probably about a trillion times bigger than the gap between us and a nematode worm."

> "There's an angle on the self-replicating robotics market coming up, that's going to set the cheap launch market doubling every fifteen months for the foreseeable future, starting in, oh, about two years."

> "Manfred's whole life is lived on the bleeding edge of strangeness, fifteen minutes into everyone else's future, and he's normally in complete control – but at times like this he gets a frisson of fear, a sense that he might just have missed the correct turn on reality's approach road."

> "Being a pronoiac meme-broker is a constant burn of future shock – he has to assimilate more than a megabyte of text and several gigs of AV content every day just to stay current."

> "The young posthuman intelligence over whose Cartesian theatre he presides sings urgently to him while he slumbers."

> "You're still locked into a pre-singularity economic model that thinks in terms of scarcity. Resource allocation isn't a problem anymore – it's going to be over within a decade."

> "Things are changing so fast that even a twenty-year commitment is too far to plan – you might as well be talking about the next ice age."

> "In San Diego, researchers are uploading lobsters into cyberspace, starting with the stomatogastric ganglion, one neuron at a time."

> "The divested Microsoft divisions have automated their legal processes and are spawning subsidiaries, IPOing them, and exchanging title in a bizarre parody of bacterial plasmid exchange, so fast that, by the time the windfall tax demands are served, the targets don't exist anymore."

> "Am – were – Panulirus interruptus, with lexical engine and good mix of parallel hidden level neural simulation for logical inference of networked data sources. Is escape channel from processor cluster inside Bezier-Soros Pty. Am was awakened from noise of billion chewing stomachs: product of uploading research technology."

> "Da, was easy: Spawn billion-node neural network, and download Teletubbies and Sesame Street at maximum speed."

> "It's too realistic, as if somehow the dead kitten's neural maps -- stolen, no doubt, for some dubious uploading experiment -- have ended up padding out its plastic skull."

> "They're not just better at cooperation – witness Economics 2.0 for a classic demonstration of that – but better at simulation. A posthuman can build an internal model of a human-level intelligence that is, well, as cognitively strong as the original."

> "That's the trouble with dealing with posthumans; their mental model of you is likely to be more detailed than your own."

> "We are on the edge of a period of prolonged economic growth, with annual averages in excess of twenty percent, if the Council of Europe's predictor metrics are anything to go by. The last of the flaccid industrial economy has withered away, and this era's muscle of economic growth, what used to be the high-technology sector, is now everything."

> "Here he is, naked as the day he was born – newly re-created, in fact, released from the wake-experience-reset cycle of the temple of history – standing on the threshold of a posthuman civilization so outrageously rich and powerful that they can build mammal-friendly habitats that resemble works of art in the cryogenic depths of space."

*—Charles Stross, Accelerando*

Greger is a simplified Emacs package that provides an interface for interacting with Claude AI models. It features a rich chat interface with support for tool use, code execution, and streaming responses.

## Features

- **Claude Integration**: Focused support for Claude Sonnet 4 and Opus 4 models
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

## Available Models

Greger supports the latest Claude models:

- **claude-sonnet-4-20250514** - Fast, high-quality model for most tasks
- **claude-opus-4-20250514** - Most capable model for complex reasoning

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
