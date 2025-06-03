# Safe Shell Commands Auto-Generation

The greger parser now automatically generates descriptive text for safe shell commands when parsing `<safe-shell-commands>` tags in system sections.

## Behavior

When you have a chat like this:

```markdown
## SYSTEM:

<safe-shell-commands>
bash script/package-lint
bash script/test --verbose
bash script/test --verbose --file test/test-parser.el
bash script/test --verbose --file test/test-greger.el
bash script/test --verbose --file test/test-greger-lib-lsp.el
</safe-shell-commands>
```

The parser will automatically generate the following system prompt:

```
You can run arbitrary shell commands with the shell-command tool, but the following are safe shell commands that will run without requiring user confirmation:

* `bash script/package-lint`
* `bash script/test --verbose`
* `bash script/test --verbose --file test/test-parser.el`
* `bash script/test --verbose --file test/test-greger.el`
* `bash script/test --verbose --file test/test-greger-lib-lsp.el`
```

## Combined Content

If you have both system content and safe shell commands:

```markdown
## SYSTEM:

You are a helpful assistant for an Emacs Lisp project.

<safe-shell-commands>
bash script/package-lint
bash script/test --verbose
</safe-shell-commands>
```

The parser will combine them:

```
You are a helpful assistant for an Emacs Lisp project.

You can run arbitrary shell commands with the shell-command tool, but the following are safe shell commands that will run without requiring user confirmation:

* `bash script/package-lint`
* `bash script/test --verbose`
```

## Metadata

The safe shell commands are still extracted as metadata and can be used by the shell-command tool to skip permission prompts for the listed commands.
