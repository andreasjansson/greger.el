# Interactive Input Handling in Greger Shell Commands

## Overview

The `shell-command` tool in greger-stdlib now supports interactive input handling when using the `use-vterm` option. This allows shell commands that prompt for user input to work seamlessly by either:
1. **Automated responses via Claude API** - Claude analyzes the context and provides appropriate responses
2. **User input via minibuffer** - When Claude cannot or should not respond, prompts are presented in the Emacs minibuffer

## Claude Integration

### Automatic Response Generation
When an interactive prompt is detected, the system:
1. Sends the command context and prompt to Claude
2. Claude evaluates whether it can provide an appropriate response
3. If Claude responds with "USER", the user is prompted in the minibuffer
4. If Claude provides a response, it's automatically sent to the shell

### Configuration
```elisp
;; Enable/disable Claude integration (default: t)
(setq greger-stdlib-claude-interactive-input t)

;; Set timeout for Claude API calls (default: 10.0 seconds)
(setq greger-stdlib-claude-interactive-timeout 15.0)
```

### Examples of Claude-Handled Prompts
- Software installation confirmations: "Install package X? [y/n]"
- License agreements: "Accept license? [y/n]"
- Default value confirmations: "Use default configuration? [y/n]"
- Common yes/no questions with clear context

### Examples of User-Handled Prompts
- Personal information requests: "Enter your name:"
- Password prompts: "Password:" (always handled by user)
- Complex configuration choices
- Ambiguous questions without clear context

## How It Works

### Detection
The system automatically detects interactive prompts by monitoring the vterm buffer for common prompt patterns:

- Lines ending with colons (`:`)
- Lines ending with question marks (`?`)
- Lines ending with closing brackets (`]`)
- Lines ending with `>>`
- Lines containing keywords like "Password:", "Enter", "y/n", "Y/N", "Press", "Continue", "Confirm", "Type", "Input", "Select"

### User Interaction
When an interactive prompt is detected:
1. The command execution pauses
2. A minibuffer prompt appears showing the shell prompt text
3. The user enters their response in the minibuffer
   - For password prompts (containing "password", "Password", or "PASS"), input is hidden using `read-passwd`
   - For regular prompts, input is shown normally using `read-from-minibuffer`
4. The response is sent to the shell command
5. The system continues monitoring for additional prompts

### Automatic Cleanup
The system automatically:
- Cancels prompt monitoring when the command completes
- Handles timeouts appropriately
- Cleans up resources when commands are cancelled
- Prevents auto-exit when interactive prompts are detected

## Usage Examples

### Basic Interactive Command
```elisp
(greger-stdlib--run-shell-command-with-vterm
 "read -p 'Enter your name: ' name && echo \"Hello, $name!\""
 default-directory
 (lambda (output error)
   (if error
       (message "Error: %s" error)
     (message "Output: %s" output)))
 30 ;; timeout
 nil ;; enable-environment
 nil ;; streaming-callback
 )
```

### With Streaming Callback
```elisp
(greger-stdlib--run-shell-command-with-vterm
 "read -p 'Enter password: ' -s pass && echo \"Password entered\""
 default-directory
 (lambda (output error)
   (message "Final result: %s" (or output error)))
 60 ;; timeout
 nil ;; enable-environment
 (lambda (streaming-output)
   (message "Streaming: %s" streaming-output))
 )
```

### Claude-Enabled Interactive Commands
```elisp
;; Claude will automatically respond to installation prompts
(greger-stdlib--run-shell-command-with-vterm
 "npm install some-package"
 default-directory
 (lambda (output error)
   (message "Installation result: %s" (or output error)))
 120 ;; timeout
 nil ;; enable-environment
 nil ;; streaming-callback
 )

;; Claude will evaluate context and respond appropriately
(greger-stdlib--run-shell-command-with-vterm
 "echo 'Configuring system...'; read -p 'Use default settings? [y/n] ' choice; echo \"Choice: $choice\""
 default-directory
 (lambda (output error)
   (message "Configuration result: %s" (or output error)))
 60 ;; timeout
 nil ;; enable-environment
 nil ;; streaming-callback
 )
```

### Multiple Prompts
```elisp
(greger-stdlib--run-shell-command-with-vterm
 "read -p 'Name: ' name; read -p 'Age: ' age; echo \"$name is $age years old\""
 default-directory
 (lambda (output error)
   (message "Result: %s" (or output error)))
 60 ;; timeout
 nil ;; enable-environment
 nil ;; streaming-callback
 )
```

## Common Use Cases

### Software Installation
Interactive package managers that prompt for confirmation:
```bash
apt install package-name
# Will prompt: "Do you want to continue? [Y/n]"
```

### Configuration Scripts
Scripts that ask for user preferences:
```bash
./configure.sh
# Will prompt for various configuration options
```

### Authentication
Commands that require password input:
```bash
sudo some-command
# Will prompt: "Password:"
```

### Database Operations
Interactive database tools:
```bash
mysql -u user -p
# Will prompt: "Enter password:"
```

## Technical Details

### Prompt Detection Algorithm
The system uses a comprehensive regex-based approach to detect interactive prompts:

1. **Pattern Matching**: Checks for common prompt endings and keywords
2. **Buffer Monitoring**: Continuously monitors the vterm buffer for changes
3. **Timeout Handling**: Respects command timeouts even during interactive input
4. **State Management**: Tracks prompt state to avoid duplicate handling

### Performance Considerations
- Prompt detection runs on a timer (0.5 second intervals) to avoid excessive CPU usage
- The system only monitors for prompts when commands are actively running
- Timers are properly cleaned up to prevent memory leaks

### Error Handling
- Timeout errors are properly handled even during interactive input
- User can cancel commands at any time
- Malformed prompts are gracefully ignored
- Buffer cleanup is guaranteed even in error conditions

## Testing

Use the provided test script to verify functionality:

```bash
# Load the test script in Emacs
(load "/tmp/test_interactive_greger.el")

# Run tests
M-x test-interactive-greger
M-x test-interactive-with-streaming
```

## Limitations

1. **Batch Mode**: Interactive input does not work in Emacs batch mode
2. **Complex Prompts**: Very complex or non-standard prompts may not be detected
3. **Performance**: Continuous monitoring adds slight overhead
4. **Concurrency**: Only one interactive prompt can be handled at a time per command

## Future Enhancements

- Support for more complex prompt patterns
- Configurable prompt detection rules
- Support for arrow key navigation in prompts
- Integration with Emacs password management
- History of previous responses
