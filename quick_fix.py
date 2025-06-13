#!/usr/bin/env python3

import re

# Read the file
with open('test/test-greger-tools.el', 'r') as f:
    content = f.read()

# Simple pattern: replace calls that start with greger-tools-execute "toolname"
# and don't already have :tool-name
def fix_call(match):
    tool_name = match.group(1)
    return f'greger-tools-execute :tool-name "{tool_name}"'

# Replace the tool-name pattern first
content = re.sub(r'greger-tools-execute "([^"]+)"', fix_call, content)

# Now fix the argument patterns step by step
# Pattern: :tool-name "name" ARGS CALLBACK BUFFER
def fix_args(match):
    before = match.group(1)
    args = match.group(2)
    callback = match.group(3) 
    buffer = match.group(4)
    
    return f'{before}:args {args}\n{before}:callback {callback}\n{before}:buffer {buffer})'

# Look for the pattern where we have tool-name followed by args
lines = content.split('\n')
output_lines = []
i = 0

while i < len(lines):
    line = lines[i]
    
    # Look for lines with greger-tools-execute :tool-name
    if 'greger-tools-execute :tool-name' in line and ':args' not in line:
        # This starts a call that needs fixing
        # Collect the entire call
        call_lines = [line]
        indent = len(line) - len(line.lstrip())
        base_indent = ' ' * indent
        
        # Get the next few lines (args, callback, buffer)
        j = i + 1
        while j < len(lines) and j < i + 4:  # Max 4 lines for a call
            call_lines.append(lines[j])
            j += 1
        
        # Try to parse the structure
        full_call = '\n'.join(call_lines)
        
        # Simple pattern: look for the basic structure
        # Line 1: greger-tools-execute :tool-name "name"
        # Line 2: args
        # Line 3: callback  
        # Line 4: buffer
        
        if len(call_lines) >= 4:
            tool_line = call_lines[0]
            args_line = call_lines[1].strip()
            callback_line = call_lines[2].strip()
            buffer_line = call_lines[3].strip()
            
            # Reconstruct with proper keywords
            new_lines = [
                tool_line,
                base_indent + f'                      :args {args_line}',
                base_indent + f'                      :callback {callback_line}',
                base_indent + f'                      :buffer {buffer_line}'
            ]
            
            output_lines.extend(new_lines)
            i = j
        else:
            output_lines.append(line)
            i += 1
    else:
        output_lines.append(line)
        i += 1

# Write the result
with open('test/test-greger-tools.el', 'w') as f:
    f.write('\n'.join(output_lines))

print("Fixed greger-tools-execute calls")
