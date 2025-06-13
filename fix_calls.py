#!/usr/bin/env python3

import re

def fix_greger_calls(content):
    """Fix greger-tools-execute calls to use keyword arguments."""
    
    # Simple pattern to fix the basic structure
    # Look for: (greger-tools-execute "tool-name" args callback buffer)
    # Replace with: (greger-tools-execute :tool-name "tool-name" :args args :callback callback :buffer buffer)
    
    lines = content.split('\n')
    result_lines = []
    i = 0
    
    while i < len(lines):
        line = lines[i]
        
        # Check if this line starts a greger-tools-execute call
        if re.search(r'\s*\(greger-tools-execute\s+"[^"]+"\s*$', line):
            # Found start of old-style call, collect the full s-expression
            call_lines = [line]
            depth = line.count('(') - line.count(')')
            i += 1
            
            while depth > 0 and i < len(lines):
                call_lines.append(lines[i])
                depth += lines[i].count('(') - lines[i].count(')')
                i += 1
            
            # Now try to parse and fix this call
            full_call = '\n'.join(call_lines)
            fixed_call = fix_single_call(full_call)
            result_lines.extend(fixed_call.split('\n'))
        else:
            result_lines.append(line)
            i += 1
    
    return '\n'.join(result_lines)

def fix_single_call(call_text):
    """Fix a single greger-tools-execute call."""
    
    # Extract the indentation from the first line
    first_line = call_text.split('\n')[0]
    indent = re.match(r'^(\s*)', first_line).group(1)
    
    # Try to match the old pattern
    pattern = r'\(greger-tools-execute\s+"([^"]+)"\s+(.*?)\s+\(lambda\s+\(([^)]*)\)\s+(.*?)\)\s+([^)]+)\)'
    match = re.search(pattern, call_text, re.DOTALL)
    
    if match:
        tool_name = match.group(1)
        args = match.group(2).strip()
        lambda_args = match.group(3)
        lambda_body = match.group(4).strip()
        buffer = match.group(5).strip()
        
        # Create the new format
        return f'''{indent}(greger-tools-execute :tool-name "{tool_name}"
{indent}                      :args {args}
{indent}                      :callback (lambda ({lambda_args}) {lambda_body})
{indent}                      :buffer {buffer})'''
    
    # If we couldn't parse it, return unchanged
    return call_text

# Read the file
with open('test/test-greger-tools.el', 'r') as f:
    content = f.read()

# Fix the calls
fixed_content = fix_greger_calls(content)

# Write back
with open('test/test-greger-tools.el', 'w') as f:
    f.write(fixed_content)

print("Fixed greger-tools-execute calls")
