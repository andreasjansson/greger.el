#!/usr/bin/env python3
import re

def fix_tool_calls(content):
    """Fix greger-tools-execute calls to use keyword arguments"""
    
    # Pattern to match the old format
    # greger-tools-execute :tool-name "tool-name" <args> <callback> <buffer>
    pattern = r'(\s*)\(greger-tools-execute\s+:tool-name\s+"([^"]+)"\s+'
    
    lines = content.split('\n')
    result_lines = []
    i = 0
    
    while i < len(lines):
        line = lines[i]
        
        # Look for lines that contain greger-tools-execute calls that need fixing
        if 'greger-tools-execute :tool-name "' in line and ':args' not in line:
            # This is a call that needs fixing
            # Find the full call (may span multiple lines)
            call_lines = []
            paren_depth = 0
            j = i
            
            # Collect all lines of the call
            while j < len(lines):
                current_line = lines[j]
                call_lines.append(current_line)
                
                # Count parentheses to find the end of the call
                paren_depth += current_line.count('(') - current_line.count(')')
                
                # If we've closed all parentheses, we're done
                if paren_depth <= 0:
                    break
                j += 1
            
            # Now parse the full call
            full_call = '\n'.join(call_lines)
            
            # Extract components using regex
            match = re.search(r'greger-tools-execute\s+:tool-name\s+"([^"]+)"\s+', full_call)
            if match:
                tool_name = match.group(1)
                
                # Find the parts after the tool name
                after_tool_name = full_call[match.end():]
                
                # Try to parse the arguments
                # The pattern is: args callback buffer
                # We need to be careful about nested structures
                
                # For now, let's do a simple approach:
                # Replace the call with a template and fill in the parts
                
                # Get the indentation
                indent_match = re.match(r'(\s*)', call_lines[0])
                indent = indent_match.group(1) if indent_match else ''
                
                # Create the new call format
                new_call = f'{indent}(greger-tools-execute :tool-name "{tool_name}"\n'
                new_call += f'{indent}                      :args ARGS_PLACEHOLDER\n'
                new_call += f'{indent}                      :callback CALLBACK_PLACEHOLDER\n'
                new_call += f'{indent}                      :buffer BUFFER_PLACEHOLDER)'
                
                # Try to extract the actual arguments
                # This is tricky because of nested structures
                # For now, let's manually handle the most common patterns
                
                # Pattern 1: simple args on same line
                simple_pattern = r'greger-tools-execute\s+:tool-name\s+"[^"]+"\s+([^)]+)\s+(\([^)]+\))\s+([^)]+)\)'
                simple_match = re.search(simple_pattern, full_call)
                
                if simple_match:
                    args = simple_match.group(1).strip()
                    callback = simple_match.group(2).strip()
                    buffer = simple_match.group(3).strip()
                    
                    new_call = f'{indent}(greger-tools-execute :tool-name "{tool_name}"\n'
                    new_call += f'{indent}                      :args {args}\n'
                    new_call += f'{indent}                      :callback {callback}\n'
                    new_call += f'{indent}                      :buffer {buffer})'
                
                # Add the new call lines
                result_lines.extend(new_call.split('\n'))
                i = j + 1
            else:
                # Couldn't parse, keep original
                result_lines.extend(call_lines)
                i = j + 1
        else:
            result_lines.append(line)
            i += 1
    
    return '\n'.join(result_lines)

# Read the file
with open('test/test-greger-tools.el', 'r') as f:
    content = f.read()

# Fix the calls
fixed_content = fix_tool_calls(content)

# Write back
with open('test/test-greger-tools.el', 'w') as f:
    f.write(fixed_content)

print("Fixed greger-tools-execute calls")
