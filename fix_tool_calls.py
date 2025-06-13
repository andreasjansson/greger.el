#!/usr/bin/env python3
import re

def fix_tool_calls(content):
    # Pattern to match greger-tools-execute calls with old format
    # greger-tools-execute "tool-name" args callback buffer
    pattern = r'greger-tools-execute\s+"([^"]+)"\s+([^,\s]+)\s+(lambda[^)]+\)\s*[^,\s]*)\s+([^)]+)\)'
    
    def replace_func(match):
        tool_name = match.group(1)
        args = match.group(2)
        callback = match.group(3)  
        buffer = match.group(4)
        
        return f'greger-tools-execute :tool-name "{tool_name}" :args {args} :callback {callback} :buffer {buffer})'
    
    # Try simpler pattern first
    simple_pattern = r'greger-tools-execute\s+"([^"]+)"\s+'
    
    lines = content.split('\n')
    result_lines = []
    i = 0
    
    while i < len(lines):
        line = lines[i]
        
        # Look for lines that start greger-tools-execute calls
        if 'greger-tools-execute "' in line and ':tool-name' not in line:
            # This is a multi-line call, collect all lines until we find the closing paren
            call_lines = [line]
            paren_count = line.count('(') - line.count(')')
            j = i + 1
            
            while j < len(lines) and paren_count > 0:
                next_line = lines[j]
                call_lines.append(next_line)
                paren_count += next_line.count('(') - next_line.count(')')
                j += 1
            
            # Now we have the full call, parse it
            full_call = '\n'.join(call_lines)
            
            # Extract the tool name
            tool_match = re.search(r'greger-tools-execute\s+"([^"]+)"', full_call)
            if tool_match:
                tool_name = tool_match.group(1)
                
                # Replace the first line with the new format
                new_first_line = re.sub(
                    r'greger-tools-execute\s+"[^"]+"',
                    f'greger-tools-execute :tool-name "{tool_name}"',
                    call_lines[0]
                )
                
                # Add :args, :callback, :buffer keywords to appropriate lines
                if len(call_lines) >= 2:
                    call_lines[1] = re.sub(r'(\s*)([^)]+)(\s*)', r'\1:args \2\3', call_lines[1])
                if len(call_lines) >= 3:
                    call_lines[2] = re.sub(r'(\s*)(.+)', r'\1:callback \2', call_lines[2])
                if len(call_lines) >= 4:
                    call_lines[3] = re.sub(r'(\s*)([^)]+)(\s*)', r'\1:buffer \2\3', call_lines[3])
                
                result_lines.extend([new_first_line] + call_lines[1:])
                i = j
            else:
                result_lines.append(line)
                i += 1
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
