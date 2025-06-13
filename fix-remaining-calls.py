#!/usr/bin/env python3

import re
import sys

def fix_greger_tools_execute_calls(content):
    """Fix greger-tools-execute calls to use keyword arguments."""
    
    # Pattern to match the old style calls
    pattern = r'''(\s*)\(greger-tools-execute\s+"([^"]+)"\s+
                  (\S.*?)
                  \s+\(lambda\s+\(([^)]*)\)\s+(.*?)\)\s+([^)]+)\)'''
    
    def replace_match(match):
        indent = match.group(1)
        tool_name = match.group(2)
        args = match.group(3).strip()
        lambda_args = match.group(4)
        lambda_body = match.group(5)
        buffer = match.group(6)
        
        return f'''{indent}(greger-tools-execute :tool-name "{tool_name}"
{indent}                      :args {args}
{indent}                      :callback (lambda ({lambda_args}) {lambda_body})
{indent}                      :buffer {buffer})'''
    
    # Use re.VERBOSE and re.DOTALL for multiline matching
    result = re.sub(pattern, replace_match, content, flags=re.VERBOSE | re.DOTALL)
    return result

def main():
    file_path = "test/test-greger-tools.el"
    
    # Read the file
    with open(file_path, 'r') as f:
        content = f.read()
    
    # Fix the calls
    fixed_content = fix_greger_tools_execute_calls(content)
    
    # Write back
    with open(file_path, 'w') as f:
        f.write(fixed_content)
    
    print("Fixed greger-tools-execute calls")

if __name__ == "__main__":
    main()
