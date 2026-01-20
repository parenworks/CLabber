#!/usr/bin/env python3
"""Check parenthesis balance in Lisp files, handling strings and comments."""

import sys

def check_parens(filename):
    with open(filename, 'r') as f:
        content = f.read()
    
    depth = 0
    in_string = False
    in_comment = False
    in_block_comment = False
    escape_next = False
    line_num = 1
    col = 0
    paren_stack = []  # (line, col, depth)
    
    i = 0
    while i < len(content):
        char = content[i]
        col += 1
        
        if char == '\n':
            line_num += 1
            col = 0
            in_comment = False
        
        if escape_next:
            escape_next = False
            i += 1
            continue
            
        if char == '\\' and in_string:
            escape_next = True
            i += 1
            continue
        
        # Block comment handling #| ... |#
        if not in_string and not in_comment:
            if i + 1 < len(content) and content[i:i+2] == '#|':
                in_block_comment = True
                i += 2
                continue
            if in_block_comment and i + 1 < len(content) and content[i:i+2] == '|#':
                in_block_comment = False
                i += 2
                continue
        
        if in_block_comment:
            i += 1
            continue
            
        # Line comment
        if char == ';' and not in_string:
            in_comment = True
            i += 1
            continue
            
        if in_comment:
            i += 1
            continue
        
        # String handling
        if char == '"' and not in_string:
            in_string = True
            i += 1
            continue
        elif char == '"' and in_string:
            in_string = False
            i += 1
            continue
            
        if in_string:
            i += 1
            continue
        
        # Parenthesis counting
        if char == '(':
            depth += 1
            paren_stack.append((line_num, col, depth))
        elif char == ')':
            if depth <= 0:
                print(f"ERROR: Unmatched ')' at line {line_num}, col {col}")
                return False
            depth -= 1
            paren_stack.pop()
        
        i += 1
    
    if depth != 0:
        print(f"ERROR: Unbalanced parentheses. Final depth: {depth}")
        if paren_stack:
            last = paren_stack[-1]
            print(f"  Last unmatched '(' at line {last[0]}, col {last[1]}")
        return False
    
    print(f"OK: Parentheses balanced in {filename}")
    return True

if __name__ == '__main__':
    if len(sys.argv) < 2:
        print("Usage: check-parens.py <file.lisp>")
        sys.exit(1)
    
    success = check_parens(sys.argv[1])
    sys.exit(0 if success else 1)
