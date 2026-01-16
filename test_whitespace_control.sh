#!/bin/bash

# Test script for whitespace control (US-020)
# Tests {{- -}} and {%- -%} syntax

set -e

echo "Testing whitespace control..."

# Test 1: {{- strips whitespace before output
echo "Test 1: {{- strips whitespace before"
bundle exec liquid-spec eval liquid-zig.rb <<'EOF'
name: test_strip_left_variable
template: "  \n  {{- x }}"
expected: "hello"
environment:
  x: hello
EOF

# Test 2: -}} strips whitespace after output
echo "Test 2: -}} strips whitespace after"
bundle exec liquid-spec eval liquid-zig.rb <<'EOF'
name: test_strip_right_variable
template: "{{ x -}}  \n  "
expected: "hello"
environment:
  x: hello
EOF

# Test 3: Both sides with variables
echo "Test 3: Both sides {{- -}}"
bundle exec liquid-spec eval liquid-zig.rb <<'EOF'
name: test_strip_both_variable
template: "  \n  {{- x -}}  \n  "
expected: "hello"
environment:
  x: hello
EOF

# Test 4: {%- strips whitespace before tag
echo "Test 4: {%- strips whitespace before tag"
bundle exec liquid-spec eval liquid-zig.rb <<'EOF'
name: test_strip_left_tag
template: "  \n  {%- assign y = 'world' %}{{ y }}"
expected: "world"
EOF

# Test 5: -%} strips whitespace after tag
echo "Test 5: -%} strips whitespace after tag"
bundle exec liquid-spec eval liquid-zig.rb <<'EOF'
name: test_strip_right_tag
template: "{% assign y = 'world' -%}  \n  {{ y }}"
expected: "world"
EOF

# Test 6: Both sides with tags
echo "Test 6: Both sides {%- -%}"
bundle exec liquid-spec eval liquid-zig.rb <<'EOF'
name: test_strip_both_tag
template: "  \n  {%- assign y = 'world' -%}  \n  {{ y }}"
expected: "world"
EOF

# Test 7: Whitespace stripping works independently
echo "Test 7: Independent stripping"
bundle exec liquid-spec eval liquid-zig.rb <<'EOF'
name: test_independent_stripping
template: "a  {{- x }}  b"
expected: "ahellob"
environment:
  x: hello
EOF

# Test 8: Multiple whitespace types (spaces, tabs, newlines)
echo "Test 8: Multiple whitespace types"
bundle exec liquid-spec eval liquid-zig.rb <<'EOF'
name: test_multiple_whitespace
template: " \t\n{{- x -}} \t\n"
expected: "hello"
environment:
  x: hello
EOF

# Test 9: Whitespace control with for loop
echo "Test 9: Whitespace control with for loop"
bundle exec liquid-spec eval liquid-zig.rb <<'EOF'
name: test_strip_for_loop
template: "  \n  {%- for item in items -%}{{ item }}{%- endfor -%}  \n  "
expected: "abc"
environment:
  items: [a, b, c]
EOF

# Test 10: Whitespace control doesn't affect non-whitespace
echo "Test 10: Non-whitespace preserved"
bundle exec liquid-spec eval liquid-zig.rb <<'EOF'
name: test_non_whitespace_preserved
template: "a{{- x -}}b"
expected: "ahellob"
environment:
  x: hello
EOF

# Test 11: Mixed whitespace control
echo "Test 11: Mixed whitespace control"
bundle exec liquid-spec eval liquid-zig.rb <<'EOF'
name: test_mixed_whitespace
template: "  {{ x -}}  {{- y }}  "
expected: "  helloworldworld"
environment:
  x: hello
  y: world
EOF

# Test 12: Whitespace control with if tag
echo "Test 12: Whitespace control with if tag"
bundle exec liquid-spec eval liquid-zig.rb <<'EOF'
name: test_strip_if_tag
template: "  \n  {%- if true -%}yes{%- endif -%}  \n  "
expected: "yes"
EOF

echo ""
echo "All whitespace control tests completed!"
