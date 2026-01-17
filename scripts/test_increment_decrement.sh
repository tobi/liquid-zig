#!/bin/bash

# Test suite for increment/decrement tags (US-018)
# Tests all acceptance criteria:
# - increment outputs number then increments
# - decrement outputs number then decrements
# - Each counter name is independent
# - Counters start at 0
# - Counter namespace separate from assign variables
# - Counters persist across template

set -e

echo "Testing increment/decrement tags (US-018)..."

# Test 1: Basic increment starting at 0
echo "Test 1: Basic increment starting at 0"
result=$((echo '{"method": "compile", "id": 1, "params": {"template": "{% increment counter %}"}}'; echo '{"method": "render", "id": 2, "params": {"template_id": "0"}}') | ./zig-out/bin/liquid-zig | tail -1 | jq -r '.result.output')
expected="0"
if [ "$result" = "$expected" ]; then
  echo "✓ Test 1 passed: Basic increment outputs 0"
else
  echo "✗ Test 1 failed: Expected '$expected', got '$result'"
  exit 1
fi

# Test 2: Multiple increments show progression
echo "Test 2: Multiple increments show progression"
result=$((echo '{"method": "compile", "id": 1, "params": {"template": "{% increment count %}{% increment count %}{% increment count %}"}}'; echo '{"method": "render", "id": 2, "params": {"template_id": "0"}}') | ./zig-out/bin/liquid-zig | tail -1 | jq -r '.result.output')
expected="012"
if [ "$result" = "$expected" ]; then
  echo "✓ Test 2 passed: Multiple increments output 0, 1, 2"
else
  echo "✗ Test 2 failed: Expected '$expected', got '$result'"
  exit 1
fi

# Test 3: Basic decrement starting at 0
echo "Test 3: Basic decrement starting at 0"
result=$((echo '{"method": "compile", "id": 1, "params": {"template": "{% decrement counter %}"}}'; echo '{"method": "render", "id": 2, "params": {"template_id": "0"}}') | ./zig-out/bin/liquid-zig | tail -1 | jq -r '.result.output')
expected="0"
if [ "$result" = "$expected" ]; then
  echo "✓ Test 3 passed: Basic decrement outputs 0"
else
  echo "✗ Test 3 failed: Expected '$expected', got '$result'"
  exit 1
fi

# Test 4: Multiple decrements show progression
echo "Test 4: Multiple decrements show negative progression"
result=$((echo '{"method": "compile", "id": 1, "params": {"template": "{% decrement count %}{% decrement count %}{% decrement count %}"}}'; echo '{"method": "render", "id": 2, "params": {"template_id": "0"}}') | ./zig-out/bin/liquid-zig | tail -1 | jq -r '.result.output')
expected="0-1-2"
if [ "$result" = "$expected" ]; then
  echo "✓ Test 4 passed: Multiple decrements output 0, -1, -2"
else
  echo "✗ Test 4 failed: Expected '$expected', got '$result'"
  exit 1
fi

# Test 5: Independent counter names
echo "Test 5: Independent counter names"
result=$((echo '{"method": "compile", "id": 1, "params": {"template": "{% increment foo %}{% increment bar %}{% increment foo %}{% increment bar %}"}}'; echo '{"method": "render", "id": 2, "params": {"template_id": "0"}}') | ./zig-out/bin/liquid-zig | tail -1 | jq -r '.result.output')
expected="0011"
if [ "$result" = "$expected" ]; then
  echo "✓ Test 5 passed: Independent counters foo and bar"
else
  echo "✗ Test 5 failed: Expected '$expected', got '$result'"
  exit 1
fi

# Test 6: Counter namespace separate from assign variables
echo "Test 6: Counter namespace separate from assign variables"
result=$((echo '{"method": "compile", "id": 1, "params": {"template": "{% assign counter = 100 %}{{ counter }}{% increment counter %}{{ counter }}"}}'; echo '{"method": "render", "id": 2, "params": {"template_id": "0"}}') | ./zig-out/bin/liquid-zig | tail -1 | jq -r '.result.output')
expected="1000100"
if [ "$result" = "$expected" ]; then
  echo "✓ Test 6 passed: Counter namespace separate from variables"
else
  echo "✗ Test 6 failed: Expected '$expected', got '$result'"
  exit 1
fi

# Test 7: Mixed increment and decrement with different names
echo "Test 7: Mixed increment and decrement with different names"
result=$((echo '{"method": "compile", "id": 1, "params": {"template": "{% increment x %}{% decrement y %}{% increment x %}{% decrement y %}"}}'; echo '{"method": "render", "id": 2, "params": {"template_id": "0"}}') | ./zig-out/bin/liquid-zig | tail -1 | jq -r '.result.output')
expected="001-1"
if [ "$result" = "$expected" ]; then
  echo "✓ Test 7 passed: Mixed increment/decrement with different names"
else
  echo "✗ Test 7 failed: Expected '$expected', got '$result'"
  exit 1
fi

# Test 8: Counters persist across different parts of template
echo "Test 8: Counters persist across template"
result=$((echo '{"method": "compile", "id": 1, "params": {"template": "Start: {% increment id %}, Middle: {% increment id %}, End: {% increment id %}"}}'; echo '{"method": "render", "id": 2, "params": {"template_id": "0"}}') | ./zig-out/bin/liquid-zig | tail -1 | jq -r '.result.output')
expected="Start: 0, Middle: 1, End: 2"
if [ "$result" = "$expected" ]; then
  echo "✓ Test 8 passed: Counters persist across template"
else
  echo "✗ Test 8 failed: Expected '$expected', got '$result'"
  exit 1
fi

# Test 9: Increment with text formatting
echo "Test 9: Increment with text formatting"
result=$((echo '{"method": "compile", "id": 1, "params": {"template": "Item-{% increment item_id %}, Item-{% increment item_id %}, Item-{% increment item_id %}"}}'; echo '{"method": "render", "id": 2, "params": {"template_id": "0"}}') | ./zig-out/bin/liquid-zig | tail -1 | jq -r '.result.output')
expected="Item-0, Item-1, Item-2"
if [ "$result" = "$expected" ]; then
  echo "✓ Test 9 passed: Increment with text formatting for unique IDs"
else
  echo "✗ Test 9 failed: Expected '$expected', got '$result'"
  exit 1
fi

# Test 10: Decrement with text formatting
echo "Test 10: Decrement with text formatting"
result=$((echo '{"method": "compile", "id": 1, "params": {"template": "Countdown: {% decrement timer %}, {% decrement timer %}, {% decrement timer %}"}}'; echo '{"method": "render", "id": 2, "params": {"template_id": "0"}}') | ./zig-out/bin/liquid-zig | tail -1 | jq -r '.result.output')
expected="Countdown: 0, -1, -2"
if [ "$result" = "$expected" ]; then
  echo "✓ Test 10 passed: Decrement with text formatting"
else
  echo "✗ Test 10 failed: Expected '$expected', got '$result'"
  exit 1
fi

# Test 11: Increment in loops (counters persist across loop iterations)
echo "Test 11: Increment in loops - counters persist"
result=$((echo '{"method": "compile", "id": 1, "params": {"template": "{% for item in items %}{% increment global_id %}{% endfor %}"}}'; echo '{"method": "render", "id": 2, "params": {"template_id": "0", "environment": {"items": ["a", "b", "c"]}}}') | ./zig-out/bin/liquid-zig | tail -1 | jq -r '.result.output')
expected="012"
if [ "$result" = "$expected" ]; then
  echo "✓ Test 11 passed: Increment in loops persists across iterations"
else
  echo "✗ Test 11 failed: Expected '$expected', got '$result'"
  exit 1
fi

# Test 12: Multiple counters with different operations
echo "Test 12: Multiple counters with different operations"
result=$((echo '{"method": "compile", "id": 1, "params": {"template": "{% increment a %}{% increment b %}{% increment c %}{% increment a %}{% decrement b %}{% increment c %}"}}'; echo '{"method": "render", "id": 2, "params": {"template_id": "0"}}') | ./zig-out/bin/liquid-zig | tail -1 | jq -r '.result.output')
expected="000111"
if [ "$result" = "$expected" ]; then
  echo "✓ Test 12 passed: Multiple counters with different operations"
else
  echo "✗ Test 12 failed: Expected '$expected', got '$result'"
  exit 1
fi

echo ""
echo "=========================================="
echo "All 12 increment/decrement tests passed! ✓"
echo "=========================================="
