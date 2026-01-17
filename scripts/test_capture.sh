#!/bin/bash

# Test capture tag implementation

set -e

echo "Building liquid engine..."
zig build

echo ""
echo "Running capture tag tests..."
echo "=============================="

# Test 1: Basic capture
echo "Test 1: Basic capture"
result=$(
{
    echo '{"method": "compile", "id": 1, "params": {"template": "{% capture greeting %}Hello World{% endcapture %}{{ greeting }}"}}'
    echo '{"method": "render", "id": 2, "params": {"template_id": "0"}}'
} | ./zig-out/bin/liquid-zig | tail -n 1 | jq -r '.result.output')
expected="Hello World"
if [ "$result" = "$expected" ]; then
    echo "✓ Test 1 passed"
else
    echo "✗ Test 1 failed: got '$result', expected '$expected'"
    exit 1
fi

# Test 2: Capture with variables
echo "Test 2: Capture with variables"
result=$(
{
    echo '{"method": "compile", "id": 3, "params": {"template": "{% capture message %}Hello {{ name }}!{% endcapture %}{{ message }}"}}'
    echo '{"method": "render", "id": 4, "params": {"template_id": "1", "environment": {"name": "Alice"}}}'
} | ./zig-out/bin/liquid-zig | tail -n 1 | jq -r '.result.output')
expected="Hello Alice!"
if [ "$result" = "$expected" ]; then
    echo "✓ Test 2 passed"
else
    echo "✗ Test 2 failed: got '$result', expected '$expected'"
    exit 1
fi

# Test 3: Capture with filters
echo "Test 3: Capture with filters"
result=$(
{
    echo '{"method": "compile", "id": 5, "params": {"template": "{% capture lower %}HELLO{% endcapture %}{{ lower | downcase }}"}}'
    echo '{"method": "render", "id": 6, "params": {"template_id": "2"}}'
} | ./zig-out/bin/liquid-zig | tail -n 1 | jq -r '.result.output')
expected="hello"
if [ "$result" = "$expected" ]; then
    echo "✓ Test 3 passed"
else
    echo "✗ Test 3 failed: got '$result', expected '$expected'"
    exit 1
fi

# Test 4: Capture variable accessible after endcapture
echo "Test 4: Capture variable accessible after endcapture"
result=$(
{
    echo '{"method": "compile", "id": 7, "params": {"template": "{% capture x %}test{% endcapture %}before:{{ x }} after:{{ x }}"}}'
    echo '{"method": "render", "id": 8, "params": {"template_id": "3"}}'
} | ./zig-out/bin/liquid-zig | tail -n 1 | jq -r '.result.output')
expected="before:test after:test"
if [ "$result" = "$expected" ]; then
    echo "✓ Test 4 passed"
else
    echo "✗ Test 4 failed: got '$result', expected '$expected'"
    exit 1
fi

# Test 5: Nested captures
echo "Test 5: Nested captures"
result=$(
{
    echo '{"method": "compile", "id": 9, "params": {"template": "{% capture outer %}A{% capture inner %}B{% endcapture %}{{ inner }}C{% endcapture %}{{ outer }}"}}'
    echo '{"method": "render", "id": 10, "params": {"template_id": "4"}}'
} | ./zig-out/bin/liquid-zig | tail -n 1 | jq -r '.result.output')
expected="ABC"
if [ "$result" = "$expected" ]; then
    echo "✓ Test 5 passed"
else
    echo "✗ Test 5 failed: got '$result', expected '$expected'"
    exit 1
fi

# Test 6: Capture with if tag
echo "Test 6: Capture with if tag"
result=$(
{
    echo '{"method": "compile", "id": 11, "params": {"template": "{% capture result %}{% if show %}visible{% endif %}{% endcapture %}{{ result }}"}}'
    echo '{"method": "render", "id": 12, "params": {"template_id": "5", "environment": {"show": true}}}'
} | ./zig-out/bin/liquid-zig | tail -n 1 | jq -r '.result.output')
expected="visible"
if [ "$result" = "$expected" ]; then
    echo "✓ Test 6 passed"
else
    echo "✗ Test 6 failed: got '$result', expected '$expected'"
    exit 1
fi

# Test 7: Capture with for loop
echo "Test 7: Capture with for loop"
result=$(
{
    echo '{"method": "compile", "id": 13, "params": {"template": "{% capture items %}{% for i in array %}{{ i }}{% endfor %}{% endcapture %}{{ items }}"}}'
    echo '{"method": "render", "id": 14, "params": {"template_id": "6", "environment": {"array": [1, 2, 3]}}}'
} | ./zig-out/bin/liquid-zig | tail -n 1 | jq -r '.result.output')
expected="123"
if [ "$result" = "$expected" ]; then
    echo "✓ Test 7 passed"
else
    echo "✗ Test 7 failed: got '$result', expected '$expected'"
    exit 1
fi

# Test 8: Multiple captures
echo "Test 8: Multiple captures"
result=$(
{
    echo '{"method": "compile", "id": 15, "params": {"template": "{% capture a %}A{% endcapture %}{% capture b %}B{% endcapture %}{{ a }}{{ b }}"}}'
    echo '{"method": "render", "id": 16, "params": {"template_id": "7"}}'
} | ./zig-out/bin/liquid-zig | tail -n 1 | jq -r '.result.output')
expected="AB"
if [ "$result" = "$expected" ]; then
    echo "✓ Test 8 passed"
else
    echo "✗ Test 8 failed: got '$result', expected '$expected'"
    exit 1
fi

# Test 9: Capture with complex string building
echo "Test 9: Capture with complex string building"
result=$(
{
    echo '{"method": "compile", "id": 17, "params": {"template": "{% capture link %}<a href=\"{{ url }}\">{{ title }}</a>{% endcapture %}{{ link }}"}}'
    echo '{"method": "render", "id": 18, "params": {"template_id": "8", "environment": {"url": "/page", "title": "Click"}}}'
} | ./zig-out/bin/liquid-zig | tail -n 1 | jq -r '.result.output')
expected='<a href="/page">Click</a>'
if [ "$result" = "$expected" ]; then
    echo "✓ Test 9 passed"
else
    echo "✗ Test 9 failed: got '$result', expected '$expected'"
    exit 1
fi

# Test 10: Empty capture
echo "Test 10: Empty capture"
result=$(
{
    echo '{"method": "compile", "id": 19, "params": {"template": "{% capture empty %}{% endcapture %}x{{ empty }}y"}}'
    echo '{"method": "render", "id": 20, "params": {"template_id": "9"}}'
} | ./zig-out/bin/liquid-zig | tail -n 1 | jq -r '.result.output')
expected="xy"
if [ "$result" = "$expected" ]; then
    echo "✓ Test 10 passed"
else
    echo "✗ Test 10 failed: got '$result', expected '$expected'"
    exit 1
fi

# Test 11: Capture overwriting variable
echo "Test 11: Capture overwriting variable"
result=$(
{
    echo '{"method": "compile", "id": 21, "params": {"template": "{% assign x = \"old\" %}{% capture x %}new{% endcapture %}{{ x }}"}}'
    echo '{"method": "render", "id": 22, "params": {"template_id": "10"}}'
} | ./zig-out/bin/liquid-zig | tail -n 1 | jq -r '.result.output')
expected="new"
if [ "$result" = "$expected" ]; then
    echo "✓ Test 11 passed"
else
    echo "✗ Test 11 failed: got '$result', expected '$expected'"
    exit 1
fi

# Test 12: Capture with math operations
echo "Test 12: Capture with math operations"
result=$(
{
    echo '{"method": "compile", "id": 23, "params": {"template": "{% capture calc %}{{ 5 | plus: 3 }}{% endcapture %}Result: {{ calc }}"}}'
    echo '{"method": "render", "id": 24, "params": {"template_id": "11"}}'
} | ./zig-out/bin/liquid-zig | tail -n 1 | jq -r '.result.output')
expected="Result: 8"
if [ "$result" = "$expected" ]; then
    echo "✓ Test 12 passed"
else
    echo "✗ Test 12 failed: got '$result', expected '$expected'"
    exit 1
fi

echo ""
echo "=============================="
echo "All capture tests passed! ✓"
