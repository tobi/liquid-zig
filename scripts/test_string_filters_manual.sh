#!/bin/bash

# Manual test for string filters (US-016)
# Tests filters using the same format as math_filters tests

set -e

echo "Testing US-016: String Filters"
echo "==============================="

# Test 1: append filter
echo -n "Test 1: append filter... "
cat > test_str_1.json << 'EOF'
{
  "template": "{{ x | append: \"!\" }}",
  "environment": {
    "x": "Hello"
  },
  "expected": "Hello!"
}
EOF
./zig-out/bin/liquid-zig test_str_1.json
rm test_str_1.json
echo "✓"

# Test 2: prepend filter
echo -n "Test 2: prepend filter... "
cat > test_str_2.json << 'EOF'
{
  "template": "{{ x | prepend: \"Mr. \" }}",
  "environment": {
    "x": "Smith"
  },
  "expected": "Mr. Smith"
}
EOF
./zig-out/bin/liquid-zig test_str_2.json
rm test_str_2.json
echo "✓"

# Test 3: remove filter
echo -n "Test 3: remove filter... "
cat > test_str_3.json << 'EOF'
{
  "template": "{{ x | remove: \"a\" }}",
  "environment": {
    "x": "banana"
  },
  "expected": "bnn"
}
EOF
./zig-out/bin/liquid-zig test_str_3.json
rm test_str_3.json
echo "✓"

# Test 4: replace filter
echo -n "Test 4: replace filter... "
cat > test_str_4.json << 'EOF'
{
  "template": "{{ x | replace: \"a\", \"o\" }}",
  "environment": {
    "x": "banana"
  },
  "expected": "bonono"
}
EOF
./zig-out/bin/liquid-zig test_str_4.json
rm test_str_4.json
echo "✓"

# Test 5: slice filter
echo -n "Test 5: slice filter... "
cat > test_str_5.json << 'EOF'
{
  "template": "{{ x | slice: 0, 5 }}",
  "environment": {
    "x": "Hello World"
  },
  "expected": "Hello"
}
EOF
./zig-out/bin/liquid-zig test_str_5.json
rm test_str_5.json
echo "✓"

# Test 6: truncate filter
echo -n "Test 6: truncate filter... "
cat > test_str_6.json << 'EOF'
{
  "template": "{{ x | truncate: 8 }}",
  "environment": {
    "x": "Hello World"
  },
  "expected": "Hello..."
}
EOF
./zig-out/bin/liquid-zig test_str_6.json
rm test_str_6.json
echo "✓"

# Test 7: url_encode filter
echo -n "Test 7: url_encode filter... "
cat > test_str_7.json << 'EOF'
{
  "template": "{{ x | url_encode }}",
  "environment": {
    "x": "hello world"
  },
  "expected": "hello+world"
}
EOF
./zig-out/bin/liquid-zig test_str_7.json
rm test_str_7.json
echo "✓"

# Test 8: url_decode filter
echo -n "Test 8: url_decode filter... "
cat > test_str_8.json << 'EOF'
{
  "template": "{{ x | url_decode }}",
  "environment": {
    "x": "hello+world"
  },
  "expected": "hello world"
}
EOF
./zig-out/bin/liquid-zig test_str_8.json
rm test_str_8.json
echo "✓"

# Test 9: strip_html filter
echo -n "Test 9: strip_html filter... "
cat > test_str_9.json << 'EOF'
{
  "template": "{{ x | strip_html }}",
  "environment": {
    "x": "<p>Hello</p>"
  },
  "expected": "Hello"
}
EOF
./zig-out/bin/liquid-zig test_str_9.json
rm test_str_9.json
echo "✓"

# Test 10: newline_to_br filter
echo -n "Test 10: newline_to_br filter... "
cat > test_str_10.json << 'EOF'
{
  "template": "{{ x | newline_to_br }}",
  "environment": {
    "x": "hello\nworld"
  },
  "expected": "hello<br>world"
}
EOF
./zig-out/bin/liquid-zig test_str_10.json
rm test_str_10.json
echo "✓"

# Test 11: escape_once filter
echo -n "Test 11: escape_once filter (no double escape)... "
cat > test_str_11.json << 'EOF'
{
  "template": "{{ x | escape_once }}",
  "environment": {
    "x": "&lt;p&gt;"
  },
  "expected": "&lt;p&gt;"
}
EOF
./zig-out/bin/liquid-zig test_str_11.json
rm test_str_11.json
echo "✓"

# Test 12: chained filters
echo -n "Test 12: chained filters (prepend + append)... "
cat > test_str_12.json << 'EOF'
{
  "template": "{{ x | prepend: \"[\" | append: \"]\" }}",
  "environment": {
    "x": "test"
  },
  "expected": "[test]"
}
EOF
./zig-out/bin/liquid-zig test_str_12.json
rm test_str_12.json
echo "✓"

echo
echo "All string filter tests passed! ✓"
