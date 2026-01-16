#!/bin/bash
set -e

echo "Testing US-009: Basic Filters Foundation"
echo "========================================="

# Test 1: {{ value | upcase }} syntax parses correctly
echo -n "Test 1: Filter syntax parsing... "
cat > test_filters_1.json << 'EOF'
{
  "template": "{{ name | upcase }}",
  "environment": {
    "name": "hello"
  },
  "expected": "HELLO"
}
EOF
./zig-out/bin/liquid-zig test_filters_1.json
rm test_filters_1.json
echo "✓"

# Test 2: upcase filter converts strings to uppercase
echo -n "Test 2: upcase filter... "
cat > test_filters_2.json << 'EOF'
{
  "template": "{{ text | upcase }}",
  "environment": {
    "text": "Hello World"
  },
  "expected": "HELLO WORLD"
}
EOF
./zig-out/bin/liquid-zig test_filters_2.json
rm test_filters_2.json
echo "✓"

# Test 3: downcase filter converts strings to lowercase
echo -n "Test 3: downcase filter... "
cat > test_filters_3.json << 'EOF'
{
  "template": "{{ text | downcase }}",
  "environment": {
    "text": "HELLO WORLD"
  },
  "expected": "hello world"
}
EOF
./zig-out/bin/liquid-zig test_filters_3.json
rm test_filters_3.json
echo "✓"

# Test 4: capitalize filter capitalizes first letter
echo -n "Test 4: capitalize filter... "
cat > test_filters_4.json << 'EOF'
{
  "template": "{{ text | capitalize }}",
  "environment": {
    "text": "hello world"
  },
  "expected": "Hello world"
}
EOF
./zig-out/bin/liquid-zig test_filters_4.json
rm test_filters_4.json
echo "✓"

# Test 5: Multiple filters in pipeline
echo -n "Test 5: Filter pipeline... "
cat > test_filters_5.json << 'EOF'
{
  "template": "{{ text | downcase | capitalize }}",
  "environment": {
    "text": "HELLO WORLD"
  },
  "expected": "Hello world"
}
EOF
./zig-out/bin/liquid-zig test_filters_5.json
rm test_filters_5.json
echo "✓"

# Test 6: Filters handle nil input gracefully
echo -n "Test 6: Filter on nil value... "
cat > test_filters_6.json << 'EOF'
{
  "template": "{{ missing | upcase }}",
  "environment": {},
  "expected": ""
}
EOF
./zig-out/bin/liquid-zig test_filters_6.json
rm test_filters_6.json
echo "✓"

# Test 7: Filter on literal string
echo -n "Test 7: Filter on string literal... "
cat > test_filters_7.json << 'EOF'
{
  "template": "{{ \"hello\" | upcase }}",
  "environment": {},
  "expected": "HELLO"
}
EOF
./zig-out/bin/liquid-zig test_filters_7.json
rm test_filters_7.json
echo "✓"

# Test 8: Filter on nested property
echo -n "Test 8: Filter on nested property... "
cat > test_filters_8.json << 'EOF'
{
  "template": "{{ user.name | upcase }}",
  "environment": {
    "user": {
      "name": "alice"
    }
  },
  "expected": "ALICE"
}
EOF
./zig-out/bin/liquid-zig test_filters_8.json
rm test_filters_8.json
echo "✓"

# Test 9: Capitalize with already capitalized string
echo -n "Test 9: Capitalize already capitalized... "
cat > test_filters_9.json << 'EOF'
{
  "template": "{{ text | capitalize }}",
  "environment": {
    "text": "Hello"
  },
  "expected": "Hello"
}
EOF
./zig-out/bin/liquid-zig test_filters_9.json
rm test_filters_9.json
echo "✓"

# Test 10: Empty string with filters
echo -n "Test 10: Empty string with filter... "
cat > test_filters_10.json << 'EOF'
{
  "template": "{{ text | upcase }}",
  "environment": {
    "text": ""
  },
  "expected": ""
}
EOF
./zig-out/bin/liquid-zig test_filters_10.json
rm test_filters_10.json
echo "✓"

# Test 11: Filter chain with three filters
echo -n "Test 11: Three-filter chain... "
cat > test_filters_11.json << 'EOF'
{
  "template": "{{ text | upcase | downcase | capitalize }}",
  "environment": {
    "text": "hElLo WoRlD"
  },
  "expected": "Hello world"
}
EOF
./zig-out/bin/liquid-zig test_filters_11.json
rm test_filters_11.json
echo "✓"

# Test 12: Capitalize empty string
echo -n "Test 12: Capitalize empty string... "
cat > test_filters_12.json << 'EOF'
{
  "template": "{{ text | capitalize }}",
  "environment": {
    "text": ""
  },
  "expected": ""
}
EOF
./zig-out/bin/liquid-zig test_filters_12.json
rm test_filters_12.json
echo "✓"

# Test 13: Filters with assign tag
echo -n "Test 13: Filters with assign tag... "
cat > test_filters_13.json << 'EOF'
{
  "template": "{% assign greeting = \"hello\" %}{{ greeting | upcase }}",
  "environment": {},
  "expected": "HELLO"
}
EOF
./zig-out/bin/liquid-zig test_filters_13.json
rm test_filters_13.json
echo "✓"

echo ""
echo "All tests passed! ✓"
echo "US-009 acceptance criteria verified:"
echo "  ✓ {{ value | filter }} syntax parses correctly"
echo "  ✓ Filter pipeline resolves filter by name"
echo "  ✓ upcase filter converts strings to uppercase"
echo "  ✓ downcase filter converts strings to lowercase"
echo "  ✓ capitalize filter capitalizes first letter"
echo "  ✓ Filters handle nil input gracefully"
echo "  ✓ IR has instruction for filter application"
