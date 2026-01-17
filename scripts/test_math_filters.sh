#!/bin/bash
set -e

echo "Testing US-012: Math Filters"
echo "============================="

# Test 1: plus filter adds numbers (integer + integer)
echo -n "Test 1: plus filter (10 + 5)... "
cat > test_math_1.json << 'EOF'
{
  "template": "{{ x | plus: 5 }}",
  "environment": {
    "x": 10
  },
  "expected": "15"
}
EOF
./zig-out/bin/liquid-zig test_math_1.json
rm test_math_1.json
echo "✓"

# Test 2: minus filter subtracts numbers
echo -n "Test 2: minus filter (10 - 3)... "
cat > test_math_2.json << 'EOF'
{
  "template": "{{ x | minus: 3 }}",
  "environment": {
    "x": 10
  },
  "expected": "7"
}
EOF
./zig-out/bin/liquid-zig test_math_2.json
rm test_math_2.json
echo "✓"

# Test 3: times filter multiplies numbers
echo -n "Test 3: times filter (7 * 3)... "
cat > test_math_3.json << 'EOF'
{
  "template": "{{ x | times: 3 }}",
  "environment": {
    "x": 7
  },
  "expected": "21"
}
EOF
./zig-out/bin/liquid-zig test_math_3.json
rm test_math_3.json
echo "✓"

# Test 4: divided_by filter divides numbers
echo -n "Test 4: divided_by filter (20 / 4)... "
cat > test_math_4.json << 'EOF'
{
  "template": "{{ x | divided_by: 4 }}",
  "environment": {
    "x": 20
  },
  "expected": "5"
}
EOF
./zig-out/bin/liquid-zig test_math_4.json
rm test_math_4.json
echo "✓"

# Test 5: modulo filter computes remainder
echo -n "Test 5: modulo filter (10 % 3)... "
cat > test_math_5.json << 'EOF'
{
  "template": "{{ x | modulo: 3 }}",
  "environment": {
    "x": 10
  },
  "expected": "1"
}
EOF
./zig-out/bin/liquid-zig test_math_5.json
rm test_math_5.json
echo "✓"

# Test 6: abs filter returns absolute value (negative)
echo -n "Test 6: abs filter (|-5|)... "
cat > test_math_6.json << 'EOF'
{
  "template": "{{ x | abs }}",
  "environment": {
    "x": -5
  },
  "expected": "5"
}
EOF
./zig-out/bin/liquid-zig test_math_6.json
rm test_math_6.json
echo "✓"

# Test 7: abs filter returns absolute value (positive)
echo -n "Test 7: abs filter (|5|)... "
cat > test_math_7.json << 'EOF'
{
  "template": "{{ x | abs }}",
  "environment": {
    "x": 5
  },
  "expected": "5"
}
EOF
./zig-out/bin/liquid-zig test_math_7.json
rm test_math_7.json
echo "✓"

# Test 8: ceil filter rounds up
echo -n "Test 8: ceil filter (ceil 4.3)... "
cat > test_math_8.json << 'EOF'
{
  "template": "{{ x | ceil }}",
  "environment": {
    "x": 4.3
  },
  "expected": "5"
}
EOF
./zig-out/bin/liquid-zig test_math_8.json
rm test_math_8.json
echo "✓"

# Test 9: floor filter rounds down
echo -n "Test 9: floor filter (floor 4.7)... "
cat > test_math_9.json << 'EOF'
{
  "template": "{{ x | floor }}",
  "environment": {
    "x": 4.7
  },
  "expected": "4"
}
EOF
./zig-out/bin/liquid-zig test_math_9.json
rm test_math_9.json
echo "✓"

# Test 10: round filter rounds to nearest integer
echo -n "Test 10: round filter (round 4.5)... "
cat > test_math_10.json << 'EOF'
{
  "template": "{{ x | round }}",
  "environment": {
    "x": 4.5
  },
  "expected": "5"
}
EOF
./zig-out/bin/liquid-zig test_math_10.json
rm test_math_10.json
echo "✓"

# Test 11: round filter with precision
echo -n "Test 11: round filter with precision (round 4.567 to 2 places)... "
cat > test_math_11.json << 'EOF'
{
  "template": "{{ x | round: 2 }}",
  "environment": {
    "x": 4.567
  },
  "expected": "4.57"
}
EOF
./zig-out/bin/liquid-zig test_math_11.json
rm test_math_11.json
echo "✓"

# Test 12: String to number coercion in plus
echo -n "Test 12: plus with string coercion (\"10\" + 5)... "
cat > test_math_12.json << 'EOF'
{
  "template": "{{ x | plus: 5 }}",
  "environment": {
    "x": "10"
  },
  "expected": "15"
}
EOF
./zig-out/bin/liquid-zig test_math_12.json
rm test_math_12.json
echo "✓"

# Test 13: String to number coercion in argument
echo -n "Test 13: plus with string argument (10 + \"5\")... "
cat > test_math_13.json << 'EOF'
{
  "template": "{{ x | plus: \"5\" }}",
  "environment": {
    "x": 10
  },
  "expected": "15"
}
EOF
./zig-out/bin/liquid-zig test_math_13.json
rm test_math_13.json
echo "✓"

# Test 14: Negative numbers with plus
echo -n "Test 14: plus with negative (-5 + 3)... "
cat > test_math_14.json << 'EOF'
{
  "template": "{{ x | plus: 3 }}",
  "environment": {
    "x": -5
  },
  "expected": "-2"
}
EOF
./zig-out/bin/liquid-zig test_math_14.json
rm test_math_14.json
echo "✓"

# Test 15: Float arithmetic with plus
echo -n "Test 15: plus with floats (4.5 + 2.3)... "
cat > test_math_15.json << 'EOF'
{
  "template": "{{ x | plus: 2.3 }}",
  "environment": {
    "x": 4.5
  },
  "expected": "6.8"
}
EOF
./zig-out/bin/liquid-zig test_math_15.json
rm test_math_15.json
echo "✓"

# Test 16: Chain math filters
echo -n "Test 16: chain filters (10 + 5 - 3)... "
cat > test_math_16.json << 'EOF'
{
  "template": "{{ x | plus: 5 | minus: 3 }}",
  "environment": {
    "x": 10
  },
  "expected": "12"
}
EOF
./zig-out/bin/liquid-zig test_math_16.json
rm test_math_16.json
echo "✓"

# Test 17: abs with float
echo -n "Test 17: abs with float (|-4.7|)... "
cat > test_math_17.json << 'EOF'
{
  "template": "{{ x | abs }}",
  "environment": {
    "x": -4.7
  },
  "expected": "4.7"
}
EOF
./zig-out/bin/liquid-zig test_math_17.json
rm test_math_17.json
echo "✓"

# Test 18: ceil with negative number
echo -n "Test 18: ceil with negative (ceil -4.3)... "
cat > test_math_18.json << 'EOF'
{
  "template": "{{ x | ceil }}",
  "environment": {
    "x": -4.3
  },
  "expected": "-4"
}
EOF
./zig-out/bin/liquid-zig test_math_18.json
rm test_math_18.json
echo "✓"

# Test 19: floor with negative number
echo -n "Test 19: floor with negative (floor -4.7)... "
cat > test_math_19.json << 'EOF'
{
  "template": "{{ x | floor }}",
  "environment": {
    "x": -4.7
  },
  "expected": "-5"
}
EOF
./zig-out/bin/liquid-zig test_math_19.json
rm test_math_19.json
echo "✓"

# Test 20: divided_by with result in float
echo -n "Test 20: divided_by with float result (10 / 3)... "
cat > test_math_20.json << 'EOF'
{
  "template": "{{ x | divided_by: 3 }}",
  "environment": {
    "x": 10
  },
  "expected": "3.33"
}
EOF
./zig-out/bin/liquid-zig test_math_20.json
rm test_math_20.json
echo "✓"

# Test 21: Complex chain (multiply, add, round)
echo -n "Test 21: complex chain (5 * 3 + 2 = 17)... "
cat > test_math_21.json << 'EOF'
{
  "template": "{{ x | times: 3 | plus: 2 }}",
  "environment": {
    "x": 5
  },
  "expected": "17"
}
EOF
./zig-out/bin/liquid-zig test_math_21.json
rm test_math_21.json
echo "✓"

# Test 22: modulo with negative number
echo -n "Test 22: modulo with negative (-10 % 3)... "
cat > test_math_22.json << 'EOF'
{
  "template": "{{ x | modulo: 3 }}",
  "environment": {
    "x": -10
  },
  "expected": "-1"
}
EOF
./zig-out/bin/liquid-zig test_math_22.json
rm test_math_22.json
echo "✓"

# Test 23: round filter with 4.4 (should round down)
echo -n "Test 23: round filter (round 4.4)... "
cat > test_math_23.json << 'EOF'
{
  "template": "{{ x | round }}",
  "environment": {
    "x": 4.4
  },
  "expected": "4"
}
EOF
./zig-out/bin/liquid-zig test_math_23.json
rm test_math_23.json
echo "✓"

# Test 24: times with zero
echo -n "Test 24: times with zero (10 * 0)... "
cat > test_math_24.json << 'EOF'
{
  "template": "{{ x | times: 0 }}",
  "environment": {
    "x": 10
  },
  "expected": "0"
}
EOF
./zig-out/bin/liquid-zig test_math_24.json
rm test_math_24.json
echo "✓"

# Test 25: plus with zero
echo -n "Test 25: plus with zero (10 + 0)... "
cat > test_math_25.json << 'EOF'
{
  "template": "{{ x | plus: 0 }}",
  "environment": {
    "x": 10
  },
  "expected": "10"
}
EOF
./zig-out/bin/liquid-zig test_math_25.json
rm test_math_25.json
echo "✓"

echo ""
echo "All tests passed! ✓"
echo "US-012 acceptance criteria verified:"
echo "  ✓ plus filter adds numbers"
echo "  ✓ minus filter subtracts numbers"
echo "  ✓ times filter multiplies numbers"
echo "  ✓ divided_by filter divides numbers"
echo "  ✓ modulo filter computes remainder"
echo "  ✓ abs filter returns absolute value"
echo "  ✓ ceil/floor/round perform rounding"
echo "  ✓ Filters handle string-to-number coercion"
