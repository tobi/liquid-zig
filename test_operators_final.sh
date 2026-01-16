#!/bin/bash

# Build the project
echo "Building..."
zig build || exit 1

echo ""
echo "Running operator tests..."
echo ""

# Run all tests and check results
cat test_operators.json | ./zig-out/bin/liquid-zig > /tmp/operator_test_output.json

# Check specific expected outputs
echo "=== Test Results ==="
echo ""

# Test 1: == operator (equal integers) - expecting "equal"
result=$(cat /tmp/operator_test_output.json | jq -r 'select(.id == 2) | .result.output')
if [ "$result" == "equal" ]; then
    echo "✓ Test 1 (== equal): PASS"
else
    echo "✗ Test 1 (== equal): FAIL (got: $result)"
fi

# Test 2: == operator (not equal integers) - expecting "not equal"
result=$(cat /tmp/operator_test_output.json | jq -r 'select(.id == 4) | .result.output')
if [ "$result" == "not equal" ]; then
    echo "✓ Test 2 (== not equal): PASS"
else
    echo "✗ Test 2 (== not equal): FAIL (got: $result)"
fi

# Test 3: != operator - expecting "not equal"
result=$(cat /tmp/operator_test_output.json | jq -r 'select(.id == 6) | .result.output')
if [ "$result" == "not equal" ]; then
    echo "✓ Test 3 (!=): PASS"
else
    echo "✗ Test 3 (!=): FAIL (got: $result)"
fi

# Test 4: < operator - expecting "less"
result=$(cat /tmp/operator_test_output.json | jq -r 'select(.id == 8) | .result.output')
if [ "$result" == "less" ]; then
    echo "✓ Test 4 (<): PASS"
else
    echo "✗ Test 4 (<): FAIL (got: $result)"
fi

# Test 5: > operator - expecting "greater"
result=$(cat /tmp/operator_test_output.json | jq -r 'select(.id == 10) | .result.output')
if [ "$result" == "greater" ]; then
    echo "✓ Test 5 (>): PASS"
else
    echo "✗ Test 5 (>): FAIL (got: $result)"
fi

# Test 6: <= operator - expecting "less or equal"
result=$(cat /tmp/operator_test_output.json | jq -r 'select(.id == 12) | .result.output')
if [ "$result" == "less or equal" ]; then
    echo "✓ Test 6 (<=): PASS"
else
    echo "✗ Test 6 (<=): FAIL (got: $result)"
fi

# Test 7: >= operator - expecting "greater or equal"
result=$(cat /tmp/operator_test_output.json | jq -r 'select(.id == 14) | .result.output')
if [ "$result" == "greater or equal" ]; then
    echo "✓ Test 7 (>=): PASS"
else
    echo "✗ Test 7 (>=): FAIL (got: $result)"
fi

# Test 8: String equality - expecting "Hello Alice"
result=$(cat /tmp/operator_test_output.json | jq -r 'select(.id == 16) | .result.output')
if [ "$result" == "Hello Alice" ]; then
    echo "✓ Test 8 (string ==): PASS"
else
    echo "✗ Test 8 (string ==): FAIL (got: $result)"
fi

# Test 9: String comparison - expecting "before Bob"
result=$(cat /tmp/operator_test_output.json | jq -r 'select(.id == 18) | .result.output')
if [ "$result" == "before Bob" ]; then
    echo "✓ Test 9 (string <): PASS"
else
    echo "✗ Test 9 (string <): FAIL (got: $result)"
fi

# Test 10: and operator (both true) - expecting "both true"
result=$(cat /tmp/operator_test_output.json | jq -r 'select(.id == 20) | .result.output')
if [ "$result" == "both true" ]; then
    echo "✓ Test 10 (and both true): PASS"
else
    echo "✗ Test 10 (and both true): FAIL (got: $result)"
fi

# Test 11: and operator (first false) - expecting "not both"
result=$(cat /tmp/operator_test_output.json | jq -r 'select(.id == 22) | .result.output')
if [ "$result" == "not both" ]; then
    echo "✓ Test 11 (and first false): PASS"
else
    echo "✗ Test 11 (and first false): FAIL (got: $result)"
fi

# Test 12: and operator (second false) - expecting "not both"
result=$(cat /tmp/operator_test_output.json | jq -r 'select(.id == 24) | .result.output')
if [ "$result" == "not both" ]; then
    echo "✓ Test 12 (and second false): PASS"
else
    echo "✗ Test 12 (and second false): FAIL (got: $result)"
fi

# Test 13: or operator (both true) - expecting "at least one"
result=$(cat /tmp/operator_test_output.json | jq -r 'select(.id == 26) | .result.output')
if [ "$result" == "at least one" ]; then
    echo "✓ Test 13 (or both true): PASS"
else
    echo "✗ Test 13 (or both true): FAIL (got: $result)"
fi

# Test 14: or operator (first true) - expecting "at least one"
result=$(cat /tmp/operator_test_output.json | jq -r 'select(.id == 28) | .result.output')
if [ "$result" == "at least one" ]; then
    echo "✓ Test 14 (or first true): PASS"
else
    echo "✗ Test 14 (or first true): FAIL (got: $result)"
fi

# Test 15: or operator (second true) - expecting "at least one"
result=$(cat /tmp/operator_test_output.json | jq -r 'select(.id == 30) | .result.output')
if [ "$result" == "at least one" ]; then
    echo "✓ Test 15 (or second true): PASS"
else
    echo "✗ Test 15 (or second true): FAIL (got: $result)"
fi

# Test 16: or operator (both false) - expecting "neither"
result=$(cat /tmp/operator_test_output.json | jq -r 'select(.id == 32) | .result.output')
if [ "$result" == "neither" ]; then
    echo "✓ Test 16 (or both false): PASS"
else
    echo "✗ Test 16 (or both false): FAIL (got: $result)"
fi

# Test 17: contains in string (found) - expecting "found"
result=$(cat /tmp/operator_test_output.json | jq -r 'select(.id == 34) | .result.output')
if [ "$result" == "found" ]; then
    echo "✓ Test 17 (contains string found): PASS"
else
    echo "✗ Test 17 (contains string found): FAIL (got: $result)"
fi

# Test 18: contains in string (not found) - expecting "not found"
result=$(cat /tmp/operator_test_output.json | jq -r 'select(.id == 36) | .result.output')
if [ "$result" == "not found" ]; then
    echo "✓ Test 18 (contains string not found): PASS"
else
    echo "✗ Test 18 (contains string not found): FAIL (got: $result)"
fi

# Test 19: contains in array (found) - expecting "has apple"
result=$(cat /tmp/operator_test_output.json | jq -r 'select(.id == 38) | .result.output')
if [ "$result" == "has apple" ]; then
    echo "✓ Test 19 (contains array found): PASS"
else
    echo "✗ Test 19 (contains array found): FAIL (got: $result)"
fi

# Test 20: contains in array (not found) - expecting "no grape"
result=$(cat /tmp/operator_test_output.json | jq -r 'select(.id == 40) | .result.output')
if [ "$result" == "no grape" ]; then
    echo "✓ Test 20 (contains array not found): PASS"
else
    echo "✗ Test 20 (contains array not found): FAIL (got: $result)"
fi

# Test 21: operator precedence (and before or) - expecting "match"
result=$(cat /tmp/operator_test_output.json | jq -r 'select(.id == 42) | .result.output')
if [ "$result" == "match" ]; then
    echo "✓ Test 21 (precedence: a false, b and c true): PASS"
else
    echo "✗ Test 21 (precedence: a false, b and c true): FAIL (got: $result)"
fi

# Test 22: operator precedence - expecting "match"
result=$(cat /tmp/operator_test_output.json | jq -r 'select(.id == 43) | .result.output')
if [ "$result" == "match" ]; then
    echo "✓ Test 22 (precedence: a true, b and c false): PASS"
else
    echo "✗ Test 22 (precedence: a true, b and c false): FAIL (got: $result)"
fi

# Test 23: operator precedence - expecting "no match"
result=$(cat /tmp/operator_test_output.json | jq -r 'select(.id == 44) | .result.output')
if [ "$result" == "no match" ]; then
    echo "✓ Test 23 (precedence: all false): PASS"
else
    echo "✗ Test 23 (precedence: all false): FAIL (got: $result)"
fi

# Test 24: complex expression - expecting "valid range"
result=$(cat /tmp/operator_test_output.json | jq -r 'select(.id == 46) | .result.output')
if [ "$result" == "valid range" ]; then
    echo "✓ Test 24 (complex: x > 5 and y < 10): PASS"
else
    echo "✗ Test 24 (complex: x > 5 and y < 10): FAIL (got: $result)"
fi

# Test 25: complex expression with != and or - expecting "continue"
result=$(cat /tmp/operator_test_output.json | jq -r 'select(.id == 48) | .result.output')
if [ "$result" == "continue" ]; then
    echo "✓ Test 25 (complex: != and or): PASS"
else
    echo "✗ Test 25 (complex: != and or): FAIL (got: $result)"
fi

# Test 26: elsif with operators - expecting "positive"
result=$(cat /tmp/operator_test_output.json | jq -r 'select(.id == 50) | .result.output')
if [ "$result" == "positive" ]; then
    echo "✓ Test 26 (elsif with <, ==, >): PASS"
else
    echo "✗ Test 26 (elsif with <, ==, >): FAIL (got: $result)"
fi

# Test 27: multiple conditions in elsif - expecting "range"
result=$(cat /tmp/operator_test_output.json | jq -r 'select(.id == 52) | .result.output')
if [ "$result" == "range" ]; then
    echo "✓ Test 27 (elsif with >= and <=): PASS"
else
    echo "✗ Test 27 (elsif with >= and <=): FAIL (got: $result)"
fi

echo ""
echo "All operator tests completed!"
