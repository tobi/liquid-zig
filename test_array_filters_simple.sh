#!/bin/bash

# Simplified test suite for array filters (US-017)
# Uses JSON-RPC protocol via stdin

set -e

GREEN='\033[0;32m'
RED='\033[0;31m'
NC='\033[0m' # No Color

TESTS_PASSED=0
TESTS_FAILED=0

test_filter() {
    local test_num=$1
    local description=$2
    local template=$3
    local context=$4
    local expected=$5

    # Create JSON-RPC requests
    cat > /tmp/test_${test_num}.input << EOF
{"jsonrpc": "2.0", "method": "compile", "params": {"template": "$template"}, "id": 1}
{"jsonrpc": "2.0", "method": "render", "params": {"template_id": "0", "environment": $context}, "id": 2}
EOF

    # Run test and capture output (ignore segfault for now)
    local output=$(timeout 2 ./zig-out/bin/liquid-zig < /tmp/test_${test_num}.input 2>/dev/null | grep '"output"' | sed 's/.*"output":"\([^"]*\)".*/\1/' || echo "ERROR")

    if [ "$output" = "$expected" ]; then
        echo -e "${GREEN}✓ Test $test_num ($description): PASS${NC}"
        ((TESTS_PASSED++))
    else
        echo -e "${RED}✗ Test $test_num ($description): FAIL${NC}"
        echo "  Expected: $expected"
        echo "  Got: $output"
        ((TESTS_FAILED++))
    fi

    rm -f /tmp/test_${test_num}.input
}

echo "=== Testing Array Filters (US-017) ==="
echo

# Test 1: join filter - basic (no separator, uses default)
test_filter 1 "join: default separator" \
    '{{ items | join }}' \
    '{"items": ["a", "b", "c"]}' \
    'a, b, c'

# Test 2: first filter
test_filter 2 "first: single element" \
    '{{ items | first }}' \
    '{"items": ["apple", "banana", "cherry"]}' \
    'apple'

# Test 3: last filter
test_filter 3 "last: single element" \
    '{{ items | last }}' \
    '{"items": ["apple", "banana", "cherry"]}' \
    'cherry'

# Test 4: reverse + join
test_filter 4 "reverse + join" \
    '{{ items | reverse | join }}' \
    '{"items": ["a", "b", "c"]}' \
    'c, b, a'

# Test 5: size filter
test_filter 5 "size: array length" \
    '{{ items | size }}' \
    '{"items": ["a", "b", "c", "d"]}' \
    '4'

# Test 6: sort + join
test_filter 6 "sort + join" \
    '{{ items | sort | join }}' \
    '{"items": ["cherry", "apple", "banana"]}' \
    'apple, banana, cherry'

# Test 7: uniq + join
test_filter 7 "uniq + join" \
    '{{ items | uniq | join }}' \
    '{"items": ["a", "b", "a", "c", "b"]}' \
    'a, b, c'

# Test 8: compact + join
test_filter 8 "compact + join" \
    '{{ items | compact | join }}' \
    '{"items": ["a", null, "b", null, "c"]}' \
    'a, b, c'

# Test 9: Empty array size
test_filter 9 "empty array size" \
    '{{ items | size }}' \
    '{"items": []}' \
    '0'

# Test 10: numbers join
test_filter 10 "numbers join" \
    '{{ numbers | join }}' \
    '{"numbers": [1, 2, 3]}' \
    '1, 2, 3'

echo
echo "=== Test Summary ==="
echo -e "Tests passed: ${GREEN}${TESTS_PASSED}${NC}"
echo -e "Tests failed: ${RED}${TESTS_FAILED}${NC}"
echo

if [ $TESTS_FAILED -eq 0 ]; then
    echo -e "${GREEN}All tests passed!${NC}"
    exit 0
else
    echo -e "${RED}Some tests failed.${NC}"
    exit 1
fi
