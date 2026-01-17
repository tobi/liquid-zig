#!/bin/bash

# Test suite for array filters (US-017)

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

    cat > /tmp/test_array_${test_num}.json << EOF
{
  "template": "$template",
  "environment": $context,
  "expected": "$expected"
}
EOF

    if ./zig-out/bin/liquid-zig /tmp/test_array_${test_num}.json 2>&1 >/dev/null; then
        echo -e "${GREEN}✓ Test $test_num ($description): PASS${NC}"
        ((TESTS_PASSED++))
    else
        echo -e "${RED}✗ Test $test_num ($description): FAIL${NC}"
        echo "  Template: $template"
        echo "  Context: $context"
        echo "  Expected: $expected"
        ((TESTS_FAILED++))
    fi

    rm -f /tmp/test_array_${test_num}.json
}

echo "=== Testing Array Filters (US-017) ==="
echo

# Test 1: join filter - basic
test_filter 1 "join: basic with default separator" \
    '{{ items | join }}' \
    '{"items": ["a", "b", "c"]}' \
    'a, b, c'

# Test 2: join filter - custom separator
test_filter 2 "join: custom separator" \
    '{{ items | join: " - " }}' \
    '{"items": ["apple", "banana", "cherry"]}' \
    'apple - banana - cherry'

# Test 3: join filter - numbers
test_filter 3 "join: with numbers" \
    '{{ numbers | join: ", " }}' \
    '{"numbers": [1, 2, 3, 4, 5]}' \
    '1, 2, 3, 4, 5'

# Test 4: first filter - single element
test_filter 4 "first: single element" \
    '{{ items | first }}' \
    '{"items": ["apple", "banana", "cherry"]}' \
    'apple'

# Test 5: last filter - single element
test_filter 5 "last: single element" \
    '{{ items | last }}' \
    '{"items": ["apple", "banana", "cherry"]}' \
    'cherry'

# Test 6: reverse filter
test_filter 6 "reverse: basic" \
    '{{ items | reverse | join: ", " }}' \
    '{"items": ["a", "b", "c"]}' \
    'c, b, a'

# Test 7: sort filter - strings
test_filter 7 "sort: strings" \
    '{{ items | sort | join: ", " }}' \
    '{"items": ["cherry", "apple", "banana"]}' \
    'apple, banana, cherry'

# Test 8: sort filter - numbers
test_filter 8 "sort: numbers" \
    '{{ numbers | sort | join: ", " }}' \
    '{"numbers": [5, 2, 8, 1, 9]}' \
    '1, 2, 5, 8, 9'

# Test 9: sort_natural filter - case insensitive
test_filter 9 "sort_natural: case insensitive" \
    '{{ items | sort_natural | join: ", " }}' \
    '{"items": ["Zebra", "apple", "Banana"]}' \
    'apple, Banana, Zebra'

# Test 10: uniq filter
test_filter 10 "uniq: remove duplicates" \
    '{{ items | uniq | join: ", " }}' \
    '{"items": ["a", "b", "a", "c", "b", "a"]}' \
    'a, b, c'

# Test 11: compact filter - remove nulls
test_filter 11 "compact: remove nulls" \
    '{{ items | compact | join: ", " }}' \
    '{"items": ["a", null, "b", null, "c"]}' \
    'a, b, c'

# Test 12: size filter - array length
test_filter 12 "size: array length" \
    '{{ items | size }}' \
    '{"items": ["a", "b", "c", "d"]}' \
    '4'

# Test 13: map filter - extract property
test_filter 13 "map: extract property" \
    '{{ users | map: "name" | join: ", " }}' \
    '{"users": [{"name": "Alice", "age": 30}, {"name": "Bob", "age": 25}]}' \
    'Alice, Bob'

# Test 14: where filter - filter by property
test_filter 14 "where: filter by property" \
    '{{ users | where: "active", "true" | map: "name" | join: ", " }}' \
    '{"users": [{"name": "Alice", "active": "true"}, {"name": "Bob", "active": "false"}, {"name": "Charlie", "active": "true"}]}' \
    'Alice, Charlie'

# Test 15: push filter - add to end
test_filter 15 "push: add element to end" \
    '{{ items | push: "d" | join: ", " }}' \
    '{"items": ["a", "b", "c"]}' \
    'a, b, c, d'

# Test 16: pop filter - remove last
test_filter 16 "pop: remove last element" \
    '{{ items | pop | join: ", " }}' \
    '{"items": ["a", "b", "c", "d"]}' \
    'a, b, c'

# Test 17: shift filter - remove first
test_filter 17 "shift: remove first element" \
    '{{ items | shift | join: ", " }}' \
    '{"items": ["a", "b", "c", "d"]}' \
    'b, c, d'

# Test 18: unshift filter - add to beginning
test_filter 18 "unshift: add element to beginning" \
    '{{ items | unshift: "z" | join: ", " }}' \
    '{"items": ["a", "b", "c"]}' \
    'z, a, b, c'

# Test 19: Filter chains - complex
test_filter 19 "chain: reverse then join" \
    '{{ items | reverse | join: " > " }}' \
    '{"items": ["first", "second", "third"]}' \
    'third > second > first'

# Test 20: Filter chains - sort and unique
test_filter 20 "chain: uniq then sort" \
    '{{ items | uniq | sort | join: ", " }}' \
    '{"items": ["c", "a", "b", "a", "c"]}' \
    'a, b, c'

# Test 21: Empty array - join
test_filter 21 "edge: empty array join" \
    '{{ items | join: ", " }}' \
    '{"items": []}' \
    ''

# Test 22: Empty array - size
test_filter 22 "edge: empty array size" \
    '{{ items | size }}' \
    '{"items": []}' \
    '0'

# Test 23: Single element array - reverse
test_filter 23 "edge: single element reverse" \
    '{{ items | reverse | join }}' \
    '{"items": ["only"]}' \
    'only'

# Test 24: Mixed types - join
test_filter 24 "mixed: numbers and strings" \
    '{{ items | join: " | " }}' \
    '{"items": ["a", 1, "b", 2]}' \
    'a | 1 | b | 2'

# Test 25: Filter chain - comprehensive
test_filter 25 "comprehensive: multi-filter chain" \
    '{{ numbers | push: "6" | uniq | sort | join: "-" }}' \
    '{"numbers": [3, 1, 4, 1, 5, 9, 2, 6]}' \
    '1-2-3-4-5-6-9'

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
