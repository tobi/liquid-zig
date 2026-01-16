#!/bin/bash

# Test suite for string filters (US-016)

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

    cat > /tmp/test_${test_num}.json << EOF
{
  "template": "$template",
  "environment": $context,
  "expected": "$expected"
}
EOF

    if ./zig-out/bin/liquid-zig /tmp/test_${test_num}.json 2>&1 >/dev/null; then
        echo -e "${GREEN}✓ Test $test_num ($description): PASS${NC}"
        ((TESTS_PASSED++))
    else
        echo -e "${RED}✗ Test $test_num ($description): FAIL${NC}"
        echo "  Template: $template"
        echo "  Context: $context"
        echo "  Expected: $expected"
        ((TESTS_FAILED++))
    fi

    rm -f /tmp/test_${test_num}.json
}

echo "=== Testing String Filters (US-016) ==="
echo

# Test 1: append filter
test_filter 1 "append: basic" \
    '{{ "hello" | append: " world" }}' \
    '{}' \
    'hello world'

# Test 2: append filter with variable
test_filter 2 "append: with variable" \
    '{{ greeting | append: "!" }}' \
    '{"greeting": "Hello"}' \
    'Hello!'

# Test 3: prepend filter
test_filter 3 "prepend: basic" \
    '{{ "world" | prepend: "Hello " }}' \
    '{}' \
    'Hello world'

# Test 4: prepend filter with variable
test_filter 4 "prepend: with variable" \
    '{{ name | prepend: "Mr. " }}' \
    '{"name": "Smith"}' \
    'Mr. Smith'

# Test 5: remove filter
test_filter 5 "remove: basic" \
    '{{ "Hello World" | remove: "World" }}' \
    '{}' \
    'Hello '

# Test 6: remove filter - multiple occurrences
test_filter 6 "remove: multiple" \
    '{{ "banana" | remove: "a" }}' \
    '{}' \
    'bnn'

# Test 7: replace filter
test_filter 7 "replace: basic" \
    '{{ "Hello World" | replace: "World", "Universe" }}' \
    '{}' \
    'Hello Universe'

# Test 8: replace filter - multiple occurrences
test_filter 8 "replace: multiple" \
    '{{ "banana" | replace: "a", "o" }}' \
    '{}' \
    'bonono'

# Test 9: slice filter - single char
test_filter 9 "slice: single char" \
    '{{ "Hello" | slice: 0 }}' \
    '{}' \
    'H'

# Test 10: slice filter - substring
test_filter 10 "slice: substring" \
    '{{ "Hello World" | slice: 0, 5 }}' \
    '{}' \
    'Hello'

# Test 11: slice filter - negative index
test_filter 11 "slice: negative index" \
    '{{ "Hello" | slice: -3, 2 }}' \
    '{}' \
    'll'

# Test 12: split filter
test_filter 12 "split: basic" \
    '{{ "a,b,c" | split: "," }}' \
    '{}' \
    '["a", "b", "c"]'

# Test 13: split filter - with spaces
test_filter 13 "split: with spaces" \
    '{{ "Hello World Test" | split: " " }}' \
    '{}' \
    '["Hello", "World", "Test"]'

# Test 14: strip filter (already exists, verify it works)
test_filter 14 "strip: whitespace" \
    '{{ "  hello  " | strip }}' \
    '{}' \
    'hello'

# Test 15: lstrip filter
test_filter 15 "lstrip: left whitespace" \
    '{{ "  hello  " | lstrip }}' \
    '{}' \
    'hello  '

# Test 16: rstrip filter
test_filter 16 "rstrip: right whitespace" \
    '{{ "  hello  " | rstrip }}' \
    '{}' \
    '  hello'

# Test 17: truncate filter
test_filter 17 "truncate: basic" \
    '{{ "Hello World" | truncate: 8 }}' \
    '{}' \
    'Hello...'

# Test 18: truncate filter - custom ellipsis
test_filter 18 "truncate: custom ellipsis" \
    '{{ "Hello World" | truncate: 8, "…" }}' \
    '{}' \
    'Hello W…'

# Test 19: truncate filter - no truncation needed
test_filter 19 "truncate: no truncation" \
    '{{ "Hello" | truncate: 10 }}' \
    '{}' \
    'Hello'

# Test 20: truncatewords filter
test_filter 20 "truncatewords: basic" \
    '{{ "Hello World Test String" | truncatewords: 2 }}' \
    '{}' \
    'Hello World...'

# Test 21: truncatewords filter - no truncation
test_filter 21 "truncatewords: no truncation" \
    '{{ "Hello World" | truncatewords: 3 }}' \
    '{}' \
    'Hello World'

# Test 22: truncatewords filter - custom ellipsis
test_filter 22 "truncatewords: custom ellipsis" \
    '{{ "One Two Three Four" | truncatewords: 2, " [more]" }}' \
    '{}' \
    'One Two [more]'

# Test 23: url_encode filter
test_filter 23 "url_encode: basic" \
    '{{ "hello world" | url_encode }}' \
    '{}' \
    'hello+world'

# Test 24: url_encode filter - special chars
test_filter 24 "url_encode: special chars" \
    '{{ "hello@world.com" | url_encode }}' \
    '{}' \
    'hello%40world.com'

# Test 25: url_decode filter
test_filter 25 "url_decode: basic" \
    '{{ "hello+world" | url_decode }}' \
    '{}' \
    'hello world'

# Test 26: url_decode filter - percent encoding
test_filter 26 "url_decode: percent encoding" \
    '{{ "hello%40world.com" | url_decode }}' \
    '{}' \
    'hello@world.com'

# Test 27: newline_to_br filter
test_filter 27 "newline_to_br: basic" \
    '{{ "hello
world" | newline_to_br }}' \
    '{}' \
    'hello<br>world'

# Test 28: strip_html filter
test_filter 28 "strip_html: basic" \
    '{{ "<p>Hello</p>" | strip_html }}' \
    '{}' \
    'Hello'

# Test 29: strip_html filter - complex
test_filter 29 "strip_html: complex" \
    '{{ "<p>Hello <strong>World</strong></p>" | strip_html }}' \
    '{}' \
    'Hello World'

# Test 30: escape filter (already exists, verify it works)
test_filter 30 "escape: HTML entities" \
    '{{ "<p>Hello & goodbye</p>" | escape }}' \
    '{}' \
    '&lt;p&gt;Hello &amp; goodbye&lt;/p&gt;'

# Test 31: escape_once filter
test_filter 31 "escape_once: basic" \
    '{{ "<p>Hello</p>" | escape_once }}' \
    '{}' \
    '&lt;p&gt;Hello&lt;/p&gt;'

# Test 32: escape_once filter - already escaped
test_filter 32 "escape_once: no double escape" \
    '{{ "&lt;p&gt;Hello&lt;/p&gt;" | escape_once }}' \
    '{}' \
    '&lt;p&gt;Hello&lt;/p&gt;'

# Test 33: escape_once filter - mixed content
test_filter 33 "escape_once: mixed content" \
    '{{ "&lt;p&gt; & <div>" | escape_once }}' \
    '{}' \
    '&lt;p&gt; &amp; &lt;div&gt;'

# Test 34: chained filters - append + upcase
test_filter 34 "chained: append + upcase" \
    '{{ "hello" | append: " world" | upcase }}' \
    '{}' \
    'HELLO WORLD'

# Test 35: chained filters - strip + capitalize
test_filter 35 "chained: strip + capitalize" \
    '{{ "  hello world  " | strip | capitalize }}' \
    '{}' \
    'Hello world'

# Test 36: chained filters - replace + truncate
test_filter 36 "chained: replace + truncate" \
    '{{ "Hello World" | replace: "World", "Universe" | truncate: 10 }}' \
    '{}' \
    'Hello U...'

# Test 37: slice with variable
test_filter 37 "slice: with variable" \
    '{{ text | slice: 0, 5 }}' \
    '{"text": "Hello World"}' \
    'Hello'

# Test 38: append/prepend together
test_filter 38 "append + prepend: chained" \
    '{{ "middle" | prepend: "start-" | append: "-end" }}' \
    '{}' \
    'start-middle-end'

# Test 39: remove empty string (should not change)
test_filter 39 "remove: empty string" \
    '{{ "hello" | remove: "" }}' \
    '{}' \
    'hello'

# Test 40: replace with empty string
test_filter 40 "replace: to empty" \
    '{{ "hello world" | replace: " ", "" }}' \
    '{}' \
    'helloworld'

echo
echo "=== Test Summary ==="
echo -e "${GREEN}Tests passed: $TESTS_PASSED${NC}"
if [ $TESTS_FAILED -gt 0 ]; then
    echo -e "${RED}Tests failed: $TESTS_FAILED${NC}"
    exit 1
else
    echo "All tests passed!"
fi
