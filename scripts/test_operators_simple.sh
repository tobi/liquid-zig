#!/bin/bash

# Build the project
echo "Building..."
zig build || exit 1

run_test() {
    local test_name="$1"
    local template="$2"
    local env="$3"

    echo "=== $test_name ==="
    echo '{"method": "compile", "id": 1, "params": {"template": "'"$template"'"}}' | ./zig-out/bin/liquid-zig
    echo '{"method": "render", "id": 2, "params": {"template_id": "0", "environment": '"$env"'}}' | ./zig-out/bin/liquid-zig
    echo ""
}

echo ""
echo "=== Comparison Operators Tests ==="
echo ""

run_test "Test 1: == operator (equal integers)" \
    "{% if x == 5 %}equal{% else %}not equal{% endif %}" \
    '{"x": 5}'

run_test "Test 2: == operator (not equal integers)" \
    "{% if x == 5 %}equal{% else %}not equal{% endif %}" \
    '{"x": 10}'

run_test "Test 3: != operator" \
    "{% if x != 5 %}not equal{% else %}equal{% endif %}" \
    '{"x": 10}'

run_test "Test 4: < operator" \
    "{% if x < 10 %}less{% else %}not less{% endif %}" \
    '{"x": 5}'

run_test "Test 5: > operator" \
    "{% if x > 10 %}greater{% else %}not greater{% endif %}" \
    '{"x": 15}'

run_test "Test 6: <= operator" \
    "{% if x <= 10 %}less or equal{% else %}greater{% endif %}" \
    '{"x": 10}'

run_test "Test 7: >= operator" \
    "{% if x >= 10 %}greater or equal{% else %}less{% endif %}" \
    '{"x": 10}'

run_test "Test 8: String equality" \
    "{% if name == \"Alice\" %}Hello Alice{% else %}Not Alice{% endif %}" \
    '{"name": "Alice"}'

run_test "Test 9: String comparison <" \
    "{% if name < \"Bob\" %}before Bob{% else %}after Bob{% endif %}" \
    '{"name": "Alice"}'

echo ""
echo "=== Logical Operators Tests ==="
echo ""

run_test "Test 10: and operator (both true)" \
    "{% if x == 5 and y == 3 %}both true{% else %}not both{% endif %}" \
    '{"x": 5, "y": 3}'

run_test "Test 11: and operator (first false)" \
    "{% if x == 5 and y == 3 %}both true{% else %}not both{% endif %}" \
    '{"x": 10, "y": 3}'

run_test "Test 12: and operator (second false)" \
    "{% if x == 5 and y == 3 %}both true{% else %}not both{% endif %}" \
    '{"x": 5, "y": 10}'

run_test "Test 13: or operator (both true)" \
    "{% if x == 5 or y == 3 %}at least one{% else %}neither{% endif %}" \
    '{"x": 5, "y": 3}'

run_test "Test 14: or operator (first true, second false)" \
    "{% if x == 5 or y == 3 %}at least one{% else %}neither{% endif %}" \
    '{"x": 5, "y": 10}'

run_test "Test 15: or operator (first false, second true)" \
    "{% if x == 5 or y == 3 %}at least one{% else %}neither{% endif %}" \
    '{"x": 10, "y": 3}'

run_test "Test 16: or operator (both false)" \
    "{% if x == 5 or y == 3 %}at least one{% else %}neither{% endif %}" \
    '{"x": 10, "y": 10}'

echo ""
echo "=== Contains Operator Tests ==="
echo ""

run_test "Test 17: contains in string (found)" \
    "{% if text contains \"world\" %}found{% else %}not found{% endif %}" \
    '{"text": "hello world"}'

run_test "Test 18: contains in string (not found)" \
    "{% if text contains \"goodbye\" %}found{% else %}not found{% endif %}" \
    '{"text": "hello world"}'

run_test "Test 19: contains in array (found)" \
    "{% if items contains \"apple\" %}has apple{% else %}no apple{% endif %}" \
    '{"items": ["apple", "banana", "orange"]}'

run_test "Test 20: contains in array (not found)" \
    "{% if items contains \"grape\" %}has grape{% else %}no grape{% endif %}" \
    '{"items": ["apple", "banana", "orange"]}'

echo ""
echo "=== Operator Precedence Tests ==="
echo ""

run_test "Test 21: and before or (b and c is true)" \
    "{% if a == 1 or b == 2 and c == 3 %}match{% else %}no match{% endif %}" \
    '{"a": 99, "b": 2, "c": 3}'

run_test "Test 22: and before or (only a is true)" \
    "{% if a == 1 or b == 2 and c == 3 %}match{% else %}no match{% endif %}" \
    '{"a": 1, "b": 99, "c": 99}'

run_test "Test 23: and before or (all false)" \
    "{% if a == 1 or b == 2 and c == 3 %}match{% else %}no match{% endif %}" \
    '{"a": 99, "b": 99, "c": 99}'

echo ""
echo "=== Complex Expressions Tests ==="
echo ""

run_test "Test 24: Complex expression with comparisons and logical operators" \
    "{% if x > 5 and y < 10 %}valid range{% else %}out of range{% endif %}" \
    '{"x": 7, "y": 8}'

run_test "Test 25: Complex expression with not equals and or" \
    "{% if status != \"error\" or retry == true %}continue{% else %}stop{% endif %}" \
    '{"status": "error", "retry": true}'

run_test "Test 26: elsif with operators" \
    "{% if x < 0 %}negative{% elsif x == 0 %}zero{% elsif x > 0 %}positive{% endif %}" \
    '{"x": 5}'

run_test "Test 27: Multiple conditions in elsif" \
    "{% if x == 1 %}one{% elsif x >= 2 and x <= 10 %}range{% else %}out{% endif %}" \
    '{"x": 5}'

echo ""
echo "All operator tests completed!"
