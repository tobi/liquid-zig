#!/bin/bash

# Build the project
echo "Building..."
zig build || exit 1

echo ""
echo "=== Comparison Operators Tests ==="
echo ""

echo "=== Test 1: == operator (equal integers) ==="
echo '{"method": "compile", "id": 1, "params": {"template": "{% if x == 5 %}equal{% else %}not equal{% endif %}"}}' | ./zig-out/bin/liquid-zig
echo '{"method": "render", "id": 2, "params": {"template_id": "0", "environment": {"x": 5}}}' | ./zig-out/bin/liquid-zig
echo ""

echo "=== Test 2: == operator (not equal integers) ==="
echo '{"method": "compile", "id": 3, "params": {"template": "{% if x == 5 %}equal{% else %}not equal{% endif %}"}}' | ./zig-out/bin/liquid-zig
echo '{"method": "render", "id": 4, "params": {"template_id": "1", "environment": {"x": 10}}}' | ./zig-out/bin/liquid-zig
echo ""

echo "=== Test 3: != operator ==="
echo '{"method": "compile", "id": 5, "params": {"template": "{% if x != 5 %}not equal{% else %}equal{% endif %}"}}' | ./zig-out/bin/liquid-zig
echo '{"method": "render", "id": 6, "params": {"template_id": "2", "environment": {"x": 10}}}' | ./zig-out/bin/liquid-zig
echo ""

echo "=== Test 4: < operator ==="
echo '{"method": "compile", "id": 7, "params": {"template": "{% if x < 10 %}less{% else %}not less{% endif %}"}}' | ./zig-out/bin/liquid-zig
echo '{"method": "render", "id": 8, "params": {"template_id": "3", "environment": {"x": 5}}}' | ./zig-out/bin/liquid-zig
echo ""

echo "=== Test 5: > operator ==="
echo '{"method": "compile", "id": 9, "params": {"template": "{% if x > 10 %}greater{% else %}not greater{% endif %}"}}' | ./zig-out/bin/liquid-zig
echo '{"method": "render", "id": 10, "params": {"template_id": "4", "environment": {"x": 15}}}' | ./zig-out/bin/liquid-zig
echo ""

echo "=== Test 6: <= operator ==="
echo '{"method": "compile", "id": 11, "params": {"template": "{% if x <= 10 %}less or equal{% else %}greater{% endif %}"}}' | ./zig-out/bin/liquid-zig
echo '{"method": "render", "id": 12, "params": {"template_id": "5", "environment": {"x": 10}}}' | ./zig-out/bin/liquid-zig
echo ""

echo "=== Test 7: >= operator ==="
echo '{"method": "compile", "id": 13, "params": {"template": "{% if x >= 10 %}greater or equal{% else %}less{% endif %}"}}' | ./zig-out/bin/liquid-zig
echo '{"method": "render", "id": 14, "params": {"template_id": "6", "environment": {"x": 10}}}' | ./zig-out/bin/liquid-zig
echo ""

echo "=== Test 8: String equality ==="
echo '{"method": "compile", "id": 15, "params": {"template": "{% if name == \"Alice\" %}Hello Alice{% else %}Not Alice{% endif %}"}}' | ./zig-out/bin/liquid-zig
echo '{"method": "render", "id": 16, "params": {"template_id": "7", "environment": {"name": "Alice"}}}' | ./zig-out/bin/liquid-zig
echo ""

echo "=== Test 9: String comparison < ==="
echo '{"method": "compile", "id": 17, "params": {"template": "{% if name < \"Bob\" %}before Bob{% else %}after Bob{% endif %}"}}' | ./zig-out/bin/liquid-zig
echo '{"method": "render", "id": 18, "params": {"template_id": "8", "environment": {"name": "Alice"}}}' | ./zig-out/bin/liquid-zig
echo ""

echo ""
echo "=== Logical Operators Tests ==="
echo ""

echo "=== Test 10: and operator (both true) ==="
echo '{"method": "compile", "id": 19, "params": {"template": "{% if x == 5 and y == 3 %}both true{% else %}not both{% endif %}"}}' | ./zig-out/bin/liquid-zig
echo '{"method": "render", "id": 20, "params": {"template_id": "9", "environment": {"x": 5, "y": 3}}}' | ./zig-out/bin/liquid-zig
echo ""

echo "=== Test 11: and operator (first false) ==="
echo '{"method": "compile", "id": 21, "params": {"template": "{% if x == 5 and y == 3 %}both true{% else %}not both{% endif %}"}}' | ./zig-out/bin/liquid-zig
echo '{"method": "render", "id": 22, "params": {"template_id": "10", "environment": {"x": 10, "y": 3}}}' | ./zig-out/bin/liquid-zig
echo ""

echo "=== Test 12: and operator (second false) ==="
echo '{"method": "compile", "id": 23, "params": {"template": "{% if x == 5 and y == 3 %}both true{% else %}not both{% endif %}"}}' | ./zig-out/bin/liquid-zig
echo '{"method": "render", "id": 24, "params": {"template_id": "11", "environment": {"x": 5, "y": 10}}}' | ./zig-out/bin/liquid-zig
echo ""

echo "=== Test 13: or operator (both true) ==="
echo '{"method": "compile", "id": 25, "params": {"template": "{% if x == 5 or y == 3 %}at least one{% else %}neither{% endif %}"}}' | ./zig-out/bin/liquid-zig
echo '{"method": "render", "id": 26, "params": {"template_id": "12", "environment": {"x": 5, "y": 3}}}' | ./zig-out/bin/liquid-zig
echo ""

echo "=== Test 14: or operator (first true, second false) ==="
echo '{"method": "compile", "id": 27, "params": {"template": "{% if x == 5 or y == 3 %}at least one{% else %}neither{% endif %}"}}' | ./zig-out/bin/liquid-zig
echo '{"method": "render", "id": 28, "params": {"template_id": "13", "environment": {"x": 5, "y": 10}}}' | ./zig-out/bin/liquid-zig
echo ""

echo "=== Test 15: or operator (first false, second true) ==="
echo '{"method": "compile", "id": 29, "params": {"template": "{% if x == 5 or y == 3 %}at least one{% else %}neither{% endif %}"}}' | ./zig-out/bin/liquid-zig
echo '{"method": "render", "id": 30, "params": {"template_id": "14", "environment": {"x": 10, "y": 3}}}' | ./zig-out/bin/liquid-zig
echo ""

echo "=== Test 16: or operator (both false) ==="
echo '{"method": "compile", "id": 31, "params": {"template": "{% if x == 5 or y == 3 %}at least one{% else %}neither{% endif %}"}}' | ./zig-out/bin/liquid-zig
echo '{"method": "render", "id": 32, "params": {"template_id": "15", "environment": {"x": 10, "y": 10}}}' | ./zig-out/bin/liquid-zig
echo ""

echo ""
echo "=== Contains Operator Tests ==="
echo ""

echo "=== Test 17: contains in string (found) ==="
echo '{"method": "compile", "id": 33, "params": {"template": "{% if text contains \"world\" %}found{% else %}not found{% endif %}"}}' | ./zig-out/bin/liquid-zig
echo '{"method": "render", "id": 34, "params": {"template_id": "16", "environment": {"text": "hello world"}}}' | ./zig-out/bin/liquid-zig
echo ""

echo "=== Test 18: contains in string (not found) ==="
echo '{"method": "compile", "id": 35, "params": {"template": "{% if text contains \"goodbye\" %}found{% else %}not found{% endif %}"}}' | ./zig-out/bin/liquid-zig
echo '{"method": "render", "id": 36, "params": {"template_id": "17", "environment": {"text": "hello world"}}}' | ./zig-out/bin/liquid-zig
echo ""

echo "=== Test 19: contains in array (found) ==="
echo '{"method": "compile", "id": 37, "params": {"template": "{% if items contains \"apple\" %}has apple{% else %}no apple{% endif %}"}}' | ./zig-out/bin/liquid-zig
echo '{"method": "render", "id": 38, "params": {"template_id": "18", "environment": {"items": ["apple", "banana", "orange"]}}}' | ./zig-out/bin/liquid-zig
echo ""

echo "=== Test 20: contains in array (not found) ==="
echo '{"method": "compile", "id": 39, "params": {"template": "{% if items contains \"grape\" %}has grape{% else %}no grape{% endif %}"}}' | ./zig-out/bin/liquid-zig
echo '{"method": "render", "id": 40, "params": {"template_id": "19", "environment": {"items": ["apple", "banana", "orange"]}}}' | ./zig-out/bin/liquid-zig
echo ""

echo ""
echo "=== Operator Precedence Tests ==="
echo ""

echo "=== Test 21: and before or (a or b and c) - b and c is true ==="
echo '{"method": "compile", "id": 41, "params": {"template": "{% if a == 1 or b == 2 and c == 3 %}match{% else %}no match{% endif %}"}}' | ./zig-out/bin/liquid-zig
echo '{"method": "render", "id": 42, "params": {"template_id": "20", "environment": {"a": 99, "b": 2, "c": 3}}}' | ./zig-out/bin/liquid-zig
echo ""

echo "=== Test 22: and before or (a or b and c) - only a is true ==="
echo '{"method": "compile", "id": 43, "params": {"template": "{% if a == 1 or b == 2 and c == 3 %}match{% else %}no match{% endif %}"}}' | ./zig-out/bin/liquid-zig
echo '{"method": "render", "id": 44, "params": {"template_id": "21", "environment": {"a": 1, "b": 99, "c": 99}}}' | ./zig-out/bin/liquid-zig
echo ""

echo "=== Test 23: and before or (a or b and c) - all false ==="
echo '{"method": "compile", "id": 45, "params": {"template": "{% if a == 1 or b == 2 and c == 3 %}match{% else %}no match{% endif %}"}}' | ./zig-out/bin/liquid-zig
echo '{"method": "render", "id": 46, "params": {"template_id": "22", "environment": {"a": 99, "b": 99, "c": 99}}}' | ./zig-out/bin/liquid-zig
echo ""

echo ""
echo "=== Complex Expressions Tests ==="
echo ""

echo "=== Test 24: Complex expression with comparisons and logical operators ==="
echo '{"method": "compile", "id": 47, "params": {"template": "{% if x > 5 and y < 10 %}valid range{% else %}out of range{% endif %}"}}' | ./zig-out/bin/liquid-zig
echo '{"method": "render", "id": 48, "params": {"template_id": "23", "environment": {"x": 7, "y": 8}}}' | ./zig-out/bin/liquid-zig
echo ""

echo "=== Test 25: Complex expression with not equals and or ==="
echo '{"method": "compile", "id": 49, "params": {"template": "{% if status != \"error\" or retry == true %}continue{% else %}stop{% endif %}"}}' | ./zig-out/bin/liquid-zig
echo '{"method": "render", "id": 50, "params": {"template_id": "24", "environment": {"status": "error", "retry": true}}}' | ./zig-out/bin/liquid-zig
echo ""

echo "=== Test 26: elsif with operators ==="
echo '{"method": "compile", "id": 51, "params": {"template": "{% if x < 0 %}negative{% elsif x == 0 %}zero{% elsif x > 0 %}positive{% endif %}"}}' | ./zig-out/bin/liquid-zig
echo '{"method": "render", "id": 52, "params": {"template_id": "25", "environment": {"x": 5}}}' | ./zig-out/bin/liquid-zig
echo ""

echo "=== Test 27: Multiple conditions in elsif ==="
echo '{"method": "compile", "id": 53, "params": {"template": "{% if x == 1 %}one{% elsif x >= 2 and x <= 10 %}range{% else %}out{% endif %}"}}' | ./zig-out/bin/liquid-zig
echo '{"method": "render", "id": 54, "params": {"template_id": "26", "environment": {"x": 5}}}' | ./zig-out/bin/liquid-zig
echo ""

echo ""
echo "All operator tests completed!"
