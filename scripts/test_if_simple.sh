#!/bin/bash

echo "Building..."
zig build || exit 1
echo ""

# Run all tests in a single session
(
echo '{"method": "compile", "id": 1, "params": {"template": "{% if show %}visible{% endif %}"}}'
echo '{"method": "render", "id": 2, "params": {"template_id": "0", "environment": {"show": true}}}'

echo '{"method": "compile", "id": 3, "params": {"template": "{% if show %}yes{% else %}no{% endif %}"}}'
echo '{"method": "render", "id": 4, "params": {"template_id": "1", "environment": {"show": true}}}'

echo '{"method": "compile", "id": 5, "params": {"template": "{% if missing %}yes{% else %}no{% endif %}"}}'
echo '{"method": "render", "id": 6, "params": {"template_id": "2", "environment": {}}}'

echo '{"method": "compile", "id": 7, "params": {"template": "{% if show %}first{% elsif x %}second{% else %}third{% endif %}"}}'
echo '{"method": "render", "id": 8, "params": {"template_id": "3", "environment": {"show": true, "x": 10}}}'

echo '{"method": "compile", "id": 9, "params": {"template": "{% if missing %}first{% elsif x %}second{% else %}third{% endif %}"}}'
echo '{"method": "render", "id": 10, "params": {"template_id": "4", "environment": {"x": 10}}}'

echo '{"method": "compile", "id": 11, "params": {"template": "{% if missing %}first{% elsif also_missing %}second{% else %}third{% endif %}"}}'
echo '{"method": "render", "id": 12, "params": {"template_id": "5", "environment": {}}}'

echo '{"method": "compile", "id": 13, "params": {"template": "{% if show %}outer{% if x %}inner{% endif %}{% endif %}"}}'
echo '{"method": "render", "id": 14, "params": {"template_id": "6", "environment": {"show": true, "x": 10}}}'

echo '{"method": "compile", "id": 15, "params": {"template": "{% if y %}zero is truthy{% else %}zero is falsy{% endif %}"}}'
echo '{"method": "render", "id": 16, "params": {"template_id": "7", "environment": {"y": 0}}}'

echo '{"method": "compile", "id": 17, "params": {"template": "{% if empty_string %}empty string is truthy{% else %}empty string is falsy{% endif %}"}}'
echo '{"method": "render", "id": 18, "params": {"template_id": "8", "environment": {"empty_string": ""}}}'

echo '{"method": "compile", "id": 19, "params": {"template": "{% if empty_array %}empty array is truthy{% else %}empty array is falsy{% endif %}"}}'
echo '{"method": "render", "id": 20, "params": {"template_id": "9", "environment": {"empty_array": []}}}'

echo '{"method": "compile", "id": 21, "params": {"template": "{% if false %}yes{% else %}no{% endif %}"}}'
echo '{"method": "render", "id": 22, "params": {"template_id": "10"}}'

echo '{"method": "compile", "id": 23, "params": {"template": "{% if nil %}yes{% else %}no{% endif %}"}}'
echo '{"method": "render", "id": 24, "params": {"template_id": "11"}}'

echo '{"method": "compile", "id": 25, "params": {"template": "{% if true %}yes{% else %}no{% endif %}"}}'
echo '{"method": "render", "id": 26, "params": {"template_id": "12"}}'
) | ./zig-out/bin/liquid-zig | jq -r 'select(.result.output != null) | "Result: " + .result.output'

echo ""
echo "All tests completed!"
