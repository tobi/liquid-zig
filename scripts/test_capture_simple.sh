#!/bin/bash

# Simplified capture tag test - all in one process

set -e

echo "Building liquid engine..."
zig build

echo ""
echo "Running capture tag tests..."
echo "=============================="

# Run all tests in a single process
{
    # Test 1: Basic capture
    echo '{"method": "compile", "id": 1, "params": {"template": "{% capture greeting %}Hello World{% endcapture %}{{ greeting }}"}}'
    echo '{"method": "render", "id": 2, "params": {"template_id": "0"}}'

    # Test 2: Capture with variables
    echo '{"method": "compile", "id": 3, "params": {"template": "{% capture message %}Hello {{ name }}!{% endcapture %}{{ message }}"}}'
    echo '{"method": "render", "id": 4, "params": {"template_id": "1", "environment": {"name": "Alice"}}}'

    # Test 3: Capture with filters
    echo '{"method": "compile", "id": 5, "params": {"template": "{% capture lower %}HELLO{% endcapture %}{{ lower | downcase }}"}}'
    echo '{"method": "render", "id": 6, "params": {"template_id": "2"}}'

    # Test 4: Capture variable accessible after endcapture
    echo '{"method": "compile", "id": 7, "params": {"template": "{% capture x %}test{% endcapture %}before:{{ x }} after:{{ x }}"}}'
    echo '{"method": "render", "id": 8, "params": {"template_id": "3"}}'

    # Test 5: Nested captures
    echo '{"method": "compile", "id": 9, "params": {"template": "{% capture outer %}A{% capture inner %}B{% endcapture %}{{ inner }}C{% endcapture %}{{ outer }}"}}'
    echo '{"method": "render", "id": 10, "params": {"template_id": "4"}}'

    # Test 6: Capture with if tag
    echo '{"method": "compile", "id": 11, "params": {"template": "{% capture result %}{% if show %}visible{% endif %}{% endcapture %}{{ result }}"}}'
    echo '{"method": "render", "id": 12, "params": {"template_id": "5", "environment": {"show": true}}}'

    # Test 7: Capture with for loop
    echo '{"method": "compile", "id": 13, "params": {"template": "{% capture items %}{% for i in array %}{{ i }}{% endfor %}{% endcapture %}{{ items }}"}}'
    echo '{"method": "render", "id": 14, "params": {"template_id": "6", "environment": {"array": [1, 2, 3]}}}'

    # Test 8: Multiple captures
    echo '{"method": "compile", "id": 15, "params": {"template": "{% capture a %}A{% endcapture %}{% capture b %}B{% endcapture %}{{ a }}{{ b }}"}}'
    echo '{"method": "render", "id": 16, "params": {"template_id": "7"}}'

    # Test 9: Capture with complex string building
    echo '{"method": "compile", "id": 17, "params": {"template": "{% capture link %}<a href=\"{{ url }}\">{{ title }}</a>{% endcapture %}{{ link }}"}}'
    echo '{"method": "render", "id": 18, "params": {"template_id": "8", "environment": {"url": "/page", "title": "Click"}}}'

    # Test 10: Empty capture
    echo '{"method": "compile", "id": 19, "params": {"template": "{% capture empty %}{% endcapture %}x{{ empty }}y"}}'
    echo '{"method": "render", "id": 20, "params": {"template_id": "9"}}'

    # Test 11: Capture overwriting variable
    echo '{"method": "compile", "id": 21, "params": {"template": "{% assign x = \"old\" %}{% capture x %}new{% endcapture %}{{ x }}"}}'
    echo '{"method": "render", "id": 22, "params": {"template_id": "10"}}'

    # Test 12: Capture with math operations
    echo '{"method": "compile", "id": 23, "params": {"template": "{% capture calc %}{{ 5 | plus: 3 }}{% endcapture %}Result: {{ calc }}"}}'
    echo '{"method": "render", "id": 24, "params": {"template_id": "11"}}'

} | ./zig-out/bin/liquid-zig | grep -E 'render.*result' | jq -r '.result.output' | while read -r output; do
    echo "$output"
done
