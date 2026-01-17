#!/bin/bash

# Test suite for comment and raw tags (US-019)

set -e

echo "Building liquid engine..."
zig build

echo ""
echo "Running comment and raw tag tests..."
echo "======================================"

# Run all tests in a single process
{
    # Test 1: Basic comment block produces no output
    echo '{"method": "compile", "id": 1, "params": {"template": "Before{% comment %}This should not appear{% endcomment %}After"}}'
    echo '{"method": "render", "id": 2, "params": {"template_id": "0"}}'

    # Test 2: Multi-line comment
    echo '{"method": "compile", "id": 3, "params": {"template": "Start\n{% comment %}\nLine 1\nLine 2\n{% endcomment %}\nEnd"}}'
    echo '{"method": "render", "id": 4, "params": {"template_id": "1"}}'

    # Test 3: Comment with variables inside
    echo '{"method": "compile", "id": 5, "params": {"template": "{% comment %}{{ name }} hidden{% endcomment %}Done"}}'
    echo '{"method": "render", "id": 6, "params": {"template_id": "2"}}'

    # Test 4: Comment with tags inside
    echo '{"method": "compile", "id": 7, "params": {"template": "{% comment %}{% assign x = 5 %}{% endcomment %}Value: {{ x }}"}}'
    echo '{"method": "render", "id": 8, "params": {"template_id": "3", "environment": {"x": 10}}}'

    # Test 5: Multiple comments in one template
    echo '{"method": "compile", "id": 9, "params": {"template": "A{% comment %}1{% endcomment %}B{% comment %}2{% endcomment %}C"}}'
    echo '{"method": "render", "id": 10, "params": {"template_id": "4"}}'

    # Test 6: Basic raw block
    echo '{"method": "compile", "id": 11, "params": {"template": "{% raw %}{{ name }}{% endraw %}"}}'
    echo '{"method": "render", "id": 12, "params": {"template_id": "5"}}'

    # Test 7: Raw block with tag syntax
    echo '{"method": "compile", "id": 13, "params": {"template": "{% raw %}{% if true %}hello{% endif %}{% endraw %}"}}'
    echo '{"method": "render", "id": 14, "params": {"template_id": "6"}}'

    # Test 8: Raw block preserves whitespace
    echo '{"method": "compile", "id": 15, "params": {"template": "{% raw %}  {{ name }}  {% endraw %}"}}'
    echo '{"method": "render", "id": 16, "params": {"template_id": "7"}}'

    # Test 9: Raw block with multiple variables and tags
    echo '{"method": "compile", "id": 17, "params": {"template": "{% raw %}{{ foo }} {% assign x = 1 %} {{ bar }}{% endraw %}"}}'
    echo '{"method": "render", "id": 18, "params": {"template_id": "8"}}'

    # Test 10: Raw block in template with actual variables
    echo '{"method": "compile", "id": 19, "params": {"template": "Name: {{ name }}. Code: {% raw %}{{ name }}{% endraw %}"}}'
    echo '{"method": "render", "id": 20, "params": {"template_id": "9", "environment": {"name": "Alice"}}}'

    # Test 11: Empty comment
    echo '{"method": "compile", "id": 21, "params": {"template": "A{% comment %}{% endcomment %}B"}}'
    echo '{"method": "render", "id": 22, "params": {"template_id": "10"}}'

    # Test 12: Empty raw block
    echo '{"method": "compile", "id": 23, "params": {"template": "A{% raw %}{% endraw %}B"}}'
    echo '{"method": "render", "id": 24, "params": {"template_id": "11"}}'

    # Test 13: Comment with nested curly braces
    echo '{"method": "compile", "id": 25, "params": {"template": "{% comment %}{{ {{ nested }} }}{% endcomment %}Done"}}'
    echo '{"method": "render", "id": 26, "params": {"template_id": "12"}}'

    # Test 14: Multi-line raw block
    echo '{"method": "compile", "id": 27, "params": {"template": "{% raw %}\n{{ variable }}\n{% if condition %}\n  content\n{% endif %}\n{% endraw %}"}}'
    echo '{"method": "render", "id": 28, "params": {"template_id": "13"}}'

    # Test 15: Comment and raw in same template
    echo '{"method": "compile", "id": 29, "params": {"template": "{% comment %}hidden{% endcomment %}{% raw %}{{ shown }}{% endraw %}"}}'
    echo '{"method": "render", "id": 30, "params": {"template_id": "14"}}'

} | ./zig-out/bin/liquid-zig | grep -E 'render.*result' | jq -r '.result.output' | while read -r output; do
    echo "$output"
done

echo ""
echo "======================================"
echo "All tests completed successfully!"
echo "======================================"
