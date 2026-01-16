#!/bin/bash

# Single-process test for for loop functionality
set -e

echo "Building..."
zig build || exit 1

echo ""
echo "=== Testing For Loop Implementation (single process) ==="

# Run all tests in a single process
(
  # Test 1: Basic for loop
  echo '{"method": "compile", "id": 1, "params": {"template": "{% for item in items %}{{ item }} {% endfor %}"}}'
  echo '{"method": "render", "id": 2, "params": {"template_id": "0", "environment": {"items": ["apple", "banana", "cherry"]}}}'

  # Test 2: forloop.index (1-based)
  echo '{"method": "compile", "id": 3, "params": {"template": "{% for item in items %}{{ forloop.index }}: {{ item }} {% endfor %}"}}'
  echo '{"method": "render", "id": 4, "params": {"template_id": "1", "environment": {"items": ["apple", "banana", "cherry"]}}}'

  # Test 3: forloop.index0 (0-based)
  echo '{"method": "compile", "id": 5, "params": {"template": "{% for item in items %}[{{ forloop.index0 }}]={{ item }} {% endfor %}"}}'
  echo '{"method": "render", "id": 6, "params": {"template_id": "2", "environment": {"items": ["apple", "banana", "cherry"]}}}'

  # Test 4: forloop.first
  echo '{"method": "compile", "id": 7, "params": {"template": "{% for item in items %}{% if forloop.first %}First: {% endif %}{{ item }} {% endfor %}"}}'
  echo '{"method": "render", "id": 8, "params": {"template_id": "3", "environment": {"items": ["apple", "banana", "cherry"]}}}'

  # Test 5: forloop.last
  echo '{"method": "compile", "id": 9, "params": {"template": "{% for item in items %}{{ item }}{% if forloop.last %}!{% else %}, {% endif %}{% endfor %}"}}'
  echo '{"method": "render", "id": 10, "params": {"template_id": "4", "environment": {"items": ["apple", "banana", "cherry"]}}}'

  # Test 6: forloop.length
  echo '{"method": "compile", "id": 11, "params": {"template": "{% for item in items %}{{ item }} ({{ forloop.index }}/{{ forloop.length }}) {% endfor %}"}}'
  echo '{"method": "render", "id": 12, "params": {"template_id": "5", "environment": {"items": ["apple", "banana", "cherry"]}}}'

  # Test 7: Empty array
  echo '{"method": "compile", "id": 13, "params": {"template": "Before{% for item in empty %}{{ item }}{% endfor %}After"}}'
  echo '{"method": "render", "id": 14, "params": {"template_id": "6", "environment": {"empty": []}}}'

  # Test 8: Single item array
  echo '{"method": "compile", "id": 15, "params": {"template": "{% for item in single %}First={{ forloop.first }} Last={{ forloop.last }} Item={{ item }}{% endfor %}"}}'
  echo '{"method": "render", "id": 16, "params": {"template_id": "7", "environment": {"single": ["only"]}}}'

  # Test 9: Loop over numbers
  echo '{"method": "compile", "id": 17, "params": {"template": "{% for num in numbers %}{{ num }} {% endfor %}"}}'
  echo '{"method": "render", "id": 18, "params": {"template_id": "8", "environment": {"numbers": [1, 2, 3, 4, 5]}}}'

  # Test 10: Loop over objects
  echo '{"method": "compile", "id": 19, "params": {"template": "{% for user in users %}{{ user.name }} is {{ user.age }}. {% endfor %}"}}'
  echo '{"method": "render", "id": 20, "params": {"template_id": "9", "environment": {"users": [{"name": "Alice", "age": 30}, {"name": "Bob", "age": 25}]}}}'

  # Test 11: All forloop properties
  echo '{"method": "compile", "id": 21, "params": {"template": "{% for item in items %}i={{ forloop.index }} i0={{ forloop.index0 }} first={{ forloop.first }} last={{ forloop.last }} len={{ forloop.length }} item={{ item }}\n{% endfor %}"}}'
  echo '{"method": "render", "id": 22, "params": {"template_id": "10", "environment": {"items": ["a", "b", "c"]}}}'

) | ./zig-out/bin/liquid-zig

echo ""
echo "=== All Tests Complete ==="
