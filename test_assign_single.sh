#!/bin/bash
# Test assign tag with various literal types in a single session

(
echo '{"method": "compile", "id": 1, "params": {"template": "{% assign x = 42 %}{{ x }}"}}'
echo '{"method": "render", "id": 2, "params": {"template_id": "0"}}'

echo '{"method": "compile", "id": 3, "params": {"template": "{% assign name = \"Alice\" %}{{ name }}"}}'
echo '{"method": "render", "id": 4, "params": {"template_id": "1"}}'

echo '{"method": "compile", "id": 5, "params": {"template": "{% assign pi = 3.14 %}{{ pi }}"}}'
echo '{"method": "render", "id": 6, "params": {"template_id": "2"}}'

echo '{"method": "compile", "id": 7, "params": {"template": "{% assign flag = true %}{{ flag }}"}}'
echo '{"method": "render", "id": 8, "params": {"template_id": "3"}}'

echo '{"method": "compile", "id": 9, "params": {"template": "{% assign flag = false %}{{ flag }}"}}'
echo '{"method": "render", "id": 10, "params": {"template_id": "4"}}'

echo '{"method": "compile", "id": 11, "params": {"template": "{% assign empty = nil %}[{{ empty }}]"}}'
echo '{"method": "render", "id": 12, "params": {"template_id": "5"}}'

echo '{"method": "compile", "id": 13, "params": {"template": "{% assign y = x %}{{ y }}"}}'
echo '{"method": "render", "id": 14, "params": {"template_id": "6", "environment": {"x": 100}}}'

echo '{"method": "compile", "id": 15, "params": {"template": "{% assign a = 1 %}{{ a }}{% assign a = 2 %}{{ a }}"}}'
echo '{"method": "render", "id": 16, "params": {"template_id": "7"}}'

echo '{"method": "compile", "id": 17, "params": {"template": "{% assign x = 10 %}{% assign y = 20 %}{{ x }} {{ y }}"}}'
echo '{"method": "render", "id": 18, "params": {"template_id": "8"}}'

echo '{"method": "compile", "id": 19, "params": {"template": "{% assign name = user.name %}{{ name }}"}}'
echo '{"method": "render", "id": 20, "params": {"template_id": "9", "environment": {"user": {"name": "Bob"}}}}'

echo '{"method": "quit"}'
) | ./zig-out/bin/liquid-zig
