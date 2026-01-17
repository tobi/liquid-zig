#!/bin/bash
# Test assign tag with various literal types

echo "Test 1: Assign integer literal"
echo '{"method": "compile", "id": 1, "params": {"template": "{% assign x = 42 %}{{ x }}"}}' | ./zig-out/bin/liquid-zig
echo '{"method": "render", "id": 2, "params": {"template_id": "0"}}' | ./zig-out/bin/liquid-zig
echo ""

echo "Test 2: Assign string literal"
echo '{"method": "compile", "id": 3, "params": {"template": "{% assign name = \"Alice\" %}{{ name }}"}}' | ./zig-out/bin/liquid-zig
echo '{"method": "render", "id": 4, "params": {"template_id": "1"}}' | ./zig-out/bin/liquid-zig
echo ""

echo "Test 3: Assign float literal"
echo '{"method": "compile", "id": 5, "params": {"template": "{% assign pi = 3.14 %}{{ pi }}"}}' | ./zig-out/bin/liquid-zig
echo '{"method": "render", "id": 6, "params": {"template_id": "2"}}' | ./zig-out/bin/liquid-zig
echo ""

echo "Test 4: Assign boolean true"
echo '{"method": "compile", "id": 7, "params": {"template": "{% assign flag = true %}{{ flag }}"}}' | ./zig-out/bin/liquid-zig
echo '{"method": "render", "id": 8, "params": {"template_id": "3"}}' | ./zig-out/bin/liquid-zig
echo ""

echo "Test 5: Assign boolean false"
echo '{"method": "compile", "id": 9, "params": {"template": "{% assign flag = false %}{{ flag }}"}}' | ./zig-out/bin/liquid-zig
echo '{"method": "render", "id": 10, "params": {"template_id": "4"}}' | ./zig-out/bin/liquid-zig
echo ""

echo "Test 6: Assign nil"
echo '{"method": "compile", "id": 11, "params": {"template": "{% assign empty = nil %}[{{ empty }}]"}}' | ./zig-out/bin/liquid-zig
echo '{"method": "render", "id": 12, "params": {"template_id": "5"}}' | ./zig-out/bin/liquid-zig
echo ""

echo "Test 7: Assign from variable"
echo '{"method": "compile", "id": 13, "params": {"template": "{% assign y = x %}{{ y }}"}}' | ./zig-out/bin/liquid-zig
echo '{"method": "render", "id": 14, "params": {"template_id": "6", "environment": {"x": 100}}}' | ./zig-out/bin/liquid-zig
echo ""

echo "Test 8: Reassign variable"
echo '{"method": "compile", "id": 15, "params": {"template": "{% assign a = 1 %}{{ a }}{% assign a = 2 %}{{ a }}"}}' | ./zig-out/bin/liquid-zig
echo '{"method": "render", "id": 16, "params": {"template_id": "7"}}' | ./zig-out/bin/liquid-zig
echo ""

echo "Test 9: Multiple assigns"
echo '{"method": "compile", "id": 17, "params": {"template": "{% assign x = 10 %}{% assign y = 20 %}{{ x }} {{ y }}"}}' | ./zig-out/bin/liquid-zig
echo '{"method": "render", "id": 18, "params": {"template_id": "8"}}' | ./zig-out/bin/liquid-zig
echo ""

echo "Test 10: Assign from nested property"
echo '{"method": "compile", "id": 19, "params": {"template": "{% assign name = user.name %}{{ name }}"}}' | ./zig-out/bin/liquid-zig
echo '{"method": "render", "id": 20, "params": {"template_id": "9", "environment": {"user": {"name": "Bob"}}}}' | ./zig-out/bin/liquid-zig
