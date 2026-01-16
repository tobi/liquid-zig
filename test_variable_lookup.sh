#!/bin/bash

# Test script for US-006: Context and variable lookup

set -e

LIQUID_BIN="./zig-out/bin/liquid-zig"

echo "Testing US-006: Context and Variable Lookup"
echo "==========================================="
echo

# Test 1: Simple variable lookup
echo "Test 1: Simple variable from environment"
echo '{"method": "compile", "params": {"template": "Hello {{ name }}!"}, "id": 1}' | $LIQUID_BIN
echo '{"method": "render", "params": {"template_id": "0", "environment": {"name": "World"}}, "id": 2}' | $LIQUID_BIN
echo "Expected: Hello World!"
echo

# Test 2: Undefined variable returns empty (not error)
echo "Test 2: Undefined variable returns empty"
echo '{"method": "compile", "params": {"template": "Value: {{ undefined_var }}"}, "id": 3}' | $LIQUID_BIN
echo '{"method": "render", "params": {"template_id": "1", "environment": {}}, "id": 4}' | $LIQUID_BIN
echo "Expected: Value: "
echo

# Test 3: Multiple variables
echo "Test 3: Multiple variables"
echo '{"method": "compile", "params": {"template": "{{ greeting }} {{ name }}!"}, "id": 5}' | $LIQUID_BIN
echo '{"method": "render", "params": {"template_id": "2", "environment": {"greeting": "Hello", "name": "Alice"}}, "id": 6}' | $LIQUID_BIN
echo "Expected: Hello Alice!"
echo

# Test 4: Nested property access with dot notation
echo "Test 4: Nested property access (dot notation)"
echo '{"method": "compile", "params": {"template": "User: {{ user.name }}, Age: {{ user.age }}"}, "id": 7}' | $LIQUID_BIN
echo '{"method": "render", "params": {"template_id": "3", "environment": {"user": {"name": "Bob", "age": 30}}}, "id": 8}' | $LIQUID_BIN
echo "Expected: User: Bob, Age: 30"
echo

# Test 5: Deeply nested property access
echo "Test 5: Deeply nested property access"
echo '{"method": "compile", "params": {"template": "City: {{ person.address.city }}"}, "id": 9}' | $LIQUID_BIN
echo '{"method": "render", "params": {"template_id": "4", "environment": {"person": {"address": {"city": "New York", "zip": "10001"}}}}, "id": 10}' | $LIQUID_BIN
echo "Expected: City: New York"
echo

# Test 6: Variables with filters
echo "Test 6: Variables with filters"
echo '{"method": "compile", "params": {"template": "{{ name | upcase }}"}, "id": 11}' | $LIQUID_BIN
echo '{"method": "render", "params": {"template_id": "5", "environment": {"name": "hello"}}, "id": 12}' | $LIQUID_BIN
echo "Expected: HELLO"
echo

# Test 7: Nested property with filters
echo "Test 7: Nested property with filters"
echo '{"method": "compile", "params": {"template": "{{ user.name | upcase }}"}, "id": 13}' | $LIQUID_BIN
echo '{"method": "render", "params": {"template_id": "6", "environment": {"user": {"name": "alice"}}}, "id": 14}' | $LIQUID_BIN
echo "Expected: ALICE"
echo

# Test 8: Non-existent nested property
echo "Test 8: Non-existent nested property returns empty"
echo '{"method": "compile", "params": {"template": "Value: {{ user.missing.field }}"}, "id": 15}' | $LIQUID_BIN
echo '{"method": "render", "params": {"template_id": "7", "environment": {"user": {"name": "test"}}}, "id": 16}' | $LIQUID_BIN
echo "Expected: Value: "
echo

echo "==========================================="
echo "All tests completed!"
