#!/bin/bash

# Test script for IR/VM functionality

echo "Testing IR/VM implementation..."

# Start the liquid-zig process
(
  printf '{"jsonrpc":"2.0","id":1,"method":"compile","params":{"template":"Hello {{ 42 }}!"}}\n'
  printf '{"jsonrpc":"2.0","id":2,"method":"render","params":{"template_id":"0","environment":null}}\n'

  printf '{"jsonrpc":"2.0","id":3,"method":"compile","params":{"template":"The answer is {{ true }}"}}\n'
  printf '{"jsonrpc":"2.0","id":4,"method":"render","params":{"template_id":"1","environment":null}}\n'

  printf '{"jsonrpc":"2.0","id":5,"method":"compile","params":{"template":"Pi is {{ 3.14 }}"}}\n'
  printf '{"jsonrpc":"2.0","id":6,"method":"render","params":{"template_id":"2","environment":null}}\n'

  printf '{"jsonrpc":"2.0","id":7,"method":"compile","params":{"template":"Name: {{ name }}"}}\n'
  printf '{"jsonrpc":"2.0","id":8,"method":"render","params":{"template_id":"3","environment":{"name":"World"}}}\n'

  printf '{"jsonrpc":"2.0","id":9,"method":"quit"}\n'
) | ./zig-out/bin/liquid-zig

echo ""
echo "Test completed!"
