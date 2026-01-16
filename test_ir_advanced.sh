#!/bin/bash

# Advanced test script for IR/VM functionality

echo "Testing IR/VM with filters and constant folding..."

(
  # Test 1: String literal with filter (should NOT be constant-folded)
  printf '{"jsonrpc":"2.0","id":1,"method":"compile","params":{"template":"{{ \\\"hello\\\" | upcase }}"}}\n'
  printf '{"jsonrpc":"2.0","id":2,"method":"render","params":{"template_id":"0","environment":null}}\n'

  # Test 2: String literal without filter (should be constant-folded)
  printf '{"jsonrpc":"2.0","id":3,"method":"compile","params":{"template":"Constant: {{ \\\"world\\\" }}"}}\n'
  printf '{"jsonrpc":"2.0","id":4,"method":"render","params":{"template_id":"1","environment":null}}\n'

  # Test 3: Integer literal (constant-folded)
  printf '{"jsonrpc":"2.0","id":5,"method":"compile","params":{"template":"Count: {{ 100 }}"}}\n'
  printf '{"jsonrpc":"2.0","id":6,"method":"render","params":{"template_id":"2","environment":null}}\n'

  # Test 4: Variable with filter (runtime evaluation)
  printf '{"jsonrpc":"2.0","id":7,"method":"compile","params":{"template":"{{ name | upcase }}"}}\n'
  printf '{"jsonrpc":"2.0","id":8,"method":"render","params":{"template_id":"3","environment":{"name":"alice"}}}\n'

  # Test 5: Mixed text and literals
  printf '{"jsonrpc":"2.0","id":9,"method":"compile","params":{"template":"Age: {{ 25 }}, Active: {{ true }}"}}\n'
  printf '{"jsonrpc":"2.0","id":10,"method":"render","params":{"template_id":"4","environment":null}}\n'

  printf '{"jsonrpc":"2.0","id":11,"method":"quit"}\n'
) | ./zig-out/bin/liquid-zig

echo ""
echo "Advanced tests completed!"
