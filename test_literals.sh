#!/bin/bash

# Test script for literal parsing
echo "Testing literal parsing..."

# Create a test file that compiles and renders in one session
cat << 'EOF' | ./zig-out/bin/liquid-zig
{"jsonrpc":"2.0","id":1,"method":"compile","params":{"template":"{{ \"hello world\" }}"}}
{"jsonrpc":"2.0","id":2,"method":"render","params":{"template_id":"0","environment":{}}}
EOF

echo ""
echo "---"

cat << 'EOF' | ./zig-out/bin/liquid-zig
{"jsonrpc":"2.0","id":1,"method":"compile","params":{"template":"{{ 42 }}"}}
{"jsonrpc":"2.0","id":2,"method":"render","params":{"template_id":"0","environment":{}}}
EOF

echo ""
echo "---"

cat << 'EOF' | ./zig-out/bin/liquid-zig
{"jsonrpc":"2.0","id":1,"method":"compile","params":{"template":"{{ 3.14 }}"}}
{"jsonrpc":"2.0","id":2,"method":"render","params":{"template_id":"0","environment":{}}}
EOF

echo ""
echo "---"

cat << 'EOF' | ./zig-out/bin/liquid-zig
{"jsonrpc":"2.0","id":1,"method":"compile","params":{"template":"{{ true }}"}}
{"jsonrpc":"2.0","id":2,"method":"render","params":{"template_id":"0","environment":{}}}
EOF

echo ""
echo "---"

cat << 'EOF' | ./zig-out/bin/liquid-zig
{"jsonrpc":"2.0","id":1,"method":"compile","params":{"template":"{{ false }}"}}
{"jsonrpc":"2.0","id":2,"method":"render","params":{"template_id":"0","environment":{}}}
EOF

echo ""
echo "---"

cat << 'EOF' | ./zig-out/bin/liquid-zig
{"jsonrpc":"2.0","id":1,"method":"compile","params":{"template":"{{ nil }}"}}
{"jsonrpc":"2.0","id":2,"method":"render","params":{"template_id":"0","environment":{}}}
EOF

echo ""
echo "---"

cat << 'EOF' | ./zig-out/bin/liquid-zig
{"jsonrpc":"2.0","id":1,"method":"compile","params":{"template":"Raw text and {{ 'single quotes' }} work"}}
{"jsonrpc":"2.0","id":2,"method":"render","params":{"template_id":"0","environment":{}}}
EOF

echo ""
echo "Done!"
