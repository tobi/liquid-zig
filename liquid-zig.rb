#!/usr/bin/env ruby
# frozen_string_literal: true

require "liquid/spec/cli/adapter_dsl"

# ==============================================================================
# JSON-RPC Liquid Adapter
# ==============================================================================
#
# This adapter communicates with a Liquid implementation subprocess via
# JSON-RPC 2.0 over stdin/stdout. Implement the protocol below in any language.
#
# Usage:
#   liquid-spec liquid-zig.rb
#   liquid-spec liquid-zig.rb --command="./my-liquid-server"
#
# ==============================================================================
# PROTOCOL SPECIFICATION
# ==============================================================================
#
# All messages are JSON-RPC 2.0, one JSON object per line (newline-delimited).
# The subprocess reads requests from stdin and writes responses to stdout.
#
# --- LIFECYCLE ---
#
# 1. initialize (parent -> subprocess)
#    Request:  {"jsonrpc":"2.0","id":1,"method":"initialize","params":{"version":"1.0"}}
#    Response: {"jsonrpc":"2.0","id":1,"result":{"version":"1.0","features":["core"]}}
#
# 2. quit (parent -> subprocess) - notification, no response expected
#    Notification: {"jsonrpc":"2.0","method":"quit","params":{}}
#    Subprocess should exit cleanly within 1 second.
#
# --- COMPILE ---
#
# Compile a template and return an ID for later rendering.
#
# Request:
#   {
#     "jsonrpc": "2.0",
#     "id": 2,
#     "method": "compile",
#     "params": {
#       "template": "{{ x | upcase }}",
#       "options": {
#         "error_mode": "strict",   // "strict", "lax", or null
#         "line_numbers": true
#       },
#       "filesystem": {             // templates for {% include %} / {% render %}
#         "snippet.liquid": "hello {{ name }}"
#       }
#     }
#   }
#
# Response (success):
#   {"jsonrpc":"2.0","id":2,"result":{"template_id":"abc123"}}
#
# Response (parse error):
#   {
#     "jsonrpc": "2.0",
#     "id": 2,
#     "error": {
#       "code": -32000,
#       "message": "Parse error",
#       "data": {"type": "parse_error", "line": 1, "message": "Unknown tag 'foo'"}
#     }
#   }
#
# --- RENDER ---
#
# Render a previously compiled template with variables.
#
# Request:
#   {
#     "jsonrpc": "2.0",
#     "id": 3,
#     "method": "render",
#     "params": {
#       "template_id": "abc123",
#       "environment": {
#         "x": "hello",
#         "items": [1, 2, 3],
#         "user": {"_rpc_drop": "drop_1", "type": "UserDrop"}
#       },
#       "options": {
#         "render_errors": false  // true = render errors as text, false = throw
#       }
#     }
#   }
#
# Response (success):
#   {"jsonrpc":"2.0","id":3,"result":{"output":"HELLO"}}
#
# Response (render error):
#   {
#     "jsonrpc": "2.0",
#     "id": 3,
#     "error": {
#       "code": -32001,
#       "message": "Render error",
#       "data": {"type": "render_error", "line": 1, "message": "undefined method"}
#     }
#   }
#
# --- RPC DROPS (subprocess -> parent) ---
#
# When environment contains {"_rpc_drop": "ID", "type": "ClassName"},
# the subprocess must call back to parent to access properties/methods.
# Send the request to stdout, read the response from stdin.
#
# drop_get - Get a property value:
#   Request:  {"jsonrpc":"2.0","id":100,"method":"drop_get","params":{"drop_id":"drop_1","property":"name"}}
#   Response: {"jsonrpc":"2.0","id":100,"result":{"value":"John"}}
#   Note: value may itself be {"_rpc_drop": "drop_2", "type": "..."} for nested drops
#
# drop_call - Call a method with arguments:
#   Request:  {"jsonrpc":"2.0","id":101,"method":"drop_call","params":{"drop_id":"drop_1","method":"calculate","args":[1,2]}}
#   Response: {"jsonrpc":"2.0","id":101,"result":{"value":3}}
#
# drop_iterate - Get all items from enumerable (for {% for %} loops):
#   Request:  {"jsonrpc":"2.0","id":102,"method":"drop_iterate","params":{"drop_id":"drop_1"}}
#   Response: {"jsonrpc":"2.0","id":102,"result":{"items":[1,2,3,4,5]}}
#
# --- ERROR CODES ---
#
# -32000  Parse error (template syntax error)
# -32001  Render error (runtime error)
# -32002  Drop access error (property/method not found)
# -32700  JSON parse error (invalid JSON received)
# -32600  Invalid request (malformed JSON-RPC)
# -32601  Method not found (unknown method name)
#
# ==============================================================================
#
# DEBUG OUTPUT
# ------------
# Your server can write to stderr for debug output. liquid-spec forwards all
# stderr to the terminal, prefixed with [server-name]. Example:
#   console.error("Compiling:", template);  // Node.js
#   eprintln!("Compiling: {}", template);   // Rust
#   print("Compiling:", template, file=sys.stderr)  # Python
#
# ==============================================================================

DEFAULT_COMMAND = "path/to/your/liquid-server"
DEFAULT_TIMEOUT = 2  # seconds - increase if your server needs more time

LiquidSpec.setup do |ctx|
  require "liquid"
  require "liquid/spec/json_rpc/adapter"

  # CLI --command flag overrides DEFAULT_COMMAND
  command = LiquidSpec.cli_options[:command] || DEFAULT_COMMAND
  timeout = LiquidSpec.cli_options[:timeout]&.to_i || DEFAULT_TIMEOUT

  ctx[:adapter] = Liquid::Spec::JsonRpc::Adapter.new(command, timeout: timeout)
  ctx[:adapter].start

  # Store features reported by subprocess for use in configure block
  ctx[:features] = ctx[:adapter].features

  at_exit { ctx[:adapter]&.shutdown }
end

LiquidSpec.configure do |config|
  # Features are reported by your subprocess in the initialize response.
  # If your subprocess doesn't support runtime_drops (bidirectional RPC),
  # declare features = [] to opt out of drop-related specs.
  #
  # Common features:
  #   :core           - Full Liquid implementation (implies :runtime_drops)
  #   :runtime_drops  - Supports drop_get/drop_call/drop_iterate callbacks
  #   :lax_parsing    - Supports error_mode: :lax
  #
  # For most JSON-RPC implementations without drop callbacks:
  config.features = []
end

LiquidSpec.compile do |ctx, source, options|
  ctx[:adapter].compile(source, options)
end

LiquidSpec.render do |ctx, template_id, assigns, options|
  ctx[:adapter].render(template_id, assigns, options)
end
