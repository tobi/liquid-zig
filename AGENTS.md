# Implementing a Liquid Template Engine

This document guides AI agents through implementing a Liquid template engine using liquid-spec for verification.

## Quick Start

```bash
# Run your adapter against the spec suite
liquid-spec run liquid-zig.rb

# Run with verbose output to see each test
liquid-spec run liquid-zig.rb -v

# Filter tests by name pattern
liquid-spec run liquid-zig.rb -n assign

# Show all failures (default stops at 10)
liquid-spec run liquid-zig.rb --no-max-failures
```

## How It Works

1. **Your adapter** defines `compile` and `render` blocks that bridge liquid-spec to your implementation
2. **liquid-spec** runs test cases in **complexity order** (simplest features first)
3. **You implement features** incrementally, fixing failing specs from lowest to highest complexity

## Complexity-Ordered Implementation

Specs are sorted by complexity score before running. This means you'll see failures for basic features first, then progressively harder ones. **Fix specs in the order they fail.**

### Implementation Phases

| Phase | Complexity | Features |
|-------|------------|----------|
| 1 | 10-60 | Raw text, literals, variables, assign, if/else |
| 2 | 70-100 | For loops, operators, math filters, capture |
| 3 | 105-150 | String filters, increment, comment, raw, arrays |
| 4 | 170-220 | Edge cases, truthy/falsy, cycle, tablerow, partials |
| 5 | 300+ | Advanced edge cases, production behaviors |

### Complexity Reference

| Score | What to Implement |
|-------|-------------------|
| 10 | Raw text passthrough (no parsing) |
| 20 | Literal values: `{{ 'hello' }}`, `{{ 42 }}`, `{{ true }}` |
| 30 | Variable lookup: `{{ name }}` |
| 40 | Basic filters: `{{ x \| upcase }}` |
| 50 | Assign tag: `{% assign x = 'foo' %}` |
| 55 | Whitespace control: `{{- x -}}` |
| 60 | If/else/unless: `{% if x %}...{% endif %}` |
| 70 | For loops: `{% for i in items %}` |
| 75 | Loop modifiers: limit, offset, reversed, break, continue |
| 80 | Filter chains, and/or, contains, comparison operators |
| 85-100 | Math filters, forloop object, capture, case/when |

## JSON-RPC Protocol (Non-Ruby Implementations)

Your implementation communicates with liquid-spec via JSON-RPC 2.0 over stdin/stdout.
Implement a server that reads JSON requests from stdin and writes responses to stdout.

### Running Your Implementation

```bash
# Run with your server command
liquid-spec run liquid-zig.rb --command="./your-liquid-server"

# Or set DEFAULT_COMMAND in the adapter file
```

### Protocol Overview

All messages are newline-delimited JSON. The lifecycle is:

1. **initialize** - liquid-spec sends version info, your server responds with supported features
2. **compile** - Parse a template, return a template_id
3. **render** - Render a compiled template with variables
4. **quit** - Notification to exit cleanly (no response expected)

### Debug Output

Your server can write debug information to **stderr** at any time. liquid-spec forwards
all stderr output to the terminal, prefixed with your server name:

```
[my-liquid-server] Compiling template: {{ x | upcase }}
[my-liquid-server] Render complete in 0.5ms
```

This is useful for debugging your implementation without interfering with the JSON-RPC
protocol (which uses stdout). Write to stderr liberally during development.

**Important:** The default timeout is 2 seconds per request. If your server doesn't
respond within 2 seconds, liquid-spec will fail with a timeout error.

### Example: Minimal Server (Node.js)

```javascript
#!/usr/bin/env node
const readline = require('readline');
const templates = new Map();
let nextId = 1;

const rl = readline.createInterface({ input: process.stdin });

rl.on('line', (line) => {
  const { id, method, params = {} } = JSON.parse(line);

  if (method === 'initialize') {
    respond(id, { version: '1.0', features: ['core'] });
  } else if (method === 'compile') {
    const templateId = `t${nextId++}`;
    templates.set(templateId, { source: params.template, filesystem: params.filesystem || {} });
    respond(id, { template_id: templateId });
  } else if (method === 'render') {
    const t = templates.get(params.template_id);
    const output = renderLiquid(t.source, params.environment || {}, t.filesystem);
    respond(id, { output });
  } else if (method === 'quit') {
    process.exit(0);  // No response needed for quit notification
  }
});

function respond(id, result) {
  console.log(JSON.stringify({ jsonrpc: '2.0', id, result }));
}

function renderLiquid(source, env, filesystem) {
  // Your Liquid implementation here!
  return source;
}
```

### Compile Request/Response

```json
// Request
{"jsonrpc":"2.0","id":1,"method":"compile","params":{
  "template": "{{ x | upcase }}",
  "options": {"error_mode": "strict"},
  "filesystem": {"snippet.liquid": "Hello"}
}}

// Success response
{"jsonrpc":"2.0","id":1,"result":{"template_id":"abc123"}}

// Parse error response
{"jsonrpc":"2.0","id":1,"error":{"code":-32000,"message":"Parse error","data":{"type":"parse_error","message":"Unknown tag"}}}
```

### Render Request/Response

```json
// Request
{"jsonrpc":"2.0","id":2,"method":"render","params":{
  "template_id": "abc123",
  "environment": {"x": "hello", "items": [1,2,3]},
  "options": {"render_errors": false}
}}

// Success response
{"jsonrpc":"2.0","id":2,"result":{"output":"HELLO"}}

// Render error response
{"jsonrpc":"2.0","id":2,"error":{"code":-32001,"message":"Render error","data":{"type":"render_error","message":"undefined variable"}}}
```

### Error Codes

| Code | Meaning |
|------|---------|
| -32000 | Parse error (template syntax error) |
| -32001 | Render error (runtime error) |
| -32700 | JSON parse error |
| -32600 | Invalid request |
| -32601 | Method not found |

See the adapter file (`liquid-zig.rb`) for the complete protocol specification.


## Debugging Failures

When a spec fails, you'll see:

```
1) test_assign_basic
   Template: "{% assign x = 'hello' %}{{ x }}"
   Expected: "hello"
   Got:      ""
```

Use `liquid-spec eval` to test individual templates:

```bash
# Quick test with comparison to reference
liquid-spec eval liquid-zig.rb -n test_assign --liquid="{% assign x = 1 %}{{ x }}"

# Test with environment variables
liquid-spec eval liquid-zig.rb -n test_var -l "{{ x | size }}" -a '{"x": [1,2,3]}'
```

## Common Implementation Mistakes

1. **Truthy/falsy**: In Liquid, only `false` and `nil` are falsy. Empty strings and `0` are truthy.
2. **Variable scope**: Assign creates variables in the current scope. For loops have their own scope.
3. **Filter arguments**: Filters can have arguments: `{{ x | slice: 0, 3 }}`
4. **Whitespace**: `{{-` and `-}}` strip adjacent whitespace.
5. **Error handling**: Undefined variables return empty string, not an error.

## Feature Flags

Some specs require specific features. Your adapter declares what it supports:

```ruby
LiquidSpec.configure do |config|
  config.features = [
    :core,           # Basic Liquid (always included)
    :lax_parsing,    # Supports error_mode: :lax
  ]
end
```

## Iterative Development Loop

1. Run `liquid-spec run liquid-zig.rb`
2. Note the first failure and its complexity score
3. Implement the minimal feature to pass that spec
4. Re-run and repeat

The complexity ordering ensures you build a solid foundation. Don't skip ahead—later features often depend on earlier ones working correctly.

## Useful Commands

```bash
# List all available specs
liquid-spec run liquid-zig.rb -l

# List available suites
liquid-spec run liquid-zig.rb --list-suites

# Run specific suite
liquid-spec run liquid-zig.rb -s basics

# Compare your output to reference implementation
liquid-spec run liquid-zig.rb --compare
```

## Reference

- [Liquid Documentation](https://shopify.github.io/liquid/)
- [liquid-spec repository](https://github.com/Shopify/liquid-spec)
- See `COMPLEXITY.md` in liquid-spec for full complexity scoring guide
