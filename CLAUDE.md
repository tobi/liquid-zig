# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

A high-performance Liquid template engine implementation in Zig, designed to pass the liquid-spec test suite. The Zig implementation communicates with the Ruby-based liquid-spec test harness via JSON-RPC 2.0 over stdin/stdout.

## Build & Test Commands

```bash
# Build (debug)
zig build

# Build (optimized)
zig build -Doptimize=ReleaseFast

# Run the full test suite
bundle exec liquid-spec run liquid-zig.rb

# Run with verbose output
bundle exec liquid-spec run liquid-zig.rb -v

# Filter tests by name pattern (supports regex with /pattern/)
bundle exec liquid-spec run liquid-zig.rb -n assign
bundle exec liquid-spec run liquid-zig.rb -n "/test_.*filter/"

# Run specific suite
bundle exec liquid-spec run liquid-zig.rb -s basics

# Show all failures (default stops at 10)
bundle exec liquid-spec run liquid-zig.rb --no-max-failures

# Compare output against reference Ruby implementation
bundle exec liquid-spec run liquid-zig.rb --compare

# List all available specs / suites
bundle exec liquid-spec run liquid-zig.rb -l
bundle exec liquid-spec run liquid-zig.rb --list-suites
```

### Debugging & Quick Testing

```bash
# Test a single template with YAML spec via heredoc
bundle exec liquid-spec eval liquid-zig.rb <<EOF
name: test_upcase
complexity: 20
template: "{{ x | upcase }}"
expected: "HI"
environment:
  x: hi
EOF

# Compare against reference implementation
bundle exec liquid-spec eval liquid-zig.rb --compare <<EOF
name: my_test
template: "{{ items | join: ', ' }}"
environment:
  items: [a, b, c]
EOF

# Inspect a failing spec in detail
bundle exec liquid-spec inspect liquid-zig.rb -n "case.*empty"

# View implementer documentation
bundle exec liquid-spec docs                    # list all docs
bundle exec liquid-spec docs core-abstractions  # view specific doc
bundle exec liquid-spec docs for-loops
bundle exec liquid-spec docs scopes
bundle exec liquid-spec docs interrupts
bundle exec liquid-spec docs partials
bundle exec liquid-spec docs cycle
```

## Architecture

### Communication Flow

```
[liquid-spec test harness]
        ↓ JSON-RPC 2.0 (stdin/stdout)
[main.zig: reads stdin, writes stdout]
        ↓
[jsonrpc.zig: Handler]
        ↓
[liquid.zig: Engine]
```

### Compilation Pipeline (liquid.zig)

Template source flows through four stages:

1. **Lexer** - Tokenizes into text/variable (`{{ }}`)/tag (`{% %}`) tokens. Handles whitespace control (`{{-`, `-}}`).

2. **Parser** - Builds AST with `Node` union (text, variable, tag). Parses expressions, filter chains, and control structures.

3. **IR Generator** - Converts AST to bytecode instructions. Labels for jump targets, optimizes constant folding.

4. **VM** - Stack-based execution. Manages loop state stack, capture buffer stack, and context (variables/counters).

### Project Structure

```
liquid-zig/
├── build.zig              # Build configuration
├── build.zig.zon          # Package manifest
├── src/
│   ├── main.zig           # Entry point, stdin/stdout I/O loop
│   ├── root.zig           # Library entry point
│   ├── jsonrpc.zig        # JSON-RPC 2.0 protocol handler
│   └── liquid.zig         # Complete template engine (~4600 lines)
├── test/
│   └── fixtures/          # Test JSON fixtures
├── scripts/               # Test shell scripts
├── docs/                  # Documentation
├── liquid-zig.rb          # Ruby adapter for liquid-spec
├── Gemfile                # Ruby dependencies
└── CLAUDE.md              # This file
```

### Key Source Files

| File | Purpose |
|------|---------|
| `src/main.zig` | Entry point, stdin/stdout I/O loop |
| `src/root.zig` | Library entry point for embedding |
| `src/jsonrpc.zig` | JSON-RPC 2.0 protocol handler |
| `src/liquid.zig` | Complete template engine |
| `liquid-zig.rb` | Ruby adapter for liquid-spec |

### JSON-RPC Methods

| Method | Purpose |
|--------|---------|
| `initialize` | Version negotiation, feature declaration |
| `compile` | Parse template → return `template_id` |
| `render` | Execute template with environment → return output |
| `quit` | Graceful shutdown (no response) |

## Core Abstractions

These five abstractions handle all Liquid's type coercion. Implement these correctly and tags become trivial.

### to_output(value)
Converts any value to output string for `{{ }}`:
- `nil` → `""` (empty, not "nil")
- Arrays → recursive concatenation: `[a,b,c]` → `"abc"`
- Ranges → string representation: `(1..5)` → `"1..5"` (NOT expanded)

### to_iterable(value)
Converts for `{% for %}` iteration:
- Ranges ARE expanded: `(1..3)` → `[1, 2, 3]`
- Strings are atomic: `"hello"` → `["hello"]` (one iteration)
- `nil`/numbers/booleans → `[]` (no iterations)

### is_empty(value)
For `== empty` comparisons:
- `nil` is NOT empty (returns false)
- Empty string/array/hash are empty
- Whitespace-only strings are NOT empty

### is_blank(value)
For `== blank` comparisons (more permissive):
- `nil`, `false`, `""`, whitespace-only strings, `[]`, `{}` are blank
- `0` and other numbers are NOT blank

### Scope Stack
- Lookup: search top to bottom, first match wins
- `assign`/`capture`: always writes to current (top) scope
- `for`: pushes new scope, pops when done

## Complexity Scoring

Specs are ordered by complexity. Fix failures in order—later features depend on earlier ones.

| Score | Features |
|-------|----------|
| 10-20 | Literals, raw text output |
| 30-50 | Variables, filters, assign |
| 55-60 | Whitespace control, if/else/unless |
| 70-80 | For loops, operators, filter chains |
| 85-100 | Math filters, forloop object, capture, case/when |
| 105-130 | String filters, increment, comment, raw, echo |
| 140-180 | Array filters, truthy/falsy, cycle, tablerow |
| 190-220 | offset:continue, parentloop, partials |
| 300+ | Edge cases, deprecated features |

## Liquid Implementation Notes

- **Truthy/falsy**: Only `false` and `nil` are falsy. Empty strings and `0` are truthy.
- **Undefined variables**: Return empty string, not an error.
- **Counters vs variables**: `increment`/`decrement` use a separate namespace from `assign`.
- **Filter chains**: Applied left-to-right: `{{ x | upcase | slice: 0, 3 }}`
- **Whitespace control**: `{{-` strips left, `-}}` strips right.
- **forloop.length**: Reflects the limit, not the collection size.
- **parentloop**: Available in nested loops; nil at top level.
- **offset:continue**: Tracked per `"variable-collection"` key in registers.

## Development Workflow

See [docs/AGENTS.md](docs/AGENTS.md) for the complete implementation guide including:
- Full JSON-RPC protocol specification with examples
- Error codes (-32000 parse, -32001 render, -32700 JSON parse, etc.)
- RPC Drops for bidirectional communication
- Common implementation mistakes

## Build Steps

```bash
# Build executable
zig build

# Run tests
zig build test

# Generate documentation
zig build docs

# Check formatting
zig build fmt

# Clean build artifacts
zig build clean
```
