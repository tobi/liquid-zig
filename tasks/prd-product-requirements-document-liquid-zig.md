# Product Requirements Document: Liquid-Zig

## Overview
A high-performance Liquid template engine implementation in Zig, designed to pass 100% of the liquid-spec test suite while delivering significantly faster performance than the reference Ruby implementation.

## Success Criteria
The command `bundle exec liquid-spec run liquid-zig.rb` must execute successfully with all specs passing.

## Architecture

### Core Components

#### 1. Liquid Library (`src/liquid.zig`)
The main library implementing the Liquid template language with the following pipeline:

**Compilation Pipeline:**
- **Lexer**: Tokenizes Liquid template source
- **Parser**: Builds Abstract Syntax Tree (AST) from tokens
- **IR Generator**: Converts AST to optimized Intermediate Representation
- **VM**: Executes IR instructions for template rendering

**Key Modules:**
- `lexer.zig` - Tokenization and source location tracking
- `parser.zig` - AST construction with syntax validation
- `ir.zig` - Intermediate representation and optimization passes
- `vm.zig` - Virtual machine executor
- `filters.zig` - All Liquid filter implementations
- `tags.zig` - Tag implementations (assign, if, for, etc.)
- `context.zig` - Variable scope and environment management
- `errors.zig` - Error types and reporting

#### 2. JSON-RPC Server (`src/main.zig`)
Standalone executable that bridges liquid-spec to the Zig implementation.

**Protocol Implementation:**
- Reads newline-delimited JSON from stdin
- Writes JSON-RPC 2.0 responses to stdout
- Writes debug output to stderr
- 2-second timeout per request requirement

**Supported Methods:**
- `initialize` - Version negotiation and feature declaration
- `compile` - Parse template, return template_id
- `render` - Execute compiled template with environment
- `quit` - Clean shutdown notification

#### 3. Ruby Adapter (`liquid-zig.rb`)
Minimal adapter file that configures liquid-spec to invoke the Zig JSON-RPC server.

**Configuration:**
```ruby
DEFAULT_COMMAND = "./zig-out/bin/liquid-zig"
```

## Technical Specifications

### Performance Targets
- **10x+ faster** than Ruby Liquid reference implementation
- Sub-millisecond rendering for typical templates
- Efficient memory usage through arena allocation

### Memory Management
- **Arena allocators** for template compilation and rendering lifecycle
- One arena per compile operation (freed after template_id stored)
- One arena per render operation (freed after output generated)
- Template storage uses separate long-lived arena

### Error Handling
- Support both **strict** and **lax** error modes as per liquid-spec
- Strict mode: fail fast on parse/render errors
- Lax mode: render errors as empty strings, continue execution
- JSON-RPC error codes:
  - `-32000`: Parse errors
  - `-32001`: Render errors
  - `-32700`: JSON parse errors
  - `-32600`: Invalid request
  - `-32601`: Method not found

### Liquid Compatibility
- **100% compatibility** with Shopify Liquid specification
- All features from Phase 1-5 (complexity 10-300+):
  - Raw text, literals, variables
  - Assign, if/else/unless, case/when
  - For loops with modifiers (limit, offset, reversed, break, continue)
  - All standard filters (string, math, array)
  - Whitespace control
  - Capture, increment, cycle, tablerow
  - Partials/includes with filesystem support
  - Comments and raw blocks

### Template Storage
- In-memory HashMap: `template_id → CompiledTemplate`
- CompiledTemplate contains:
  - IR bytecode
  - Source template (for debugging)
  - Filesystem map (for partials)
  - Compilation metadata

### Optimization Strategies
- Constant folding in IR generation
- Dead code elimination
- Filter chain optimization
- Variable access caching in VM

## Build System

### Build Configuration (`build.zig`)
```zig
const exe = b.addExecutable(.{
    .name = "liquid-zig",
    .root_source_file = .{ .path = "src/main.zig" },
    .target = target,
    .optimize = optimize,
});
```

**Build Modes:**
- `zig build` - Debug build for development
- `zig build -Doptimize=ReleaseFast` - Production optimized build

**Output:**
- Executable: `zig-out/bin/liquid-zig`

## Testing Strategy

### Primary Validation
- **liquid-spec test suite** - Comprehensive compatibility testing
- Run via: `bundle exec liquid-spec run liquid-zig.rb`
- All complexity levels (10-300+) must pass

### Supplementary Testing
- **Unit tests** for individual components (lexer, parser, filters)
- **Benchmarks** comparing to Ruby Liquid
- **Fuzz testing** for parser robustness (optional, helpful)
- **Memory leak detection** using Zig's testing allocator

### Test Commands
```bash
# Run all specs
bundle exec liquid-spec run liquid-zig.rb

# Verbose output
bundle exec liquid-spec run liquid-zig.rb -v

# Filter by name
bundle exec liquid-spec run liquid-zig.rb -n assign

# Show all failures
bundle exec liquid-spec run liquid-zig.rb --no-max-failures

# Run Zig unit tests
zig build test
```

## Development Phases

### Phase 1: Foundation (Complexity 10-60)
- Lexer and basic parser
- Raw text passthrough
- Literals and variable lookup
- Basic assign tag
- Simple if/else

**Deliverable:** First 20% of specs passing

### Phase 2: Core Features (Complexity 70-100)
- For loops with forloop object
- Operators (comparison, logical)
- Math filters
- Filter chains
- Capture and case/when

**Deliverable:** 50% of specs passing

### Phase 3: Advanced Features (Complexity 105-150)
- String filters (upcase, downcase, slice, etc.)
- Array filters (join, sort, map, etc.)
- Increment/decrement
- Comment and raw blocks
- Whitespace control

**Deliverable:** 80% of specs passing

### Phase 4: Edge Cases (Complexity 170-220)
- Truthy/falsy semantics
- Cycle and tablerow
- Partials/includes with filesystem
- Complex nested scoping
- Loop modifiers (break, continue, limit, offset)

**Deliverable:** 95% of specs passing

### Phase 5: Production Hardening (Complexity 300+)
- Advanced edge cases
- Error mode compliance
- Performance optimization
- Memory efficiency tuning

**Deliverable:** 100% of specs passing

## File Structure
```
liquid-zig/
├── build.zig                 # Build configuration
├── liquid-zig.rb             # Ruby adapter for liquid-spec
├── src/
│   ├── main.zig             # JSON-RPC server entry point
│   ├── liquid.zig           # Main library module
│   ├── lexer.zig            # Tokenization
│   ├── parser.zig           # AST construction
│   ├── ir.zig               # Intermediate representation
│   ├── vm.zig               # Virtual machine executor
│   ├── filters.zig          # Filter implementations
│   ├── tags.zig             # Tag implementations
│   ├── context.zig          # Scope and variables
│   ├── errors.zig           # Error types
│   └── json_rpc.zig         # JSON-RPC protocol handling
└── zig-out/
    └── bin/
        └── liquid-zig       # Compiled executable
```

## Non-Functional Requirements

### Reliability
- Zero crashes on valid liquid-spec inputs
- Graceful error handling for malformed JSON-RPC
- Proper cleanup on quit signal

### Maintainability
- Idiomatic Zig code style
- Comprehensive error messages with source locations
- Modular architecture for easy feature additions

### Debuggability
- Stderr logging for compilation/render lifecycle
- Source location tracking in errors
- Optional IR dump for debugging

### Portability
- Cross-platform (Linux, macOS, Windows)
- No platform-specific dependencies
- Standard Zig toolchain only

## Constraints

### Technical Constraints
- Must use Zig standard library only (no external dependencies)
- JSON-RPC over stdin/stdout (no network)
- 2-second timeout per operation
- Must compile with Zig 0.11.0+

### Compatibility Constraints
- 100% Liquid spec compliance required
- Must match Ruby Liquid output exactly
- Support both strict and lax error modes

### Performance Constraints
- 10x+ faster than Ruby Liquid
- Memory usage reasonable for server deployments
- No memory leaks

## Success Metrics

### Primary Metric
✅ **All liquid-spec tests pass**: `bundle exec liquid-spec run liquid-zig.rb` exits with code 0

### Secondary Metrics
- **Performance**: Benchmark suite shows 10x+ improvement
- **Memory**: No leaks detected in test runs
- **Reliability**: Zero crashes across full spec suite
- **Code Quality**: Passes `zig fmt --check` and `zig build test`

## Risks & Mitigations

### Risk: Complex Liquid semantics
**Mitigation:** Incremental development following complexity order, frequent spec validation

### Risk: Performance vs. correctness tradeoffs
**Mitigation:** Correctness first, optimize after specs pass

### Risk: Memory management bugs
**Mitigation:** Arena allocation simplifies lifecycle, extensive testing with leak detection

### Risk: JSON-RPC protocol issues
**Mitigation:** Reference implementation examples in AGENTS.md, stderr debugging

## Future Enhancements (Out of Scope)
- JIT compilation for hot templates
- Persistent template cache
- C API for embedding
- WASM compilation target
- Language server protocol support