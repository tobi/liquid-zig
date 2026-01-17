# Default recipe - show available commands
default:
    @just --list

# Build the executable (debug)
build:
    zig build

# Build optimized release
release:
    zig build -Doptimize=ReleaseFast

# Run the full liquid-spec test suite
test: build
    bundle exec liquid-spec run liquid-zig.rb

# Run tests with verbose output
test-verbose: build
    bundle exec liquid-spec run liquid-zig.rb -v

# Run a specific test suite (e.g., just test-suite basics)
test-suite suite: build
    bundle exec liquid-spec run liquid-zig.rb -s {{suite}}

# Run tests matching a pattern (e.g., just test-filter assign)
test-filter pattern: build
    bundle exec liquid-spec run liquid-zig.rb -n "{{pattern}}"

# Run tests and show all failures (no limit)
test-all-failures: build
    bundle exec liquid-spec run liquid-zig.rb --no-max-failures

# Compare output against reference Ruby implementation
test-compare: build
    bundle exec liquid-spec run liquid-zig.rb --compare

# List all available test suites
list-suites:
    bundle exec liquid-spec run liquid-zig.rb --list-suites

# List all available specs
list-specs:
    bundle exec liquid-spec run liquid-zig.rb -l

# Run Zig unit tests
test-zig:
    zig build test

# Quick eval a template (usage: just eval 'template' 'expected' 'env_json')
eval template expected='""' env='{}':
    #!/usr/bin/env bash
    bundle exec liquid-spec eval liquid-zig.rb <<EOF
    name: quick_test
    template: "{{template}}"
    expected: {{expected}}
    environment: {{env}}
    EOF

# Check source formatting
fmt:
    zig build fmt

# Format source files in place
fmt-fix:
    zig fmt src/

# Generate documentation
docs:
    zig build docs

# Clean build artifacts
clean:
    zig build clean

# Install Ruby dependencies
setup:
    bundle install

# Watch for changes and rebuild (requires entr)
watch:
    find src -name '*.zig' | entr -c zig build
