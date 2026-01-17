# Default recipe - show available commands
default:
    @just --list

# Build the executable (debug)
build:
    zig build

# Internal: update test results cache for statusline
[private]
_update-cache:
    #!/usr/bin/env bash
    output=$(cat /tmp/liquid-test-out.txt 2>/dev/null || echo "")
    # Parse the Total line: "Total: 2892 passed, 1472 failed, 206 errors."
    if [[ "$output" =~ Total:\ ([0-9]+)\ passed,\ ([0-9]+)\ failed,\ ([0-9]+)\ errors ]]; then
        passed="${BASH_REMATCH[1]}"
        failed="${BASH_REMATCH[2]}"
        errors="${BASH_REMATCH[3]}"
        total=$((passed + failed + errors))
        echo "PASSED=$passed" > .liquid-test-results
        echo "TOTAL=$total" >> .liquid-test-results
    # Parse: "Total: 554 passed." (no failures)
    elif [[ "$output" =~ Total:\ ([0-9]+)\ passed\. ]]; then
        echo "PASSED=${BASH_REMATCH[1]}" > .liquid-test-results
        echo "TOTAL=${BASH_REMATCH[1]}" >> .liquid-test-results
    # Parse single suite: "554/554 passed"
    elif [[ "$output" =~ ([0-9]+)/([0-9]+)\ passed ]]; then
        echo "PASSED=${BASH_REMATCH[1]}" > .liquid-test-results
        echo "TOTAL=${BASH_REMATCH[2]}" >> .liquid-test-results
    fi

# Build optimized release
release:
    zig build -Doptimize=ReleaseFast

# Run the full liquid-spec test suite
test: build
    #!/usr/bin/env bash
    set -o pipefail
    bundle exec liquid-spec run liquid-zig.rb 2>&1 | tee /tmp/liquid-test-out.txt
    just _update-cache

# Run tests with verbose output
test-verbose: build
    #!/usr/bin/env bash
    set -o pipefail
    bundle exec liquid-spec run liquid-zig.rb -v 2>&1 | tee /tmp/liquid-test-out.txt
    just _update-cache

# Run a specific test suite (e.g., just test-suite basics)
test-suite suite: build
    #!/usr/bin/env bash
    set -o pipefail
    bundle exec liquid-spec run liquid-zig.rb -s {{suite}} 2>&1 | tee /tmp/liquid-test-out.txt
    just _update-cache

# Run tests matching a pattern (e.g., just test-filter assign)
test-filter pattern: build
    #!/usr/bin/env bash
    set -o pipefail
    bundle exec liquid-spec run liquid-zig.rb -n "{{pattern}}" 2>&1 | tee /tmp/liquid-test-out.txt
    just _update-cache

# Run tests and show all failures (no limit)
test-all-failures: build
    #!/usr/bin/env bash
    set -o pipefail
    bundle exec liquid-spec run liquid-zig.rb --no-max-failures 2>&1 | tee /tmp/liquid-test-out.txt
    just _update-cache

# Compare output against reference Ruby implementation
test-compare: build
    #!/usr/bin/env bash
    set -o pipefail
    bundle exec liquid-spec run liquid-zig.rb --compare 2>&1 | tee /tmp/liquid-test-out.txt
    just _update-cache

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
