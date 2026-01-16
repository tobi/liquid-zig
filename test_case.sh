#!/usr/bin/env bash

set -euo pipefail

# Colors for output
GREEN='\033[0;32m'
RED='\033[0;31m'
NC='\033[0m' # No Color

# Test counter
TEST_COUNT=0
PASS_COUNT=0

# Function to run a test
run_test() {
    local test_name=$1
    local template=$2
    local env=$3
    local expected=$4
    local template_id=$5

    TEST_COUNT=$((TEST_COUNT + 1))

    # Compile template
    echo "{\"method\": \"compile\", \"id\": $((template_id * 2)), \"params\": {\"template\": $(echo "$template" | jq -Rs .)}}"

    # Render template
    if [ "$env" = "{}" ]; then
        echo "{\"method\": \"render\", \"id\": $((template_id * 2 + 1)), \"params\": {\"template_id\": \"$template_id\"}}"
    else
        echo "{\"method\": \"render\", \"id\": $((template_id * 2 + 1)), \"params\": {\"template_id\": \"$template_id\", \"environment\": $env}}"
    fi
}

# Generate all test requests
(
    run_test "basic case with match" '{% case status %}{% when "active" %}Active user{% endcase %}' '{"status": "active"}' 'Active user' 0
    run_test "case with no match, no else" '{% case status %}{% when "active" %}Active user{% endcase %}' '{"status": "inactive"}' '' 1
    run_test "case with else" '{% case status %}{% when "active" %}Active user{% else %}Unknown status{% endcase %}' '{"status": "inactive"}' 'Unknown status' 2
    run_test "multiple when clauses" '{% case color %}{% when "red" %}Red{% when "blue" %}Blue{% when "green" %}Green{% endcase %}' '{"color": "blue"}' 'Blue' 3
    run_test "first matching when" '{% case x %}{% when 1 %}First{% when 1 %}Second{% endcase %}' '{"x": 1}' 'First' 4
    run_test "case with integers" '{% case count %}{% when 0 %}Zero{% when 1 %}One{% when 2 %}Two{% else %}Many{% endcase %}' '{"count": 2}' 'Two' 5
    run_test "case with variable" '{% case user_type %}{% when "admin" %}Administrator{% when "user" %}Regular User{% when "guest" %}Guest{% endcase %}' '{"user_type": "admin"}' 'Administrator' 6
    run_test "case with surrounding text" 'Status: {% case status %}{% when "active" %}Active{% when "pending" %}Pending{% else %}Unknown{% endcase %}!' '{"status": "pending"}' 'Status: Pending!' 7
    run_test "case with variable output" '{% case role %}{% when "admin" %}{{ name }} is admin{% when "user" %}{{ name }} is user{% endcase %}' '{"role": "admin", "name": "Alice"}' 'Alice is admin' 8
    run_test "case with string match" '{% case fruit %}{% when "apple" %}Red fruit{% when "banana" %}Yellow fruit{% else %}Unknown fruit{% endcase %}' '{"fruit": "banana"}' 'Yellow fruit' 9
    run_test "case with boolean" '{% case flag %}{% when true %}Yes{% when false %}No{% endcase %}' '{"flag": true}' 'Yes' 10
    run_test "case falling to else" '{% case value %}{% when 1 %}One{% when 2 %}Two{% else %}Other{% endcase %}' '{"value": 5}' 'Other' 11
    run_test "empty when block" '{% case x %}{% when 1 %}{% when 2 %}Two{% endcase %}' '{"x": 1}' '' 12
    run_test "case with nil" '{% case value %}{% when nil %}Nil value{% when 1 %}One{% else %}Other{% endcase %}' '{"value": null}' 'Nil value' 13
    run_test "when with variable comparison" '{% assign expected = "test" %}{% case status %}{% when expected %}Match!{% else %}No match{% endcase %}' '{"status": "test"}' 'Match!' 14
    run_test "case with floats" '{% case price %}{% when 9.99 %}Standard{% when 19.99 %}Premium{% else %}Custom{% endcase %}' '{"price": 9.99}' 'Standard' 15
    run_test "many whens with else" '{% case day %}{% when "Mon" %}Monday{% when "Tue" %}Tuesday{% when "Wed" %}Wednesday{% else %}Other day{% endcase %}' '{"day": "Thu"}' 'Other day' 16
    run_test "complex when body" '{% case type %}{% when "greeting" %}Hello {% if name %}{{ name }}{% else %}there{% endif %}!{% endcase %}' '{"type": "greeting", "name": "World"}' 'Hello World!' 17
    run_test "assign in when block" '{% case x %}{% when 1 %}{% assign msg = "one" %}{{ msg }}{% endcase %}' '{"x": 1}' 'one' 18
    run_test "empty else block" '{% case x %}{% when 1 %}One{% else %}{% endcase %}' '{"x": 2}' '' 19
    run_test "numeric string comparison" '{% case code %}{% when "100" %}Continue{% when "200" %}OK{% when "404" %}Not Found{% endcase %}' '{"code": "200"}' 'OK' 20
    run_test "only else no when" '{% case x %}{% else %}Default{% endcase %}' '{"x": 1}' 'Default' 21
    run_test "for loop in when" '{% case show %}{% when true %}{% for i in items %}{{ i }}{% endfor %}{% endcase %}' '{"show": true, "items": [1, 2, 3]}' '123' 22
) | ./zig-out/bin/liquid-zig > /tmp/case_test_output.json

# Now parse and check results
echo "Testing case/when/else/endcase tags..."
echo

declare -a EXPECTED_OUTPUTS=(
    'Active user'
    ''
    'Unknown status'
    'Blue'
    'First'
    'Two'
    'Administrator'
    'Status: Pending!'
    'Alice is admin'
    'Yellow fruit'
    'Yes'
    'Other'
    ''
    'Nil value'
    'Match!'
    'Standard'
    'Other day'
    'Hello World!'
    'one'
    ''
    'OK'
    'Default'
    '123'
)

declare -a TEST_NAMES=(
    'basic case with match'
    'case with no match, no else'
    'case with else'
    'multiple when clauses'
    'first matching when'
    'case with integers'
    'case with variable'
    'case with surrounding text'
    'case with variable output'
    'case with string match'
    'case with boolean'
    'case falling to else'
    'empty when block'
    'case with nil'
    'when with variable comparison'
    'case with floats'
    'many whens with else'
    'complex when body'
    'assign in when block'
    'empty else block'
    'numeric string comparison'
    'only else no when'
    'for loop in when'
)

# Parse output and check each result
for i in "${!EXPECTED_OUTPUTS[@]}"; do
    TEST_COUNT=$((TEST_COUNT + 1))
    template_id=$i
    render_id=$((template_id * 2 + 1))
    expected="${EXPECTED_OUTPUTS[$i]}"
    test_name="${TEST_NAMES[$i]}"

    # Extract the result from the JSON response with the matching ID
    result=$(jq -r "select(.id == $render_id) | .result.output // \"\"" < /tmp/case_test_output.json)

    if [ "$result" = "$expected" ]; then
        echo -e "${GREEN}✓${NC} Test $TEST_COUNT ($test_name): PASS"
        PASS_COUNT=$((PASS_COUNT + 1))
    else
        echo -e "${RED}✗${NC} Test $TEST_COUNT ($test_name): FAIL"
        echo "  Expected: $expected"
        echo "  Got: $result"
    fi
done

echo
echo "================================"
echo "Tests passed: $PASS_COUNT/$TEST_COUNT"
echo "================================"

if [ $PASS_COUNT -eq $TEST_COUNT ]; then
    echo -e "${GREEN}All tests passed!${NC}"
    exit 0
else
    echo -e "${RED}Some tests failed!${NC}"
    exit 1
fi
