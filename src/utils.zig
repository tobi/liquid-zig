const std = @import("std");
const json = std.json;

/// Convert a JSON value to a string representation for output
pub fn valueToString(allocator: std.mem.Allocator, value: json.Value) ![]u8 {
    return switch (value) {
        .string => |s| try allocator.dupe(u8, s),
        .integer => |i| try std.fmt.allocPrint(allocator, "{d}", .{i}),
        .float => |f| try floatToString(allocator, f),
        .bool => |b| try allocator.dupe(u8, if (b) "true" else "false"),
        .number_string => |s| try allocator.dupe(u8, s),
        .null => try allocator.dupe(u8, ""),
        else => try allocator.dupe(u8, ""),
    };
}

/// Convert a float to string, preserving decimal point for whole numbers
fn floatToString(allocator: std.mem.Allocator, f: f64) ![]u8 {
    // Handle special cases
    if (std.math.isNan(f)) return try allocator.dupe(u8, "NaN");
    if (std.math.isInf(f)) return try allocator.dupe(u8, if (f > 0) "Infinity" else "-Infinity");

    // Check if it's a whole number and within safe integer range
    const rounded = @round(f);
    const max_safe: f64 = 9007199254740991.0; // 2^53 - 1
    const min_safe: f64 = -9007199254740991.0;
    if (@abs(f - rounded) < 0.0000001 and rounded >= min_safe and rounded <= max_safe) {
        // It's a whole number within safe range - output with .0 suffix
        const int_val: i64 = @intFromFloat(rounded);
        return try std.fmt.allocPrint(allocator, "{d}.0", .{int_val});
    }
    // It has decimal places or is too large, format normally
    return try std.fmt.allocPrint(allocator, "{d}", .{f});
}

/// Convert a JSON value to a number (f64)
/// In Ruby Liquid, booleans convert to 0 for math operations
pub fn valueToNumber(value: json.Value) !f64 {
    return switch (value) {
        .integer => |i| @floatFromInt(i),
        .float => |f| f,
        .string => |s| try stringToNumber(s),
        .bool => 0.0, // Ruby Liquid: booleans convert to 0
        .null => 0.0,
        else => 0.0,
    };
}

/// Parse a string as a number (integer or float)
/// Uses Ruby-style lax parsing: "6-3" -> 6, "3.5abc" -> 3.5
pub fn stringToNumber(s: []const u8) !f64 {
    // Try to parse as integer first (strict)
    if (std.fmt.parseInt(i64, s, 10)) |int_value| {
        return @floatFromInt(int_value);
    } else |_| {
        // Try to parse as float (strict)
        if (std.fmt.parseFloat(f64, s)) |float_value| {
            return float_value;
        } else |_| {
            // Fall back to lax Ruby-style parsing: extract leading number
            const trimmed = std.mem.trim(u8, s, " \t\n\r");
            if (trimmed.len == 0) return 0.0;

            var i: usize = 0;
            var negative = false;
            var has_dot = false;

            // Handle leading sign
            if (trimmed[i] == '-') {
                negative = true;
                i += 1;
            } else if (trimmed[i] == '+') {
                i += 1;
            }

            const num_start = i;
            var int_part: f64 = 0;
            var frac_part: f64 = 0;
            var frac_divisor: f64 = 1;

            // Parse integer part
            while (i < trimmed.len and trimmed[i] >= '0' and trimmed[i] <= '9') : (i += 1) {
                int_part = int_part * 10 + @as(f64, @floatFromInt(trimmed[i] - '0'));
            }

            // Check for decimal point
            if (i < trimmed.len and trimmed[i] == '.') {
                has_dot = true;
                i += 1;
                // Parse fractional part
                while (i < trimmed.len and trimmed[i] >= '0' and trimmed[i] <= '9') : (i += 1) {
                    frac_part = frac_part * 10 + @as(f64, @floatFromInt(trimmed[i] - '0'));
                    frac_divisor *= 10;
                }
            }

            // If no digits were parsed, return 0
            if (i == num_start or (i == num_start + 1 and has_dot)) {
                return 0.0;
            }

            const result = int_part + frac_part / frac_divisor;
            return if (negative) -result else result;
        }
    }
}

/// Check if a string represents an integer (no decimal point)
pub fn isIntegerString(s: []const u8) bool {
    if (std.fmt.parseInt(i64, s, 10)) |_| {
        return true;
    } else |_| {
        return false;
    }
}

/// Ruby-style lax string to integer parsing
/// "6-3" -> 6, "3abc" -> 3, "abc" -> 0
pub fn rubyStringToInt(s: []const u8) i64 {
    // Try strict parsing first
    if (std.fmt.parseInt(i64, s, 10)) |int_value| {
        return int_value;
    } else |_| {
        // Lax parsing: extract leading integer
        const trimmed = std.mem.trim(u8, s, " \t\n\r");
        if (trimmed.len == 0) return 0;

        var i: usize = 0;
        var negative = false;

        // Handle leading sign
        if (trimmed[i] == '-') {
            negative = true;
            i += 1;
        } else if (trimmed[i] == '+') {
            i += 1;
        }

        if (i >= trimmed.len) return 0;

        const num_start = i;
        var result: i64 = 0;

        // Parse integer part
        while (i < trimmed.len and trimmed[i] >= '0' and trimmed[i] <= '9') : (i += 1) {
            result = result *| 10 +| @as(i64, trimmed[i] - '0');
        }

        // If no digits were parsed, return 0
        if (i == num_start) return 0;

        return if (negative) -result else result;
    }
}

/// Convert a number to string, formatting as integer when possible
pub fn numberToString(allocator: std.mem.Allocator, num: f64) ![]u8 {
    // Handle special cases
    if (std.math.isNan(num)) return try allocator.dupe(u8, "NaN");
    if (std.math.isInf(num)) return try allocator.dupe(u8, if (num > 0) "Infinity" else "-Infinity");

    // Check if number is effectively an integer
    const rounded = @round(num);
    if (@abs(num - rounded) < 0.0000001) {
        // Check if within i64 range before converting
        // Use conservative bounds due to float precision issues
        const max_safe: f64 = 9007199254740991.0; // 2^53 - 1, max safe integer in f64
        const min_safe: f64 = -9007199254740991.0;
        if (rounded >= min_safe and rounded <= max_safe) {
            const int_val: i64 = @intFromFloat(rounded);
            return try std.fmt.allocPrint(allocator, "{d}", .{int_val});
        }
    }
    // It's a float or too large for i64, format with decimal places
    return try std.fmt.allocPrint(allocator, "{d}", .{num});
}

/// Convert a number to string, always including decimal point
pub fn numberToStringForceFloat(allocator: std.mem.Allocator, num: f64) ![]u8 {
    // Handle special cases
    if (std.math.isNan(num)) return try allocator.dupe(u8, "NaN");
    if (std.math.isInf(num)) return try allocator.dupe(u8, if (num > 0) "Infinity" else "-Infinity");

    // Always format as float (with decimal point)
    const rounded = @round(num);
    if (@abs(num - rounded) < 0.0000001) {
        // Check if within safe integer range before converting
        const max_safe: f64 = 9007199254740991.0; // 2^53 - 1, max safe integer in f64
        const min_safe: f64 = -9007199254740991.0;
        if (rounded >= min_safe and rounded <= max_safe) {
            const int_val: i64 = @intFromFloat(rounded);
            return try std.fmt.allocPrint(allocator, "{d}.0", .{int_val});
        }
    }
    // It's a float or too large for i64, format with decimal places
    return try std.fmt.allocPrint(allocator, "{d}", .{num});
}

/// Check if a JSON value represents a float
pub fn valueIsFloat(value: json.Value) bool {
    return switch (value) {
        .float => true,
        .string => |s| !isIntegerString(s) and (std.fmt.parseFloat(f64, s) catch null) != null,
        else => false,
    };
}

/// Check if a string represents a float (has decimal point)
pub fn stringIsFloat(s: []const u8) bool {
    if (isIntegerString(s)) return false;
    if (std.fmt.parseFloat(f64, s)) |_| {
        return true;
    } else |_| {
        return false;
    }
}

/// Render a JSON value to output buffer (for {{ }} output)
pub fn renderValue(value: json.Value, output: *std.ArrayList(u8), allocator: std.mem.Allocator) !void {
    switch (value) {
        .string => |s| try output.appendSlice(allocator, s),
        .integer => |i| try output.writer(allocator).print("{d}", .{i}),
        .float => |f| {
            // Use floatToString to preserve .0 for whole numbers
            const str = try floatToString(allocator, f);
            defer allocator.free(str);
            try output.appendSlice(allocator, str);
        },
        .bool => |b| try output.appendSlice(allocator, if (b) "true" else "false"),
        .number_string => |s| try output.appendSlice(allocator, s),
        .null => {},
        .array => |arr| {
            // Recursively render array elements, concatenating them
            for (arr.items) |item| {
                try renderValue(item, output, allocator);
            }
        },
        .object => |obj| {
            // Render object in Ruby inspect format: {"key"=>value, ...}
            try output.appendSlice(allocator, "{");
            var first = true;
            var iter = obj.iterator();
            while (iter.next()) |entry| {
                if (!first) {
                    try output.appendSlice(allocator, ", ");
                }
                first = false;
                try output.appendSlice(allocator, "\"");
                try output.appendSlice(allocator, entry.key_ptr.*);
                try output.appendSlice(allocator, "\"=>");
                // Render value (may need special handling for nested types)
                switch (entry.value_ptr.*) {
                    .string => |s| {
                        try output.appendSlice(allocator, "\"");
                        try output.appendSlice(allocator, s);
                        try output.appendSlice(allocator, "\"");
                    },
                    .integer => |i| try output.writer(allocator).print("{d}", .{i}),
                    .float => |f| {
                        const str = try floatToString(allocator, f);
                        defer allocator.free(str);
                        try output.appendSlice(allocator, str);
                    },
                    .bool => |b| try output.appendSlice(allocator, if (b) "true" else "false"),
                    .number_string => |s| try output.appendSlice(allocator, s),
                    .null => try output.appendSlice(allocator, "nil"),
                    .array => |nested_arr| {
                        // Render nested array in Ruby format: ["foo", "bar"]
                        try output.appendSlice(allocator, "[");
                        for (nested_arr.items, 0..) |item, i| {
                            if (i > 0) try output.appendSlice(allocator, ", ");
                            try renderValueRubyInspect(item, output, allocator);
                        }
                        try output.appendSlice(allocator, "]");
                    },
                    .object => {
                        // Recursively render nested objects
                        try renderValue(entry.value_ptr.*, output, allocator);
                    },
                }
            }
            try output.appendSlice(allocator, "}");
        },
    }
}

/// Render a JSON value in Ruby inspect format (for nested values in hashes)
fn renderValueRubyInspect(value: json.Value, output: *std.ArrayList(u8), allocator: std.mem.Allocator) !void {
    switch (value) {
        .string => |s| {
            try output.appendSlice(allocator, "\"");
            try output.appendSlice(allocator, s);
            try output.appendSlice(allocator, "\"");
        },
        .integer => |i| try output.writer(allocator).print("{d}", .{i}),
        .float => |f| {
            const str = try floatToString(allocator, f);
            defer allocator.free(str);
            try output.appendSlice(allocator, str);
        },
        .bool => |b| try output.appendSlice(allocator, if (b) "true" else "false"),
        .number_string => |s| try output.appendSlice(allocator, s),
        .null => try output.appendSlice(allocator, "nil"),
        .array => |arr| {
            try output.appendSlice(allocator, "[");
            for (arr.items, 0..) |item, i| {
                if (i > 0) try output.appendSlice(allocator, ", ");
                try renderValueRubyInspect(item, output, allocator);
            }
            try output.appendSlice(allocator, "]");
        },
        .object => |obj| {
            try output.appendSlice(allocator, "{");
            var first = true;
            var iter = obj.iterator();
            while (iter.next()) |entry| {
                if (!first) try output.appendSlice(allocator, ", ");
                first = false;
                try output.appendSlice(allocator, "\"");
                try output.appendSlice(allocator, entry.key_ptr.*);
                try output.appendSlice(allocator, "\"=>");
                try renderValueRubyInspect(entry.value_ptr.*, output, allocator);
            }
            try output.appendSlice(allocator, "}");
        },
    }
}

/// Check if a value is "empty" for == empty comparisons
/// Empty: empty string "", empty array [], empty object {}
/// NOT empty: nil, numbers, booleans, whitespace strings
pub fn isEmptyValue(value: ?json.Value) bool {
    if (value == null) return false; // nil is NOT empty
    const v = value.?;
    return switch (v) {
        .string => |s| s.len == 0,
        .array => |arr| arr.items.len == 0,
        .object => |obj| obj.count() == 0,
        else => false,
    };
}

/// Check if a value is "blank" for == blank comparisons
/// Blank: nil, false, empty string "", whitespace-only strings, empty array [], empty object {}
/// NOT blank: 0 and other numbers
pub fn isBlankValue(value: ?json.Value) bool {
    if (value == null) return true; // nil is blank
    const v = value.?;
    return switch (v) {
        .null => true,
        .bool => |b| !b, // false is blank, true is not
        .string => |s| {
            if (s.len == 0) return true;
            // Check if string is all whitespace
            for (s) |c| {
                if (c != ' ' and c != '\t' and c != '\n' and c != '\r') return false;
            }
            return true; // All whitespace
        },
        .array => |arr| arr.items.len == 0,
        .object => |obj| obj.count() == 0,
        else => false, // Numbers (including 0) are not blank
    };
}

/// Extract the underlying value from a materialized drop (instantiate:*Drop pattern)
/// Returns the wrapped value for drops, or the original value if not a drop
pub fn unwrapDrop(value: json.Value) json.Value {
    if (value != .object) return value;

    const obj = value.object;
    var key_iter = obj.iterator();
    while (key_iter.next()) |entry| {
        const key = entry.key_ptr.*;
        // Check for instantiate:*Drop pattern
        if (std.mem.startsWith(u8, key, "instantiate:") and std.mem.endsWith(u8, key, "Drop")) {
            // Found a drop, extract its value
            if (entry.value_ptr.* == .object) {
                if (entry.value_ptr.object.get("value")) |inner_value| {
                    return inner_value;
                }
            }
        }
    }
    return value;
}

/// Check if a value is a BooleanDrop with false value (for truthiness checks)
/// BooleanDrops with false should be falsy, even though the object itself exists
pub fn isBooleanDropFalse(value: json.Value) bool {
    if (value != .object) return false;

    const obj = value.object;
    if (obj.get("instantiate:BooleanDrop")) |drop_value| {
        if (drop_value == .object) {
            if (drop_value.object.get("value")) |inner_value| {
                if (inner_value == .bool) {
                    return !inner_value.bool;
                }
            }
        }
    }
    return false;
}

/// In Liquid, only false and nil are falsy. Everything else is truthy.
/// This includes: 0, empty string "", empty arrays [], etc.
/// Exception: BooleanDrop with value=false should also be falsy
pub fn isFalsy(value: json.Value) bool {
    // Check for BooleanDrop with false value first
    if (isBooleanDropFalse(value)) return true;

    return switch (value) {
        .bool => |b| !b, // Only false is falsy
        .null => true, // nil/null is falsy
        else => false, // Everything else is truthy (including 0, "", [])
    };
}

/// Convert an array to JSON string representation
pub fn arrayToJsonString(allocator: std.mem.Allocator, items: []const json.Value) error{OutOfMemory}![]u8 {
    var result: std.ArrayList(u8) = .{};
    try result.append(allocator, '[');
    for (items, 0..) |item, i| {
        if (i > 0) try result.appendSlice(allocator, ", ");
        const item_str = try valueToJsonString(allocator, item);
        try result.appendSlice(allocator, item_str);
        allocator.free(item_str);
    }
    try result.append(allocator, ']');
    return try result.toOwnedSlice(allocator);
}

/// Convert a json.Value to a JSON string representation
pub fn valueToJsonString(allocator: std.mem.Allocator, value: json.Value) error{OutOfMemory}![]u8 {
    return switch (value) {
        .string => |s| try std.fmt.allocPrint(allocator, "\"{s}\"", .{s}),
        .integer => |i| try std.fmt.allocPrint(allocator, "{d}", .{i}),
        .float => |f| try std.fmt.allocPrint(allocator, "{d}", .{f}),
        .number_string => |s| try std.fmt.allocPrint(allocator, "\"{s}\"", .{s}),
        .bool => |b| try allocator.dupe(u8, if (b) "true" else "false"),
        .null => try allocator.dupe(u8, "null"),
        .array => |arr| try arrayToJsonString(allocator, arr.items),
        .object => try allocator.dupe(u8, "{}"), // Simplified object representation
    };
}

/// Comparison function for sorting JSON values (case-sensitive)
pub fn compareJsonValues(_: void, a: json.Value, b: json.Value) bool {
    // If both values are actual numbers (integer/float), compare numerically
    if (a == .integer or a == .float) {
        if (b == .integer or b == .float) {
            const a_num: f64 = if (a == .integer) @floatFromInt(a.integer) else a.float;
            const b_num: f64 = if (b == .integer) @floatFromInt(b.integer) else b.float;
            return a_num < b_num;
        }
    }

    // For strings and other types, use string comparison
    const a_str = valueToString(std.heap.page_allocator, a) catch return false;
    defer std.heap.page_allocator.free(a_str);
    const b_str = valueToString(std.heap.page_allocator, b) catch return false;
    defer std.heap.page_allocator.free(b_str);

    return std.mem.order(u8, a_str, b_str) == .lt;
}

/// Comparison function for natural (case-insensitive) sorting
pub fn compareJsonValuesNatural(_: void, a: json.Value, b: json.Value) bool {
    const a_str = valueToString(std.heap.page_allocator, a) catch return false;
    defer std.heap.page_allocator.free(a_str);
    const b_str = valueToString(std.heap.page_allocator, b) catch return false;
    defer std.heap.page_allocator.free(b_str);

    // Case-insensitive comparison
    return std.ascii.lessThanIgnoreCase(a_str, b_str);
}

/// Context for property-based sorting
pub const PropertySortContext = struct {
    property: []const u8,
};

/// Comparison function for sorting JSON objects by a property value
pub fn compareJsonValuesByProperty(ctx: PropertySortContext, a: json.Value, b: json.Value) bool {
    // Extract property values from objects
    const a_prop = getPropertyValue(a, ctx.property);
    const b_prop = getPropertyValue(b, ctx.property);

    // Items without the property sort at the end (nil values sort last)
    const a_has_prop = a_prop != .null;
    const b_has_prop = b_prop != .null;
    if (a_has_prop and !b_has_prop) return true; // a before b
    if (!a_has_prop and b_has_prop) return false; // b before a
    if (!a_has_prop and !b_has_prop) return false; // keep order

    // Compare the property values
    return compareJsonValues({}, a_prop, b_prop);
}

/// Comparison function for natural (case-insensitive) sorting JSON objects by a property value
pub fn compareJsonValuesByPropertyNatural(ctx: PropertySortContext, a: json.Value, b: json.Value) bool {
    // Extract property values from objects
    const a_prop = getPropertyValue(a, ctx.property);
    const b_prop = getPropertyValue(b, ctx.property);

    // Put items without the property at the end (nil values sort last in natural sort)
    const a_has_prop = a_prop != .null;
    const b_has_prop = b_prop != .null;
    if (a_has_prop and !b_has_prop) return true; // a before b
    if (!a_has_prop and b_has_prop) return false; // b before a
    if (!a_has_prop and !b_has_prop) return false; // keep order

    // Compare the property values using natural (case-insensitive) comparison
    return compareJsonValuesNatural({}, a_prop, b_prop);
}

fn getPropertyValue(value: json.Value, property: []const u8) json.Value {
    if (value == .object) {
        if (value.object.get(property)) |prop| {
            return prop;
        }
    }
    return json.Value{ .null = {} };
}

/// Trim whitespace from left side of string
pub fn trimLeft(s: []const u8) []const u8 {
    var start: usize = 0;
    while (start < s.len) : (start += 1) {
        const c = s[start];
        if (c != ' ' and c != '\t' and c != '\n' and c != '\r') break;
    }
    return s[start..];
}

/// Trim whitespace from right side of string
pub fn trimRight(s: []const u8) []const u8 {
    var end: usize = s.len;
    while (end > 0) {
        const c = s[end - 1];
        if (c != ' ' and c != '\t' and c != '\n' and c != '\r') break;
        end -= 1;
    }
    return s[0..end];
}
