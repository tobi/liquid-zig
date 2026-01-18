const std = @import("std");
const json = std.json;
const utils = @import("utils.zig");

pub const FilterKind = enum {
    abs,
    append,
    at_least,
    at_most,
    capitalize,
    ceil,
    compact,
    concat,
    date,
    default,
    divided_by,
    downcase,
    escape,
    escape_once,
    first,
    floor,
    join,
    last,
    lstrip,
    map,
    minus,
    modulo,
    newline_to_br,
    plus,
    pop,
    prepend,
    push,
    remove,
    remove_first,
    remove_last,
    replace,
    replace_first,
    replace_last,
    reverse,
    round,
    rstrip,
    shift,
    size,
    slice,
    sort,
    sort_natural,
    split,
    strip,
    strip_html,
    strip_newlines,
    times,
    truncate,
    truncatewords,
    uniq,
    unshift,
    upcase,
    url_decode,
    url_encode,
    where,
    unknown,
};

pub const filter_map = std.StaticStringMap(FilterKind).initComptime(.{
    .{ "abs", .abs },
    .{ "append", .append },
    .{ "at_least", .at_least },
    .{ "at_most", .at_most },
    .{ "capitalize", .capitalize },
    .{ "ceil", .ceil },
    .{ "compact", .compact },
    .{ "concat", .concat },
    .{ "date", .date },
    .{ "default", .default },
    .{ "divided_by", .divided_by },
    .{ "downcase", .downcase },
    .{ "escape", .escape },
    .{ "escape_once", .escape_once },
    .{ "first", .first },
    .{ "floor", .floor },
    .{ "join", .join },
    .{ "last", .last },
    .{ "lstrip", .lstrip },
    .{ "map", .map },
    .{ "minus", .minus },
    .{ "modulo", .modulo },
    .{ "newline_to_br", .newline_to_br },
    .{ "plus", .plus },
    .{ "pop", .pop },
    .{ "prepend", .prepend },
    .{ "push", .push },
    .{ "remove", .remove },
    .{ "remove_first", .remove_first },
    .{ "remove_last", .remove_last },
    .{ "replace", .replace },
    .{ "replace_first", .replace_first },
    .{ "replace_last", .replace_last },
    .{ "reverse", .reverse },
    .{ "round", .round },
    .{ "rstrip", .rstrip },
    .{ "shift", .shift },
    .{ "size", .size },
    .{ "slice", .slice },
    .{ "sort", .sort },
    .{ "sort_natural", .sort_natural },
    .{ "split", .split },
    .{ "strip", .strip },
    .{ "strip_html", .strip_html },
    .{ "strip_newlines", .strip_newlines },
    .{ "times", .times },
    .{ "truncate", .truncate },
    .{ "truncatewords", .truncatewords },
    .{ "uniq", .uniq },
    .{ "unshift", .unshift },
    .{ "upcase", .upcase },
    .{ "url_decode", .url_decode },
    .{ "url_encode", .url_encode },
    .{ "where", .where },
});

pub const FilterResult = union(enum) {
    string: []u8,
    json_value: json.Value,
    error_message: []u8,

    pub fn toValue(self: FilterResult, allocator: std.mem.Allocator) !json.Value {
        switch (self) {
            .string => |s| return json.Value{ .string = s },
            .json_value => |v| {
                _ = allocator;
                return v;
            },
            .error_message => |s| return json.Value{ .string = s },
        }
    }

    pub fn toString(self: FilterResult, allocator: std.mem.Allocator) ![]u8 {
        switch (self) {
            .string => |s| return s,
            .json_value => |v| return try utils.valueToString(allocator, v),
            .error_message => |s| return s,
        }
    }
};

pub const Filter = struct {
    name: []const u8,
    kind: FilterKind,
    args: [][]const u8,
    args_are_literals: []bool,

    pub fn parse(allocator: std.mem.Allocator, content: []const u8) !Filter {
        const trimmed = std.mem.trim(u8, content, " \t\n\r");

        // Find the colon separating filter name from args (but not inside quotes)
        var colon_pos: ?usize = null;
        var in_single_quote = false;
        var in_double_quote = false;
        for (trimmed, 0..) |c, idx| {
            if (c == '\'' and !in_double_quote) {
                in_single_quote = !in_single_quote;
            } else if (c == '"' and !in_single_quote) {
                in_double_quote = !in_double_quote;
            } else if (c == ':' and !in_single_quote and !in_double_quote) {
                colon_pos = idx;
                break;
            }
        }

        const name = if (colon_pos) |pos|
            std.mem.trim(u8, trimmed[0..pos], " \t\n\r")
        else
            std.mem.trim(u8, trimmed, " \t\n\r");

        var args: std.ArrayList([]const u8) = .{};
        errdefer args.deinit(allocator);
        var literals: std.ArrayList(bool) = .{};
        errdefer literals.deinit(allocator);

        if (colon_pos) |pos| {
            const args_str = trimmed[pos + 1 ..];
            var start: usize = 0;
            in_single_quote = false;
            in_double_quote = false;
            for (args_str, 0..) |c, idx| {
                if (c == '\'' and !in_double_quote) {
                    in_single_quote = !in_single_quote;
                } else if (c == '"' and !in_single_quote) {
                    in_double_quote = !in_double_quote;
                } else if (c == ',' and !in_single_quote and !in_double_quote) {
                    const arg = std.mem.trim(u8, args_str[start..idx], " \t\n\r");
                    const is_literal = isQuotedString(arg) or isNumericString(arg);
                    const unquoted = unquoteString(arg);
                    try args.append(allocator, try allocator.dupe(u8, unquoted));
                    try literals.append(allocator, is_literal);
                    start = idx + 1;
                }
            }
            if (start < args_str.len) {
                const arg = std.mem.trim(u8, args_str[start..], " \t\n\r");
                const is_literal = isQuotedString(arg) or isNumericString(arg);
                const unquoted = unquoteString(arg);
                try args.append(allocator, try allocator.dupe(u8, unquoted));
                try literals.append(allocator, is_literal);
            }
        }

        return Filter{
            .name = try allocator.dupe(u8, name),
            .kind = filter_map.get(name) orelse .unknown,
            .args = try args.toOwnedSlice(allocator),
            .args_are_literals = try literals.toOwnedSlice(allocator),
        };
    }

    fn isQuotedString(s: []const u8) bool {
        if (s.len >= 2) {
            if ((s[0] == '"' and s[s.len - 1] == '"') or
                (s[0] == '\'' and s[s.len - 1] == '\''))
            {
                return true;
            }
        }
        return false;
    }

    fn isNumericString(s: []const u8) bool {
        if (s.len == 0) return false;
        var has_digit = false;
        var has_dot = false;
        for (s, 0..) |c, i| {
            if (c == '-' and i == 0) continue;
            if (c == '.' and !has_dot) {
                has_dot = true;
                continue;
            }
            if (c >= '0' and c <= '9') {
                has_digit = true;
            } else {
                return false;
            }
        }
        return has_digit;
    }

    fn unquoteString(s: []const u8) []const u8 {
        if (s.len >= 2) {
            if ((s[0] == '"' and s[s.len - 1] == '"') or
                (s[0] == '\'' and s[s.len - 1] == '\''))
            {
                return s[1 .. s.len - 1];
            }
        }
        return s;
    }

    pub fn deinit(self: *Filter, allocator: std.mem.Allocator) void {
        allocator.free(self.name);
        for (self.args) |arg| {
            allocator.free(arg);
        }
        allocator.free(self.args);
        allocator.free(self.args_are_literals);
    }

    pub fn clone(self: *const Filter, allocator: std.mem.Allocator) !Filter {
        const name = try allocator.dupe(u8, self.name);
        errdefer allocator.free(name);

        var args: std.ArrayList([]const u8) = .{};
        errdefer {
            for (args.items) |arg| {
                allocator.free(arg);
            }
            args.deinit(allocator);
        }

        for (self.args) |arg| {
            try args.append(allocator, try allocator.dupe(u8, arg));
        }

        const args_slice = try args.toOwnedSlice(allocator);
        errdefer allocator.free(args_slice);

        const args_are_literals = try allocator.dupe(bool, self.args_are_literals);

        return Filter{
            .name = name,
            .kind = self.kind,
            .args = args_slice,
            .args_are_literals = args_are_literals,
        };
    }

    /// Apply filter to a value and return string result
    pub fn apply(self: *Filter, allocator: std.mem.Allocator, value: json.Value) ![]u8 {
        // Handle array-specific filters first
        if (value == .array) {
            if (std.mem.eql(u8, self.name, "join")) {
                const separator = if (self.args.len > 0) self.args[0] else ", ";
                var result: std.ArrayList(u8) = .{};
                const items = value.array.items;
                for (items, 0..) |item, i| {
                    if (i > 0) {
                        try result.appendSlice(allocator, separator);
                    }
                    const item_str = try utils.valueToString(allocator, item);
                    try result.appendSlice(allocator, item_str);
                    if (item == .integer or item == .float) {
                        allocator.free(item_str);
                    }
                }
                return try result.toOwnedSlice(allocator);
            } else if (std.mem.eql(u8, self.name, "first")) {
                const items = value.array.items;
                if (items.len == 0) {
                    return try allocator.dupe(u8, "");
                }
                if (self.args.len > 0) {
                    const n = try std.fmt.parseInt(usize, self.args[0], 10);
                    const count = @min(n, items.len);
                    var result: std.ArrayList(u8) = .{};
                    try result.append(allocator, '[');
                    for (items[0..count], 0..) |item, i| {
                        if (i > 0) try result.appendSlice(allocator, ", ");
                        const item_str = try utils.valueToJsonString(allocator, item);
                        try result.appendSlice(allocator, item_str);
                    }
                    try result.append(allocator, ']');
                    return try result.toOwnedSlice(allocator);
                } else {
                    const str = try utils.valueToString(allocator, items[0]);
                    return try allocator.dupe(u8, str);
                }
            } else if (std.mem.eql(u8, self.name, "last")) {
                const items = value.array.items;
                if (items.len == 0) {
                    return try allocator.dupe(u8, "");
                }
                if (self.args.len > 0) {
                    const n = try std.fmt.parseInt(usize, self.args[0], 10);
                    const count = @min(n, items.len);
                    const start = items.len - count;
                    var result: std.ArrayList(u8) = .{};
                    try result.append(allocator, '[');
                    for (items[start..], 0..) |item, i| {
                        if (i > 0) try result.appendSlice(allocator, ", ");
                        const item_str = try utils.valueToJsonString(allocator, item);
                        try result.appendSlice(allocator, item_str);
                    }
                    try result.append(allocator, ']');
                    return try result.toOwnedSlice(allocator);
                } else {
                    const str = try utils.valueToString(allocator, items[items.len - 1]);
                    return try allocator.dupe(u8, str);
                }
            } else if (std.mem.eql(u8, self.name, "reverse")) {
                const items = value.array.items;
                var result: std.ArrayList(u8) = .{};
                try result.append(allocator, '[');
                var i = items.len;
                while (i > 0) {
                    i -= 1;
                    if (i < items.len - 1) try result.appendSlice(allocator, ", ");
                    const item_str = try utils.valueToJsonString(allocator, items[i]);
                    try result.appendSlice(allocator, item_str);
                }
                try result.append(allocator, ']');
                return try result.toOwnedSlice(allocator);
            } else if (std.mem.eql(u8, self.name, "sort")) {
                const items = value.array.items;
                const sorted = try allocator.alloc(json.Value, items.len);
                defer allocator.free(sorted);
                @memcpy(sorted, items);

                if (self.args.len > 0) {
                    // Sort by property
                    const property = self.args[0];
                    std.mem.sort(json.Value, sorted, utils.PropertySortContext{ .property = property }, utils.compareJsonValuesByProperty);
                } else {
                    std.mem.sort(json.Value, sorted, {}, utils.compareJsonValues);
                }

                var result: std.ArrayList(u8) = .{};
                try result.append(allocator, '[');
                for (sorted, 0..) |item, i| {
                    if (i > 0) try result.appendSlice(allocator, ", ");
                    const item_str = try utils.valueToJsonString(allocator, item);
                    try result.appendSlice(allocator, item_str);
                }
                try result.append(allocator, ']');
                return try result.toOwnedSlice(allocator);
            } else if (std.mem.eql(u8, self.name, "sort_natural")) {
                const items = value.array.items;
                const sorted = try allocator.alloc(json.Value, items.len);
                defer allocator.free(sorted);
                @memcpy(sorted, items);
                std.mem.sort(json.Value, sorted, {}, utils.compareJsonValuesNatural);

                var result: std.ArrayList(u8) = .{};
                try result.append(allocator, '[');
                for (sorted, 0..) |item, i| {
                    if (i > 0) try result.appendSlice(allocator, ", ");
                    const item_str = try utils.valueToJsonString(allocator, item);
                    try result.appendSlice(allocator, item_str);
                }
                try result.append(allocator, ']');
                return try result.toOwnedSlice(allocator);
            } else if (std.mem.eql(u8, self.name, "uniq")) {
                const items = value.array.items;
                var seen = std.StringHashMap(void).init(allocator);
                defer {
                    var key_iter = seen.keyIterator();
                    while (key_iter.next()) |key| {
                        allocator.free(key.*);
                    }
                    seen.deinit();
                }
                var result: std.ArrayList(u8) = .{};
                try result.append(allocator, '[');
                var first = true;
                for (items) |item| {
                    const item_str = try utils.valueToJsonString(allocator, item);
                    if (!seen.contains(item_str)) {
                        try seen.put(item_str, {});
                        if (!first) try result.appendSlice(allocator, ", ");
                        first = false;
                        try result.appendSlice(allocator, item_str);
                    } else {
                        allocator.free(item_str);
                    }
                }
                try result.append(allocator, ']');
                return try result.toOwnedSlice(allocator);
            } else if (std.mem.eql(u8, self.name, "compact")) {
                const items = value.array.items;
                var result: std.ArrayList(u8) = .{};
                try result.append(allocator, '[');
                var first = true;
                for (items) |item| {
                    if (item != .null) {
                        if (!first) try result.appendSlice(allocator, ", ");
                        first = false;
                        const item_str = try utils.valueToJsonString(allocator, item);
                        try result.appendSlice(allocator, item_str);
                        allocator.free(item_str);
                    }
                }
                try result.append(allocator, ']');
                return try result.toOwnedSlice(allocator);
            } else if (std.mem.eql(u8, self.name, "size")) {
                return try std.fmt.allocPrint(allocator, "{d}", .{value.array.items.len});
            } else if (std.mem.eql(u8, self.name, "concat")) {
                const items = value.array.items;
                var result: std.ArrayList(u8) = .{};
                try result.append(allocator, '[');
                for (items, 0..) |item, i| {
                    if (i > 0) try result.appendSlice(allocator, ", ");
                    const item_str = try utils.valueToJsonString(allocator, item);
                    try result.appendSlice(allocator, item_str);
                    allocator.free(item_str);
                }
                try result.append(allocator, ']');
                return try result.toOwnedSlice(allocator);
            } else if (std.mem.eql(u8, self.name, "map")) {
                if (self.args.len == 0) {
                    return try utils.arrayToJsonString(allocator, value.array.items);
                }
                const property = self.args[0];
                const items = value.array.items;
                var result: std.ArrayList(u8) = .{};
                try result.append(allocator, '[');
                for (items, 0..) |item, i| {
                    if (i > 0) try result.appendSlice(allocator, ", ");
                    if (item == .object) {
                        if (item.object.get(property)) |prop_value| {
                            const prop_str = try utils.valueToJsonString(allocator, prop_value);
                            try result.appendSlice(allocator, prop_str);
                            allocator.free(prop_str);
                        } else {
                            try result.appendSlice(allocator, "null");
                        }
                    } else {
                        try result.appendSlice(allocator, "null");
                    }
                }
                try result.append(allocator, ']');
                return try result.toOwnedSlice(allocator);
            } else if (std.mem.eql(u8, self.name, "where")) {
                if (self.args.len < 2) {
                    return try utils.arrayToJsonString(allocator, value.array.items);
                }
                const property = self.args[0];
                const target_value = self.args[1];
                const items = value.array.items;
                var result: std.ArrayList(u8) = .{};
                try result.append(allocator, '[');
                var first = true;
                for (items) |item| {
                    if (item == .object) {
                        if (item.object.get(property)) |prop_value| {
                            const prop_str = try utils.valueToString(allocator, prop_value);
                            if (std.mem.eql(u8, prop_str, target_value)) {
                                if (!first) try result.appendSlice(allocator, ", ");
                                first = false;
                                const item_str = try utils.valueToJsonString(allocator, item);
                                try result.appendSlice(allocator, item_str);
                                allocator.free(item_str);
                            }
                        }
                    }
                }
                try result.append(allocator, ']');
                return try result.toOwnedSlice(allocator);
            } else if (std.mem.eql(u8, self.name, "push")) {
                if (self.args.len == 0) {
                    return try utils.arrayToJsonString(allocator, value.array.items);
                }
                const items = value.array.items;
                var result: std.ArrayList(u8) = .{};
                try result.append(allocator, '[');
                for (items, 0..) |item, i| {
                    if (i > 0) try result.appendSlice(allocator, ", ");
                    const item_str = try utils.valueToJsonString(allocator, item);
                    try result.appendSlice(allocator, item_str);
                    allocator.free(item_str);
                }
                if (items.len > 0) try result.appendSlice(allocator, ", ");
                try result.append(allocator, '"');
                try result.appendSlice(allocator, self.args[0]);
                try result.append(allocator, '"');
                try result.append(allocator, ']');
                return try result.toOwnedSlice(allocator);
            } else if (std.mem.eql(u8, self.name, "pop")) {
                const items = value.array.items;
                if (items.len == 0) {
                    return try allocator.dupe(u8, "[]");
                }
                var result: std.ArrayList(u8) = .{};
                try result.append(allocator, '[');
                for (items[0 .. items.len - 1], 0..) |item, i| {
                    if (i > 0) try result.appendSlice(allocator, ", ");
                    const item_str = try utils.valueToJsonString(allocator, item);
                    try result.appendSlice(allocator, item_str);
                    allocator.free(item_str);
                }
                try result.append(allocator, ']');
                return try result.toOwnedSlice(allocator);
            } else if (std.mem.eql(u8, self.name, "shift")) {
                const items = value.array.items;
                if (items.len == 0) {
                    return try allocator.dupe(u8, "[]");
                }
                var result: std.ArrayList(u8) = .{};
                try result.append(allocator, '[');
                for (items[1..], 0..) |item, i| {
                    if (i > 0) try result.appendSlice(allocator, ", ");
                    const item_str = try utils.valueToJsonString(allocator, item);
                    try result.appendSlice(allocator, item_str);
                    allocator.free(item_str);
                }
                try result.append(allocator, ']');
                return try result.toOwnedSlice(allocator);
            } else if (std.mem.eql(u8, self.name, "unshift")) {
                if (self.args.len == 0) {
                    return try utils.arrayToJsonString(allocator, value.array.items);
                }
                const items = value.array.items;
                var result: std.ArrayList(u8) = .{};
                try result.append(allocator, '[');
                try result.append(allocator, '"');
                try result.appendSlice(allocator, self.args[0]);
                try result.append(allocator, '"');
                if (items.len > 0) try result.appendSlice(allocator, ", ");
                for (items, 0..) |item, i| {
                    if (i > 0) try result.appendSlice(allocator, ", ");
                    const item_str = try utils.valueToJsonString(allocator, item);
                    try result.appendSlice(allocator, item_str);
                    allocator.free(item_str);
                }
                try result.append(allocator, ']');
                return try result.toOwnedSlice(allocator);
            }
        }

        // Fall through to string filters
        const str = try utils.valueToString(allocator, value);
        defer allocator.free(str);

        return try self.applyString(allocator, str, value);
    }

    /// Apply filter to a string value
    fn applyString(self: *Filter, allocator: std.mem.Allocator, str: []const u8, original_value: json.Value) ![]u8 {
        if (std.mem.eql(u8, self.name, "upcase")) {
            return try std.ascii.allocUpperString(allocator, str);
        } else if (std.mem.eql(u8, self.name, "downcase")) {
            return try std.ascii.allocLowerString(allocator, str);
        } else if (std.mem.eql(u8, self.name, "capitalize")) {
            const result = try allocator.dupe(u8, str);
            if (result.len > 0) {
                result[0] = std.ascii.toUpper(result[0]);
            }
            return result;
        } else if (std.mem.eql(u8, self.name, "strip")) {
            const trimmed = std.mem.trim(u8, str, " \t\n\r");
            return try allocator.dupe(u8, trimmed);
        } else if (std.mem.eql(u8, self.name, "lstrip")) {
            const trimmed = std.mem.trimLeft(u8, str, " \t\n\r");
            return try allocator.dupe(u8, trimmed);
        } else if (std.mem.eql(u8, self.name, "rstrip")) {
            const trimmed = std.mem.trimRight(u8, str, " \t\n\r");
            return try allocator.dupe(u8, trimmed);
        } else if (std.mem.eql(u8, self.name, "size")) {
            return try std.fmt.allocPrint(allocator, "{d}", .{str.len});
        } else if (std.mem.eql(u8, self.name, "reverse")) {
            const result = try allocator.dupe(u8, str);
            std.mem.reverse(u8, result);
            return result;
        } else if (std.mem.eql(u8, self.name, "escape")) {
            return try escapeHtml(allocator, str);
        } else if (std.mem.eql(u8, self.name, "default")) {
            if (str.len == 0 and self.args.len > 0) {
                return try allocator.dupe(u8, self.args[0]);
            }
            return try allocator.dupe(u8, str);
        } else if (std.mem.eql(u8, self.name, "plus")) {
            return try applyMathFilter(allocator, original_value, self.args, .plus);
        } else if (std.mem.eql(u8, self.name, "minus")) {
            return try applyMathFilter(allocator, original_value, self.args, .minus);
        } else if (std.mem.eql(u8, self.name, "times")) {
            return try applyMathFilter(allocator, original_value, self.args, .times);
        } else if (std.mem.eql(u8, self.name, "divided_by")) {
            return try applyDividedBy(allocator, original_value, self.args);
        } else if (std.mem.eql(u8, self.name, "modulo")) {
            return try applyModulo(allocator, original_value, self.args);
        } else if (std.mem.eql(u8, self.name, "abs")) {
            const num = try utils.valueToNumber(original_value);
            const result = @abs(num);
            return try utils.numberToString(allocator, result);
        } else if (std.mem.eql(u8, self.name, "ceil")) {
            const num = try utils.valueToNumber(original_value);
            const result = @ceil(num);
            return try std.fmt.allocPrint(allocator, "{d}", .{@as(i64, @intFromFloat(result))});
        } else if (std.mem.eql(u8, self.name, "floor")) {
            const num = try utils.valueToNumber(original_value);
            const result = @floor(num);
            return try std.fmt.allocPrint(allocator, "{d}", .{@as(i64, @intFromFloat(result))});
        } else if (std.mem.eql(u8, self.name, "round")) {
            return try applyRound(allocator, original_value, self.args);
        } else if (std.mem.eql(u8, self.name, "at_least")) {
            const num = try utils.valueToNumber(original_value);
            if (self.args.len == 0) return try allocator.dupe(u8, str);
            const min_val = try utils.stringToNumber(self.args[0]);
            return try utils.numberToString(allocator, @max(num, min_val));
        } else if (std.mem.eql(u8, self.name, "at_most")) {
            const num = try utils.valueToNumber(original_value);
            if (self.args.len == 0) return try allocator.dupe(u8, str);
            const max_val = try utils.stringToNumber(self.args[0]);
            return try utils.numberToString(allocator, @min(num, max_val));
        } else if (std.mem.eql(u8, self.name, "append")) {
            if (self.args.len == 0) return try allocator.dupe(u8, str);
            return try std.mem.concat(allocator, u8, &[_][]const u8{ str, self.args[0] });
        } else if (std.mem.eql(u8, self.name, "prepend")) {
            if (self.args.len == 0) return try allocator.dupe(u8, str);
            return try std.mem.concat(allocator, u8, &[_][]const u8{ self.args[0], str });
        } else if (std.mem.eql(u8, self.name, "remove")) {
            return try applyRemove(allocator, str, self.args, .all);
        } else if (std.mem.eql(u8, self.name, "remove_first")) {
            return try applyRemove(allocator, str, self.args, .first);
        } else if (std.mem.eql(u8, self.name, "remove_last")) {
            return try applyRemove(allocator, str, self.args, .last);
        } else if (std.mem.eql(u8, self.name, "replace")) {
            return try applyReplace(allocator, str, self.args, .all);
        } else if (std.mem.eql(u8, self.name, "replace_first")) {
            return try applyReplace(allocator, str, self.args, .first);
        } else if (std.mem.eql(u8, self.name, "replace_last")) {
            return try applyReplace(allocator, str, self.args, .last);
        } else if (std.mem.eql(u8, self.name, "slice")) {
            return try applySlice(allocator, str, self.args);
        } else if (std.mem.eql(u8, self.name, "split")) {
            return try applySplit(allocator, str, self.args);
        } else if (std.mem.eql(u8, self.name, "truncate")) {
            return try applyTruncate(allocator, str, self.args);
        } else if (std.mem.eql(u8, self.name, "truncatewords")) {
            return try applyTruncateWords(allocator, str, self.args);
        } else if (std.mem.eql(u8, self.name, "url_encode")) {
            return try applyUrlEncode(allocator, str);
        } else if (std.mem.eql(u8, self.name, "url_decode")) {
            return try applyUrlDecode(allocator, str);
        } else if (std.mem.eql(u8, self.name, "newline_to_br")) {
            return try applyNewlineToBr(allocator, str);
        } else if (std.mem.eql(u8, self.name, "strip_html")) {
            return try applyStripHtml(allocator, str);
        } else if (std.mem.eql(u8, self.name, "strip_newlines")) {
            return try applyStripNewlines(allocator, str);
        } else if (std.mem.eql(u8, self.name, "escape_once")) {
            return try applyEscapeOnce(allocator, str);
        } else if (std.mem.eql(u8, self.name, "first")) {
            // first on string returns first character
            if (str.len == 0) return try allocator.dupe(u8, "");
            return try allocator.dupe(u8, str[0..1]);
        } else if (std.mem.eql(u8, self.name, "last")) {
            // last on string returns last character
            if (str.len == 0) return try allocator.dupe(u8, "");
            return try allocator.dupe(u8, str[str.len - 1 .. str.len]);
        } else {
            // Unknown filter, return as-is
            return try allocator.dupe(u8, str);
        }
    }

    /// Apply filter and return FilterResult to preserve arrays for chaining
    pub fn applyValue(self: *Filter, allocator: std.mem.Allocator, value: json.Value) !FilterResult {
        // Handle default filter
        if (std.mem.eql(u8, self.name, "default")) {
            const is_falsy = switch (value) {
                .null => true,
                .bool => |b| !b,
                .string => |s| s.len == 0,
                .array => |arr| arr.items.len == 0,
                else => false,
            };
            if (is_falsy and self.args.len > 0) {
                return FilterResult{ .string = try allocator.dupe(u8, self.args[0]) };
            }
            const str = try utils.valueToString(allocator, value);
            return FilterResult{ .string = str };
        }

        // Handle array-specific filters that should return arrays
        if (value == .array) {
            const items = value.array.items;

            if (std.mem.eql(u8, self.name, "reverse")) {
                if (items.len == 0) {
                    const arr = json.Array.init(allocator);
                    return FilterResult{ .json_value = json.Value{ .array = arr } };
                }
                var arr = json.Array.init(allocator);
                var i = items.len;
                while (i > 0) {
                    i -= 1;
                    try arr.append(items[i]);
                }
                return FilterResult{ .json_value = json.Value{ .array = arr } };
            } else if (std.mem.eql(u8, self.name, "sort")) {
                if (items.len == 0) {
                    const arr = json.Array.init(allocator);
                    return FilterResult{ .json_value = json.Value{ .array = arr } };
                }

                const sorted = try allocator.alloc(json.Value, items.len);
                defer allocator.free(sorted);
                @memcpy(sorted, items);

                if (self.args.len > 0) {
                    // Sort by property
                    const property = self.args[0];
                    std.mem.sort(json.Value, sorted, utils.PropertySortContext{ .property = property }, utils.compareJsonValuesByProperty);
                } else {
                    // Check for incompatible types
                    if (items.len > 1) {
                        var has_number = false;
                        var has_string = false;
                        var has_bool = false;
                        for (items) |item| {
                            switch (item) {
                                .integer, .float => has_number = true,
                                .string => has_string = true,
                                .bool => has_bool = true,
                                else => {},
                            }
                        }
                        const type_count = @as(u8, if (has_number) 1 else 0) + @as(u8, if (has_string) 1 else 0) + @as(u8, if (has_bool) 1 else 0);
                        if (type_count > 1) {
                            return FilterResult{ .error_message = try std.fmt.allocPrint(allocator, "Liquid error (line 1): cannot sort values of incompatible types", .{}) };
                        }
                    }
                    std.mem.sort(json.Value, sorted, {}, utils.compareJsonValues);
                }

                var arr = json.Array.init(allocator);
                for (sorted) |item| {
                    try arr.append(item);
                }
                return FilterResult{ .json_value = json.Value{ .array = arr } };
            } else if (std.mem.eql(u8, self.name, "sort_natural")) {
                if (items.len == 0) {
                    const arr = json.Array.init(allocator);
                    return FilterResult{ .json_value = json.Value{ .array = arr } };
                }
                const sorted = try allocator.alloc(json.Value, items.len);
                defer allocator.free(sorted);
                @memcpy(sorted, items);
                std.mem.sort(json.Value, sorted, {}, utils.compareJsonValuesNatural);
                var arr = json.Array.init(allocator);
                for (sorted) |item| {
                    try arr.append(item);
                }
                return FilterResult{ .json_value = json.Value{ .array = arr } };
            } else if (std.mem.eql(u8, self.name, "uniq")) {
                if (items.len == 0) {
                    const arr = json.Array.init(allocator);
                    return FilterResult{ .json_value = json.Value{ .array = arr } };
                }
                var seen = std.StringHashMap(void).init(allocator);
                defer {
                    var key_iter = seen.keyIterator();
                    while (key_iter.next()) |key| {
                        allocator.free(key.*);
                    }
                    seen.deinit();
                }
                var arr = json.Array.init(allocator);
                for (items) |item| {
                    const item_str = try utils.valueToJsonString(allocator, item);
                    if (!seen.contains(item_str)) {
                        try seen.put(item_str, {});
                        try arr.append(item);
                    } else {
                        allocator.free(item_str);
                    }
                }
                return FilterResult{ .json_value = json.Value{ .array = arr } };
            } else if (std.mem.eql(u8, self.name, "compact")) {
                var arr = json.Array.init(allocator);
                for (items) |item| {
                    if (item != .null) {
                        try arr.append(item);
                    }
                }
                return FilterResult{ .json_value = json.Value{ .array = arr } };
            } else if (std.mem.eql(u8, self.name, "map")) {
                if (self.args.len == 0) {
                    const arr = json.Array.init(allocator);
                    return FilterResult{ .json_value = json.Value{ .array = arr } };
                }
                const property = self.args[0];
                var arr = json.Array.init(allocator);
                for (items) |item| {
                    if (item == .object) {
                        if (item.object.get(property)) |prop_value| {
                            try arr.append(prop_value);
                        } else {
                            try arr.append(json.Value{ .null = {} });
                        }
                    } else {
                        try arr.append(json.Value{ .null = {} });
                    }
                }
                return FilterResult{ .json_value = json.Value{ .array = arr } };
            } else if (std.mem.eql(u8, self.name, "where")) {
                if (self.args.len == 0) {
                    const arr = json.Array.init(allocator);
                    return FilterResult{ .json_value = json.Value{ .array = arr } };
                }
                const property = self.args[0];
                const target_value = if (self.args.len > 1) self.args[1] else null;

                var arr = json.Array.init(allocator);
                for (items) |item| {
                    if (item == .object) {
                        if (item.object.get(property)) |prop_value| {
                            const matches = if (target_value) |tv| blk: {
                                const prop_str = try utils.valueToString(allocator, prop_value);
                                defer allocator.free(prop_str);
                                break :blk std.mem.eql(u8, prop_str, tv);
                            } else !utils.isFalsy(prop_value);
                            if (matches) {
                                try arr.append(item);
                            }
                        }
                    }
                }
                return FilterResult{ .json_value = json.Value{ .array = arr } };
            } else if (std.mem.eql(u8, self.name, "concat")) {
                var arr = json.Array.init(allocator);
                for (items) |item| {
                    try arr.append(item);
                }
                return FilterResult{ .json_value = json.Value{ .array = arr } };
            } else if (std.mem.eql(u8, self.name, "join")) {
                const separator = if (self.args.len > 0) self.args[0] else ", ";
                var result: std.ArrayList(u8) = .{};
                for (items, 0..) |item, i| {
                    if (i > 0) {
                        try result.appendSlice(allocator, separator);
                    }
                    const item_str = try utils.valueToString(allocator, item);
                    try result.appendSlice(allocator, item_str);
                    if (item == .integer or item == .float) {
                        allocator.free(item_str);
                    }
                }
                return FilterResult{ .string = try result.toOwnedSlice(allocator) };
            } else if (std.mem.eql(u8, self.name, "first")) {
                if (items.len == 0) {
                    return FilterResult{ .json_value = json.Value{ .null = {} } };
                }
                return FilterResult{ .json_value = items[0] };
            } else if (std.mem.eql(u8, self.name, "last")) {
                if (items.len == 0) {
                    return FilterResult{ .json_value = json.Value{ .null = {} } };
                }
                return FilterResult{ .json_value = items[items.len - 1] };
            } else if (std.mem.eql(u8, self.name, "size")) {
                return FilterResult{ .json_value = json.Value{ .integer = @intCast(items.len) } };
            }
        }

        // Handle split specially - it returns an array
        if (std.mem.eql(u8, self.name, "split")) {
            const str = try utils.valueToString(allocator, value);
            defer allocator.free(str);

            if (str.len == 0) {
                const arr = json.Array.init(allocator);
                return FilterResult{ .json_value = json.Value{ .array = arr } };
            }

            const separator = if (self.args.len > 0) self.args[0] else " ";
            var arr = json.Array.init(allocator);

            if (separator.len == 0) {
                for (str) |c| {
                    const char_str = try allocator.alloc(u8, 1);
                    char_str[0] = c;
                    try arr.append(json.Value{ .string = char_str });
                }
            } else {
                var it = std.mem.splitSequence(u8, str, separator);
                while (it.next()) |part| {
                    const part_str = try allocator.dupe(u8, part);
                    try arr.append(json.Value{ .string = part_str });
                }
            }
            return FilterResult{ .json_value = json.Value{ .array = arr } };
        }

        // Fall back to string-based apply for all other filters
        const result = try self.apply(allocator, value);
        return FilterResult{ .string = result };
    }
};

// Helper functions for filter implementation

fn escapeHtml(allocator: std.mem.Allocator, str: []const u8) ![]u8 {
    var result: std.ArrayList(u8) = .{};
    for (str) |c| {
        switch (c) {
            '&' => try result.appendSlice(allocator, "&amp;"),
            '<' => try result.appendSlice(allocator, "&lt;"),
            '>' => try result.appendSlice(allocator, "&gt;"),
            '"' => try result.appendSlice(allocator, "&quot;"),
            '\'' => try result.appendSlice(allocator, "&#39;"),
            else => try result.append(allocator, c),
        }
    }
    return try result.toOwnedSlice(allocator);
}

const MathOp = enum { plus, minus, times };

fn applyMathFilter(allocator: std.mem.Allocator, value: json.Value, args: [][]const u8, op: MathOp) ![]u8 {
    const num = try utils.valueToNumber(value);
    if (args.len == 0) {
        return try utils.valueToString(allocator, value);
    }
    const arg_num = try utils.stringToNumber(args[0]);
    const result = switch (op) {
        .plus => num + arg_num,
        .minus => num - arg_num,
        .times => num * arg_num,
    };
    const input_is_float = utils.valueIsFloat(value);
    const arg_is_float = utils.stringIsFloat(args[0]);
    if (input_is_float or arg_is_float) {
        return try utils.numberToStringForceFloat(allocator, result);
    }
    return try utils.numberToString(allocator, result);
}

fn applyDividedBy(allocator: std.mem.Allocator, value: json.Value, args: [][]const u8) ![]u8 {
    if (args.len == 0) {
        return try utils.valueToString(allocator, value);
    }

    const value_is_int = value == .integer;
    const arg_is_int = utils.isIntegerString(args[0]);

    if (value_is_int and arg_is_int) {
        const num_int = value.integer;
        const arg_int = std.fmt.parseInt(i64, args[0], 10) catch 0;
        if (arg_int == 0) {
            return try allocator.dupe(u8, "Liquid error (line 1): divided by 0");
        }
        const result = @divTrunc(num_int, arg_int);
        return try std.fmt.allocPrint(allocator, "{d}", .{result});
    } else {
        const num = try utils.valueToNumber(value);
        const arg_num = try utils.stringToNumber(args[0]);
        if (arg_num == 0) {
            if (num > 0) return try allocator.dupe(u8, "Infinity");
            if (num < 0) return try allocator.dupe(u8, "-Infinity");
            return try allocator.dupe(u8, "NaN");
        }
        return try utils.numberToString(allocator, num / arg_num);
    }
}

fn applyModulo(allocator: std.mem.Allocator, value: json.Value, args: [][]const u8) ![]u8 {
    const num = try utils.valueToNumber(value);
    if (args.len == 0) {
        return try utils.valueToString(allocator, value);
    }
    const arg_num = try utils.stringToNumber(args[0]);
    if (arg_num == 0) {
        return try allocator.dupe(u8, "Liquid error (line 1): divided by 0");
    }
    return try utils.numberToString(allocator, @rem(num, arg_num));
}

fn applyRound(allocator: std.mem.Allocator, value: json.Value, args: [][]const u8) ![]u8 {
    const num = try utils.valueToNumber(value);
    if (args.len > 0) {
        const precision_int = try std.fmt.parseInt(i32, args[0], 10);
        const precision: f64 = @floatFromInt(precision_int);
        const multiplier = std.math.pow(f64, 10.0, precision);
        const result = @round(num * multiplier) / multiplier;
        return try std.fmt.allocPrint(allocator, "{d}", .{result});
    } else {
        const result = @round(num);
        return try std.fmt.allocPrint(allocator, "{d}", .{@as(i64, @intFromFloat(result))});
    }
}

const RemoveMode = enum { all, first, last };

fn applyRemove(allocator: std.mem.Allocator, str: []const u8, args: [][]const u8, mode: RemoveMode) ![]u8 {
    if (args.len == 0) return try allocator.dupe(u8, str);
    const search = args[0];
    if (search.len == 0) return try allocator.dupe(u8, str);

    switch (mode) {
        .all => {
            var result: std.ArrayList(u8) = .{};
            var i: usize = 0;
            while (i < str.len) {
                if (i + search.len <= str.len and std.mem.eql(u8, str[i .. i + search.len], search)) {
                    i += search.len;
                } else {
                    try result.append(allocator, str[i]);
                    i += 1;
                }
            }
            return try result.toOwnedSlice(allocator);
        },
        .first => {
            if (std.mem.indexOf(u8, str, search)) |pos| {
                var result: std.ArrayList(u8) = .{};
                try result.appendSlice(allocator, str[0..pos]);
                try result.appendSlice(allocator, str[pos + search.len ..]);
                return try result.toOwnedSlice(allocator);
            }
            return try allocator.dupe(u8, str);
        },
        .last => {
            if (std.mem.lastIndexOf(u8, str, search)) |pos| {
                var result: std.ArrayList(u8) = .{};
                try result.appendSlice(allocator, str[0..pos]);
                try result.appendSlice(allocator, str[pos + search.len ..]);
                return try result.toOwnedSlice(allocator);
            }
            return try allocator.dupe(u8, str);
        },
    }
}

fn applyReplace(allocator: std.mem.Allocator, str: []const u8, args: [][]const u8, mode: RemoveMode) ![]u8 {
    if (args.len < 2) return try allocator.dupe(u8, str);
    const search = args[0];
    const replace_with = args[1];

    if (search.len == 0 and mode == .all) {
        // Empty search string inserts replacement between every character
        var result: std.ArrayList(u8) = .{};
        try result.appendSlice(allocator, replace_with);
        for (str) |c| {
            try result.append(allocator, c);
            try result.appendSlice(allocator, replace_with);
        }
        return try result.toOwnedSlice(allocator);
    }

    if (search.len == 0) return try allocator.dupe(u8, str);

    switch (mode) {
        .all => {
            var result: std.ArrayList(u8) = .{};
            var i: usize = 0;
            while (i < str.len) {
                if (i + search.len <= str.len and std.mem.eql(u8, str[i .. i + search.len], search)) {
                    try result.appendSlice(allocator, replace_with);
                    i += search.len;
                } else {
                    try result.append(allocator, str[i]);
                    i += 1;
                }
            }
            return try result.toOwnedSlice(allocator);
        },
        .first => {
            if (std.mem.indexOf(u8, str, search)) |pos| {
                var result: std.ArrayList(u8) = .{};
                try result.appendSlice(allocator, str[0..pos]);
                try result.appendSlice(allocator, replace_with);
                try result.appendSlice(allocator, str[pos + search.len ..]);
                return try result.toOwnedSlice(allocator);
            }
            return try allocator.dupe(u8, str);
        },
        .last => {
            if (std.mem.lastIndexOf(u8, str, search)) |pos| {
                var result: std.ArrayList(u8) = .{};
                try result.appendSlice(allocator, str[0..pos]);
                try result.appendSlice(allocator, replace_with);
                try result.appendSlice(allocator, str[pos + search.len ..]);
                return try result.toOwnedSlice(allocator);
            }
            return try allocator.dupe(u8, str);
        },
    }
}

fn applySlice(allocator: std.mem.Allocator, str: []const u8, args: [][]const u8) ![]u8 {
    if (args.len == 0) return try allocator.dupe(u8, str);

    const start_int = std.fmt.parseInt(i64, args[0], 10) catch {
        return try std.fmt.allocPrint(allocator, "Liquid error (line 1): invalid integer", .{});
    };
    const start: usize = if (start_int < 0)
        if (@as(i64, @intCast(str.len)) + start_int < 0) 0 else @intCast(@as(i64, @intCast(str.len)) + start_int)
    else
        @intCast(@min(start_int, @as(i64, @intCast(str.len))));

    if (start >= str.len) return try allocator.dupe(u8, "");

    if (args.len > 1) {
        const len_i64 = std.fmt.parseInt(i64, args[1], 10) catch {
            return try std.fmt.allocPrint(allocator, "Liquid error (line 1): invalid integer", .{});
        };
        if (len_i64 < 0) return try allocator.dupe(u8, "");
        const len: usize = @intCast(len_i64);
        const end = @min(start + len, str.len);
        return try allocator.dupe(u8, str[start..end]);
    } else {
        const end = @min(start + 1, str.len);
        return try allocator.dupe(u8, str[start..end]);
    }
}

fn applySplit(allocator: std.mem.Allocator, str: []const u8, args: [][]const u8) ![]u8 {
    if (args.len == 0) {
        return try std.fmt.allocPrint(allocator, "[\"{s}\"]", .{str});
    }
    const delimiter = args[0];

    if (delimiter.len == 0) {
        var result: std.ArrayList(u8) = .{};
        try result.append(allocator, '[');
        var first = true;
        for (str) |c| {
            if (!first) try result.appendSlice(allocator, ", ");
            first = false;
            try result.append(allocator, '"');
            try result.append(allocator, c);
            try result.append(allocator, '"');
        }
        try result.append(allocator, ']');
        return try result.toOwnedSlice(allocator);
    }

    var result: std.ArrayList(u8) = .{};
    try result.append(allocator, '[');
    var iter = std.mem.splitSequence(u8, str, delimiter);
    var first = true;
    while (iter.next()) |part| {
        if (!first) try result.appendSlice(allocator, ", ");
        first = false;
        try result.append(allocator, '"');
        try result.appendSlice(allocator, part);
        try result.append(allocator, '"');
    }
    try result.append(allocator, ']');
    return try result.toOwnedSlice(allocator);
}

fn applyTruncate(allocator: std.mem.Allocator, str: []const u8, args: [][]const u8) ![]u8 {
    if (args.len == 0) return try allocator.dupe(u8, str);

    const ellipsis = if (args.len > 1) args[1] else "...";

    // Parse max_len, handling overflow and negative numbers gracefully
    const max_len: usize = blk: {
        // First try parsing as signed to detect negative numbers
        if (std.fmt.parseInt(i64, args[0], 10)) |signed_val| {
            if (signed_val < 0) {
                // Negative length - return just ellipsis
                break :blk 0;
            }
            break :blk @intCast(signed_val);
        } else |_| {
            // Try parsing as usize directly (might be very large positive)
            if (std.fmt.parseInt(usize, args[0], 10)) |val| {
                break :blk val;
            } else |_| {
                // Overflow or invalid - return string as-is
                return try allocator.dupe(u8, str);
            }
        }
    };

    if (str.len <= max_len) return try allocator.dupe(u8, str);
    if (max_len <= ellipsis.len) return try allocator.dupe(u8, ellipsis);

    const text_len = max_len - ellipsis.len;
    return try std.mem.concat(allocator, u8, &[_][]const u8{ str[0..text_len], ellipsis });
}

fn applyTruncateWords(allocator: std.mem.Allocator, str: []const u8, args: [][]const u8) ![]u8 {
    if (args.len == 0) return try allocator.dupe(u8, str);

    const ellipsis = if (args.len > 1) args[1] else "...";

    // Parse max_words, handling overflow and negative numbers gracefully
    const max_words: usize = blk: {
        // First try parsing as signed to detect negative numbers
        if (std.fmt.parseInt(i64, args[0], 10)) |signed_val| {
            if (signed_val < 0) {
                // Negative word count - use 1 word
                break :blk 1;
            }
            break :blk @intCast(signed_val);
        } else |_| {
            // Try parsing as usize directly (might be very large positive)
            if (std.fmt.parseInt(usize, args[0], 10)) |val| {
                break :blk val;
            } else |_| {
                // Overflow or invalid - return string as-is
                return try allocator.dupe(u8, str);
            }
        }
    };

    var word_count: usize = 0;
    var i: usize = 0;
    var in_word = false;
    var last_word_end: usize = 0;

    while (i < str.len) : (i += 1) {
        const is_space = str[i] == ' ' or str[i] == '\t' or str[i] == '\n' or str[i] == '\r';
        if (!is_space and !in_word) {
            in_word = true;
            word_count += 1;
            if (word_count > max_words) {
                return try std.mem.concat(allocator, u8, &[_][]const u8{ str[0..last_word_end], ellipsis });
            }
        } else if (is_space and in_word) {
            in_word = false;
            last_word_end = i;
        }
    }

    return try allocator.dupe(u8, str);
}

fn applyUrlEncode(allocator: std.mem.Allocator, str: []const u8) ![]u8 {
    var result: std.ArrayList(u8) = .{};
    for (str) |c| {
        if (std.ascii.isAlphanumeric(c) or c == '-' or c == '_' or c == '.' or c == '~') {
            try result.append(allocator, c);
        } else if (c == ' ') {
            try result.append(allocator, '+');
        } else {
            const encoded = try std.fmt.allocPrint(allocator, "%{X:0>2}", .{c});
            defer allocator.free(encoded);
            try result.appendSlice(allocator, encoded);
        }
    }
    return try result.toOwnedSlice(allocator);
}

fn applyUrlDecode(allocator: std.mem.Allocator, str: []const u8) ![]u8 {
    var result: std.ArrayList(u8) = .{};
    var i: usize = 0;
    while (i < str.len) {
        if (str[i] == '%' and i + 2 < str.len) {
            const hex = str[i + 1 .. i + 3];
            if (std.fmt.parseInt(u8, hex, 16)) |byte| {
                try result.append(allocator, byte);
                i += 3;
            } else |_| {
                try result.append(allocator, str[i]);
                i += 1;
            }
        } else if (str[i] == '+') {
            try result.append(allocator, ' ');
            i += 1;
        } else {
            try result.append(allocator, str[i]);
            i += 1;
        }
    }
    return try result.toOwnedSlice(allocator);
}

fn applyNewlineToBr(allocator: std.mem.Allocator, str: []const u8) ![]u8 {
    var result: std.ArrayList(u8) = .{};
    for (str) |c| {
        if (c == '\n') {
            try result.appendSlice(allocator, "<br />\n");
        } else {
            try result.append(allocator, c);
        }
    }
    return try result.toOwnedSlice(allocator);
}

fn applyStripHtml(allocator: std.mem.Allocator, str: []const u8) ![]u8 {
    var result: std.ArrayList(u8) = .{};
    var in_tag = false;
    for (str) |c| {
        if (c == '<') {
            in_tag = true;
        } else if (c == '>') {
            in_tag = false;
        } else if (!in_tag) {
            try result.append(allocator, c);
        }
    }
    return try result.toOwnedSlice(allocator);
}

fn applyStripNewlines(allocator: std.mem.Allocator, str: []const u8) ![]u8 {
    var result: std.ArrayList(u8) = .{};
    for (str) |c| {
        if (c != '\n' and c != '\r') {
            try result.append(allocator, c);
        }
    }
    return try result.toOwnedSlice(allocator);
}

fn applyEscapeOnce(allocator: std.mem.Allocator, str: []const u8) ![]u8 {
    var result: std.ArrayList(u8) = .{};
    var i: usize = 0;
    while (i < str.len) {
        if (str[i] == '&') {
            // Check for already-escaped entities
            if (i + 5 <= str.len and std.mem.eql(u8, str[i .. i + 5], "&amp;")) {
                try result.appendSlice(allocator, "&amp;");
                i += 5;
            } else if (i + 4 <= str.len and std.mem.eql(u8, str[i .. i + 4], "&lt;")) {
                try result.appendSlice(allocator, "&lt;");
                i += 4;
            } else if (i + 4 <= str.len and std.mem.eql(u8, str[i .. i + 4], "&gt;")) {
                try result.appendSlice(allocator, "&gt;");
                i += 4;
            } else if (i + 6 <= str.len and std.mem.eql(u8, str[i .. i + 6], "&quot;")) {
                try result.appendSlice(allocator, "&quot;");
                i += 6;
            } else if (i + 5 <= str.len and std.mem.eql(u8, str[i .. i + 5], "&#39;")) {
                try result.appendSlice(allocator, "&#39;");
                i += 5;
            } else {
                try result.appendSlice(allocator, "&amp;");
                i += 1;
            }
        } else {
            switch (str[i]) {
                '<' => try result.appendSlice(allocator, "&lt;"),
                '>' => try result.appendSlice(allocator, "&gt;"),
                '"' => try result.appendSlice(allocator, "&quot;"),
                '\'' => try result.appendSlice(allocator, "&#39;"),
                else => try result.append(allocator, str[i]),
            }
            i += 1;
        }
    }
    return try result.toOwnedSlice(allocator);
}

/// Clone an array of filters with proper deep copy
pub fn cloneFilters(allocator: std.mem.Allocator, filters: []const Filter) ![]Filter {
    if (filters.len == 0) return &.{};

    var cloned: std.ArrayList(Filter) = .{};
    errdefer {
        for (cloned.items) |*f| {
            f.deinit(allocator);
        }
        cloned.deinit(allocator);
    }

    for (filters) |*f| {
        try cloned.append(allocator, try f.clone(allocator));
    }

    return try cloned.toOwnedSlice(allocator);
}
