const std = @import("std");
const json = std.json;
const Liquid = @import("liquid.zig");

// Parse ISO8601 timestamp to unix timestamp
fn parseIso8601Timestamp(str: []const u8) ?i64 {
    // Format: 2024-01-01T00:01:58+00:00 or 2024-01-01T00:01:58Z
    if (str.len < 19) return null;

    const year = std.fmt.parseInt(i32, str[0..4], 10) catch return null;
    if (str[4] != '-') return null;
    const month = std.fmt.parseInt(u8, str[5..7], 10) catch return null;
    if (str[7] != '-') return null;
    const day = std.fmt.parseInt(u8, str[8..10], 10) catch return null;
    if (str[10] != 'T' and str[10] != ' ') return null;
    const hour = std.fmt.parseInt(u8, str[11..13], 10) catch return null;
    if (str[13] != ':') return null;
    const minute = std.fmt.parseInt(u8, str[14..16], 10) catch return null;
    if (str[16] != ':') return null;
    const second = std.fmt.parseInt(u8, str[17..19], 10) catch return null;

    // Convert to unix timestamp (simplified)
    const epoch_days = epochDaysFromYmd(year, month, day);
    return epoch_days * 86400 + @as(i64, hour) * 3600 + @as(i64, minute) * 60 + @as(i64, second);
}

fn epochDaysFromYmd(year: i32, month: u8, day: u8) i64 {
    var y = @as(i64, year);
    var m = @as(i64, month);
    if (m <= 2) {
        y -= 1;
        m += 12;
    }
    const era: i64 = @divFloor(if (y >= 0) y else y - 399, 400);
    const yoe: i64 = y - era * 400;
    const doy: i64 = @divFloor(153 * (m - 3) + 2, 5) + @as(i64, day) - 1;
    const doe: i64 = yoe * 365 + @divFloor(yoe, 4) - @divFloor(yoe, 100) + doy;
    return era * 146097 + doe - 719468;
}

pub const Handler = struct {
    allocator: std.mem.Allocator,
    liquid: *Liquid.Engine,
    initialized: bool,

    pub fn init(allocator: std.mem.Allocator, liquid: *Liquid.Engine) Handler {
        return .{
            .allocator = allocator,
            .liquid = liquid,
            .initialized = false,
        };
    }

    pub fn deinit(self: *Handler) void {
        _ = self;
    }

    fn writeJsonValue(writer: anytype, value: json.Value) !void {
        switch (value) {
            .null => try writer.writeAll("null"),
            .bool => |b| try writer.writeAll(if (b) "true" else "false"),
            .integer => |i| try writer.print("{d}", .{i}),
            .float => |f| try writer.print("{d}", .{f}),
            .number_string => |s| try writer.writeAll(s),
            .string => |s| {
                try writeJsonString(writer, s);
            },
            .array => |arr| {
                try writer.writeByte('[');
                for (arr.items, 0..) |item, i| {
                    if (i > 0) try writer.writeByte(',');
                    try writeJsonValue(writer, item);
                }
                try writer.writeByte(']');
            },
            .object => |obj| {
                try writer.writeByte('{');
                var iter = obj.iterator();
                var first = true;
                while (iter.next()) |entry| {
                    if (!first) try writer.writeByte(',');
                    first = false;
                    try writer.writeByte('"');
                    try writer.writeAll(entry.key_ptr.*);
                    try writer.writeAll("\":");
                    try writeJsonValue(writer, entry.value_ptr.*);
                }
                try writer.writeByte('}');
            },
        }
    }

    fn writeJsonString(writer: anytype, s: []const u8) !void {
        try writer.writeByte('"');
        for (s) |c| {
            switch (c) {
                '"' => try writer.writeAll("\\\""),
                '\\' => try writer.writeAll("\\\\"),
                '\n' => try writer.writeAll("\\n"),
                '\r' => try writer.writeAll("\\r"),
                '\t' => try writer.writeAll("\\t"),
                0x08 => try writer.writeAll("\\b"), // backspace
                0x0C => try writer.writeAll("\\f"), // form feed
                0x00...0x07, 0x0B, 0x0E...0x1F => {
                    // Other control characters: use \uXXXX
                    try writer.print("\\u{x:0>4}", .{c});
                },
                else => try writer.writeByte(c),
            }
        }
        try writer.writeByte('"');
    }

    pub fn handleRequest(self: *Handler, request_json: []const u8) !?[]const u8 {
        const parsed = json.parseFromSlice(json.Value, self.allocator, request_json, .{}) catch {
            return try self.errorResponse(null, -32700, "Parse error", "error");
        };
        defer parsed.deinit();

        const root = parsed.value;
        if (root != .object) {
            return try self.errorResponse(null, -32600, "Invalid Request", "error");
        }

        const obj = root.object;

        const method_value = obj.get("method") orelse {
            return try self.errorResponse(null, -32600, "Invalid Request", "error");
        };

        if (method_value != .string) {
            return try self.errorResponse(null, -32600, "Invalid Request", "error");
        }

        const method = method_value.string;
        const id = obj.get("id");

        if (std.mem.eql(u8, method, "initialize")) {
            return try self.handleInitialize(id);
        } else if (std.mem.eql(u8, method, "compile")) {
            return try self.handleCompile(id, obj);
        } else if (std.mem.eql(u8, method, "render")) {
            return try self.handleRender(id, obj);
        } else if (std.mem.eql(u8, method, "quit")) {
            std.process.exit(0);
        } else {
            return try self.errorResponse(id, -32601, "Method not found", "error");
        }
    }

    fn handleInitialize(self: *Handler, id: ?json.Value) ![]const u8 {
        self.initialized = true;

        var output: std.ArrayList(u8) = .{};
        errdefer output.deinit(self.allocator);

        var writer = output.writer(self.allocator);
        try writer.writeAll("{\"jsonrpc\":\"2.0\",\"id\":");
        if (id) |id_val| {
            try writeJsonValue(writer, id_val);
        } else {
            try writer.writeAll("null");
        }
        try writer.writeAll(",\"result\":{\"version\":\"1.0\",\"features\":[\"inline_errors\",\"runtime_drops\"]}}");

        return try output.toOwnedSlice(self.allocator);
    }

    fn handleCompile(self: *Handler, id: ?json.Value, obj: json.ObjectMap) ![]const u8 {
        const params = obj.get("params") orelse {
            return try self.errorResponse(id, -32600, "Invalid params", "error");
        };

        if (params != .object) {
            return try self.errorResponse(id, -32600, "Invalid params", "error");
        }

        const template_value = params.object.get("template") orelse {
            return try self.errorResponse(id, -32600, "Invalid params", "error");
        };

        if (template_value != .string) {
            return try self.errorResponse(id, -32600, "Invalid params", "error");
        }

        const template = template_value.string;

        // Extract filesystem from params (optional)
        const filesystem: ?json.ObjectMap = if (params.object.get("filesystem")) |fs|
            if (fs == .object) fs.object else null
        else
            null;

        // Extract error_mode from params or params.options (optional, defaults to lax)
        const error_mode: Liquid.ErrorMode = blk: {
            // First check params.error_mode
            if (params.object.get("error_mode")) |em| {
                if (em == .string and std.mem.eql(u8, em.string, "strict")) {
                    break :blk .strict;
                }
            }
            // Then check params.options.error_mode
            if (params.object.get("options")) |opts| {
                if (opts == .object) {
                    if (opts.object.get("error_mode")) |em| {
                        if (em == .string and std.mem.eql(u8, em.string, "strict")) {
                            break :blk .strict;
                        }
                    }
                }
            }
            break :blk .lax;
        };

        const template_id = self.liquid.compile(template, filesystem, error_mode) catch |err| {
            const err_msg = switch (err) {
                error.UnknownTag => try std.fmt.allocPrint(self.allocator, "Unknown tag", .{}),
                error.UnclosedTag => try std.fmt.allocPrint(self.allocator, "'if' tag was never closed", .{}),
                error.InvalidRender => try std.fmt.allocPrint(self.allocator, "syntax error: Template name must be a quoted string", .{}),
                else => try std.fmt.allocPrint(self.allocator, "Parse error: {}", .{err}),
            };
            defer self.allocator.free(err_msg);
            return try self.errorResponse(id, -32000, err_msg, "parse_error");
        };

        var output: std.ArrayList(u8) = .{};
        errdefer output.deinit(self.allocator);

        var writer = output.writer(self.allocator);
        try writer.writeAll("{\"jsonrpc\":\"2.0\",\"id\":");
        if (id) |id_val| {
            try writeJsonValue(writer, id_val);
        } else {
            try writer.writeAll("null");
        }
        try writer.print(",\"result\":{{\"template_id\":\"{d}\"}}}}", .{template_id});

        return try output.toOwnedSlice(self.allocator);
    }

    fn handleRender(self: *Handler, id: ?json.Value, obj: json.ObjectMap) ![]const u8 {
        const params = obj.get("params") orelse {
            return try self.errorResponse(id, -32600, "Invalid params", "error");
        };

        if (params != .object) {
            return try self.errorResponse(id, -32600, "Invalid params", "error");
        }

        const template_id_value = params.object.get("template_id") orelse {
            return try self.errorResponse(id, -32600, "Invalid params", "error");
        };

        if (template_id_value != .string) {
            return try self.errorResponse(id, -32600, "Invalid params", "error");
        }

        const template_id_str = template_id_value.string;
        const template_id = std.fmt.parseInt(usize, template_id_str, 10) catch {
            return try self.errorResponse(id, -32600, "Invalid template_id", "error");
        };

        const environment = params.object.get("environment");

        // Extract frozen_time if present (ISO8601 format)
        const frozen_time: ?i64 = if (params.object.get("frozen_time")) |ft|
            if (ft == .string) parseIso8601Timestamp(ft.string) else null
        else
            null;

        const rendered_output = self.liquid.render(template_id, environment, frozen_time) catch |err| {
            const err_msg = try std.fmt.allocPrint(self.allocator, "Render error: {}", .{err});
            defer self.allocator.free(err_msg);
            return try self.errorResponse(id, -32001, err_msg, "render_error");
        };
        defer self.allocator.free(rendered_output);

        var output: std.ArrayList(u8) = .{};
        errdefer output.deinit(self.allocator);

        var writer = output.writer(self.allocator);
        try writer.writeAll("{\"jsonrpc\":\"2.0\",\"id\":");
        if (id) |id_val| {
            try writeJsonValue(writer, id_val);
        } else {
            try writer.writeAll("null");
        }
        try writer.writeAll(",\"result\":{\"output\":");
        try writeJsonString(writer, rendered_output);
        try writer.writeAll("}}");

        return try output.toOwnedSlice(self.allocator);
    }

    fn errorResponse(self: *Handler, id: ?json.Value, code: i32, message: []const u8, error_type: []const u8) ![]const u8 {
        var output: std.ArrayList(u8) = .{};
        errdefer output.deinit(self.allocator);

        var writer = output.writer(self.allocator);
        try writer.writeAll("{\"jsonrpc\":\"2.0\",\"id\":");
        if (id) |id_val| {
            try writeJsonValue(writer, id_val);
        } else {
            try writer.writeAll("null");
        }
        try writer.print(",\"error\":{{\"code\":{d},\"message\":", .{code});
        try writeJsonString(writer, message);
        try writer.writeAll(",\"data\":{\"type\":\"");
        try writer.writeAll(error_type);
        try writer.writeAll("\",\"message\":");
        try writeJsonString(writer, message);
        try writer.writeAll("}}}");

        return try output.toOwnedSlice(self.allocator);
    }
};
