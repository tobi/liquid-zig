const std = @import("std");
const json = std.json;
const Liquid = @import("liquid.zig");

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
                try writer.writeByte('"');
                for (s) |c| {
                    switch (c) {
                        '"' => try writer.writeAll("\\\""),
                        '\\' => try writer.writeAll("\\\\"),
                        '\n' => try writer.writeAll("\\n"),
                        '\r' => try writer.writeAll("\\r"),
                        '\t' => try writer.writeAll("\\t"),
                        else => try writer.writeByte(c),
                    }
                }
                try writer.writeByte('"');
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
                else => try writer.writeByte(c),
            }
        }
        try writer.writeByte('"');
    }

    pub fn handleRequest(self: *Handler, request_json: []const u8) !?[]const u8 {
        const parsed = json.parseFromSlice(json.Value, self.allocator, request_json, .{}) catch |err| {
            return try self.errorResponse(null, -32700, "Parse error", err);
        };
        defer parsed.deinit();

        const root = parsed.value;
        if (root != .object) {
            return try self.errorResponse(null, -32600, "Invalid Request", error.NotAnObject);
        }

        const obj = root.object;

        const method_value = obj.get("method") orelse {
            return try self.errorResponse(null, -32600, "Invalid Request", error.NoMethod);
        };

        if (method_value != .string) {
            return try self.errorResponse(null, -32600, "Invalid Request", error.MethodNotString);
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
            return try self.errorResponse(id, -32601, "Method not found", error.UnknownMethod);
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
        try writer.writeAll(",\"result\":{\"version\":\"1.0\",\"features\":{}}}");

        return try output.toOwnedSlice(self.allocator);
    }

    fn handleCompile(self: *Handler, id: ?json.Value, obj: json.ObjectMap) ![]const u8 {
        const params = obj.get("params") orelse {
            return try self.errorResponse(id, -32600, "Invalid params", error.NoParams);
        };

        if (params != .object) {
            return try self.errorResponse(id, -32600, "Invalid params", error.ParamsNotObject);
        }

        const template_value = params.object.get("template") orelse {
            return try self.errorResponse(id, -32600, "Invalid params", error.NoTemplate);
        };

        if (template_value != .string) {
            return try self.errorResponse(id, -32600, "Invalid params", error.TemplateNotString);
        }

        const template = template_value.string;

        const template_id = self.liquid.compile(template) catch |err| {
            const err_msg = try std.fmt.allocPrint(self.allocator, "Parse error: {}", .{err});
            defer self.allocator.free(err_msg);
            return try self.errorResponse(id, -32000, err_msg, err);
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
            return try self.errorResponse(id, -32600, "Invalid params", error.NoParams);
        };

        if (params != .object) {
            return try self.errorResponse(id, -32600, "Invalid params", error.ParamsNotObject);
        }

        const template_id_value = params.object.get("template_id") orelse {
            return try self.errorResponse(id, -32600, "Invalid params", error.NoTemplateId);
        };

        if (template_id_value != .string) {
            return try self.errorResponse(id, -32600, "Invalid params", error.TemplateIdNotString);
        }

        const template_id_str = template_id_value.string;
        const template_id = std.fmt.parseInt(usize, template_id_str, 10) catch {
            return try self.errorResponse(id, -32600, "Invalid template_id", error.InvalidTemplateId);
        };

        const environment = params.object.get("environment");

        const rendered_output = self.liquid.render(template_id, environment) catch |err| {
            const err_msg = try std.fmt.allocPrint(self.allocator, "Render error: {}", .{err});
            defer self.allocator.free(err_msg);
            return try self.errorResponse(id, -32001, err_msg, err);
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

    fn errorResponse(self: *Handler, id: ?json.Value, code: i32, message: []const u8, _: anyerror) ![]const u8 {
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
        try writer.writeAll("}}");

        return try output.toOwnedSlice(self.allocator);
    }
};
