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

        const response_obj = .{
            .jsonrpc = "2.0",
            .id = id,
            .result = .{
                .version = "1.0",
                .features = .{},
            },
        };

        return try std.fmt.allocPrint(self.allocator, "{any}", .{std.json.fmt(response_obj, .{})});
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

        const id_str = try std.fmt.allocPrint(self.allocator, "{d}", .{template_id});
        defer self.allocator.free(id_str);

        const response_obj = .{
            .jsonrpc = "2.0",
            .id = id,
            .result = .{
                .template_id = id_str,
            },
        };

        return try std.fmt.allocPrint(self.allocator, "{any}", .{std.json.fmt(response_obj, .{})});
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

        const output = self.liquid.render(template_id, environment) catch |err| {
            const err_msg = try std.fmt.allocPrint(self.allocator, "Render error: {}", .{err});
            defer self.allocator.free(err_msg);
            return try self.errorResponse(id, -32001, err_msg, err);
        };
        defer self.allocator.free(output);

        const response_obj = .{
            .jsonrpc = "2.0",
            .id = id,
            .result = .{
                .output = output,
            },
        };

        return try std.fmt.allocPrint(self.allocator, "{any}", .{std.json.fmt(response_obj, .{})});
    }

    fn errorResponse(self: *Handler, id: ?json.Value, code: i32, message: []const u8, _: anyerror) ![]const u8 {
        const response_obj = .{
            .jsonrpc = "2.0",
            .id = id,
            .@"error" = .{
                .code = code,
                .message = message,
            },
        };

        return try std.fmt.allocPrint(self.allocator, "{any}", .{std.json.fmt(response_obj, .{})});
    }
};
