const std = @import("std");
const json = std.json;

pub const Engine = struct {
    allocator: std.mem.Allocator,
    templates: std.ArrayList(Template),
    next_id: usize,

    pub fn init(allocator: std.mem.Allocator) Engine {
        return .{
            .allocator = allocator,
            .templates = .{},
            .next_id = 0,
        };
    }

    pub fn deinit(self: *Engine) void {
        for (self.templates.items) |*template| {
            template.deinit();
        }
        self.templates.deinit(self.allocator);
    }

    pub fn compile(self: *Engine, source: []const u8) !usize {
        var template = try Template.parse(self.allocator, source);
        errdefer template.deinit();

        try self.templates.append(self.allocator, template);
        const id = self.next_id;
        self.next_id += 1;
        return id;
    }

    pub fn render(self: *Engine, template_id: usize, environment: ?json.Value) ![]const u8 {
        if (template_id >= self.templates.items.len) {
            return error.TemplateNotFound;
        }

        const template = &self.templates.items[template_id];
        return try template.render(self.allocator, environment);
    }
};

const Template = struct {
    allocator: std.mem.Allocator,
    nodes: std.ArrayList(Node),

    pub fn parse(allocator: std.mem.Allocator, source: []const u8) !Template {
        var nodes: std.ArrayList(Node) = .{};
        errdefer {
            for (nodes.items) |*node| {
                node.deinit(allocator);
            }
            nodes.deinit(allocator);
        }

        var lexer = Lexer.init(source);
        while (try lexer.next(allocator)) |token| {
            defer token.deinit(allocator);

            const node = try Node.fromToken(allocator, token);
            try nodes.append(allocator, node);
        }

        return Template{
            .allocator = allocator,
            .nodes = nodes,
        };
    }

    pub fn deinit(self: *Template) void {
        for (self.nodes.items) |*node| {
            node.deinit(self.allocator);
        }
        self.nodes.deinit(self.allocator);
    }

    pub fn render(self: *Template, allocator: std.mem.Allocator, environment: ?json.Value) ![]const u8 {
        var ctx = Context.init(allocator, environment);
        defer ctx.deinit();

        var output: std.ArrayList(u8) = .{};
        errdefer output.deinit(allocator);

        for (self.nodes.items) |*node| {
            try node.render(&ctx, &output, allocator);
        }

        return try output.toOwnedSlice(allocator);
    }
};

const Context = struct {
    allocator: std.mem.Allocator,
    environment: ?json.Value,
    variables: std.StringHashMap(json.Value),

    pub fn init(allocator: std.mem.Allocator, environment: ?json.Value) Context {
        return .{
            .allocator = allocator,
            .environment = environment,
            .variables = std.StringHashMap(json.Value).init(allocator),
        };
    }

    pub fn deinit(self: *Context) void {
        self.variables.deinit();
    }

    pub fn get(self: *Context, key: []const u8) ?json.Value {
        // Check local variables first
        if (self.variables.get(key)) |value| {
            return value;
        }

        // Check environment
        if (self.environment) |env| {
            if (env == .object) {
                return env.object.get(key);
            }
        }

        return null;
    }

    pub fn set(self: *Context, key: []const u8, value: json.Value) !void {
        try self.variables.put(key, value);
    }
};

const Node = union(enum) {
    text: []const u8,
    variable: Variable,
    tag: Tag,

    pub fn fromToken(allocator: std.mem.Allocator, token: Token) !Node {
        return switch (token.kind) {
            .text => Node{ .text = try allocator.dupe(u8, token.content) },
            .variable => Node{ .variable = try Variable.parse(allocator, token.content) },
            .tag => Node{ .tag = try Tag.parse(allocator, token.content) },
        };
    }

    pub fn deinit(self: *Node, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .text => |t| allocator.free(t),
            .variable => |*v| v.deinit(allocator),
            .tag => |*t| t.deinit(allocator),
        }
    }

    pub fn render(self: *Node, ctx: *Context, output: *std.ArrayList(u8), allocator: std.mem.Allocator) !void {
        switch (self.*) {
            .text => |t| try output.appendSlice(allocator, t),
            .variable => |*v| try v.render(ctx, output, allocator),
            .tag => |*t| try t.render(ctx, output, allocator),
        }
    }
};

const Variable = struct {
    path: []const u8,
    filters: []Filter,

    pub fn parse(allocator: std.mem.Allocator, content: []const u8) !Variable {
        const trimmed = std.mem.trim(u8, content, " \t\n\r");

        var parts = std.mem.splitScalar(u8, trimmed, '|');
        const path = std.mem.trim(u8, parts.first(), " \t\n\r");

        var filters: std.ArrayList(Filter) = .{};
        errdefer {
            for (filters.items) |*f| {
                f.deinit(allocator);
            }
            filters.deinit(allocator);
        }

        while (parts.next()) |filter_str| {
            const filter = try Filter.parse(allocator, filter_str);
            try filters.append(allocator, filter);
        }

        return Variable{
            .path = try allocator.dupe(u8, path),
            .filters = try filters.toOwnedSlice(allocator),
        };
    }

    pub fn deinit(self: *Variable, allocator: std.mem.Allocator) void {
        allocator.free(self.path);
        for (self.filters) |*f| {
            f.deinit(allocator);
        }
        allocator.free(self.filters);
    }

    pub fn render(self: *Variable, ctx: *Context, output: *std.ArrayList(u8), allocator: std.mem.Allocator) !void {
        const value = ctx.get(self.path);
        if (value == null) {
            return;
        }

        var current_value = value.?;
        var temp_strings: std.ArrayList([]u8) = .{};
        defer {
            for (temp_strings.items) |s| {
                ctx.allocator.free(s);
            }
            temp_strings.deinit(ctx.allocator);
        }

        for (self.filters) |*filter| {
            const result = try filter.apply(ctx.allocator, current_value);
            try temp_strings.append(ctx.allocator, result);
            current_value = json.Value{ .string = result };
        }

        try renderValue(current_value, output, allocator);
    }
};

const Filter = struct {
    name: []const u8,
    args: [][]const u8,

    pub fn parse(allocator: std.mem.Allocator, content: []const u8) !Filter {
        const trimmed = std.mem.trim(u8, content, " \t\n\r");

        var parts = std.mem.splitScalar(u8, trimmed, ':');
        const name = std.mem.trim(u8, parts.first(), " \t\n\r");

        var args: std.ArrayList([]const u8) = .{};
        errdefer args.deinit(allocator);

        if (parts.next()) |args_str| {
            var arg_parts = std.mem.splitScalar(u8, args_str, ',');
            while (arg_parts.next()) |arg| {
                const trimmed_arg = std.mem.trim(u8, arg, " \t\n\r\"'");
                try args.append(allocator, try allocator.dupe(u8, trimmed_arg));
            }
        }

        return Filter{
            .name = try allocator.dupe(u8, name),
            .args = try args.toOwnedSlice(allocator),
        };
    }

    pub fn deinit(self: *Filter, allocator: std.mem.Allocator) void {
        allocator.free(self.name);
        for (self.args) |arg| {
            allocator.free(arg);
        }
        allocator.free(self.args);
    }

    pub fn apply(self: *Filter, allocator: std.mem.Allocator, value: json.Value) ![]u8 {
        const str = try valueToString(allocator, value);

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
            const size_str = try std.fmt.allocPrint(allocator, "{d}", .{str.len});
            return size_str;
        } else if (std.mem.eql(u8, self.name, "reverse")) {
            const result = try allocator.dupe(u8, str);
            std.mem.reverse(u8, result);
            return result;
        } else if (std.mem.eql(u8, self.name, "escape")) {
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
        } else if (std.mem.eql(u8, self.name, "default")) {
            if (str.len == 0 and self.args.len > 0) {
                return try allocator.dupe(u8, self.args[0]);
            }
            return try allocator.dupe(u8, str);
        } else {
            // Unknown filter, return as-is
            return try allocator.dupe(u8, str);
        }
    }
};

const Tag = struct {
    tag_type: TagType,
    content: []const u8,
    children: []Node,
    else_children: []Node,

    const TagType = enum {
        assign,
        if_tag,
        unless,
        for_tag,
        comment,
        raw,
        capture,
        unknown,
    };

    pub fn parse(allocator: std.mem.Allocator, content: []const u8) !Tag {
        const trimmed = std.mem.trim(u8, content, " \t\n\r");

        // For now, just store the content without full tag parsing
        // This is a simplified implementation
        if (std.mem.startsWith(u8, trimmed, "assign ")) {
            return Tag{
                .tag_type = .assign,
                .content = try allocator.dupe(u8, trimmed),
                .children = &.{},
                .else_children = &.{},
            };
        } else if (std.mem.startsWith(u8, trimmed, "if ")) {
            return Tag{
                .tag_type = .if_tag,
                .content = try allocator.dupe(u8, trimmed),
                .children = &.{},
                .else_children = &.{},
            };
        } else if (std.mem.startsWith(u8, trimmed, "comment")) {
            return Tag{
                .tag_type = .comment,
                .content = try allocator.dupe(u8, trimmed),
                .children = &.{},
                .else_children = &.{},
            };
        } else {
            return Tag{
                .tag_type = .unknown,
                .content = try allocator.dupe(u8, trimmed),
                .children = &.{},
                .else_children = &.{},
            };
        }
    }

    pub fn deinit(self: *Tag, allocator: std.mem.Allocator) void {
        allocator.free(self.content);
        for (self.children) |*child| {
            child.deinit(allocator);
        }
        allocator.free(self.children);
        for (self.else_children) |*child| {
            child.deinit(allocator);
        }
        allocator.free(self.else_children);
    }

    pub fn render(self: *Tag, ctx: *Context, output: *std.ArrayList(u8), allocator: std.mem.Allocator) !void {
        _ = allocator;
        switch (self.tag_type) {
            .assign => try self.renderAssign(ctx),
            .if_tag => try self.renderIf(ctx, output),
            .comment => {}, // Comments render nothing
            else => {},
        }
    }

    fn renderAssign(self: *Tag, ctx: *Context) !void {
        // Parse: assign variable = value
        const content = std.mem.trim(u8, self.content[6..], " \t\n\r"); // Skip "assign"

        var parts = std.mem.splitScalar(u8, content, '=');
        const var_name = std.mem.trim(u8, parts.first(), " \t\n\r");
        const value_str = if (parts.next()) |v| std.mem.trim(u8, v, " \t\n\r\"'") else "";

        // Store as a string value
        const value = json.Value{ .string = value_str };
        try ctx.set(var_name, value);
    }

    fn renderIf(_: *Tag, _: *Context, _: *std.ArrayList(u8)) !void {
        // Simplified: if tags are not fully implemented in this basic version
    }
};

const SourceLocation = struct {
    line: usize,
    column: usize,
    offset: usize,
};

const Token = struct {
    kind: TokenKind,
    content: []const u8,
    location: SourceLocation,
    strip_left: bool,
    strip_right: bool,

    const TokenKind = enum {
        text,
        variable,
        tag,
    };

    pub fn deinit(self: Token, allocator: std.mem.Allocator) void {
        allocator.free(self.content);
    }
};

const Lexer = struct {
    source: []const u8,
    pos: usize,
    line: usize,
    column: usize,

    pub fn init(source: []const u8) Lexer {
        return .{
            .source = source,
            .pos = 0,
            .line = 1,
            .column = 1,
        };
    }

    fn getCurrentLocation(self: *Lexer) SourceLocation {
        return .{
            .line = self.line,
            .column = self.column,
            .offset = self.pos,
        };
    }

    fn advance(self: *Lexer, count: usize) void {
        var i: usize = 0;
        while (i < count and self.pos < self.source.len) : (i += 1) {
            if (self.source[self.pos] == '\n') {
                self.line += 1;
                self.column = 1;
            } else {
                self.column += 1;
            }
            self.pos += 1;
        }
    }

    pub fn next(self: *Lexer, allocator: std.mem.Allocator) !?Token {
        if (self.pos >= self.source.len) {
            return null;
        }

        // Look for {{ or {%
        const start = self.pos;
        const start_location = self.getCurrentLocation();
        var i = start;

        while (i < self.source.len) : (i += 1) {
            if (i + 1 < self.source.len and self.source[i] == '{') {
                if (self.source[i + 1] == '{') {
                    // Found {{
                    if (i > start) {
                        // Return text before {{
                        const text = try allocator.dupe(u8, self.source[start..i]);
                        self.advance(i - start);
                        return Token{
                            .kind = .text,
                            .content = text,
                            .location = start_location,
                            .strip_left = false,
                            .strip_right = false,
                        };
                    }

                    // Check for whitespace control {{-
                    var content_start = i + 2;
                    const strip_left = content_start < self.source.len and self.source[content_start] == '-';
                    if (strip_left) {
                        content_start += 1;
                    }

                    // Find closing }}
                    var j = content_start;
                    var strip_right = false;
                    while (j + 1 < self.source.len) : (j += 1) {
                        if (self.source[j] == '}' and self.source[j + 1] == '}') {
                            // Check for -}} before closing
                            var content_end = j;
                            if (j > content_start and self.source[j - 1] == '-') {
                                strip_right = true;
                                content_end -= 1;
                            }

                            const content = try allocator.dupe(u8, self.source[content_start..content_end]);
                            self.advance(j + 2 - start);
                            return Token{
                                .kind = .variable,
                                .content = content,
                                .location = start_location,
                                .strip_left = strip_left,
                                .strip_right = strip_right,
                            };
                        }
                    }
                    return error.UnclosedVariable;
                } else if (self.source[i + 1] == '%') {
                    // Found {%
                    if (i > start) {
                        // Return text before {%
                        const text = try allocator.dupe(u8, self.source[start..i]);
                        self.advance(i - start);
                        return Token{
                            .kind = .text,
                            .content = text,
                            .location = start_location,
                            .strip_left = false,
                            .strip_right = false,
                        };
                    }

                    // Check for whitespace control {%-
                    var content_start = i + 2;
                    const strip_left = content_start < self.source.len and self.source[content_start] == '-';
                    if (strip_left) {
                        content_start += 1;
                    }

                    // Find closing %}
                    var j = content_start;
                    var strip_right = false;
                    while (j + 1 < self.source.len) : (j += 1) {
                        if (self.source[j] == '%' and self.source[j + 1] == '}') {
                            // Check for -%} before closing
                            var content_end = j;
                            if (j > content_start and self.source[j - 1] == '-') {
                                strip_right = true;
                                content_end -= 1;
                            }

                            const content = try allocator.dupe(u8, self.source[content_start..content_end]);
                            self.advance(j + 2 - start);
                            return Token{
                                .kind = .tag,
                                .content = content,
                                .location = start_location,
                                .strip_left = strip_left,
                                .strip_right = strip_right,
                            };
                        }
                    }
                    return error.UnclosedTag;
                }
            }
        }

        // Rest is text
        if (i > start) {
            const text = try allocator.dupe(u8, self.source[start..]);
            self.advance(i - start);
            return Token{
                .kind = .text,
                .content = text,
                .location = start_location,
                .strip_left = false,
                .strip_right = false,
            };
        }

        return null;
    }
};

fn valueToString(allocator: std.mem.Allocator, value: json.Value) ![]const u8 {
    return switch (value) {
        .string => |s| s,
        .integer => |i| try std.fmt.allocPrint(allocator, "{d}", .{i}),
        .float => |f| try std.fmt.allocPrint(allocator, "{d}", .{f}),
        .bool => |b| if (b) "true" else "false",
        .null => "",
        else => "",
    };
}

fn renderValue(value: json.Value, output: *std.ArrayList(u8), allocator: std.mem.Allocator) !void {
    switch (value) {
        .string => |s| try output.appendSlice(allocator, s),
        .integer => |i| try output.writer(allocator).print("{d}", .{i}),
        .float => |f| try output.writer(allocator).print("{d}", .{f}),
        .bool => |b| try output.appendSlice(allocator, if (b) "true" else "false"),
        .null => {},
        else => {},
    }
}
