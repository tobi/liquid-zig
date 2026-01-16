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
    ir: ?IR,

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

        var template = Template{
            .allocator = allocator,
            .nodes = nodes,
            .ir = null,
        };

        // Generate IR from AST
        template.ir = try IR.fromAST(allocator, template.nodes.items);

        return template;
    }

    pub fn deinit(self: *Template) void {
        for (self.nodes.items) |*node| {
            node.deinit(self.allocator);
        }
        self.nodes.deinit(self.allocator);
        if (self.ir) |*ir| {
            ir.deinit();
        }
    }

    pub fn render(self: *Template, allocator: std.mem.Allocator, environment: ?json.Value) ![]const u8 {
        if (self.ir) |*ir| {
            // Use VM to execute IR
            var vm = VM.init(allocator, environment);
            defer vm.deinit();
            return try vm.execute(ir);
        } else {
            // Fallback to direct AST rendering (shouldn't happen)
            var ctx = Context.init(allocator, environment);
            defer ctx.deinit();

            var output: std.ArrayList(u8) = .{};
            errdefer output.deinit(allocator);

            for (self.nodes.items) |*node| {
                try node.render(&ctx, &output, allocator);
            }

            return try output.toOwnedSlice(allocator);
        }
    }
};

// IR (Intermediate Representation) instruction set
const Instruction = union(enum) {
    output_text: []const u8, // Output raw text
    output_string: []const u8, // Output string literal
    output_integer: i64, // Output integer literal
    output_float: f64, // Output float literal
    output_boolean: bool, // Output boolean literal
    output_nil: void, // Output nil (empty)
    output_variable: OutputVariable, // Output variable with filters
    output_literal_with_filters: OutputLiteralWithFilters, // Output literal value with filters
    assign: Assign, // Variable assignment

    const OutputVariable = struct {
        path: []const u8,
        filters: []Filter,

        pub fn deinit(self: *OutputVariable, allocator: std.mem.Allocator) void {
            allocator.free(self.path);
            for (self.filters) |*f| {
                f.deinit(allocator);
            }
            allocator.free(self.filters);
        }
    };

    const OutputLiteralWithFilters = struct {
        literal: json.Value,
        filters: []Filter,
        owned_string: ?[]const u8, // For string literals that need to be freed

        pub fn deinit(self: *OutputLiteralWithFilters, allocator: std.mem.Allocator) void {
            if (self.owned_string) |s| {
                allocator.free(s);
            }
            for (self.filters) |*f| {
                f.deinit(allocator);
            }
            allocator.free(self.filters);
        }
    };

    const Assign = struct {
        variable_name: []const u8,
        expression: Expression,

        pub fn deinit(self: *Assign, allocator: std.mem.Allocator) void {
            allocator.free(self.variable_name);
            var expr = self.expression;
            expr.deinit(allocator);
        }
    };

    pub fn deinit(self: *Instruction, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .output_text => |t| allocator.free(t),
            .output_string => |s| allocator.free(s),
            .output_variable => |*v| v.deinit(allocator),
            .output_literal_with_filters => |*l| l.deinit(allocator),
            .assign => |*a| a.deinit(allocator),
            else => {},
        }
    }
};

// IR represents the intermediate representation of a template
const IR = struct {
    allocator: std.mem.Allocator,
    instructions: []Instruction,

    pub fn fromAST(allocator: std.mem.Allocator, nodes: []Node) !IR {
        var instructions: std.ArrayList(Instruction) = .{};
        errdefer {
            for (instructions.items) |*inst| {
                inst.deinit(allocator);
            }
            instructions.deinit(allocator);
        }

        for (nodes) |*node| {
            try convertNodeToIR(allocator, node, &instructions);
        }

        return IR{
            .allocator = allocator,
            .instructions = try instructions.toOwnedSlice(allocator),
        };
    }

    pub fn deinit(self: *IR) void {
        for (self.instructions) |*inst| {
            inst.deinit(self.allocator);
        }
        self.allocator.free(self.instructions);
    }
};

fn convertNodeToIR(allocator: std.mem.Allocator, node: *const Node, instructions: *std.ArrayList(Instruction)) !void {
    switch (node.*) {
        .text => |t| {
            const text = try allocator.dupe(u8, t);
            try instructions.append(allocator, Instruction{ .output_text = text });
        },
        .variable => |*v| {
            // Apply constant folding optimization for literals
            switch (v.expression) {
                .string => |s| {
                    if (v.filters.len == 0) {
                        // Constant string with no filters - fold to output_string
                        const str = try allocator.dupe(u8, s);
                        try instructions.append(allocator, Instruction{ .output_string = str });
                    } else {
                        // String literal with filters
                        const str = try allocator.dupe(u8, s);
                        const filters = try allocator.dupe(Filter, v.filters);
                        try instructions.append(allocator, Instruction{
                            .output_literal_with_filters = .{
                                .literal = json.Value{ .string = str },
                                .filters = filters,
                                .owned_string = str,
                            },
                        });
                    }
                },
                .integer => |i| {
                    if (v.filters.len == 0) {
                        // Constant integer with no filters - fold to output_integer
                        try instructions.append(allocator, Instruction{ .output_integer = i });
                    } else {
                        // Integer literal with filters
                        const filters = try allocator.dupe(Filter, v.filters);
                        try instructions.append(allocator, Instruction{
                            .output_literal_with_filters = .{
                                .literal = json.Value{ .integer = i },
                                .filters = filters,
                                .owned_string = null,
                            },
                        });
                    }
                },
                .float => |f| {
                    if (v.filters.len == 0) {
                        // Constant float with no filters - fold to output_float
                        try instructions.append(allocator, Instruction{ .output_float = f });
                    } else {
                        // Float literal with filters
                        const filters = try allocator.dupe(Filter, v.filters);
                        try instructions.append(allocator, Instruction{
                            .output_literal_with_filters = .{
                                .literal = json.Value{ .float = f },
                                .filters = filters,
                                .owned_string = null,
                            },
                        });
                    }
                },
                .boolean => |b| {
                    if (v.filters.len == 0) {
                        // Constant boolean with no filters - fold to output_boolean
                        try instructions.append(allocator, Instruction{ .output_boolean = b });
                    } else {
                        // Boolean literal with filters
                        const filters = try allocator.dupe(Filter, v.filters);
                        try instructions.append(allocator, Instruction{
                            .output_literal_with_filters = .{
                                .literal = json.Value{ .bool = b },
                                .filters = filters,
                                .owned_string = null,
                            },
                        });
                    }
                },
                .nil => {
                    if (v.filters.len == 0) {
                        // Constant nil with no filters - fold to output_nil
                        try instructions.append(allocator, Instruction{ .output_nil = {} });
                    } else {
                        // Nil literal with filters
                        const filters = try allocator.dupe(Filter, v.filters);
                        try instructions.append(allocator, Instruction{
                            .output_literal_with_filters = .{
                                .literal = json.Value{ .null = {} },
                                .filters = filters,
                                .owned_string = null,
                            },
                        });
                    }
                },
                .variable => |path| {
                    // Variable reference - always needs runtime evaluation
                    const var_path = try allocator.dupe(u8, path);
                    const filters = try allocator.dupe(Filter, v.filters);
                    try instructions.append(allocator, Instruction{
                        .output_variable = .{
                            .path = var_path,
                            .filters = filters,
                        },
                    });
                },
            }
        },
        .tag => |*t| {
            // Handle assign tags
            if (t.tag_type == .assign) {
                try convertAssignToIR(allocator, t, instructions);
            }
            // Other tags are not yet converted to IR (future enhancement)
        },
    }
}

fn convertAssignToIR(allocator: std.mem.Allocator, tag: *const Tag, instructions: *std.ArrayList(Instruction)) !void {
    // Parse: assign variable = value
    const content = std.mem.trim(u8, tag.content[6..], " \t\n\r"); // Skip "assign"

    var parts = std.mem.splitScalar(u8, content, '=');
    const var_name = std.mem.trim(u8, parts.first(), " \t\n\r");
    const value_str = if (parts.next()) |v| std.mem.trim(u8, v, " \t\n\r") else "";

    if (var_name.len == 0) {
        return error.InvalidAssign;
    }

    // Parse the expression
    const expression = try parseExpression(allocator, value_str);
    errdefer {
        var expr = expression;
        expr.deinit(allocator);
    }

    const name = try allocator.dupe(u8, var_name);
    try instructions.append(allocator, Instruction{
        .assign = .{
            .variable_name = name,
            .expression = expression,
        },
    });
}

// VM executes IR instructions
const VM = struct {
    allocator: std.mem.Allocator,
    context: Context,
    output: std.ArrayList(u8),

    pub fn init(allocator: std.mem.Allocator, environment: ?json.Value) VM {
        return .{
            .allocator = allocator,
            .context = Context.init(allocator, environment),
            .output = .{},
        };
    }

    pub fn deinit(self: *VM) void {
        self.context.deinit();
        self.output.deinit(self.allocator);
    }

    pub fn execute(self: *VM, ir: *const IR) ![]const u8 {
        for (ir.instructions) |*inst| {
            try self.executeInstruction(inst);
        }
        return try self.output.toOwnedSlice(self.allocator);
    }

    fn executeInstruction(self: *VM, inst: *const Instruction) !void {
        switch (inst.*) {
            .output_text => |text| {
                try self.output.appendSlice(self.allocator, text);
            },
            .output_string => |s| {
                try self.output.appendSlice(self.allocator, s);
            },
            .output_integer => |i| {
                try self.output.writer(self.allocator).print("{d}", .{i});
            },
            .output_float => |f| {
                try self.output.writer(self.allocator).print("{d}", .{f});
            },
            .output_boolean => |b| {
                try self.output.appendSlice(self.allocator, if (b) "true" else "false");
            },
            .output_nil => {
                // Output nothing for nil
            },
            .output_variable => |*var_inst| {
                // Look up variable and apply filters
                const value = self.context.get(var_inst.path);
                if (value == null) {
                    return; // Variable not found, output nothing
                }

                var current_value = value.?;
                var temp_strings: std.ArrayList([]u8) = .{};
                defer {
                    for (temp_strings.items) |s| {
                        self.allocator.free(s);
                    }
                    temp_strings.deinit(self.allocator);
                }

                for (var_inst.filters) |*filter| {
                    const result = try filter.apply(self.allocator, current_value);
                    try temp_strings.append(self.allocator, result);
                    current_value = json.Value{ .string = result };
                }

                try renderValue(current_value, &self.output, self.allocator);
            },
            .output_literal_with_filters => |*lit_inst| {
                // Apply filters to a literal value
                var current_value = lit_inst.literal;
                var temp_strings: std.ArrayList([]u8) = .{};
                defer {
                    for (temp_strings.items) |s| {
                        self.allocator.free(s);
                    }
                    temp_strings.deinit(self.allocator);
                }

                for (lit_inst.filters) |*filter| {
                    const result = try filter.apply(self.allocator, current_value);
                    try temp_strings.append(self.allocator, result);
                    current_value = json.Value{ .string = result };
                }

                try renderValue(current_value, &self.output, self.allocator);
            },
            .assign => |*assign_inst| {
                // Evaluate the expression
                var expr = assign_inst.expression;
                const value = expr.evaluate(&self.context) orelse json.Value{ .null = {} };

                // Store the value in the context
                try self.context.set(assign_inst.variable_name, value);
            },
        }
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
        // Handle dot notation for nested property access
        if (std.mem.indexOfScalar(u8, key, '.')) |dot_index| {
            const first_key = key[0..dot_index];
            const rest = key[dot_index + 1 ..];

            // Get the first part
            const first_value = self.getSimple(first_key) orelse return null;

            // Navigate through the rest of the path
            return self.getNestedValue(first_value, rest);
        }

        // Simple key lookup (no dots)
        return self.getSimple(key);
    }

    fn getSimple(self: *Context, key: []const u8) ?json.Value {
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

    fn getNestedValue(self: *Context, value: json.Value, path: []const u8) ?json.Value {
        if (path.len == 0) {
            return value;
        }

        // Find next dot or use entire path
        const dot_index = std.mem.indexOfScalar(u8, path, '.');
        const key = if (dot_index) |idx| path[0..idx] else path;
        const rest = if (dot_index) |idx| path[idx + 1 ..] else "";

        // Navigate into the current value
        const next_value = switch (value) {
            .object => |obj| obj.get(key) orelse return null,
            else => return null, // Can't navigate into non-objects
        };

        // Continue recursively if there's more path
        if (rest.len > 0) {
            return self.getNestedValue(next_value, rest);
        }

        return next_value;
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

// Expression represents a parsed expression value in the template
const Expression = union(enum) {
    variable: []const u8,
    string: []const u8,
    integer: i64,
    float: f64,
    boolean: bool,
    nil: void,

    pub fn deinit(self: *Expression, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .variable => |v| allocator.free(v),
            .string => |s| allocator.free(s),
            else => {},
        }
    }

    pub fn evaluate(self: *Expression, ctx: *Context) ?json.Value {
        return switch (self.*) {
            .variable => |path| ctx.get(path),
            .string => |s| json.Value{ .string = s },
            .integer => |i| json.Value{ .integer = i },
            .float => |f| json.Value{ .float = f },
            .boolean => |b| json.Value{ .bool = b },
            .nil => json.Value{ .null = {} },
        };
    }
};

const Variable = struct {
    expression: Expression,
    filters: []Filter,

    pub fn parse(allocator: std.mem.Allocator, content: []const u8) !Variable {
        const trimmed = std.mem.trim(u8, content, " \t\n\r");

        var parts = std.mem.splitScalar(u8, trimmed, '|');
        const expr_str = std.mem.trim(u8, parts.first(), " \t\n\r");

        // Parse the expression
        const expression = try parseExpression(allocator, expr_str);
        errdefer {
            var expr = expression;
            expr.deinit(allocator);
        }

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
            .expression = expression,
            .filters = try filters.toOwnedSlice(allocator),
        };
    }

    pub fn deinit(self: *Variable, allocator: std.mem.Allocator) void {
        var expr = self.expression;
        expr.deinit(allocator);
        for (self.filters) |*f| {
            f.deinit(allocator);
        }
        allocator.free(self.filters);
    }

    pub fn render(self: *Variable, ctx: *Context, output: *std.ArrayList(u8), allocator: std.mem.Allocator) !void {
        var expr = self.expression;
        const value = expr.evaluate(ctx);
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

// Parse an expression from a string
fn parseExpression(allocator: std.mem.Allocator, source: []const u8) !Expression {
    const trimmed = std.mem.trim(u8, source, " \t\n\r");

    if (trimmed.len == 0) {
        return Expression{ .nil = {} };
    }

    // Check for string literals (single or double quotes)
    if (trimmed[0] == '"' or trimmed[0] == '\'') {
        const quote = trimmed[0];
        if (trimmed.len < 2 or trimmed[trimmed.len - 1] != quote) {
            return error.UnterminatedString;
        }
        const string_content = trimmed[1 .. trimmed.len - 1];
        return Expression{ .string = try allocator.dupe(u8, string_content) };
    }

    // Check for boolean literals
    if (std.mem.eql(u8, trimmed, "true")) {
        return Expression{ .boolean = true };
    }
    if (std.mem.eql(u8, trimmed, "false")) {
        return Expression{ .boolean = false };
    }

    // Check for nil literal
    if (std.mem.eql(u8, trimmed, "nil") or std.mem.eql(u8, trimmed, "null")) {
        return Expression{ .nil = {} };
    }

    // Check for number literals
    // Try to parse as integer first
    if (std.fmt.parseInt(i64, trimmed, 10)) |int_value| {
        return Expression{ .integer = int_value };
    } else |_| {
        // Try to parse as float
        if (std.fmt.parseFloat(f64, trimmed)) |float_value| {
            return Expression{ .float = float_value };
        } else |_| {
            // Not a number, treat as variable reference
        }
    }

    // Default: treat as variable reference
    return Expression{ .variable = try allocator.dupe(u8, trimmed) };
}

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
