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
        var tokens: std.ArrayList(Token) = .{};
        defer {
            for (tokens.items) |token| {
                token.deinit(allocator);
            }
            tokens.deinit(allocator);
        }

        // First, lex all tokens
        var lexer = Lexer.init(source);
        while (try lexer.next(allocator)) |token| {
            try tokens.append(allocator, token);
        }

        // Apply whitespace stripping based on strip_left/strip_right flags
        try applyWhitespaceControl(allocator, &tokens);

        // Parse tokens into nodes
        var nodes: std.ArrayList(Node) = .{};
        errdefer {
            for (nodes.items) |*node| {
                node.deinit(allocator);
            }
            nodes.deinit(allocator);
        }

        var i: usize = 0;
        while (i < tokens.items.len) {
            const parsed = try parseNode(allocator, tokens.items, &i);
            try nodes.append(allocator, parsed);
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
    label: usize, // Label for jumps
    jump: usize, // Unconditional jump to label
    jump_if_false: JumpIfFalse, // Jump to label if condition is falsy
    jump_if_true: JumpIfTrue, // Jump to label if condition is truthy
    for_loop: ForLoop, // For loop instruction
    end_for_loop: void, // End of for loop
    start_capture: []const u8, // Start capturing output into variable
    end_capture: void, // End capturing and assign to variable
    increment: []const u8, // Increment counter and output value
    decrement: []const u8, // Decrement counter and output value

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

    const JumpIfFalse = struct {
        condition: Expression,
        target_label: usize,

        pub fn deinit(self: *JumpIfFalse, allocator: std.mem.Allocator) void {
            var expr = self.condition;
            expr.deinit(allocator);
        }
    };

    const JumpIfTrue = struct {
        condition: Expression,
        target_label: usize,

        pub fn deinit(self: *JumpIfTrue, allocator: std.mem.Allocator) void {
            var expr = self.condition;
            expr.deinit(allocator);
        }
    };

    const ForLoop = struct {
        item_name: []const u8,
        collection_expr: Expression,
        end_label: usize,

        pub fn deinit(self: *ForLoop, allocator: std.mem.Allocator) void {
            allocator.free(self.item_name);
            var expr = self.collection_expr;
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
            .jump_if_false => |*j| j.deinit(allocator),
            .jump_if_true => |*j| j.deinit(allocator),
            .for_loop => |*f| f.deinit(allocator),
            .start_capture => |var_name| allocator.free(var_name),
            .increment => |name| allocator.free(name),
            .decrement => |name| allocator.free(name),
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
                .binary_op => {
                    // Binary operations in output context - this shouldn't normally happen
                    // but we need to handle it. For now, just output nothing.
                    // In practice, binary ops are used in conditions, not outputs
                    try instructions.append(allocator, Instruction{ .output_nil = {} });
                },
            }
        },
        .tag => |*t| {
            // Handle assign tags
            if (t.tag_type == .assign) {
                try convertAssignToIR(allocator, t, instructions);
            } else if (t.tag_type == .if_tag) {
                try convertIfToIR(allocator, t, instructions);
            } else if (t.tag_type == .unless) {
                try convertUnlessToIR(allocator, t, instructions);
            } else if (t.tag_type == .for_tag) {
                try convertForToIR(allocator, t, instructions);
            } else if (t.tag_type == .capture) {
                try convertCaptureToIR(allocator, t, instructions);
            } else if (t.tag_type == .case_tag) {
                try convertCaseToIR(allocator, t, instructions);
            } else if (t.tag_type == .increment) {
                try convertIncrementToIR(allocator, t, instructions);
            } else if (t.tag_type == .decrement) {
                try convertDecrementToIR(allocator, t, instructions);
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

var next_label_id: usize = 0;

fn generateLabelId() usize {
    const id = next_label_id;
    next_label_id += 1;
    return id;
}

fn convertIfToIR(allocator: std.mem.Allocator, tag: *const Tag, instructions: *std.ArrayList(Instruction)) (std.mem.Allocator.Error || error{ UnterminatedString, InvalidAssign, InvalidFor, InvalidCapture, InvalidIncrement, InvalidDecrement })!void {
    // Parse the condition from "if condition"
    const condition_str = std.mem.trim(u8, tag.content[3..], " \t\n\r"); // Skip "if "
    const condition = try parseExpression(allocator, condition_str);
    errdefer {
        var expr = condition;
        expr.deinit(allocator);
    }

    // Generate labels
    const end_label = generateLabelId();
    var next_branch_label = generateLabelId();

    // Jump to next branch if condition is false
    try instructions.append(allocator, Instruction{
        .jump_if_false = .{
            .condition = condition,
            .target_label = next_branch_label,
        },
    });

    // Generate IR for if block children
    for (tag.children) |*child| {
        try convertNodeToIR(allocator, child, instructions);
    }

    // Jump to end after if block
    try instructions.append(allocator, Instruction{ .jump = end_label });

    // Handle elsif branches
    for (tag.elsif_branches) |*elsif_branch| {
        // Label for this branch
        try instructions.append(allocator, Instruction{ .label = next_branch_label });

        // Parse elsif condition
        const elsif_condition = try parseExpression(allocator, elsif_branch.condition);
        errdefer {
            var expr = elsif_condition;
            expr.deinit(allocator);
        }

        // Generate next branch label
        next_branch_label = generateLabelId();

        // Jump to next branch if condition is false
        try instructions.append(allocator, Instruction{
            .jump_if_false = .{
                .condition = elsif_condition,
                .target_label = next_branch_label,
            },
        });

        // Generate IR for elsif block children
        for (elsif_branch.children) |*child| {
            try convertNodeToIR(allocator, child, instructions);
        }

        // Jump to end after elsif block
        try instructions.append(allocator, Instruction{ .jump = end_label });
    }

    // Handle else block
    if (tag.else_children.len > 0) {
        // Label for else branch
        try instructions.append(allocator, Instruction{ .label = next_branch_label });

        // Generate IR for else block children
        for (tag.else_children) |*child| {
            try convertNodeToIR(allocator, child, instructions);
        }
    } else {
        // No else block, just put the label
        try instructions.append(allocator, Instruction{ .label = next_branch_label });
    }

    // End label
    try instructions.append(allocator, Instruction{ .label = end_label });
}

fn convertUnlessToIR(allocator: std.mem.Allocator, tag: *const Tag, instructions: *std.ArrayList(Instruction)) (std.mem.Allocator.Error || error{ UnterminatedString, InvalidAssign, InvalidFor, InvalidCapture, InvalidIncrement, InvalidDecrement })!void {
    // Parse the condition from "unless condition"
    const condition_str = std.mem.trim(u8, tag.content[6..], " \t\n\r"); // Skip "unless "
    const condition = try parseExpression(allocator, condition_str);
    errdefer {
        var expr = condition;
        expr.deinit(allocator);
    }

    // Generate labels
    const end_label = generateLabelId();
    var next_branch_label = generateLabelId();

    // Jump to next branch if condition is TRUE (inverted from if)
    try instructions.append(allocator, Instruction{
        .jump_if_true = .{
            .condition = condition,
            .target_label = next_branch_label,
        },
    });

    // Generate IR for unless block children (executed when condition is false)
    for (tag.children) |*child| {
        try convertNodeToIR(allocator, child, instructions);
    }

    // Jump to end after unless block
    try instructions.append(allocator, Instruction{ .jump = end_label });

    // Handle elsif branches (these are evaluated as regular if conditions, not inverted)
    for (tag.elsif_branches) |*elsif_branch| {
        // Label for this branch
        try instructions.append(allocator, Instruction{ .label = next_branch_label });

        // Parse elsif condition
        const elsif_condition = try parseExpression(allocator, elsif_branch.condition);
        errdefer {
            var expr = elsif_condition;
            expr.deinit(allocator);
        }

        // Generate next branch label
        next_branch_label = generateLabelId();

        // Jump to next branch if condition is false (normal if behavior)
        try instructions.append(allocator, Instruction{
            .jump_if_false = .{
                .condition = elsif_condition,
                .target_label = next_branch_label,
            },
        });

        // Generate IR for elsif block children
        for (elsif_branch.children) |*child| {
            try convertNodeToIR(allocator, child, instructions);
        }

        // Jump to end after elsif block
        try instructions.append(allocator, Instruction{ .jump = end_label });
    }

    // Handle else block
    if (tag.else_children.len > 0) {
        // Label for else branch
        try instructions.append(allocator, Instruction{ .label = next_branch_label });

        // Generate IR for else block children
        for (tag.else_children) |*child| {
            try convertNodeToIR(allocator, child, instructions);
        }
    } else {
        // No else block, just put the label
        try instructions.append(allocator, Instruction{ .label = next_branch_label });
    }

    // End label
    try instructions.append(allocator, Instruction{ .label = end_label });
}

fn convertForToIR(allocator: std.mem.Allocator, tag: *const Tag, instructions: *std.ArrayList(Instruction)) (std.mem.Allocator.Error || error{ UnterminatedString, InvalidAssign, InvalidFor, InvalidCapture, InvalidIncrement, InvalidDecrement })!void {
    // Parse: for item in collection
    const content = std.mem.trim(u8, tag.content[3..], " \t\n\r"); // Skip "for"

    // Split by " in "
    var parts = std.mem.splitSequence(u8, content, " in ");
    const item_name = std.mem.trim(u8, parts.first(), " \t\n\r");
    const collection_str = if (parts.next()) |c| std.mem.trim(u8, c, " \t\n\r") else "";

    if (item_name.len == 0 or collection_str.len == 0) {
        return error.InvalidFor;
    }

    // Parse the collection expression
    const collection_expr = try parseExpression(allocator, collection_str);
    errdefer {
        var expr = collection_expr;
        expr.deinit(allocator);
    }

    const end_label = generateLabelId();
    const loop_start_label = generateLabelId();

    // Emit for_loop instruction
    const item_name_owned = try allocator.dupe(u8, item_name);
    try instructions.append(allocator, Instruction{
        .for_loop = .{
            .item_name = item_name_owned,
            .collection_expr = collection_expr,
            .end_label = end_label,
        },
    });

    // Label for loop start
    try instructions.append(allocator, Instruction{ .label = loop_start_label });

    // Generate IR for loop body children
    for (tag.children) |*child| {
        try convertNodeToIR(allocator, child, instructions);
    }

    // End of loop iteration
    try instructions.append(allocator, Instruction{ .end_for_loop = {} });

    // Jump back to loop start (will be handled by VM)
    try instructions.append(allocator, Instruction{ .jump = loop_start_label });

    // End label
    try instructions.append(allocator, Instruction{ .label = end_label });
}

fn convertCaptureToIR(allocator: std.mem.Allocator, tag: *const Tag, instructions: *std.ArrayList(Instruction)) (std.mem.Allocator.Error || error{ UnterminatedString, InvalidAssign, InvalidFor, InvalidCapture, InvalidIncrement, InvalidDecrement })!void {
    // Parse: capture variable_name
    const content = std.mem.trim(u8, tag.content[7..], " \t\n\r"); // Skip "capture"
    const var_name = std.mem.trim(u8, content, " \t\n\r");

    if (var_name.len == 0) {
        return error.InvalidCapture;
    }

    // Emit start_capture instruction
    const var_name_owned = try allocator.dupe(u8, var_name);
    try instructions.append(allocator, Instruction{ .start_capture = var_name_owned });

    // Generate IR for capture body children
    for (tag.children) |*child| {
        try convertNodeToIR(allocator, child, instructions);
    }

    // Emit end_capture instruction
    try instructions.append(allocator, Instruction{ .end_capture = {} });
}

fn convertIncrementToIR(allocator: std.mem.Allocator, tag: *const Tag, instructions: *std.ArrayList(Instruction)) !void {
    // Parse: increment counter_name
    const content = std.mem.trim(u8, tag.content[9..], " \t\n\r"); // Skip "increment"
    const counter_name = std.mem.trim(u8, content, " \t\n\r");

    if (counter_name.len == 0) {
        return error.InvalidIncrement;
    }

    // Emit increment instruction
    const counter_name_owned = try allocator.dupe(u8, counter_name);
    try instructions.append(allocator, Instruction{ .increment = counter_name_owned });
}

fn convertDecrementToIR(allocator: std.mem.Allocator, tag: *const Tag, instructions: *std.ArrayList(Instruction)) !void {
    // Parse: decrement counter_name
    const content = std.mem.trim(u8, tag.content[9..], " \t\n\r"); // Skip "decrement"
    const counter_name = std.mem.trim(u8, content, " \t\n\r");

    if (counter_name.len == 0) {
        return error.InvalidDecrement;
    }

    // Emit decrement instruction
    const counter_name_owned = try allocator.dupe(u8, counter_name);
    try instructions.append(allocator, Instruction{ .decrement = counter_name_owned });
}

fn convertCaseToIR(allocator: std.mem.Allocator, tag: *const Tag, instructions: *std.ArrayList(Instruction)) (std.mem.Allocator.Error || error{ UnterminatedString, InvalidAssign, InvalidFor, InvalidCapture, InvalidIncrement, InvalidDecrement })!void {
    // Parse: case variable
    const condition_str = std.mem.trim(u8, tag.content[4..], " \t\n\r"); // Skip "case"

    // Generate labels
    const end_label = generateLabelId();
    var next_branch_label = generateLabelId();

    // Generate IR for each when branch
    for (tag.when_branches) |*when_branch| {
        // Label for this branch
        try instructions.append(allocator, Instruction{ .label = next_branch_label });

        // Parse the case expression (fresh for each when branch)
        const case_expr = try parseExpression(allocator, condition_str);
        errdefer {
            var expr = case_expr;
            expr.deinit(allocator);
        }

        // Parse the when value as an expression
        const when_expr = try parseExpression(allocator, when_branch.value);
        errdefer {
            var expr = when_expr;
            expr.deinit(allocator);
        }

        // Create equality comparison: case_expr == when_expr
        const eq_op = try allocator.create(Expression.BinaryOp);
        eq_op.* = .{
            .operator = .eq,
            .left = case_expr,
            .right = when_expr,
        };
        const comparison = Expression{ .binary_op = eq_op };

        // Generate next branch label
        next_branch_label = generateLabelId();

        // Jump to next branch if comparison is false
        try instructions.append(allocator, Instruction{
            .jump_if_false = .{
                .condition = comparison,
                .target_label = next_branch_label,
            },
        });

        // Generate IR for when block children
        for (when_branch.children) |*child| {
            try convertNodeToIR(allocator, child, instructions);
        }

        // Jump to end after when block
        try instructions.append(allocator, Instruction{ .jump = end_label });
    }

    // Handle else block
    if (tag.else_children.len > 0) {
        // Label for else branch
        try instructions.append(allocator, Instruction{ .label = next_branch_label });

        // Generate IR for else block children
        for (tag.else_children) |*child| {
            try convertNodeToIR(allocator, child, instructions);
        }
    } else {
        // No else block, just put the label
        try instructions.append(allocator, Instruction{ .label = next_branch_label });
    }

    // End label
    try instructions.append(allocator, Instruction{ .label = end_label });
}

// Loop state for tracking for loops
const LoopState = struct {
    collection: []const json.Value, // Array elements
    current_index: usize,
    item_name: []const u8,
    end_label: usize,
    owned_array: ?[]json.Value, // Owned copy if needed
};

// Capture state for tracking capture blocks
const CaptureState = struct {
    variable_name: []const u8,
    captured_output: std.ArrayList(u8),
};

// VM executes IR instructions
const VM = struct {
    allocator: std.mem.Allocator,
    context: Context,
    output: std.ArrayList(u8),
    loop_stack: std.ArrayList(LoopState),
    capture_stack: std.ArrayList(CaptureState),

    pub fn init(allocator: std.mem.Allocator, environment: ?json.Value) VM {
        return .{
            .allocator = allocator,
            .context = Context.init(allocator, environment),
            .output = .{},
            .loop_stack = .{},
            .capture_stack = .{},
        };
    }

    pub fn deinit(self: *VM) void {
        // Clean up loop stack
        for (self.loop_stack.items) |*loop_state| {
            if (loop_state.owned_array) |arr| {
                self.allocator.free(arr);
            }
        }
        self.loop_stack.deinit(self.allocator);
        // Clean up capture stack
        for (self.capture_stack.items) |*capture_state| {
            capture_state.captured_output.deinit(self.allocator);
        }
        self.capture_stack.deinit(self.allocator);
        self.context.deinit();
        self.output.deinit(self.allocator);
    }

    pub fn execute(self: *VM, ir: *const IR) ![]const u8 {
        // Build label map for jumps
        var label_map = std.AutoHashMap(usize, usize).init(self.allocator);
        defer label_map.deinit();

        for (ir.instructions, 0..) |*inst, i| {
            if (inst.* == .label) {
                try label_map.put(inst.label, i);
            }
        }

        // Execute instructions with PC (program counter)
        var pc: usize = 0;
        while (pc < ir.instructions.len) {
            const inst = &ir.instructions[pc];
            const next_pc = try self.executeInstruction(inst, &label_map);
            if (next_pc) |new_pc| {
                pc = new_pc;
            } else {
                pc += 1;
            }
        }

        return try self.output.toOwnedSlice(self.allocator);
    }

    fn executeInstruction(self: *VM, inst: *const Instruction, label_map: *const std.AutoHashMap(usize, usize)) !?usize {
        switch (inst.*) {
            .output_text => |text| {
                try self.getActiveOutput().appendSlice(self.allocator, text);
            },
            .output_string => |s| {
                try self.getActiveOutput().appendSlice(self.allocator, s);
            },
            .output_integer => |i| {
                try self.getActiveOutput().writer(self.allocator).print("{d}", .{i});
            },
            .output_float => |f| {
                try self.getActiveOutput().writer(self.allocator).print("{d}", .{f});
            },
            .output_boolean => |b| {
                try self.getActiveOutput().appendSlice(self.allocator, if (b) "true" else "false");
            },
            .output_nil => {
                // Output nothing for nil
            },
            .output_variable => |*var_inst| {
                // Look up variable and apply filters
                const value = self.context.get(var_inst.path);
                if (value == null) {
                    return null; // Variable not found, output nothing
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

                try renderValue(current_value, self.getActiveOutput(), self.allocator);
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

                try renderValue(current_value, self.getActiveOutput(), self.allocator);
            },
            .assign => |*assign_inst| {
                // Evaluate the expression
                var expr = assign_inst.expression;
                const value = expr.evaluate(&self.context) orelse json.Value{ .null = {} };

                // Store the value in the context
                try self.context.set(assign_inst.variable_name, value);
            },
            .label => {
                // Labels are just markers, no execution needed
            },
            .jump => |target_label| {
                // Unconditional jump
                const target_pc = label_map.get(target_label) orelse return error.InvalidLabel;
                return target_pc;
            },
            .jump_if_false => |*jump_inst| {
                // Evaluate condition
                var expr = jump_inst.condition;
                const value = expr.evaluate(&self.context);

                // Check if value is falsy (only false and nil are falsy)
                const is_falsy = if (value) |v| isFalsy(v) else true;

                if (is_falsy) {
                    const target_pc = label_map.get(jump_inst.target_label) orelse return error.InvalidLabel;
                    return target_pc;
                }
            },
            .jump_if_true => |*jump_inst| {
                // Evaluate condition
                var expr = jump_inst.condition;
                const value = expr.evaluate(&self.context);

                // Check if value is truthy (everything except false and nil)
                const is_truthy = if (value) |v| !isFalsy(v) else false;

                if (is_truthy) {
                    const target_pc = label_map.get(jump_inst.target_label) orelse return error.InvalidLabel;
                    return target_pc;
                }
            },
            .for_loop => |*for_inst| {
                // Evaluate the collection expression
                var expr = for_inst.collection_expr;
                const collection_value = expr.evaluate(&self.context);

                if (collection_value) |coll| {
                    // Handle array collections
                    if (coll == .array) {
                        const items = coll.array.items;
                        if (items.len == 0) {
                            // Empty array - jump to end
                            const target_pc = label_map.get(for_inst.end_label) orelse return error.InvalidLabel;
                            return target_pc;
                        }

                        // Push loop state onto stack
                        try self.loop_stack.append(self.allocator, LoopState{
                            .collection = items,
                            .current_index = 0,
                            .item_name = for_inst.item_name,
                            .end_label = for_inst.end_label,
                            .owned_array = null,
                        });

                        // Set up the first iteration
                        try self.setupLoopIteration();
                    } else {
                        // Non-array - jump to end (no iteration)
                        const target_pc = label_map.get(for_inst.end_label) orelse return error.InvalidLabel;
                        return target_pc;
                    }
                } else {
                    // Null collection - jump to end
                    const target_pc = label_map.get(for_inst.end_label) orelse return error.InvalidLabel;
                    return target_pc;
                }
            },
            .end_for_loop => {
                // Check if we have more iterations
                if (self.loop_stack.items.len > 0) {
                    const loop_state = &self.loop_stack.items[self.loop_stack.items.len - 1];
                    loop_state.current_index += 1;

                    if (loop_state.current_index < loop_state.collection.len) {
                        // Set up next iteration
                        try self.setupLoopIteration();
                        // Continue to next instruction (which will be the jump back)
                    } else {
                        // Loop is done - pop state and jump to end
                        const end_label = loop_state.end_label;
                        if (self.loop_stack.pop()) |popped_state| {
                            if (popped_state.owned_array) |arr| {
                                self.allocator.free(arr);
                            }
                        }
                        const target_pc = label_map.get(end_label) orelse return error.InvalidLabel;
                        return target_pc;
                    }
                }
            },
            .start_capture => |var_name| {
                // Start capturing output into a new buffer
                try self.capture_stack.append(self.allocator, CaptureState{
                    .variable_name = var_name,
                    .captured_output = .{},
                });
            },
            .end_capture => {
                // End capturing and assign the captured output to variable
                if (self.capture_stack.items.len > 0) {
                    const last_idx = self.capture_stack.items.len - 1;
                    var capture_state = self.capture_stack.items[last_idx];
                    const captured_str = try capture_state.captured_output.toOwnedSlice(self.allocator);
                    const value = json.Value{ .string = captured_str };
                    try self.context.set(capture_state.variable_name, value);
                    _ = self.capture_stack.orderedRemove(last_idx);
                }
            },
            .increment => |counter_name| {
                // Increment counter and output the value BEFORE increment
                const value = try self.context.increment(counter_name);
                try self.getActiveOutput().writer(self.allocator).print("{d}", .{value});
            },
            .decrement => |counter_name| {
                // Decrement counter and output the value BEFORE decrement
                const value = try self.context.decrement(counter_name);
                try self.getActiveOutput().writer(self.allocator).print("{d}", .{value});
            },
        }
        return null; // Continue to next instruction
    }

    fn getActiveOutput(self: *VM) *std.ArrayList(u8) {
        // If we're inside a capture block, write to the capture buffer
        if (self.capture_stack.items.len > 0) {
            return &self.capture_stack.items[self.capture_stack.items.len - 1].captured_output;
        }
        // Otherwise, write to the main output
        return &self.output;
    }

    fn setupLoopIteration(self: *VM) !void {
        if (self.loop_stack.items.len == 0) return;

        const loop_state = &self.loop_stack.items[self.loop_stack.items.len - 1];
        const item = loop_state.collection[loop_state.current_index];

        // Set the loop item variable
        try self.context.set(loop_state.item_name, item);

        // Set forloop object with properties
        const index = loop_state.current_index;
        const length = loop_state.collection.len;

        // Create forloop object
        var forloop_obj = json.ObjectMap.init(self.allocator);
        try forloop_obj.put("index", json.Value{ .integer = @intCast(index + 1) }); // 1-based
        try forloop_obj.put("index0", json.Value{ .integer = @intCast(index) }); // 0-based
        try forloop_obj.put("first", json.Value{ .bool = index == 0 });
        try forloop_obj.put("last", json.Value{ .bool = index == length - 1 });
        try forloop_obj.put("length", json.Value{ .integer = @intCast(length) });

        try self.context.set("forloop", json.Value{ .object = forloop_obj });
    }
};

const Context = struct {
    allocator: std.mem.Allocator,
    environment: ?json.Value,
    variables: std.StringHashMap(json.Value),
    counters: std.StringHashMap(i64), // Separate namespace for increment/decrement counters

    pub fn init(allocator: std.mem.Allocator, environment: ?json.Value) Context {
        return .{
            .allocator = allocator,
            .environment = environment,
            .variables = std.StringHashMap(json.Value).init(allocator),
            .counters = std.StringHashMap(i64).init(allocator),
        };
    }

    pub fn deinit(self: *Context) void {
        self.variables.deinit();
        self.counters.deinit();
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

    // Get counter value, returns current value (defaults to 0 if not exists)
    pub fn getCounter(self: *Context, name: []const u8) i64 {
        return self.counters.get(name) orelse 0;
    }

    // Increment counter and return the value BEFORE increment
    pub fn increment(self: *Context, name: []const u8) !i64 {
        const current = self.getCounter(name);
        try self.counters.put(name, current + 1);
        return current;
    }

    // Decrement counter and return the value BEFORE decrement
    pub fn decrement(self: *Context, name: []const u8) !i64 {
        const current = self.getCounter(name);
        try self.counters.put(name, current - 1);
        return current;
    }
};

// Parse a single node from token stream
fn parseNode(allocator: std.mem.Allocator, tokens: []Token, index: *usize) (std.mem.Allocator.Error || error{UnterminatedString})!Node {
    const token = tokens[index.*];
    index.* += 1;

    return switch (token.kind) {
        .text => Node{ .text = try allocator.dupe(u8, token.content) },
        .variable => Node{ .variable = try Variable.parse(allocator, token.content) },
        .tag => blk: {
            const trimmed = std.mem.trim(u8, token.content, " \t\n\r");

            // Check if it's an if tag
            if (std.mem.startsWith(u8, trimmed, "if ")) {
                break :blk try parseIfTag(allocator, tokens, index, trimmed);
            } else if (std.mem.startsWith(u8, trimmed, "unless ")) {
                break :blk try parseUnlessTag(allocator, tokens, index, trimmed);
            } else if (std.mem.startsWith(u8, trimmed, "for ")) {
                break :blk try parseForTag(allocator, tokens, index, trimmed);
            } else if (std.mem.startsWith(u8, trimmed, "capture ")) {
                break :blk try parseCaptureTag(allocator, tokens, index, trimmed);
            } else if (std.mem.startsWith(u8, trimmed, "case ")) {
                break :blk try parseCaseTag(allocator, tokens, index, trimmed);
            } else if (std.mem.eql(u8, trimmed, "comment")) {
                break :blk try parseCommentTag(allocator, tokens, index, trimmed);
            } else if (std.mem.eql(u8, trimmed, "raw")) {
                break :blk try parseRawTag(allocator, tokens, index, trimmed);
            } else if (std.mem.startsWith(u8, trimmed, "assign ")) {
                break :blk Node{ .tag = Tag{
                    .tag_type = .assign,
                    .content = try allocator.dupe(u8, trimmed),
                    .children = &.{},
                    .else_children = &.{},
                    .elsif_branches = &.{},
                } };
            } else if (std.mem.startsWith(u8, trimmed, "increment ")) {
                break :blk Node{ .tag = Tag{
                    .tag_type = .increment,
                    .content = try allocator.dupe(u8, trimmed),
                    .children = &.{},
                    .else_children = &.{},
                    .elsif_branches = &.{},
                } };
            } else if (std.mem.startsWith(u8, trimmed, "decrement ")) {
                break :blk Node{ .tag = Tag{
                    .tag_type = .decrement,
                    .content = try allocator.dupe(u8, trimmed),
                    .children = &.{},
                    .else_children = &.{},
                    .elsif_branches = &.{},
                } };
            } else {
                break :blk Node{ .tag = Tag{
                    .tag_type = .unknown,
                    .content = try allocator.dupe(u8, trimmed),
                    .children = &.{},
                    .else_children = &.{},
                    .elsif_branches = &.{},
                } };
            }
        },
    };
}

// Parse if/elsif/else/endif block
fn parseIfTag(allocator: std.mem.Allocator, tokens: []Token, index: *usize, initial_content: []const u8) (std.mem.Allocator.Error || error{UnterminatedString})!Node {
    var children: std.ArrayList(Node) = .{};
    errdefer {
        for (children.items) |*child| {
            child.deinit(allocator);
        }
        children.deinit(allocator);
    }

    var elsif_branches: std.ArrayList(ElsifBranch) = .{};
    errdefer {
        for (elsif_branches.items) |*branch| {
            allocator.free(branch.condition);
            for (branch.children) |*child| {
                child.deinit(allocator);
            }
            allocator.free(branch.children);
        }
        elsif_branches.deinit(allocator);
    }

    var else_children: std.ArrayList(Node) = .{};
    errdefer {
        for (else_children.items) |*child| {
            child.deinit(allocator);
        }
        else_children.deinit(allocator);
    }

    var current_section: enum { if_block, elsif_block, else_block } = .if_block;
    var current_elsif_children: std.ArrayList(Node) = .{};
    var current_elsif_condition: ?[]const u8 = null;

    while (index.* < tokens.len) {
        const token = tokens[index.*];

        if (token.kind == .tag) {
            const trimmed = std.mem.trim(u8, token.content, " \t\n\r");

            if (std.mem.eql(u8, trimmed, "endif")) {
                index.* += 1;
                break;
            } else if (std.mem.startsWith(u8, trimmed, "elsif ")) {
                // Save previous elsif branch if any
                if (current_section == .elsif_block) {
                    try elsif_branches.append(allocator, ElsifBranch{
                        .condition = current_elsif_condition.?,
                        .children = try current_elsif_children.toOwnedSlice(allocator),
                    });
                    current_elsif_children = .{};
                }

                current_section = .elsif_block;
                const condition = std.mem.trim(u8, trimmed[6..], " \t\n\r");
                current_elsif_condition = try allocator.dupe(u8, condition);
                index.* += 1;
                continue;
            } else if (std.mem.eql(u8, trimmed, "else")) {
                // Save elsif branch if we were in one
                if (current_section == .elsif_block) {
                    try elsif_branches.append(allocator, ElsifBranch{
                        .condition = current_elsif_condition.?,
                        .children = try current_elsif_children.toOwnedSlice(allocator),
                    });
                    current_elsif_children = .{};
                }

                current_section = .else_block;
                index.* += 1;
                continue;
            }
        }

        // Parse child node
        const child = try parseNode(allocator, tokens, index);

        switch (current_section) {
            .if_block => try children.append(allocator, child),
            .elsif_block => try current_elsif_children.append(allocator, child),
            .else_block => try else_children.append(allocator, child),
        }
    }

    // Save final elsif branch if any
    if (current_section == .elsif_block) {
        try elsif_branches.append(allocator, ElsifBranch{
            .condition = current_elsif_condition.?,
            .children = try current_elsif_children.toOwnedSlice(allocator),
        });
    } else {
        current_elsif_children.deinit(allocator);
    }

    return Node{ .tag = Tag{
        .tag_type = .if_tag,
        .content = try allocator.dupe(u8, initial_content),
        .children = try children.toOwnedSlice(allocator),
        .else_children = try else_children.toOwnedSlice(allocator),
        .elsif_branches = try elsif_branches.toOwnedSlice(allocator),
    } };
}

// Parse unless/elsif/else/endunless block
fn parseUnlessTag(allocator: std.mem.Allocator, tokens: []Token, index: *usize, initial_content: []const u8) (std.mem.Allocator.Error || error{UnterminatedString})!Node {
    var children: std.ArrayList(Node) = .{};
    errdefer {
        for (children.items) |*child| {
            child.deinit(allocator);
        }
        children.deinit(allocator);
    }

    var elsif_branches: std.ArrayList(ElsifBranch) = .{};
    errdefer {
        for (elsif_branches.items) |*branch| {
            allocator.free(branch.condition);
            for (branch.children) |*child| {
                child.deinit(allocator);
            }
            allocator.free(branch.children);
        }
        elsif_branches.deinit(allocator);
    }

    var else_children: std.ArrayList(Node) = .{};
    errdefer {
        for (else_children.items) |*child| {
            child.deinit(allocator);
        }
        else_children.deinit(allocator);
    }

    var current_section: enum { unless_block, elsif_block, else_block } = .unless_block;
    var current_elsif_children: std.ArrayList(Node) = .{};
    var current_elsif_condition: ?[]const u8 = null;

    while (index.* < tokens.len) {
        const token = tokens[index.*];

        if (token.kind == .tag) {
            const trimmed = std.mem.trim(u8, token.content, " \t\n\r");

            if (std.mem.eql(u8, trimmed, "endunless")) {
                index.* += 1;
                break;
            } else if (std.mem.startsWith(u8, trimmed, "elsif ")) {
                // Save previous elsif branch if any
                if (current_section == .elsif_block) {
                    try elsif_branches.append(allocator, ElsifBranch{
                        .condition = current_elsif_condition.?,
                        .children = try current_elsif_children.toOwnedSlice(allocator),
                    });
                    current_elsif_children = .{};
                }

                current_section = .elsif_block;
                const condition = std.mem.trim(u8, trimmed[6..], " \t\n\r");
                current_elsif_condition = try allocator.dupe(u8, condition);
                index.* += 1;
                continue;
            } else if (std.mem.eql(u8, trimmed, "else")) {
                // Save elsif branch if we were in one
                if (current_section == .elsif_block) {
                    try elsif_branches.append(allocator, ElsifBranch{
                        .condition = current_elsif_condition.?,
                        .children = try current_elsif_children.toOwnedSlice(allocator),
                    });
                    current_elsif_children = .{};
                }

                current_section = .else_block;
                index.* += 1;
                continue;
            }
        }

        // Parse child node
        const child = try parseNode(allocator, tokens, index);

        switch (current_section) {
            .unless_block => try children.append(allocator, child),
            .elsif_block => try current_elsif_children.append(allocator, child),
            .else_block => try else_children.append(allocator, child),
        }
    }

    // Save final elsif branch if any
    if (current_section == .elsif_block) {
        try elsif_branches.append(allocator, ElsifBranch{
            .condition = current_elsif_condition.?,
            .children = try current_elsif_children.toOwnedSlice(allocator),
        });
    } else {
        current_elsif_children.deinit(allocator);
    }

    return Node{ .tag = Tag{
        .tag_type = .unless,
        .content = try allocator.dupe(u8, initial_content),
        .children = try children.toOwnedSlice(allocator),
        .else_children = try else_children.toOwnedSlice(allocator),
        .elsif_branches = try elsif_branches.toOwnedSlice(allocator),
    } };
}

// Parse for/endfor block
fn parseForTag(allocator: std.mem.Allocator, tokens: []Token, index: *usize, initial_content: []const u8) (std.mem.Allocator.Error || error{UnterminatedString})!Node {
    var children: std.ArrayList(Node) = .{};
    errdefer {
        for (children.items) |*child| {
            child.deinit(allocator);
        }
        children.deinit(allocator);
    }

    while (index.* < tokens.len) {
        const token = tokens[index.*];

        if (token.kind == .tag) {
            const trimmed = std.mem.trim(u8, token.content, " \t\n\r");

            if (std.mem.eql(u8, trimmed, "endfor")) {
                index.* += 1;
                break;
            }
        }

        // Parse child node
        const child = try parseNode(allocator, tokens, index);
        try children.append(allocator, child);
    }

    return Node{ .tag = Tag{
        .tag_type = .for_tag,
        .content = try allocator.dupe(u8, initial_content),
        .children = try children.toOwnedSlice(allocator),
        .else_children = &.{},
        .elsif_branches = &.{},
    } };
}

// Parse capture/endcapture block
fn parseCaptureTag(allocator: std.mem.Allocator, tokens: []Token, index: *usize, initial_content: []const u8) (std.mem.Allocator.Error || error{UnterminatedString})!Node {
    var children: std.ArrayList(Node) = .{};
    errdefer {
        for (children.items) |*child| {
            child.deinit(allocator);
        }
        children.deinit(allocator);
    }

    while (index.* < tokens.len) {
        const token = tokens[index.*];

        if (token.kind == .tag) {
            const trimmed = std.mem.trim(u8, token.content, " \t\n\r");

            if (std.mem.eql(u8, trimmed, "endcapture")) {
                index.* += 1;
                break;
            }
        }

        // Parse child node
        const child = try parseNode(allocator, tokens, index);
        try children.append(allocator, child);
    }

    return Node{ .tag = Tag{
        .tag_type = .capture,
        .content = try allocator.dupe(u8, initial_content),
        .children = try children.toOwnedSlice(allocator),
        .else_children = &.{},
        .elsif_branches = &.{},
    } };
}

// Parse case/when/else/endcase block
fn parseCaseTag(allocator: std.mem.Allocator, tokens: []Token, index: *usize, initial_content: []const u8) (std.mem.Allocator.Error || error{UnterminatedString})!Node {
    var when_branches: std.ArrayList(WhenBranch) = .{};
    errdefer {
        for (when_branches.items) |*branch| {
            allocator.free(branch.value);
            for (branch.children) |*child| {
                child.deinit(allocator);
            }
            allocator.free(branch.children);
        }
        when_branches.deinit(allocator);
    }

    var else_children: std.ArrayList(Node) = .{};
    errdefer {
        for (else_children.items) |*child| {
            child.deinit(allocator);
        }
        else_children.deinit(allocator);
    }

    var current_section: enum { no_when, when_block, else_block } = .no_when;
    var current_when_children: std.ArrayList(Node) = .{};
    var current_when_value: ?[]const u8 = null;

    while (index.* < tokens.len) {
        const token = tokens[index.*];

        if (token.kind == .tag) {
            const trimmed = std.mem.trim(u8, token.content, " \t\n\r");

            if (std.mem.eql(u8, trimmed, "endcase")) {
                // Save final when branch if any
                if (current_section == .when_block) {
                    try when_branches.append(allocator, WhenBranch{
                        .value = current_when_value.?,
                        .children = try current_when_children.toOwnedSlice(allocator),
                    });
                }
                index.* += 1;
                break;
            } else if (std.mem.startsWith(u8, trimmed, "when ")) {
                // Save previous when branch if any
                if (current_section == .when_block) {
                    try when_branches.append(allocator, WhenBranch{
                        .value = current_when_value.?,
                        .children = try current_when_children.toOwnedSlice(allocator),
                    });
                    current_when_children = .{};
                }

                current_section = .when_block;
                const value = std.mem.trim(u8, trimmed[5..], " \t\n\r");
                current_when_value = try allocator.dupe(u8, value);
                index.* += 1;
                continue;
            } else if (std.mem.eql(u8, trimmed, "else")) {
                // Save when branch if we were in one
                if (current_section == .when_block) {
                    try when_branches.append(allocator, WhenBranch{
                        .value = current_when_value.?,
                        .children = try current_when_children.toOwnedSlice(allocator),
                    });
                    current_when_children = .{};
                }

                current_section = .else_block;
                index.* += 1;
                continue;
            }
        }

        // Parse child node
        var child = try parseNode(allocator, tokens, index);

        switch (current_section) {
            .no_when => {
                // Before first when, ignore content (shouldn't happen in valid templates)
                child.deinit(allocator);
            },
            .when_block => try current_when_children.append(allocator, child),
            .else_block => try else_children.append(allocator, child),
        }
    }

    // Clean up current_when_children if not used
    if (current_section != .when_block or when_branches.items.len > 0) {
        current_when_children.deinit(allocator);
    }

    return Node{ .tag = Tag{
        .tag_type = .case_tag,
        .content = try allocator.dupe(u8, initial_content),
        .children = &.{}, // case doesn't use children, uses when_branches
        .else_children = try else_children.toOwnedSlice(allocator),
        .elsif_branches = &.{},
        .when_branches = try when_branches.toOwnedSlice(allocator),
    } };
}

// Parse comment/endcomment block
fn parseCommentTag(allocator: std.mem.Allocator, tokens: []Token, index: *usize, initial_content: []const u8) (std.mem.Allocator.Error || error{UnterminatedString})!Node {
    // Comments consume all content until endcomment, but don't parse it
    while (index.* < tokens.len) {
        const token = tokens[index.*];
        index.* += 1;

        if (token.kind == .tag) {
            const trimmed = std.mem.trim(u8, token.content, " \t\n\r");
            if (std.mem.eql(u8, trimmed, "endcomment")) {
                break;
            }
        }
    }

    return Node{ .tag = Tag{
        .tag_type = .comment,
        .content = try allocator.dupe(u8, initial_content),
        .children = &.{},
        .else_children = &.{},
        .elsif_branches = &.{},
    } };
}

// Parse raw/endraw block
fn parseRawTag(allocator: std.mem.Allocator, tokens: []Token, index: *usize, initial_content: []const u8) (std.mem.Allocator.Error || error{UnterminatedString})!Node {
    _ = initial_content; // Not needed for raw tags since we output literal content
    var raw_content: std.ArrayList(u8) = .{};
    errdefer raw_content.deinit(allocator);

    while (index.* < tokens.len) {
        const token = tokens[index.*];

        if (token.kind == .tag) {
            const trimmed = std.mem.trim(u8, token.content, " \t\n\r");
            if (std.mem.eql(u8, trimmed, "endraw")) {
                index.* += 1;
                break;
            }
        }

        // Append the literal token content including delimiters
        switch (token.kind) {
            .text => try raw_content.appendSlice(allocator, token.content),
            .variable => {
                try raw_content.appendSlice(allocator, "{{");
                try raw_content.appendSlice(allocator, token.content);
                try raw_content.appendSlice(allocator, "}}");
            },
            .tag => {
                try raw_content.appendSlice(allocator, "{%");
                try raw_content.appendSlice(allocator, token.content);
                try raw_content.appendSlice(allocator, "%}");
            },
        }

        index.* += 1;
    }

    // Create a text node with the raw content
    return Node{ .text = try raw_content.toOwnedSlice(allocator) };
}

const ElsifBranch = struct {
    condition: []const u8,
    children: []Node,
};

const WhenBranch = struct {
    value: []const u8,
    children: []Node,
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
    binary_op: *BinaryOp,

    const BinaryOp = struct {
        operator: Operator,
        left: Expression,
        right: Expression,

        const Operator = enum {
            eq, // ==
            ne, // !=
            lt, // <
            gt, // >
            le, // <=
            ge, // >=
            and_op, // and
            or_op, // or
            contains, // contains
        };
    };

    pub fn deinit(self: *Expression, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .variable => |v| allocator.free(v),
            .string => |s| allocator.free(s),
            .binary_op => |op| {
                var left = op.*.left;
                left.deinit(allocator);
                var right = op.*.right;
                right.deinit(allocator);
                allocator.destroy(op);
            },
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
            .binary_op => |op| evaluateBinaryOp(op, ctx),
        };
    }
};

fn evaluateBinaryOp(op: *const Expression.BinaryOp, ctx: *Context) ?json.Value {
    var left_expr = op.*.left;
    var right_expr = op.*.right;

    switch (op.*.operator) {
        .and_op => {
            // Logical and: both must be truthy
            const left = left_expr.evaluate(ctx);
            if (left == null or isFalsy(left.?)) {
                return json.Value{ .bool = false };
            }
            const right = right_expr.evaluate(ctx);
            if (right == null or isFalsy(right.?)) {
                return json.Value{ .bool = false };
            }
            return json.Value{ .bool = true };
        },
        .or_op => {
            // Logical or: either can be truthy
            const left = left_expr.evaluate(ctx);
            if (left != null and !isFalsy(left.?)) {
                return json.Value{ .bool = true };
            }
            const right = right_expr.evaluate(ctx);
            if (right != null and !isFalsy(right.?)) {
                return json.Value{ .bool = true };
            }
            return json.Value{ .bool = false };
        },
        .eq => {
            // Equality check
            const left = left_expr.evaluate(ctx);
            const right = right_expr.evaluate(ctx);
            return json.Value{ .bool = compareValues(left, right, .eq) };
        },
        .ne => {
            // Inequality check
            const left = left_expr.evaluate(ctx);
            const right = right_expr.evaluate(ctx);
            return json.Value{ .bool = compareValues(left, right, .ne) };
        },
        .lt => {
            // Less than
            const left = left_expr.evaluate(ctx);
            const right = right_expr.evaluate(ctx);
            return json.Value{ .bool = compareValues(left, right, .lt) };
        },
        .gt => {
            // Greater than
            const left = left_expr.evaluate(ctx);
            const right = right_expr.evaluate(ctx);
            return json.Value{ .bool = compareValues(left, right, .gt) };
        },
        .le => {
            // Less than or equal
            const left = left_expr.evaluate(ctx);
            const right = right_expr.evaluate(ctx);
            return json.Value{ .bool = compareValues(left, right, .le) };
        },
        .ge => {
            // Greater than or equal
            const left = left_expr.evaluate(ctx);
            const right = right_expr.evaluate(ctx);
            return json.Value{ .bool = compareValues(left, right, .ge) };
        },
        .contains => {
            // Contains check
            const left = left_expr.evaluate(ctx);
            const right = right_expr.evaluate(ctx);
            return json.Value{ .bool = checkContains(left, right) };
        },
    }
}

fn compareValues(left: ?json.Value, right: ?json.Value, op: Expression.BinaryOp.Operator) bool {
    // Handle null cases
    if (left == null and right == null) {
        return op == .eq or op == .le or op == .ge;
    }
    if (left == null or right == null) {
        return op == .ne;
    }

    const l = left.?;
    const r = right.?;

    // Type-specific comparisons
    switch (l) {
        .integer => |li| {
            switch (r) {
                .integer => |ri| {
                    return switch (op) {
                        .eq => li == ri,
                        .ne => li != ri,
                        .lt => li < ri,
                        .gt => li > ri,
                        .le => li <= ri,
                        .ge => li >= ri,
                        else => false,
                    };
                },
                .float => |rf| {
                    const lf: f64 = @floatFromInt(li);
                    return switch (op) {
                        .eq => lf == rf,
                        .ne => lf != rf,
                        .lt => lf < rf,
                        .gt => lf > rf,
                        .le => lf <= rf,
                        .ge => lf >= rf,
                        else => false,
                    };
                },
                else => return op == .ne,
            }
        },
        .float => |lf| {
            switch (r) {
                .integer => |ri| {
                    const rf: f64 = @floatFromInt(ri);
                    return switch (op) {
                        .eq => lf == rf,
                        .ne => lf != rf,
                        .lt => lf < rf,
                        .gt => lf > rf,
                        .le => lf <= rf,
                        .ge => lf >= rf,
                        else => false,
                    };
                },
                .float => |rf| {
                    return switch (op) {
                        .eq => lf == rf,
                        .ne => lf != rf,
                        .lt => lf < rf,
                        .gt => lf > rf,
                        .le => lf <= rf,
                        .ge => lf >= rf,
                        else => false,
                    };
                },
                else => return op == .ne,
            }
        },
        .string => |ls| {
            switch (r) {
                .string => |rs| {
                    const cmp = std.mem.order(u8, ls, rs);
                    return switch (op) {
                        .eq => cmp == .eq,
                        .ne => cmp != .eq,
                        .lt => cmp == .lt,
                        .gt => cmp == .gt,
                        .le => cmp == .lt or cmp == .eq,
                        .ge => cmp == .gt or cmp == .eq,
                        else => false,
                    };
                },
                else => return op == .ne,
            }
        },
        .bool => |lb| {
            switch (r) {
                .bool => |rb| {
                    return switch (op) {
                        .eq => lb == rb,
                        .ne => lb != rb,
                        else => false,
                    };
                },
                else => return op == .ne,
            }
        },
        .null => {
            return switch (op) {
                .eq => r == .null,
                .ne => r != .null,
                else => false,
            };
        },
        else => return op == .ne,
    }
}

fn checkContains(haystack: ?json.Value, needle: ?json.Value) bool {
    if (haystack == null or needle == null) return false;

    const h = haystack.?;
    const n = needle.?;

    switch (h) {
        .string => |s| {
            // String contains check
            switch (n) {
                .string => |substr| return std.mem.indexOf(u8, s, substr) != null,
                else => return false,
            }
        },
        .array => |arr| {
            // Array contains check
            for (arr.items) |item| {
                // Use compareValues with .eq to check equality
                if (compareValues(item, n, .eq)) {
                    return true;
                }
            }
            return false;
        },
        else => return false,
    }
}

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
    // First parse with logical OR (lowest precedence)
    return try parseOrExpression(allocator, source);
}

// Parse OR expressions (lowest precedence)
fn parseOrExpression(allocator: std.mem.Allocator, source: []const u8) !Expression {
    const trimmed = std.mem.trim(u8, source, " \t\n\r");

    // Look for " or " operator (word boundaries)
    var pos: usize = 0;
    while (pos + 4 <= trimmed.len) : (pos += 1) {
        if (pos > 0 and trimmed[pos - 1] != ' ') continue;
        if (trimmed[pos] == 'o' and trimmed[pos + 1] == 'r' and trimmed[pos + 2] == ' ') {
            const left_str = std.mem.trim(u8, trimmed[0..pos], " \t\n\r");
            const right_str = std.mem.trim(u8, trimmed[pos + 2 ..], " \t\n\r");

            const left = try parseAndExpression(allocator, left_str);
            errdefer {
                var l = left;
                l.deinit(allocator);
            }

            const right = try parseOrExpression(allocator, right_str);
            errdefer {
                var r = right;
                r.deinit(allocator);
            }

            const op = try allocator.create(Expression.BinaryOp);
            op.* = .{
                .operator = .or_op,
                .left = left,
                .right = right,
            };

            return Expression{ .binary_op = op };
        }
    }

    return try parseAndExpression(allocator, trimmed);
}

// Parse AND expressions (higher precedence than OR)
fn parseAndExpression(allocator: std.mem.Allocator, source: []const u8) !Expression {
    const trimmed = std.mem.trim(u8, source, " \t\n\r");

    // Look for " and " operator (word boundaries)
    var pos: usize = 0;
    while (pos + 5 <= trimmed.len) : (pos += 1) {
        if (pos > 0 and trimmed[pos - 1] != ' ') continue;
        if (trimmed[pos] == 'a' and trimmed[pos + 1] == 'n' and trimmed[pos + 2] == 'd' and trimmed[pos + 3] == ' ') {
            const left_str = std.mem.trim(u8, trimmed[0..pos], " \t\n\r");
            const right_str = std.mem.trim(u8, trimmed[pos + 3 ..], " \t\n\r");

            const left = try parseComparisonExpression(allocator, left_str);
            errdefer {
                var l = left;
                l.deinit(allocator);
            }

            const right = try parseAndExpression(allocator, right_str);
            errdefer {
                var r = right;
                r.deinit(allocator);
            }

            const op = try allocator.create(Expression.BinaryOp);
            op.* = .{
                .operator = .and_op,
                .left = left,
                .right = right,
            };

            return Expression{ .binary_op = op };
        }
    }

    return try parseComparisonExpression(allocator, trimmed);
}

// Parse comparison expressions (==, !=, <, >, <=, >=, contains)
fn parseComparisonExpression(allocator: std.mem.Allocator, source: []const u8) !Expression {
    const trimmed = std.mem.trim(u8, source, " \t\n\r");

    // Look for comparison operators
    // Check for two-character operators first (==, !=, <=, >=)
    var i: usize = 0;
    while (i + 1 < trimmed.len) : (i += 1) {
        const op_type: ?Expression.BinaryOp.Operator = blk: {
            if (trimmed[i] == '=' and trimmed[i + 1] == '=') {
                break :blk .eq;
            } else if (trimmed[i] == '!' and trimmed[i + 1] == '=') {
                break :blk .ne;
            } else if (trimmed[i] == '<' and trimmed[i + 1] == '=') {
                break :blk .le;
            } else if (trimmed[i] == '>' and trimmed[i + 1] == '=') {
                break :blk .ge;
            }
            break :blk null;
        };

        if (op_type) |op| {
            const left_str = std.mem.trim(u8, trimmed[0..i], " \t\n\r");
            const right_str = std.mem.trim(u8, trimmed[i + 2 ..], " \t\n\r");

            const left = try parsePrimaryExpression(allocator, left_str);
            errdefer {
                var l = left;
                l.deinit(allocator);
            }

            const right = try parsePrimaryExpression(allocator, right_str);
            errdefer {
                var r = right;
                r.deinit(allocator);
            }

            const bin_op = try allocator.create(Expression.BinaryOp);
            bin_op.* = .{
                .operator = op,
                .left = left,
                .right = right,
            };

            return Expression{ .binary_op = bin_op };
        }
    }

    // Check for single-character operators (<, >)
    i = 0;
    while (i < trimmed.len) : (i += 1) {
        const op_type: ?Expression.BinaryOp.Operator = blk: {
            if (trimmed[i] == '<') {
                break :blk .lt;
            } else if (trimmed[i] == '>') {
                break :blk .gt;
            }
            break :blk null;
        };

        if (op_type) |op| {
            const left_str = std.mem.trim(u8, trimmed[0..i], " \t\n\r");
            const right_str = std.mem.trim(u8, trimmed[i + 1 ..], " \t\n\r");

            const left = try parsePrimaryExpression(allocator, left_str);
            errdefer {
                var l = left;
                l.deinit(allocator);
            }

            const right = try parsePrimaryExpression(allocator, right_str);
            errdefer {
                var r = right;
                r.deinit(allocator);
            }

            const bin_op = try allocator.create(Expression.BinaryOp);
            bin_op.* = .{
                .operator = op,
                .left = left,
                .right = right,
            };

            return Expression{ .binary_op = bin_op };
        }
    }

    // Check for "contains" operator (word boundary)
    i = 0;
    while (i + 9 <= trimmed.len) : (i += 1) {
        if (i > 0 and trimmed[i - 1] != ' ') continue;
        if (std.mem.startsWith(u8, trimmed[i..], "contains ")) {
            const left_str = std.mem.trim(u8, trimmed[0..i], " \t\n\r");
            const right_str = std.mem.trim(u8, trimmed[i + 9 ..], " \t\n\r");

            const left = try parsePrimaryExpression(allocator, left_str);
            errdefer {
                var l = left;
                l.deinit(allocator);
            }

            const right = try parsePrimaryExpression(allocator, right_str);
            errdefer {
                var r = right;
                r.deinit(allocator);
            }

            const op = try allocator.create(Expression.BinaryOp);
            op.* = .{
                .operator = .contains,
                .left = left,
                .right = right,
            };

            return Expression{ .binary_op = op };
        }
    }

    return try parsePrimaryExpression(allocator, trimmed);
}

// Parse primary expressions (literals and variables)
fn parsePrimaryExpression(allocator: std.mem.Allocator, source: []const u8) !Expression {
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
        // Handle array-specific filters first
        if (value == .array) {
            if (std.mem.eql(u8, self.name, "join")) {
                // join: convert array to string with separator
                const separator = if (self.args.len > 0) self.args[0] else ", ";
                var result: std.ArrayList(u8) = .{};
                const items = value.array.items;
                for (items, 0..) |item, i| {
                    if (i > 0) {
                        try result.appendSlice(allocator, separator);
                    }
                    const item_str = try valueToString(allocator, item);
                    try result.appendSlice(allocator, item_str);
                    // Free if it was allocated (integers, floats)
                    if (item == .integer or item == .float) {
                        allocator.free(item_str);
                    }
                }
                return try result.toOwnedSlice(allocator);
            } else if (std.mem.eql(u8, self.name, "first")) {
                // first: get first element(s)
                const items = value.array.items;
                if (items.len == 0) {
                    return try allocator.dupe(u8, "");
                }
                if (self.args.len > 0) {
                    // Get first N elements
                    const n = try std.fmt.parseInt(usize, self.args[0], 10);
                    const count = @min(n, items.len);
                    var result: std.ArrayList(u8) = .{};
                    try result.append(allocator, '[');
                    for (items[0..count], 0..) |item, i| {
                        if (i > 0) try result.appendSlice(allocator, ", ");
                        const item_str = try valueToJsonString(allocator, item);
                        try result.appendSlice(allocator, item_str);
                    }
                    try result.append(allocator, ']');
                    return try result.toOwnedSlice(allocator);
                } else {
                    // Get first element as string
                    const str = try valueToString(allocator, items[0]);
                    return try allocator.dupe(u8, str);
                }
            } else if (std.mem.eql(u8, self.name, "last")) {
                // last: get last element(s)
                const items = value.array.items;
                if (items.len == 0) {
                    return try allocator.dupe(u8, "");
                }
                if (self.args.len > 0) {
                    // Get last N elements
                    const n = try std.fmt.parseInt(usize, self.args[0], 10);
                    const count = @min(n, items.len);
                    const start = items.len - count;
                    var result: std.ArrayList(u8) = .{};
                    try result.append(allocator, '[');
                    for (items[start..], 0..) |item, i| {
                        if (i > 0) try result.appendSlice(allocator, ", ");
                        const item_str = try valueToJsonString(allocator, item);
                        try result.appendSlice(allocator, item_str);
                    }
                    try result.append(allocator, ']');
                    return try result.toOwnedSlice(allocator);
                } else {
                    // Get last element as string
                    const str = try valueToString(allocator, items[items.len - 1]);
                    return try allocator.dupe(u8, str);
                }
            } else if (std.mem.eql(u8, self.name, "reverse")) {
                // reverse: reverse array order
                const items = value.array.items;
                var result: std.ArrayList(u8) = .{};
                try result.append(allocator, '[');
                var i = items.len;
                while (i > 0) {
                    i -= 1;
                    if (i < items.len - 1) try result.appendSlice(allocator, ", ");
                    const item_str = try valueToJsonString(allocator, items[i]);
                    try result.appendSlice(allocator, item_str);
                }
                try result.append(allocator, ']');
                return try result.toOwnedSlice(allocator);
            } else if (std.mem.eql(u8, self.name, "sort")) {
                // sort: sort array
                const items = value.array.items;
                const sorted = try allocator.alloc(json.Value, items.len);
                defer allocator.free(sorted);
                @memcpy(sorted, items);
                std.mem.sort(json.Value, sorted, {}, compareJsonValues);

                var result: std.ArrayList(u8) = .{};
                try result.append(allocator, '[');
                for (sorted, 0..) |item, i| {
                    if (i > 0) try result.appendSlice(allocator, ", ");
                    const item_str = try valueToJsonString(allocator, item);
                    try result.appendSlice(allocator, item_str);
                }
                try result.append(allocator, ']');
                return try result.toOwnedSlice(allocator);
            } else if (std.mem.eql(u8, self.name, "sort_natural")) {
                // sort_natural: sort array case-insensitively
                const items = value.array.items;
                const sorted = try allocator.alloc(json.Value, items.len);
                defer allocator.free(sorted);
                @memcpy(sorted, items);
                std.mem.sort(json.Value, sorted, {}, compareJsonValuesNatural);

                var result: std.ArrayList(u8) = .{};
                try result.append(allocator, '[');
                for (sorted, 0..) |item, i| {
                    if (i > 0) try result.appendSlice(allocator, ", ");
                    const item_str = try valueToJsonString(allocator, item);
                    try result.appendSlice(allocator, item_str);
                }
                try result.append(allocator, ']');
                return try result.toOwnedSlice(allocator);
            } else if (std.mem.eql(u8, self.name, "uniq")) {
                // uniq: remove duplicates
                const items = value.array.items;
                var seen = std.StringHashMap(void).init(allocator);
                defer seen.deinit();
                var result: std.ArrayList(u8) = .{};
                try result.append(allocator, '[');
                var first = true;
                for (items) |item| {
                    const item_str = try valueToJsonString(allocator, item);
                    defer allocator.free(item_str);
                    if (!seen.contains(item_str)) {
                        try seen.put(item_str, {});
                        if (!first) try result.appendSlice(allocator, ", ");
                        first = false;
                        const item_str_copy = try valueToJsonString(allocator, item);
                        try result.appendSlice(allocator, item_str_copy);
                        allocator.free(item_str_copy);
                    }
                }
                try result.append(allocator, ']');
                return try result.toOwnedSlice(allocator);
            } else if (std.mem.eql(u8, self.name, "compact")) {
                // compact: remove nil values
                const items = value.array.items;
                var result: std.ArrayList(u8) = .{};
                try result.append(allocator, '[');
                var first = true;
                for (items) |item| {
                    if (item != .null) {
                        if (!first) try result.appendSlice(allocator, ", ");
                        first = false;
                        const item_str = try valueToJsonString(allocator, item);
                        try result.appendSlice(allocator, item_str);
                        allocator.free(item_str);
                    }
                }
                try result.append(allocator, ']');
                return try result.toOwnedSlice(allocator);
            } else if (std.mem.eql(u8, self.name, "size")) {
                // size: return array length
                return try std.fmt.allocPrint(allocator, "{d}", .{value.array.items.len});
            } else if (std.mem.eql(u8, self.name, "concat")) {
                // concat: combine arrays (note: arg would need to be evaluated as array)
                // For now, return original array as string
                const items = value.array.items;
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
            } else if (std.mem.eql(u8, self.name, "map")) {
                // map: extract property from array of objects
                if (self.args.len == 0) {
                    return try arrayToJsonString(allocator, value.array.items);
                }
                const property = self.args[0];
                const items = value.array.items;
                var result: std.ArrayList(u8) = .{};
                try result.append(allocator, '[');
                for (items, 0..) |item, i| {
                    if (i > 0) try result.appendSlice(allocator, ", ");
                    if (item == .object) {
                        if (item.object.get(property)) |prop_value| {
                            const prop_str = try valueToJsonString(allocator, prop_value);
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
                // where: filter array by property value
                if (self.args.len < 2) {
                    return try arrayToJsonString(allocator, value.array.items);
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
                            const prop_str = try valueToString(allocator, prop_value);
                            if (std.mem.eql(u8, prop_str, target_value)) {
                                if (!first) try result.appendSlice(allocator, ", ");
                                first = false;
                                const item_str = try valueToJsonString(allocator, item);
                                try result.appendSlice(allocator, item_str);
                                allocator.free(item_str);
                            }
                        }
                    }
                }
                try result.append(allocator, ']');
                return try result.toOwnedSlice(allocator);
            } else if (std.mem.eql(u8, self.name, "push")) {
                // push: add element to end
                if (self.args.len == 0) {
                    return try arrayToJsonString(allocator, value.array.items);
                }
                const items = value.array.items;
                var result: std.ArrayList(u8) = .{};
                try result.append(allocator, '[');
                for (items, 0..) |item, i| {
                    if (i > 0) try result.appendSlice(allocator, ", ");
                    const item_str = try valueToJsonString(allocator, item);
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
                // pop: remove last element
                const items = value.array.items;
                if (items.len == 0) {
                    return try allocator.dupe(u8, "[]");
                }
                var result: std.ArrayList(u8) = .{};
                try result.append(allocator, '[');
                for (items[0 .. items.len - 1], 0..) |item, i| {
                    if (i > 0) try result.appendSlice(allocator, ", ");
                    const item_str = try valueToJsonString(allocator, item);
                    try result.appendSlice(allocator, item_str);
                    allocator.free(item_str);
                }
                try result.append(allocator, ']');
                return try result.toOwnedSlice(allocator);
            } else if (std.mem.eql(u8, self.name, "shift")) {
                // shift: remove first element
                const items = value.array.items;
                if (items.len == 0) {
                    return try allocator.dupe(u8, "[]");
                }
                var result: std.ArrayList(u8) = .{};
                try result.append(allocator, '[');
                for (items[1..], 0..) |item, i| {
                    if (i > 0) try result.appendSlice(allocator, ", ");
                    const item_str = try valueToJsonString(allocator, item);
                    try result.appendSlice(allocator, item_str);
                    allocator.free(item_str);
                }
                try result.append(allocator, ']');
                return try result.toOwnedSlice(allocator);
            } else if (std.mem.eql(u8, self.name, "unshift")) {
                // unshift: add element to beginning
                if (self.args.len == 0) {
                    return try arrayToJsonString(allocator, value.array.items);
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
                    const item_str = try valueToJsonString(allocator, item);
                    try result.appendSlice(allocator, item_str);
                    allocator.free(item_str);
                }
                try result.append(allocator, ']');
                return try result.toOwnedSlice(allocator);
            }
        }

        // Fall through to string filters
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
        } else if (std.mem.eql(u8, self.name, "plus")) {
            // plus: add numbers
            const num = try valueToNumber(value);
            if (self.args.len == 0) {
                return try allocator.dupe(u8, str);
            }
            const arg_num = try stringToNumber(self.args[0]);
            const result = num + arg_num;
            return try numberToString(allocator, result);
        } else if (std.mem.eql(u8, self.name, "minus")) {
            // minus: subtract numbers
            const num = try valueToNumber(value);
            if (self.args.len == 0) {
                return try allocator.dupe(u8, str);
            }
            const arg_num = try stringToNumber(self.args[0]);
            const result = num - arg_num;
            return try numberToString(allocator, result);
        } else if (std.mem.eql(u8, self.name, "times")) {
            // times: multiply numbers
            const num = try valueToNumber(value);
            if (self.args.len == 0) {
                return try allocator.dupe(u8, str);
            }
            const arg_num = try stringToNumber(self.args[0]);
            const result = num * arg_num;
            return try numberToString(allocator, result);
        } else if (std.mem.eql(u8, self.name, "divided_by")) {
            // divided_by: divide numbers
            const num = try valueToNumber(value);
            if (self.args.len == 0) {
                return try allocator.dupe(u8, str);
            }
            const arg_num = try stringToNumber(self.args[0]);
            if (arg_num == 0) {
                // Division by zero - return 0 (Liquid behavior)
                return try allocator.dupe(u8, "0");
            }
            const result = num / arg_num;
            return try numberToString(allocator, result);
        } else if (std.mem.eql(u8, self.name, "modulo")) {
            // modulo: compute remainder
            const num = try valueToNumber(value);
            if (self.args.len == 0) {
                return try allocator.dupe(u8, str);
            }
            const arg_num = try stringToNumber(self.args[0]);
            if (arg_num == 0) {
                // Modulo by zero - return 0
                return try allocator.dupe(u8, "0");
            }
            const result = @rem(num, arg_num);
            return try numberToString(allocator, result);
        } else if (std.mem.eql(u8, self.name, "abs")) {
            // abs: absolute value
            const num = try valueToNumber(value);
            const result = @abs(num);
            return try numberToString(allocator, result);
        } else if (std.mem.eql(u8, self.name, "ceil")) {
            // ceil: round up
            const num = try valueToNumber(value);
            const result = @ceil(num);
            return try std.fmt.allocPrint(allocator, "{d}", .{@as(i64, @intFromFloat(result))});
        } else if (std.mem.eql(u8, self.name, "floor")) {
            // floor: round down
            const num = try valueToNumber(value);
            const result = @floor(num);
            return try std.fmt.allocPrint(allocator, "{d}", .{@as(i64, @intFromFloat(result))});
        } else if (std.mem.eql(u8, self.name, "round")) {
            // round: round to nearest integer or specified precision
            const num = try valueToNumber(value);
            if (self.args.len > 0) {
                // Round to specified decimal places
                const precision_int = try std.fmt.parseInt(i32, self.args[0], 10);
                const precision: f64 = @floatFromInt(precision_int);
                const multiplier = std.math.pow(f64, 10.0, precision);
                const result = @round(num * multiplier) / multiplier;
                return try std.fmt.allocPrint(allocator, "{d}", .{result});
            } else {
                // Round to nearest integer
                const result = @round(num);
                return try std.fmt.allocPrint(allocator, "{d}", .{@as(i64, @intFromFloat(result))});
            }
        } else if (std.mem.eql(u8, self.name, "append")) {
            // append: add string to end
            if (self.args.len == 0) {
                return try allocator.dupe(u8, str);
            }
            return try std.mem.concat(allocator, u8, &[_][]const u8{ str, self.args[0] });
        } else if (std.mem.eql(u8, self.name, "prepend")) {
            // prepend: add string to start
            if (self.args.len == 0) {
                return try allocator.dupe(u8, str);
            }
            return try std.mem.concat(allocator, u8, &[_][]const u8{ self.args[0], str });
        } else if (std.mem.eql(u8, self.name, "remove")) {
            // remove: remove all occurrences of substring
            if (self.args.len == 0) {
                return try allocator.dupe(u8, str);
            }
            const search = self.args[0];
            if (search.len == 0) {
                return try allocator.dupe(u8, str);
            }
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
        } else if (std.mem.eql(u8, self.name, "replace")) {
            // replace: replace all occurrences of substring
            if (self.args.len < 2) {
                return try allocator.dupe(u8, str);
            }
            const search = self.args[0];
            const replace_with = self.args[1];
            if (search.len == 0) {
                return try allocator.dupe(u8, str);
            }
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
        } else if (std.mem.eql(u8, self.name, "slice")) {
            // slice: extract substring by position
            if (self.args.len == 0) {
                return try allocator.dupe(u8, str);
            }
            const start_int = try std.fmt.parseInt(i64, self.args[0], 10);
            const start: usize = if (start_int < 0)
                if (@as(i64, @intCast(str.len)) + start_int < 0) 0 else @intCast(@as(i64, @intCast(str.len)) + start_int)
            else
                @intCast(@min(start_int, @as(i64, @intCast(str.len))));

            if (start >= str.len) {
                return try allocator.dupe(u8, "");
            }

            if (self.args.len > 1) {
                const len = try std.fmt.parseInt(usize, self.args[1], 10);
                const end = @min(start + len, str.len);
                return try allocator.dupe(u8, str[start..end]);
            } else {
                // Single character slice
                const end = @min(start + 1, str.len);
                return try allocator.dupe(u8, str[start..end]);
            }
        } else if (std.mem.eql(u8, self.name, "split")) {
            // split: creates array from string
            // Note: This returns a JSON array representation
            if (self.args.len == 0) {
                return try std.fmt.allocPrint(allocator, "[\"{s}\"]", .{str});
            }
            const delimiter = self.args[0];

            // Handle empty delimiter - split into individual characters
            if (delimiter.len == 0) {
                var result: std.ArrayList(u8) = .{};
                try result.append(allocator, '[');
                var first = true;
                for (str) |c| {
                    if (!first) {
                        try result.appendSlice(allocator, ", ");
                    }
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
                if (!first) {
                    try result.appendSlice(allocator, ", ");
                }
                first = false;
                try result.append(allocator, '"');
                try result.appendSlice(allocator, part);
                try result.append(allocator, '"');
            }

            try result.append(allocator, ']');
            return try result.toOwnedSlice(allocator);
        } else if (std.mem.eql(u8, self.name, "truncate")) {
            // truncate: limit string length
            if (self.args.len == 0) {
                return try allocator.dupe(u8, str);
            }
            const max_len = try std.fmt.parseInt(usize, self.args[0], 10);
            const ellipsis = if (self.args.len > 1) self.args[1] else "...";

            if (str.len <= max_len) {
                return try allocator.dupe(u8, str);
            }

            if (max_len <= ellipsis.len) {
                return try allocator.dupe(u8, ellipsis[0..@min(max_len, ellipsis.len)]);
            }

            const text_len = max_len - ellipsis.len;
            return try std.mem.concat(allocator, u8, &[_][]const u8{ str[0..text_len], ellipsis });
        } else if (std.mem.eql(u8, self.name, "truncatewords")) {
            // truncatewords: limit word count
            if (self.args.len == 0) {
                return try allocator.dupe(u8, str);
            }
            const max_words = try std.fmt.parseInt(usize, self.args[0], 10);
            const ellipsis = if (self.args.len > 1) self.args[1] else "...";

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
        } else if (std.mem.eql(u8, self.name, "url_encode")) {
            // url_encode: percent-encode URL components
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
        } else if (std.mem.eql(u8, self.name, "url_decode")) {
            // url_decode: decode percent-encoded URLs
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
        } else if (std.mem.eql(u8, self.name, "newline_to_br")) {
            // newline_to_br: convert newlines to <br>
            var result: std.ArrayList(u8) = .{};
            for (str) |c| {
                if (c == '\n') {
                    try result.appendSlice(allocator, "<br>");
                } else {
                    try result.append(allocator, c);
                }
            }
            return try result.toOwnedSlice(allocator);
        } else if (std.mem.eql(u8, self.name, "strip_html")) {
            // strip_html: remove HTML tags
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
        } else if (std.mem.eql(u8, self.name, "escape_once")) {
            // escape_once: escape HTML but don't double-escape
            var result: std.ArrayList(u8) = .{};
            var i: usize = 0;
            while (i < str.len) {
                // Check for already-escaped entities
                if (str[i] == '&') {
                    // Check for common entities
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
    elsif_branches: []ElsifBranch = &.{},
    when_branches: []WhenBranch = &.{},

    const TagType = enum {
        assign,
        if_tag,
        unless,
        for_tag,
        comment,
        raw,
        capture,
        case_tag,
        increment,
        decrement,
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
        for (self.elsif_branches) |*branch| {
            allocator.free(branch.condition);
            for (branch.children) |*child| {
                child.deinit(allocator);
            }
            allocator.free(branch.children);
        }
        allocator.free(self.elsif_branches);
        for (self.when_branches) |*branch| {
            allocator.free(branch.value);
            for (branch.children) |*child| {
                child.deinit(allocator);
            }
            allocator.free(branch.children);
        }
        allocator.free(self.when_branches);
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

// Apply whitespace control based on strip_left and strip_right flags
fn applyWhitespaceControl(allocator: std.mem.Allocator, tokens: *std.ArrayList(Token)) !void {
    var i: usize = 0;
    while (i < tokens.items.len) : (i += 1) {
        const token = &tokens.items[i];

        // If this token has strip_left=true, strip whitespace from the end of the previous text token
        if (token.strip_left and i > 0) {
            var prev_token = &tokens.items[i - 1];
            if (prev_token.kind == .text) {
                const trimmed = trimRight(prev_token.content);
                if (trimmed.len != prev_token.content.len) {
                    const new_content = try allocator.dupe(u8, trimmed);
                    allocator.free(prev_token.content);
                    prev_token.content = new_content;
                }
            }
        }

        // If this token has strip_right=true, strip whitespace from the start of the next text token
        if (token.strip_right and i + 1 < tokens.items.len) {
            var next_token = &tokens.items[i + 1];
            if (next_token.kind == .text) {
                const trimmed = trimLeft(next_token.content);
                if (trimmed.len != next_token.content.len) {
                    const new_content = try allocator.dupe(u8, trimmed);
                    allocator.free(next_token.content);
                    next_token.content = new_content;
                }
            }
        }
    }
}

// Trim whitespace from the left (spaces, tabs, newlines, carriage returns)
fn trimLeft(s: []const u8) []const u8 {
    var start: usize = 0;
    while (start < s.len) : (start += 1) {
        const c = s[start];
        if (c != ' ' and c != '\t' and c != '\n' and c != '\r') {
            break;
        }
    }
    return s[start..];
}

// Trim whitespace from the right (spaces, tabs, newlines, carriage returns)
fn trimRight(s: []const u8) []const u8 {
    var end: usize = s.len;
    while (end > 0) {
        const c = s[end - 1];
        if (c != ' ' and c != '\t' and c != '\n' and c != '\r') {
            break;
        }
        end -= 1;
    }
    return s[0..end];
}

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

fn valueToNumber(value: json.Value) !f64 {
    return switch (value) {
        .integer => |i| @floatFromInt(i),
        .float => |f| f,
        .string => |s| try stringToNumber(s),
        .bool => |b| if (b) 1.0 else 0.0,
        .null => 0.0,
        else => 0.0,
    };
}

fn stringToNumber(s: []const u8) !f64 {
    // Try to parse as integer first
    if (std.fmt.parseInt(i64, s, 10)) |int_value| {
        return @floatFromInt(int_value);
    } else |_| {
        // Try to parse as float
        if (std.fmt.parseFloat(f64, s)) |float_value| {
            return float_value;
        } else |_| {
            // Not a number, return 0
            return 0.0;
        }
    }
}

fn numberToString(allocator: std.mem.Allocator, num: f64) ![]u8 {
    // Check if number is effectively an integer
    const rounded = @round(num);
    if (@abs(num - rounded) < 0.0000001) {
        // It's an integer, format without decimal point
        const int_val: i64 = @intFromFloat(rounded);
        return try std.fmt.allocPrint(allocator, "{d}", .{int_val});
    } else {
        // It's a float, format with decimal places
        return try std.fmt.allocPrint(allocator, "{d}", .{num});
    }
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

// In Liquid, only false and nil are falsy. Everything else is truthy.
// This includes: 0, empty string "", empty arrays [], etc.
fn isFalsy(value: json.Value) bool {
    return switch (value) {
        .bool => |b| !b, // Only false is falsy
        .null => true, // nil/null is falsy
        else => false, // Everything else is truthy (including 0, "", [])
    };
}

// Helper function to convert an array to JSON string representation
fn arrayToJsonString(allocator: std.mem.Allocator, items: []const json.Value) error{OutOfMemory}![]u8 {
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

// Helper function to convert a json.Value to a JSON string representation
fn valueToJsonString(allocator: std.mem.Allocator, value: json.Value) error{OutOfMemory}![]u8 {
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

// Comparison function for sorting JSON values
fn compareJsonValues(_: void, a: json.Value, b: json.Value) bool {
    // Compare based on type and value
    const a_str = valueToString(std.heap.page_allocator, a) catch return false;
    defer std.heap.page_allocator.free(a_str);
    const b_str = valueToString(std.heap.page_allocator, b) catch return false;
    defer std.heap.page_allocator.free(b_str);

    // Try numeric comparison first
    if (std.fmt.parseFloat(f64, a_str)) |a_num| {
        if (std.fmt.parseFloat(f64, b_str)) |b_num| {
            return a_num < b_num;
        } else |_| {}
    } else |_| {}

    // Fall back to string comparison
    return std.mem.order(u8, a_str, b_str) == .lt;
}

// Comparison function for natural (case-insensitive) sorting
fn compareJsonValuesNatural(_: void, a: json.Value, b: json.Value) bool {
    const a_str = valueToString(std.heap.page_allocator, a) catch return false;
    defer std.heap.page_allocator.free(a_str);
    const b_str = valueToString(std.heap.page_allocator, b) catch return false;
    defer std.heap.page_allocator.free(b_str);

    // Case-insensitive comparison
    return std.ascii.lessThanIgnoreCase(a_str, b_str);
}
