const std = @import("std");
const json = std.json;

// Import modular components
const utils = @import("utils.zig");
const lexer_mod = @import("lexer.zig");
const filters_mod = @import("filters.zig");
const strftime = @import("strftime.zig");

// Re-export types from modules for use within this file
const Lexer = lexer_mod.Lexer;
const Token = lexer_mod.Token;
const SourceLocation = lexer_mod.SourceLocation;
const applyWhitespaceControl = lexer_mod.applyWhitespaceControl;

const FilterKind = filters_mod.FilterKind;
const filter_map = filters_mod.filter_map;
const FilterResult = filters_mod.FilterResult;
// Note: cloneFilters is defined locally since it works with the local Filter type

// Re-export utility functions
const valueToString = utils.valueToString;
const valueToNumber = utils.valueToNumber;
const stringToNumber = utils.stringToNumber;
const isIntegerString = utils.isIntegerString;
const numberToString = utils.numberToString;
const numberToStringForceFloat = utils.numberToStringForceFloat;
const valueIsFloat = utils.valueIsFloat;
const stringIsFloat = utils.stringIsFloat;
const renderValue = utils.renderValue;
const isEmptyValue = utils.isEmptyValue;
const isBlankValue = utils.isBlankValue;
const unwrapDrop = utils.unwrapDrop;
const isBooleanDropFalse = utils.isBooleanDropFalse;
const isFalsy = utils.isFalsy;
const arrayToJsonString = utils.arrayToJsonString;
const valueToJsonString = utils.valueToJsonString;
const compareJsonValues = utils.compareJsonValues;
const compareJsonValuesNatural = utils.compareJsonValuesNatural;
const compareJsonValuesByProperty = utils.compareJsonValuesByProperty;
const PropertySortContext = utils.PropertySortContext;
const trimLeft = utils.trimLeft;
const trimRight = utils.trimRight;

/// Find the first occurrence of a character outside of quoted strings
/// Returns null if not found
fn indexOfOutsideStrings(s: []const u8, char: u8) ?usize {
    var in_single_quote = false;
    var in_double_quote = false;
    for (s, 0..) |c, i| {
        if (c == '\'' and !in_double_quote) {
            in_single_quote = !in_single_quote;
        } else if (c == '"' and !in_single_quote) {
            in_double_quote = !in_double_quote;
        } else if (c == char and !in_single_quote and !in_double_quote) {
            return i;
        }
    }
    return null;
}

/// Split by a character outside of quoted strings
/// Returns an ArrayList of string slices
fn splitByCharOutsideStrings(allocator: std.mem.Allocator, s: []const u8, char: u8) !std.ArrayList([]const u8) {
    var parts: std.ArrayList([]const u8) = .{};
    var in_single_quote = false;
    var in_double_quote = false;
    var start: usize = 0;

    for (s, 0..) |c, i| {
        if (c == '\'' and !in_double_quote) {
            in_single_quote = !in_single_quote;
        } else if (c == '"' and !in_single_quote) {
            in_double_quote = !in_double_quote;
        } else if (c == char and !in_single_quote and !in_double_quote) {
            try parts.append(allocator, s[start..i]);
            start = i + 1;
        }
    }
    // Add the last part
    if (start <= s.len) {
        try parts.append(allocator, s[start..]);
    }
    return parts;
}

/// Split by comma outside of quoted strings
fn splitByCommaOutsideStrings(allocator: std.mem.Allocator, s: []const u8) !std.ArrayList([]const u8) {
    return splitByCharOutsideStrings(allocator, s, ',');
}

/// Split by pipe outside of quoted strings (for filter chains)
fn splitByPipeOutsideStrings(allocator: std.mem.Allocator, s: []const u8) !std.ArrayList([]const u8) {
    return splitByCharOutsideStrings(allocator, s, '|');
}

pub const ErrorMode = enum {
    lax, // Default: continue on errors, render unknown tags as text
    strict, // Fail on parse errors like unknown tags
};

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

    pub fn compile(self: *Engine, source: []const u8, filesystem: ?json.ObjectMap, error_mode: ErrorMode) !usize {
        var template = try Template.parse(self.allocator, source, filesystem, error_mode);
        errdefer template.deinit();

        try self.templates.append(self.allocator, template);
        const id = self.next_id;
        self.next_id += 1;
        return id;
    }

    pub fn render(self: *Engine, template_id: usize, environment: ?json.Value, frozen_time: ?i64) ![]const u8 {
        if (template_id >= self.templates.items.len) {
            return error.TemplateNotFound;
        }

        const template = &self.templates.items[template_id];
        return try template.render(self.allocator, environment, frozen_time);
    }
};

const Template = struct {
    allocator: std.mem.Allocator,
    nodes: std.ArrayList(Node),
    ir: ?IR,
    filesystem: std.StringHashMap([]const u8), // Map of partial name -> template source
    error_mode: ErrorMode,

    pub fn parse(allocator: std.mem.Allocator, source: []const u8, filesystem_param: ?json.ObjectMap, error_mode: ErrorMode) !Template {
        // Build filesystem map from JSON
        var filesystem = std.StringHashMap([]const u8).init(allocator);
        errdefer {
            var it = filesystem.iterator();
            while (it.next()) |entry| {
                allocator.free(entry.key_ptr.*);
                allocator.free(entry.value_ptr.*);
            }
            filesystem.deinit();
        }

        if (filesystem_param) |fs| {
            var it = fs.iterator();
            while (it.next()) |entry| {
                const key = try allocator.dupe(u8, entry.key_ptr.*);
                errdefer allocator.free(key);

                const value = if (entry.value_ptr.* == .string)
                    try allocator.dupe(u8, entry.value_ptr.string)
                else
                    try allocator.dupe(u8, "");
                errdefer allocator.free(value);

                try filesystem.put(key, value);
            }
        }

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
            const parsed = try parseNode(allocator, tokens.items, &i, error_mode);
            try nodes.append(allocator, parsed);
        }

        var template = Template{
            .allocator = allocator,
            .nodes = nodes,
            .ir = null,
            .filesystem = filesystem,
            .error_mode = error_mode,
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
        // Free filesystem entries
        var it = self.filesystem.iterator();
        while (it.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
            self.allocator.free(entry.value_ptr.*);
        }
        self.filesystem.deinit();
    }

    pub fn render(self: *Template, allocator: std.mem.Allocator, environment: ?json.Value, frozen_time: ?i64) ![]const u8 {
        if (self.ir) |*ir| {
            // Use VM to execute IR
            var vm = VM.init(allocator, environment, &self.filesystem, self.error_mode, frozen_time);
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
    output_range: OutputRange, // Output range as string "X..Y"
    output_variable: OutputVariable, // Output variable with filters
    output_literal_with_filters: OutputLiteralWithFilters, // Output literal value with filters
    assign: Assign, // Variable assignment
    label: usize, // Label for jumps
    jump: usize, // Unconditional jump to label
    jump_if_false: JumpIfFalse, // Jump to label if condition is falsy
    jump_if_true: JumpIfTrue, // Jump to label if condition is truthy
    for_loop: ForLoop, // For loop instruction
    end_for_loop: void, // End of for loop
    tablerow_loop: TableRowLoop, // Tablerow loop instruction
    end_tablerow_loop: void, // End of tablerow loop
    start_capture: []const u8, // Start capturing output into variable
    end_capture: void, // End capturing and assign to variable
    increment: []const u8, // Increment counter and output value
    decrement: []const u8, // Decrement counter and output value
    cycle: Cycle, // Cycle through values
    include: Include, // Include partial with parent scope
    render: Render, // Render partial with isolated scope
    loop_break: void, // Break out of current loop
    loop_continue: void, // Continue to next iteration of loop
    start_ifchanged: void, // Start ifchanged block (capture output)
    end_ifchanged: void, // End ifchanged block (output if different from last)

    const Include = struct {
        partial_name: Expression, // Name of partial (can be variable or string)
        variables: []NamedArg, // Variables to pass: var: value
        for_collection: ?Expression, // For 'for items' syntax
        item_name: ?[]const u8, // Variable name for each item (usually partial name)

        pub fn deinit(self: *Include, allocator: std.mem.Allocator) void {
            var expr = self.partial_name;
            expr.deinit(allocator);
            for (self.variables) |*v| {
                allocator.free(v.name);
                var val = v.value;
                val.deinit(allocator);
            }
            allocator.free(self.variables);
            if (self.for_collection) |*fc| {
                var fcv = fc.*;
                fcv.deinit(allocator);
            }
            if (self.item_name) |name| {
                allocator.free(name);
            }
        }
    };

    const Render = struct {
        partial_name: []const u8, // Name of partial (must be string literal)
        variables: []NamedArg, // Variables to pass: var: value
        with_value: ?Expression, // For 'with' syntax
        with_alias: ?[]const u8, // For 'with obj as alias' syntax
        for_collection: ?Expression, // For 'for items' syntax
        for_alias: ?[]const u8, // For 'for items as alias' syntax

        pub fn deinit(self: *Render, allocator: std.mem.Allocator) void {
            allocator.free(self.partial_name);
            for (self.variables) |*v| {
                allocator.free(v.name);
                var val = v.value;
                val.deinit(allocator);
            }
            allocator.free(self.variables);
            if (self.with_value) |*w| {
                var wv = w.*;
                wv.deinit(allocator);
            }
            if (self.with_alias) |wa| {
                allocator.free(wa);
            }
            if (self.for_collection) |*fc| {
                var fcv = fc.*;
                fcv.deinit(allocator);
            }
            if (self.for_alias) |fa| {
                allocator.free(fa);
            }
        }
    };

    const NamedArg = struct {
        name: []const u8,
        value: Expression,
    };

    const Cycle = struct {
        name: ?Expression, // Named cycle (e.g., "group1") or null for unnamed
        values: []Expression, // Values to cycle through
        identity_key: []const u8, // Computed identity key for this cycle
        has_variable: bool, // Whether cycle contains a variable (affects identity)

        pub fn deinit(self: *Cycle, allocator: std.mem.Allocator) void {
            if (self.name) |*n| {
                n.deinit(allocator);
            }
            for (self.values) |*v| {
                v.deinit(allocator);
            }
            allocator.free(self.values);
            allocator.free(self.identity_key);
        }
    };

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

    const OutputRange = struct {
        start: Expression,
        end: Expression,
        filters: []Filter,

        pub fn deinit(self: *OutputRange, allocator: std.mem.Allocator) void {
            var start = self.start;
            start.deinit(allocator);
            var end = self.end;
            end.deinit(allocator);
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
        filters: []Filter,

        pub fn deinit(self: *Assign, allocator: std.mem.Allocator) void {
            allocator.free(self.variable_name);
            var expr = self.expression;
            expr.deinit(allocator);
            for (self.filters) |*f| {
                var filter = f.*;
                filter.deinit(allocator);
            }
            allocator.free(self.filters);
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
        collection_name: []const u8, // Used as key for offset:continue tracking
        end_label: usize,
        else_label: ?usize, // else label for empty collections
        limit: ?Expression, // limit parameter
        offset: ?Expression, // offset parameter
        reversed: bool, // reversed flag
        offset_continue: bool, // Use stored continue offset

        pub fn deinit(self: *ForLoop, allocator: std.mem.Allocator) void {
            allocator.free(self.item_name);
            allocator.free(self.collection_name);
            var expr = self.collection_expr;
            expr.deinit(allocator);
            if (self.limit) |*l| {
                var limit_expr = l.*;
                limit_expr.deinit(allocator);
            }
            if (self.offset) |*o| {
                var offset_expr = o.*;
                offset_expr.deinit(allocator);
            }
        }
    };

    const TableRowLoop = struct {
        item_name: []const u8,
        collection_expr: Expression,
        cols: ?Expression, // cols parameter
        limit: ?Expression, // limit parameter
        offset: ?Expression, // offset parameter
        end_label: usize,

        pub fn deinit(self: *TableRowLoop, allocator: std.mem.Allocator) void {
            allocator.free(self.item_name);
            var expr = self.collection_expr;
            expr.deinit(allocator);
            if (self.cols) |*c| {
                var cols_expr = c.*;
                cols_expr.deinit(allocator);
            }
            if (self.limit) |*l| {
                var limit_expr = l.*;
                limit_expr.deinit(allocator);
            }
            if (self.offset) |*o| {
                var offset_expr = o.*;
                offset_expr.deinit(allocator);
            }
        }
    };

    pub fn deinit(self: *Instruction, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .output_text => |t| allocator.free(t),
            .output_string => |s| allocator.free(s),
            .output_range => |*r| r.deinit(allocator),
            .output_variable => |*v| v.deinit(allocator),
            .output_literal_with_filters => |*l| l.deinit(allocator),
            .assign => |*a| a.deinit(allocator),
            .jump_if_false => |*j| j.deinit(allocator),
            .jump_if_true => |*j| j.deinit(allocator),
            .for_loop => |*f| f.deinit(allocator),
            .tablerow_loop => |*t| t.deinit(allocator),
            .start_capture => |var_name| allocator.free(var_name),
            .increment => |name| allocator.free(name),
            .decrement => |name| allocator.free(name),
            .cycle => |*c| c.deinit(allocator),
            .include => |*i| i.deinit(allocator),
            .render => |*r| r.deinit(allocator),
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
                        const filters = try cloneFilters(allocator, v.filters);
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
                        const filters = try cloneFilters(allocator, v.filters);
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
                        const filters = try cloneFilters(allocator, v.filters);
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
                        const filters = try cloneFilters(allocator, v.filters);
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
                        const filters = try cloneFilters(allocator, v.filters);
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
                    const filters = try cloneFilters(allocator, v.filters);
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
                .range => |r| {
                    // Range in output context - output as "start..end" string
                    const filters = try cloneFilters(allocator, v.filters);
                    try instructions.append(allocator, Instruction{
                        .output_range = .{
                            .start = r.*.start,
                            .end = r.*.end,
                            .filters = filters,
                        },
                    });
                },
                .empty, .blank => {
                    // empty/blank keywords output nothing on their own
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
            } else if (t.tag_type == .tablerow) {
                try convertTableRowToIR(allocator, t, instructions);
            } else if (t.tag_type == .capture) {
                try convertCaptureToIR(allocator, t, instructions);
            } else if (t.tag_type == .case_tag) {
                try convertCaseToIR(allocator, t, instructions);
            } else if (t.tag_type == .increment) {
                try convertIncrementToIR(allocator, t, instructions);
            } else if (t.tag_type == .decrement) {
                try convertDecrementToIR(allocator, t, instructions);
            } else if (t.tag_type == .cycle) {
                try convertCycleToIR(allocator, t, instructions);
            } else if (t.tag_type == .include) {
                try convertIncludeToIR(allocator, t, instructions);
            } else if (t.tag_type == .render) {
                try convertRenderToIR(allocator, t, instructions);
            } else if (t.tag_type == .loop_break) {
                try instructions.append(allocator, Instruction{ .loop_break = {} });
            } else if (t.tag_type == .loop_continue) {
                try instructions.append(allocator, Instruction{ .loop_continue = {} });
            } else if (t.tag_type == .echo) {
                try convertEchoToIR(allocator, t, instructions);
            } else if (t.tag_type == .liquid_tag) {
                try convertLiquidTagToIR(allocator, t, instructions);
            } else if (t.tag_type == .ifchanged) {
                try convertIfChangedToIR(allocator, t, instructions);
            }
            // Other tags are not yet converted to IR (future enhancement)
        },
    }
}

fn convertAssignToIR(allocator: std.mem.Allocator, tag: *const Tag, instructions: *std.ArrayList(Instruction)) !void {
    // Parse: assign variable = value | filter1 | filter2
    const content = std.mem.trim(u8, tag.content[6..], " \t\n\r"); // Skip "assign"

    var parts = std.mem.splitScalar(u8, content, '=');
    const var_name = std.mem.trim(u8, parts.first(), " \t\n\r");
    const value_str = if (parts.next()) |v| std.mem.trim(u8, v, " \t\n\r") else "";

    if (var_name.len == 0) {
        return error.InvalidAssign;
    }

    // Parse expression and filters using smart pipe splitting that respects strings
    var filters: std.ArrayList(Filter) = .{};
    errdefer {
        for (filters.items) |*f| {
            f.deinit(allocator);
        }
        filters.deinit(allocator);
    }

    // Split by | but respect quoted strings
    var expr_str: []const u8 = value_str;
    var pipe_parts: std.ArrayList([]const u8) = .{};
    defer pipe_parts.deinit(allocator);

    // Smart split by | that respects string boundaries
    var i: usize = 0;
    var start: usize = 0;
    var in_single_quote = false;
    var in_double_quote = false;

    while (i < value_str.len) : (i += 1) {
        const c = value_str[i];
        if (c == '\'' and !in_double_quote) {
            in_single_quote = !in_single_quote;
        } else if (c == '"' and !in_single_quote) {
            in_double_quote = !in_double_quote;
        } else if (c == '|' and !in_single_quote and !in_double_quote) {
            try pipe_parts.append(allocator, std.mem.trim(u8, value_str[start..i], " \t\n\r"));
            start = i + 1;
        }
    }
    // Add the last part
    if (start < value_str.len) {
        try pipe_parts.append(allocator, std.mem.trim(u8, value_str[start..], " \t\n\r"));
    }

    if (pipe_parts.items.len > 0) {
        expr_str = pipe_parts.items[0];

        // Parse remaining parts as filters
        for (pipe_parts.items[1..]) |filter_str| {
            const filter = try Filter.parse(allocator, filter_str);
            try filters.append(allocator, filter);
        }
    }

    // Parse the expression (without filters)
    const expression = try parseExpression(allocator, expr_str);
    errdefer {
        var expr = expression;
        expr.deinit(allocator);
    }

    const name = try allocator.dupe(u8, var_name);
    try instructions.append(allocator, Instruction{
        .assign = .{
            .variable_name = name,
            .expression = expression,
            .filters = try filters.toOwnedSlice(allocator),
        },
    });
}

var next_label_id: usize = 0;

fn generateLabelId() usize {
    const id = next_label_id;
    next_label_id += 1;
    return id;
}

fn convertIfToIR(allocator: std.mem.Allocator, tag: *const Tag, instructions: *std.ArrayList(Instruction)) (std.mem.Allocator.Error || error{ UnterminatedString, InvalidAssign, InvalidFor, InvalidCapture, InvalidIncrement, InvalidDecrement, InvalidCycle, InvalidInclude, InvalidRender })!void {
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

fn convertUnlessToIR(allocator: std.mem.Allocator, tag: *const Tag, instructions: *std.ArrayList(Instruction)) (std.mem.Allocator.Error || error{ UnterminatedString, InvalidAssign, InvalidFor, InvalidCapture, InvalidIncrement, InvalidDecrement, InvalidCycle, InvalidInclude, InvalidRender })!void {
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

fn convertForToIR(allocator: std.mem.Allocator, tag: *const Tag, instructions: *std.ArrayList(Instruction)) (std.mem.Allocator.Error || error{ UnterminatedString, InvalidAssign, InvalidFor, InvalidCapture, InvalidIncrement, InvalidDecrement, InvalidCycle, InvalidInclude, InvalidRender })!void {
    // Parse: for item in collection [limit:n] [offset:n] [reversed]
    const content = std.mem.trim(u8, tag.content[3..], " \t\n\r"); // Skip "for"

    // Split by " in "
    var parts = std.mem.splitSequence(u8, content, " in ");
    const item_name = std.mem.trim(u8, parts.first(), " \t\n\r");
    const rest = if (parts.next()) |c| std.mem.trim(u8, c, " \t\n\r") else "";

    if (item_name.len == 0 or rest.len == 0) {
        return error.InvalidFor;
    }

    // Parse collection and modifiers
    // Format: collection [limit:n] [offset:n] [reversed]
    var collection_str: []const u8 = rest;
    var limit: ?Expression = null;
    var offset: ?Expression = null;
    var reversed: bool = false;
    var offset_continue: bool = false;

    // Look for modifiers after the collection
    // Split by spaces and look for keywords
    var words = std.mem.tokenizeAny(u8, rest, " \t\n\r");
    const first_word = words.next() orelse return error.InvalidFor;
    collection_str = first_word;

    errdefer {
        if (limit) |*l| {
            var limit_expr = l.*;
            limit_expr.deinit(allocator);
        }
        if (offset) |*o| {
            var offset_expr = o.*;
            offset_expr.deinit(allocator);
        }
    }

    while (words.next()) |word| {
        if (std.mem.startsWith(u8, word, "limit:")) {
            const limit_str = word[6..];
            limit = try parseExpression(allocator, limit_str);
        } else if (std.mem.startsWith(u8, word, "offset:")) {
            const offset_str = word[7..];
            if (std.mem.eql(u8, offset_str, "continue")) {
                offset_continue = true;
            } else {
                offset = try parseExpression(allocator, offset_str);
            }
        } else if (std.mem.eql(u8, word, "reversed")) {
            reversed = true;
        }
    }

    // Parse the collection expression
    const collection_expr = try parseExpression(allocator, collection_str);

    const end_label = generateLabelId();
    const loop_start_label = generateLabelId();
    // If there's an else block, we need an else_label
    const else_label: ?usize = if (tag.else_children.len > 0) generateLabelId() else null;

    // Emit for_loop instruction
    const item_name_owned = try allocator.dupe(u8, item_name);
    // Create continue key from "item_name-collection_str"
    const continue_key = try std.fmt.allocPrint(allocator, "{s}-{s}", .{ item_name, collection_str });
    try instructions.append(allocator, Instruction{
        .for_loop = .{
            .item_name = item_name_owned,
            .collection_expr = collection_expr,
            .collection_name = continue_key,
            .end_label = end_label,
            .else_label = else_label, // Jump here if collection is empty
            .limit = limit,
            .offset = offset,
            .reversed = reversed,
            .offset_continue = offset_continue,
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

    // Handle else block
    if (else_label) |el| {
        // Else label
        try instructions.append(allocator, Instruction{ .label = el });

        // Generate IR for else block children
        for (tag.else_children) |*child| {
            try convertNodeToIR(allocator, child, instructions);
        }
    }

    // End label
    try instructions.append(allocator, Instruction{ .label = end_label });
}

fn convertTableRowToIR(allocator: std.mem.Allocator, tag: *const Tag, instructions: *std.ArrayList(Instruction)) (std.mem.Allocator.Error || error{ UnterminatedString, InvalidAssign, InvalidFor, InvalidCapture, InvalidIncrement, InvalidDecrement, InvalidCycle, InvalidInclude, InvalidRender })!void {
    // Parse: tablerow item in collection [cols:n] [limit:n] [offset:n]
    const content = std.mem.trim(u8, tag.content[8..], " \t\n\r"); // Skip "tablerow"

    // Split by " in "
    var parts = std.mem.splitSequence(u8, content, " in ");
    const item_name = std.mem.trim(u8, parts.first(), " \t\n\r");
    var collection_and_params_str = if (parts.next()) |c| std.mem.trim(u8, c, " \t\n\r") else "";

    if (item_name.len == 0 or collection_and_params_str.len == 0) {
        return error.InvalidFor;
    }

    // Parse parameters (cols:n, limit:n, offset:n)
    var cols_expr: ?Expression = null;
    var limit_expr: ?Expression = null;
    var offset_expr: ?Expression = null;
    var collection_str = collection_and_params_str;

    // Look for parameters (cols:, limit:, offset:)
    var current_pos: usize = 0;
    while (current_pos < collection_and_params_str.len) {
        // Find next space
        const space_pos = std.mem.indexOfScalarPos(u8, collection_and_params_str, current_pos, ' ');

        if (space_pos) |pos| {
            const segment = std.mem.trim(u8, collection_and_params_str[pos..], " \t\n\r");

            if (std.mem.startsWith(u8, segment, "cols:")) {
                // Found cols parameter - everything before this is the collection
                collection_str = std.mem.trim(u8, collection_and_params_str[0..pos], " \t\n\r");
                const cols_str = std.mem.trim(u8, segment[5..], " \t\n\r");

                // Find end of cols value (next space or end)
                const cols_end = std.mem.indexOfScalar(u8, cols_str, ' ') orelse cols_str.len;
                const cols_value = cols_str[0..cols_end];

                cols_expr = try parseExpression(allocator, cols_value);
                current_pos = pos + 5 + cols_end;
            } else if (std.mem.startsWith(u8, segment, "limit:")) {
                // Found limit parameter
                if (collection_str.len == collection_and_params_str.len) {
                    collection_str = std.mem.trim(u8, collection_and_params_str[0..pos], " \t\n\r");
                }
                const limit_str = std.mem.trim(u8, segment[6..], " \t\n\r");

                const limit_end = std.mem.indexOfScalar(u8, limit_str, ' ') orelse limit_str.len;
                const limit_value = limit_str[0..limit_end];

                limit_expr = try parseExpression(allocator, limit_value);
                current_pos = pos + 6 + limit_end;
            } else if (std.mem.startsWith(u8, segment, "offset:")) {
                // Found offset parameter
                if (collection_str.len == collection_and_params_str.len) {
                    collection_str = std.mem.trim(u8, collection_and_params_str[0..pos], " \t\n\r");
                }
                const offset_str = std.mem.trim(u8, segment[7..], " \t\n\r");

                const offset_end = std.mem.indexOfScalar(u8, offset_str, ' ') orelse offset_str.len;
                const offset_value = offset_str[0..offset_end];

                offset_expr = try parseExpression(allocator, offset_value);
                current_pos = pos + 7 + offset_end;
            } else {
                current_pos = pos + 1;
            }
        } else {
            break;
        }
    }

    // Parse the collection expression
    const collection_expr = try parseExpression(allocator, collection_str);
    errdefer {
        var expr = collection_expr;
        expr.deinit(allocator);
        if (cols_expr) |*c| {
            var ce = c.*;
            ce.deinit(allocator);
        }
        if (limit_expr) |*l| {
            var le = l.*;
            le.deinit(allocator);
        }
        if (offset_expr) |*o| {
            var oe = o.*;
            oe.deinit(allocator);
        }
    }

    const end_label = generateLabelId();
    const loop_start_label = generateLabelId();

    // Emit tablerow_loop instruction
    const item_name_owned = try allocator.dupe(u8, item_name);
    try instructions.append(allocator, Instruction{
        .tablerow_loop = .{
            .item_name = item_name_owned,
            .collection_expr = collection_expr,
            .cols = cols_expr,
            .limit = limit_expr,
            .offset = offset_expr,
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
    try instructions.append(allocator, Instruction{ .end_tablerow_loop = {} });

    // Jump back to loop start (will be handled by VM)
    try instructions.append(allocator, Instruction{ .jump = loop_start_label });

    // End label
    try instructions.append(allocator, Instruction{ .label = end_label });
}

fn convertCaptureToIR(allocator: std.mem.Allocator, tag: *const Tag, instructions: *std.ArrayList(Instruction)) (std.mem.Allocator.Error || error{ UnterminatedString, InvalidAssign, InvalidFor, InvalidCapture, InvalidIncrement, InvalidDecrement, InvalidCycle, InvalidInclude, InvalidRender })!void {
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

fn convertIfChangedToIR(allocator: std.mem.Allocator, tag: *const Tag, instructions: *std.ArrayList(Instruction)) (std.mem.Allocator.Error || error{ UnterminatedString, InvalidAssign, InvalidFor, InvalidCapture, InvalidIncrement, InvalidDecrement, InvalidCycle, InvalidInclude, InvalidRender })!void {
    // Emit start_ifchanged instruction
    try instructions.append(allocator, Instruction{ .start_ifchanged = {} });

    // Generate IR for ifchanged body children
    for (tag.children) |*child| {
        try convertNodeToIR(allocator, child, instructions);
    }

    // Emit end_ifchanged instruction
    try instructions.append(allocator, Instruction{ .end_ifchanged = {} });
}

fn convertIncrementToIR(allocator: std.mem.Allocator, tag: *const Tag, instructions: *std.ArrayList(Instruction)) !void {
    // Parse: increment [counter_name] - counter_name is optional (empty string if omitted)
    const trimmed_tag = std.mem.trim(u8, tag.content, " \t\n\r");
    const counter_name = if (trimmed_tag.len > 9)
        std.mem.trim(u8, trimmed_tag[9..], " \t\n\r") // Skip "increment"
    else
        ""; // Anonymous increment

    // Emit increment instruction
    const counter_name_owned = try allocator.dupe(u8, counter_name);
    try instructions.append(allocator, Instruction{ .increment = counter_name_owned });
}

fn convertDecrementToIR(allocator: std.mem.Allocator, tag: *const Tag, instructions: *std.ArrayList(Instruction)) !void {
    // Parse: decrement [counter_name] - counter_name is optional (empty string if omitted)
    const trimmed_tag = std.mem.trim(u8, tag.content, " \t\n\r");
    const counter_name = if (trimmed_tag.len > 9)
        std.mem.trim(u8, trimmed_tag[9..], " \t\n\r") // Skip "decrement"
    else
        ""; // Anonymous decrement

    // Emit decrement instruction
    const counter_name_owned = try allocator.dupe(u8, counter_name);
    try instructions.append(allocator, Instruction{ .decrement = counter_name_owned });
}

fn convertCycleToIR(allocator: std.mem.Allocator, tag: *const Tag, instructions: *std.ArrayList(Instruction)) !void {
    // Parse: cycle value1, value2, ... or cycle name: value1, value2, ...
    const content = std.mem.trim(u8, tag.content[5..], " \t\n\r"); // Skip "cycle"

    if (content.len == 0) {
        return error.InvalidCycle;
    }

    var name: ?Expression = null;
    var values_str: []const u8 = content;

    // Check if it's a named cycle (contains ':' before first comma, outside quoted strings)
    if (indexOfOutsideStrings(content, ':')) |colon_pos| {
        const comma_pos = indexOfOutsideStrings(content, ',');

        // If colon appears before comma (or there's no comma), it's a named cycle
        if (comma_pos == null or colon_pos < comma_pos.?) {
            const name_part = std.mem.trim(u8, content[0..colon_pos], " \t\n\r");
            values_str = std.mem.trim(u8, content[colon_pos + 1 ..], " \t\n\r");

            // Parse the name as an expression
            name = try parseExpression(allocator, name_part);
        }
    }

    // Parse the values (comma-separated expressions, respecting quoted strings)
    var value_list: std.ArrayList(Expression) = .{};
    errdefer {
        for (value_list.items) |*v| {
            v.deinit(allocator);
        }
        value_list.deinit(allocator);
    }

    var parts = try splitByCommaOutsideStrings(allocator, values_str);
    defer parts.deinit(allocator);

    for (parts.items) |value_part| {
        const trimmed = std.mem.trim(u8, value_part, " \t\n\r");
        if (trimmed.len > 0) {
            const expr = try parseExpression(allocator, trimmed);
            try value_list.append(allocator, expr);
        }
    }

    if (value_list.items.len == 0) {
        if (name) |*n| n.deinit(allocator);
        return error.InvalidCycle;
    }

    // Check if any value contains a variable
    var has_variable = false;
    for (value_list.items) |expr| {
        if (expr == .variable) {
            has_variable = true;
            break;
        }
    }

    // Compute identity key
    // For unnamed cycles with literals, use the stringified value list
    // For named cycles, the key will be computed at runtime by evaluating the name
    var identity_key: []const u8 = undefined;

    if (name != null) {
        // Named cycle - set empty key, will be computed at runtime
        identity_key = try allocator.dupe(u8, "");
    } else {
        // Unnamed cycle - create identity from the expression strings
        // This is a simplified approach; real Liquid has complex identity rules
        var key_builder: std.ArrayList(u8) = .{};
        defer key_builder.deinit(allocator);

        for (value_list.items, 0..) |expr, i| {
            if (i > 0) try key_builder.append(allocator, ',');
            // Create a string representation of the expression
            const expr_str = try expressionToString(allocator, &expr);
            defer allocator.free(expr_str);
            try key_builder.appendSlice(allocator, expr_str);
        }
        identity_key = try key_builder.toOwnedSlice(allocator);
    }

    const values_owned = try value_list.toOwnedSlice(allocator);

    try instructions.append(allocator, Instruction{ .cycle = .{
        .name = name,
        .values = values_owned,
        .identity_key = identity_key,
        .has_variable = has_variable,
    } });
}

/// Extract the first expression from a case condition string.
/// Ruby ignores trailing content like "case 1 bar" -> just uses "1"
fn extractFirstExpression(source: []const u8) []const u8 {
    const trimmed = std.mem.trim(u8, source, " \t\n\r");
    if (trimmed.len == 0) return trimmed;

    // Handle quoted strings - find matching end quote
    if (trimmed[0] == '"' or trimmed[0] == '\'') {
        const quote = trimmed[0];
        var i: usize = 1;
        while (i < trimmed.len) : (i += 1) {
            if (trimmed[i] == quote) {
                return trimmed[0 .. i + 1];
            }
        }
        return trimmed; // Unterminated string, return as-is
    }

    // Handle ranges - find closing paren
    if (trimmed[0] == '(') {
        var i: usize = 1;
        var depth: usize = 1;
        while (i < trimmed.len) : (i += 1) {
            if (trimmed[i] == '(') depth += 1;
            if (trimmed[i] == ')') {
                depth -= 1;
                if (depth == 0) return trimmed[0 .. i + 1];
            }
        }
        return trimmed;
    }

    // Handle negative numbers: -123
    var start: usize = 0;
    if (trimmed[0] == '-' and trimmed.len > 1) {
        start = 1;
    }

    // For other tokens, find the end of the expression
    // This allows for expressions like "foo=>bar" (property access via =>)
    var i: usize = start;
    while (i < trimmed.len) : (i += 1) {
        const c = trimmed[i];
        // Stop at whitespace (but not inside the expression)
        if (c == ' ' or c == '\t' or c == '\n' or c == '\r') {
            break;
        }
    }
    return trimmed[0..i];
}

// Helper to convert expression to string for identity key
fn expressionToString(allocator: std.mem.Allocator, expr: *const Expression) ![]const u8 {
    return switch (expr.*) {
        .string => |s| try std.fmt.allocPrint(allocator, "\"{s}\"", .{s}),
        .integer => |i| try std.fmt.allocPrint(allocator, "{d}", .{i}),
        .float => |f| try std.fmt.allocPrint(allocator, "{d}", .{f}),
        .boolean => |b| try std.fmt.allocPrint(allocator, "{}", .{b}),
        .nil => try allocator.dupe(u8, "nil"),
        .empty => try allocator.dupe(u8, "empty"),
        .blank => try allocator.dupe(u8, "blank"),
        .variable => |v| try std.fmt.allocPrint(allocator, "var_{s}", .{v}),
        .binary_op => try allocator.dupe(u8, "binop"),
        .range => try allocator.dupe(u8, "range"),
    };
}

fn convertIncludeToIR(allocator: std.mem.Allocator, tag: *const Tag, instructions: *std.ArrayList(Instruction)) !void {
    // Parse include syntax:
    //   include 'partial'
    //   include 'partial', var: value
    //   include 'partial' with obj
    //   include 'partial' for items
    const content = std.mem.trim(u8, tag.content[7..], " \t\n\r"); // Skip "include"

    if (content.len == 0) {
        return error.InvalidInclude;
    }

    // Extract partial name first (everything up to first space, comma, or end)
    var partial_end: usize = 0;
    var in_quote = false;
    var quote_char: u8 = 0;
    while (partial_end < content.len) : (partial_end += 1) {
        const c = content[partial_end];
        if ((c == '\'' or c == '"') and !in_quote) {
            in_quote = true;
            quote_char = c;
        } else if (c == quote_char and in_quote) {
            in_quote = false;
            partial_end += 1; // Include closing quote
            break;
        }
    }

    const partial_name_str = std.mem.trim(u8, content[0..partial_end], " \t\n\r");
    const rest = if (partial_end < content.len)
        std.mem.trim(u8, content[partial_end..], " \t\n\r")
    else
        "";

    // Parse the partial name (can be string or variable)
    const partial_name = try parseExpression(allocator, partial_name_str);
    errdefer {
        var expr = partial_name;
        expr.deinit(allocator);
    }

    // Parse variables (key: value pairs)
    var variables: std.ArrayList(Instruction.NamedArg) = .{};
    errdefer {
        for (variables.items) |*v| {
            allocator.free(v.name);
            var val = v.value;
            val.deinit(allocator);
        }
        variables.deinit(allocator);
    }

    // Parse rest of the content
    if (rest.len > 0) {
        if (std.mem.startsWith(u8, rest, "with ")) {
            // Parse: with obj [as alias] [, key: value ...]
            const with_rest = std.mem.trim(u8, rest[5..], " \t\n\r");
            // Find where " as ", comma, or end
            var with_end: usize = with_rest.len;
            var as_pos: ?usize = null;
            var i: usize = 0;
            while (i + 3 < with_rest.len) : (i += 1) {
                // Check for " as "
                if (with_rest[i] == ' ' and std.mem.startsWith(u8, with_rest[i + 1 ..], "as ")) {
                    as_pos = i;
                    break;
                }
            }
            if (std.mem.indexOfScalar(u8, with_rest, ',')) |comma_pos| {
                if (as_pos == null or comma_pos < as_pos.?) {
                    with_end = comma_pos;
                }
            }

            // Parse the with expression
            const with_expr_end = if (as_pos) |ap| ap else with_end;
            const with_expr_str = std.mem.trim(u8, with_rest[0..with_expr_end], " \t\n\r");
            const with_value = try parseExpression(allocator, with_expr_str);

            // Determine variable name (use alias if "as" present, otherwise partial name)
            const var_name: []const u8 = if (as_pos) |ap| blk: {
                // Skip " as " and find end before comma
                const alias_start = ap + 4;
                var alias_end = with_rest.len;
                if (std.mem.indexOfScalar(u8, with_rest[alias_start..], ',')) |cp| {
                    alias_end = alias_start + cp;
                }
                break :blk std.mem.trim(u8, with_rest[alias_start..alias_end], " \t\n\r");
            } else if (partial_name == .string) partial_name.string else "include";

            const name = try allocator.dupe(u8, var_name);
            try variables.append(allocator, .{ .name = name, .value = with_value });

            // Parse remaining variables after comma
            const comma_start = if (as_pos) |ap| blk: {
                const alias_start = ap + 4;
                if (std.mem.indexOfScalar(u8, with_rest[alias_start..], ',')) |cp| {
                    break :blk alias_start + cp;
                }
                break :blk with_rest.len;
            } else with_end;
            if (comma_start < with_rest.len) {
                try parseRenderVariables(allocator, with_rest[comma_start + 1 ..], &variables);
            }
        } else if (std.mem.startsWith(u8, rest, "for ")) {
            // Parse: for items [as alias] [, key: value ...]
            const for_rest = std.mem.trim(u8, rest[4..], " \t\n\r");
            // Find where " as ", comma, or end
            var for_end: usize = for_rest.len;
            var as_pos: ?usize = null;
            var i: usize = 0;
            while (i + 3 < for_rest.len) : (i += 1) {
                if (for_rest[i] == ' ' and std.mem.startsWith(u8, for_rest[i + 1 ..], "as ")) {
                    as_pos = i;
                    break;
                }
            }
            if (std.mem.indexOfScalar(u8, for_rest, ',')) |comma_pos| {
                if (as_pos == null or comma_pos < as_pos.?) {
                    for_end = comma_pos;
                }
            }

            // Parse the collection expression
            const for_expr_end = if (as_pos) |ap| ap else for_end;
            const for_expr_str = std.mem.trim(u8, for_rest[0..for_expr_end], " \t\n\r");
            const for_collection = try parseExpression(allocator, for_expr_str);
            errdefer {
                var fc = for_collection;
                fc.deinit(allocator);
            }

            // Determine item name (use alias if "as" present, otherwise partial name)
            const var_name: []const u8 = if (as_pos) |ap| blk: {
                const alias_start = ap + 4;
                var alias_end = for_rest.len;
                if (std.mem.indexOfScalar(u8, for_rest[alias_start..], ',')) |cp| {
                    alias_end = alias_start + cp;
                }
                break :blk std.mem.trim(u8, for_rest[alias_start..alias_end], " \t\n\r");
            } else if (partial_name == .string) partial_name.string else "include";

            const item_name = try allocator.dupe(u8, var_name);
            errdefer allocator.free(item_name);

            // Parse remaining variables after comma
            const comma_start = if (as_pos) |ap| blk: {
                const alias_start = ap + 4;
                if (std.mem.indexOfScalar(u8, for_rest[alias_start..], ',')) |cp| {
                    break :blk alias_start + cp;
                }
                break :blk for_rest.len;
            } else for_end;
            if (comma_start < for_rest.len) {
                try parseRenderVariables(allocator, for_rest[comma_start + 1 ..], &variables);
            }

            // Emit include instruction with for_collection
            try instructions.append(allocator, Instruction{ .include = .{
                .partial_name = partial_name,
                .variables = try variables.toOwnedSlice(allocator),
                .for_collection = for_collection,
                .item_name = item_name,
            } });
            return;
        } else if (rest[0] == ',') {
            // Just variables: , var: value, var2: value2
            try parseRenderVariables(allocator, rest[1..], &variables);
        }
    }

    try instructions.append(allocator, Instruction{ .include = .{
        .partial_name = partial_name,
        .variables = try variables.toOwnedSlice(allocator),
        .for_collection = null,
        .item_name = null,
    } });
}

fn convertRenderToIR(allocator: std.mem.Allocator, tag: *const Tag, instructions: *std.ArrayList(Instruction)) !void {
    // Parse render syntax:
    //   render 'partial'
    //   render 'partial', var: value
    //   render 'partial' with obj
    //   render 'partial' with obj as alias
    //   render 'partial' with obj, var: value
    //   render 'partial' for items
    //   render 'partial' for items as alias
    const content = std.mem.trim(u8, tag.content[6..], " \t\n\r"); // Skip "render"

    if (content.len == 0) {
        return error.InvalidRender;
    }

    // Extract partial name first (everything up to first space, comma, or end)
    var partial_end: usize = 0;
    var in_quote = false;
    var quote_char: u8 = 0;
    while (partial_end < content.len) : (partial_end += 1) {
        const c = content[partial_end];
        if ((c == '\'' or c == '"') and !in_quote) {
            in_quote = true;
            quote_char = c;
        } else if (c == quote_char and in_quote) {
            in_quote = false;
            partial_end += 1; // Include closing quote
            break;
        }
    }

    const partial_name_str = std.mem.trim(u8, content[0..partial_end], " \t\n\r");
    const rest = if (partial_end < content.len)
        std.mem.trim(u8, content[partial_end..], " \t\n\r")
    else
        "";

    // Parse the partial name (must be quoted string literal)
    if (partial_name_str.len < 2 or
        ((partial_name_str[0] != '\'' or partial_name_str[partial_name_str.len - 1] != '\'') and
        (partial_name_str[0] != '"' or partial_name_str[partial_name_str.len - 1] != '"')))
    {
        return error.InvalidRender;
    }

    const partial_name = try allocator.dupe(u8, partial_name_str[1 .. partial_name_str.len - 1]);
    errdefer allocator.free(partial_name);

    var with_value: ?Expression = null;
    var with_alias: ?[]const u8 = null;
    var for_collection: ?Expression = null;
    var for_alias: ?[]const u8 = null;
    var variables: std.ArrayList(Instruction.NamedArg) = .{};

    errdefer {
        if (with_value) |*w| {
            var wv = w.*;
            wv.deinit(allocator);
        }
        if (with_alias) |wa| allocator.free(wa);
        if (for_collection) |*fc| {
            var fcv = fc.*;
            fcv.deinit(allocator);
        }
        if (for_alias) |fa| allocator.free(fa);
        for (variables.items) |*v| {
            allocator.free(v.name);
            var val = v.value;
            val.deinit(allocator);
        }
        variables.deinit(allocator);
    }

    // Parse rest of the content
    if (rest.len > 0) {
        if (std.mem.startsWith(u8, rest, "with ")) {
            // Parse: with obj [as alias] [, var: value...]
            const with_rest = std.mem.trim(u8, rest[5..], " \t\n\r");

            // Find where 'as' or ',' or end
            var with_end: usize = 0;
            var i: usize = 0;
            while (i < with_rest.len) {
                if (i + 3 <= with_rest.len and std.mem.eql(u8, with_rest[i .. i + 3], " as")) {
                    with_end = i;
                    break;
                } else if (with_rest[i] == ',') {
                    with_end = i;
                    break;
                }
                i += 1;
            }
            if (with_end == 0) with_end = with_rest.len;

            const with_expr_str = std.mem.trim(u8, with_rest[0..with_end], " \t\n\r");
            with_value = try parseExpression(allocator, with_expr_str);

            const after_with = if (with_end < with_rest.len)
                std.mem.trim(u8, with_rest[with_end..], " \t\n\r")
            else
                "";

            // Check for 'as alias'
            if (std.mem.startsWith(u8, after_with, "as ")) {
                const alias_rest = std.mem.trim(u8, after_with[3..], " \t\n\r");
                // Find end of alias (comma or end)
                var alias_end: usize = alias_rest.len;
                if (std.mem.indexOfScalar(u8, alias_rest, ',')) |comma_pos| {
                    alias_end = comma_pos;
                }
                with_alias = try allocator.dupe(u8, std.mem.trim(u8, alias_rest[0..alias_end], " \t\n\r"));
                // Parse remaining variables after comma
                if (alias_end < alias_rest.len) {
                    try parseRenderVariables(allocator, alias_rest[alias_end + 1 ..], &variables);
                }
            } else if (std.mem.startsWith(u8, after_with, ",")) {
                // Parse variables after comma
                try parseRenderVariables(allocator, after_with[1..], &variables);
            }
        } else if (std.mem.startsWith(u8, rest, "for ")) {
            // Parse: for items [as alias] [, var: value...]
            const for_rest = std.mem.trim(u8, rest[4..], " \t\n\r");

            // Find where 'as' or ',' or end
            var for_end: usize = for_rest.len;
            var j: usize = 0;
            while (j < for_rest.len) {
                if (j + 3 <= for_rest.len and std.mem.eql(u8, for_rest[j .. j + 3], " as")) {
                    for_end = j;
                    break;
                } else if (for_rest[j] == ',') {
                    for_end = j;
                    break;
                }
                j += 1;
            }

            const for_expr_str = std.mem.trim(u8, for_rest[0..for_end], " \t\n\r");
            for_collection = try parseExpression(allocator, for_expr_str);

            const after_for = if (for_end < for_rest.len)
                std.mem.trim(u8, for_rest[for_end..], " \t\n\r")
            else
                "";

            // Check for 'as alias'
            if (std.mem.startsWith(u8, after_for, "as ")) {
                const alias_rest = std.mem.trim(u8, after_for[3..], " \t\n\r");
                // Find end of alias (comma or end)
                var alias_end: usize = alias_rest.len;
                if (std.mem.indexOfScalar(u8, alias_rest, ',')) |comma_pos| {
                    alias_end = comma_pos;
                }
                for_alias = try allocator.dupe(u8, std.mem.trim(u8, alias_rest[0..alias_end], " \t\n\r"));
                // Parse remaining variables after comma
                if (alias_end < alias_rest.len) {
                    try parseRenderVariables(allocator, alias_rest[alias_end + 1 ..], &variables);
                }
            } else if (std.mem.startsWith(u8, after_for, ",")) {
                // Parse variables after comma
                try parseRenderVariables(allocator, after_for[1..], &variables);
            }
        } else if (rest[0] == ',') {
            // Just variables: , var: value, var2: value2
            try parseRenderVariables(allocator, rest[1..], &variables);
        } else if (std.mem.indexOfScalar(u8, rest, ':') != null) {
            // Named arguments without comma: key: value, key2: value2
            try parseRenderVariables(allocator, rest, &variables);
        }
    }

    try instructions.append(allocator, Instruction{ .render = .{
        .partial_name = partial_name,
        .variables = try variables.toOwnedSlice(allocator),
        .with_value = with_value,
        .with_alias = with_alias,
        .for_collection = for_collection,
        .for_alias = for_alias,
    } });
}

fn parseRenderVariables(allocator: std.mem.Allocator, content: []const u8, variables: *std.ArrayList(Instruction.NamedArg)) !void {
    // Parse comma-separated key: value pairs
    var parts = std.mem.splitScalar(u8, content, ',');
    while (parts.next()) |part| {
        const trimmed = std.mem.trim(u8, part, " \t\n\r");
        if (trimmed.len == 0) continue;

        // Split by colon to get key: value
        if (std.mem.indexOfScalar(u8, trimmed, ':')) |colon_pos| {
            const key = std.mem.trim(u8, trimmed[0..colon_pos], " \t\n\r");
            const value_str = std.mem.trim(u8, trimmed[colon_pos + 1 ..], " \t\n\r");

            const name = try allocator.dupe(u8, key);
            errdefer allocator.free(name);

            const value = try parseExpression(allocator, value_str);

            try variables.append(allocator, .{ .name = name, .value = value });
        }
    }
}

fn convertEchoToIR(allocator: std.mem.Allocator, tag: *const Tag, instructions: *std.ArrayList(Instruction)) !void {
    // Parse: echo expression | filter1 | filter2
    const content = if (std.mem.startsWith(u8, tag.content, "echo "))
        std.mem.trim(u8, tag.content[5..], " \t\n\r")
    else
        "";

    if (content.len == 0) {
        // Empty echo outputs nothing
        return;
    }

    // Parse expression with filters (like Variable parsing)
    var filters: std.ArrayList(Filter) = .{};
    errdefer {
        for (filters.items) |*f| {
            f.deinit(allocator);
        }
        filters.deinit(allocator);
    }

    // Split by | but respect quoted strings
    var pipe_parts: std.ArrayList([]const u8) = .{};
    defer pipe_parts.deinit(allocator);

    var i: usize = 0;
    var start: usize = 0;
    var in_single_quote = false;
    var in_double_quote = false;
    while (i < content.len) : (i += 1) {
        const c = content[i];
        if (c == '\'' and !in_double_quote) {
            in_single_quote = !in_single_quote;
        } else if (c == '"' and !in_single_quote) {
            in_double_quote = !in_double_quote;
        } else if (c == '|' and !in_single_quote and !in_double_quote) {
            // Check for || (logical or in expressions)
            if (i + 1 < content.len and content[i + 1] == '|') {
                i += 1;
                continue;
            }
            try pipe_parts.append(allocator, content[start..i]);
            start = i + 1;
        }
    }
    if (start < content.len) {
        try pipe_parts.append(allocator, content[start..]);
    }

    if (pipe_parts.items.len == 0) {
        return;
    }

    const expr_str = std.mem.trim(u8, pipe_parts.items[0], " \t\n\r");
    const expr = try parseExpression(allocator, expr_str);
    errdefer {
        var e = expr;
        e.deinit(allocator);
    }

    // Parse filters
    for (pipe_parts.items[1..]) |filter_str| {
        const filter = try Filter.parse(allocator, filter_str);
        errdefer {
            var f = filter;
            f.deinit(allocator);
        }
        try filters.append(allocator, filter);
    }

    // Generate output instruction based on expression type
    switch (expr) {
        .string => |s| {
            if (filters.items.len == 0) {
                try instructions.append(allocator, Instruction{ .output_string = try allocator.dupe(u8, s) });
            } else {
                try instructions.append(allocator, Instruction{
                    .output_literal_with_filters = .{
                        .literal = json.Value{ .string = s },
                        .filters = try filters.toOwnedSlice(allocator),
                        .owned_string = null,
                    },
                });
            }
        },
        .integer => |int| {
            if (filters.items.len == 0) {
                try instructions.append(allocator, Instruction{ .output_integer = int });
            } else {
                try instructions.append(allocator, Instruction{
                    .output_literal_with_filters = .{
                        .literal = json.Value{ .integer = int },
                        .filters = try filters.toOwnedSlice(allocator),
                        .owned_string = null,
                    },
                });
            }
        },
        .variable => |path| {
            const var_path = try allocator.dupe(u8, path);
            try instructions.append(allocator, Instruction{
                .output_variable = .{
                    .path = var_path,
                    .filters = try filters.toOwnedSlice(allocator),
                },
            });
        },
        else => {
            // For other expressions, output nothing
            for (filters.items) |*f| {
                f.deinit(allocator);
            }
        },
    }
}

fn convertLiquidTagToIR(allocator: std.mem.Allocator, tag: *const Tag, instructions: *std.ArrayList(Instruction)) !void {
    // Parse multi-line liquid content
    // Collect all lines first for block structure parsing
    const content = tag.content;

    var line_list: std.ArrayList([]const u8) = .{};
    defer line_list.deinit(allocator);

    var lines_iter = std.mem.splitAny(u8, content, "\n\r");
    while (lines_iter.next()) |line| {
        const trimmed = std.mem.trim(u8, line, " \t");
        if (trimmed.len == 0) continue;
        if (trimmed[0] == '#') continue; // Skip comments
        try line_list.append(allocator, trimmed);
    }

    const lines = line_list.items;
    var idx: usize = 0;

    while (idx < lines.len) {
        const trimmed = lines[idx];

        // Parse each line as a statement
        if (std.mem.startsWith(u8, trimmed, "assign ")) {
            var temp_tag = Tag{
                .tag_type = .assign,
                .content = trimmed,
                .children = &.{},
                .else_children = &.{},
                .elsif_branches = &.{},
            };
            try convertAssignToIR(allocator, &temp_tag, instructions);
            idx += 1;
        } else if (std.mem.startsWith(u8, trimmed, "echo ") or std.mem.eql(u8, trimmed, "echo")) {
            var temp_tag = Tag{
                .tag_type = .echo,
                .content = trimmed,
                .children = &.{},
                .else_children = &.{},
                .elsif_branches = &.{},
            };
            try convertEchoToIR(allocator, &temp_tag, instructions);
            idx += 1;
        } else if (std.mem.startsWith(u8, trimmed, "increment ") or std.mem.eql(u8, trimmed, "increment")) {
            var temp_tag = Tag{
                .tag_type = .increment,
                .content = trimmed,
                .children = &.{},
                .else_children = &.{},
                .elsif_branches = &.{},
            };
            try convertIncrementToIR(allocator, &temp_tag, instructions);
            idx += 1;
        } else if (std.mem.startsWith(u8, trimmed, "decrement ") or std.mem.eql(u8, trimmed, "decrement")) {
            var temp_tag = Tag{
                .tag_type = .decrement,
                .content = trimmed,
                .children = &.{},
                .else_children = &.{},
                .elsif_branches = &.{},
            };
            try convertDecrementToIR(allocator, &temp_tag, instructions);
            idx += 1;
        } else if (std.mem.startsWith(u8, trimmed, "cycle ")) {
            var temp_tag = Tag{
                .tag_type = .cycle,
                .content = trimmed,
                .children = &.{},
                .else_children = &.{},
                .elsif_branches = &.{},
            };
            try convertCycleToIR(allocator, &temp_tag, instructions);
            idx += 1;
        } else if (std.mem.startsWith(u8, trimmed, "for ")) {
            // Handle for...endfor block
            // Find matching endfor
            var depth: usize = 1;
            var end_idx = idx + 1;
            while (end_idx < lines.len) : (end_idx += 1) {
                const inner = lines[end_idx];
                if (std.mem.startsWith(u8, inner, "for ")) {
                    depth += 1;
                } else if (std.mem.eql(u8, inner, "endfor")) {
                    depth -= 1;
                    if (depth == 0) break;
                }
            }

            // Generate for loop IR
            const loop_label = generateLabelId();
            const end_label = generateLabelId();

            // Parse for header: "for i in collection [limit:N] [offset:N] [reversed]"
            const for_content = trimmed[4..]; // Skip "for "
            try convertForHeaderToIR(allocator, for_content, loop_label, end_label, instructions);

            // Process body lines (between for and endfor)
            var body_idx = idx + 1;
            while (body_idx < end_idx) : (body_idx += 1) {
                const body_line = lines[body_idx];
                // Process each body line recursively
                try processLiquidLine(allocator, body_line, instructions);
            }

            // End loop
            try instructions.append(allocator, Instruction{ .end_for_loop = {} });
            try instructions.append(allocator, Instruction{ .jump = loop_label });
            try instructions.append(allocator, Instruction{ .label = end_label });

            idx = end_idx + 1; // Skip past endfor
        } else if (std.mem.eql(u8, trimmed, "endfor")) {
            // Standalone endfor without matching for - skip
            idx += 1;
        } else {
            idx += 1;
        }
    }
}

fn processLiquidLine(allocator: std.mem.Allocator, trimmed: []const u8, instructions: *std.ArrayList(Instruction)) !void {
    // Process a single line within a liquid tag block
    if (std.mem.startsWith(u8, trimmed, "assign ")) {
        var temp_tag = Tag{
            .tag_type = .assign,
            .content = trimmed,
            .children = &.{},
            .else_children = &.{},
            .elsif_branches = &.{},
        };
        try convertAssignToIR(allocator, &temp_tag, instructions);
    } else if (std.mem.startsWith(u8, trimmed, "echo ") or std.mem.eql(u8, trimmed, "echo")) {
        var temp_tag = Tag{
            .tag_type = .echo,
            .content = trimmed,
            .children = &.{},
            .else_children = &.{},
            .elsif_branches = &.{},
        };
        try convertEchoToIR(allocator, &temp_tag, instructions);
    } else if (std.mem.startsWith(u8, trimmed, "increment ") or std.mem.eql(u8, trimmed, "increment")) {
        var temp_tag = Tag{
            .tag_type = .increment,
            .content = trimmed,
            .children = &.{},
            .else_children = &.{},
            .elsif_branches = &.{},
        };
        try convertIncrementToIR(allocator, &temp_tag, instructions);
    } else if (std.mem.startsWith(u8, trimmed, "decrement ") or std.mem.eql(u8, trimmed, "decrement")) {
        var temp_tag = Tag{
            .tag_type = .decrement,
            .content = trimmed,
            .children = &.{},
            .else_children = &.{},
            .elsif_branches = &.{},
        };
        try convertDecrementToIR(allocator, &temp_tag, instructions);
    } else if (std.mem.startsWith(u8, trimmed, "cycle ")) {
        var temp_tag = Tag{
            .tag_type = .cycle,
            .content = trimmed,
            .children = &.{},
            .else_children = &.{},
            .elsif_branches = &.{},
        };
        try convertCycleToIR(allocator, &temp_tag, instructions);
    }
}

fn convertForHeaderToIR(allocator: std.mem.Allocator, for_content: []const u8, loop_label: usize, end_label: usize, instructions: *std.ArrayList(Instruction)) !void {
    // Parse: "i in collection [limit:N] [offset:N] [reversed]"
    const trimmed_content = std.mem.trim(u8, for_content, " \t\n\r");

    // Find "in" keyword
    var in_pos: ?usize = null;
    var i: usize = 0;
    while (i < trimmed_content.len) {
        if (i + 2 < trimmed_content.len and
            trimmed_content[i] == ' ' and
            trimmed_content[i + 1] == 'i' and
            trimmed_content[i + 2] == 'n' and
            (i + 3 >= trimmed_content.len or trimmed_content[i + 3] == ' '))
        {
            in_pos = i;
            break;
        }
        i += 1;
    }

    if (in_pos == null) return;

    const item_name = std.mem.trim(u8, trimmed_content[0..in_pos.?], " \t");
    const rest = std.mem.trim(u8, trimmed_content[in_pos.? + 3 ..], " \t");

    // Parse collection and modifiers
    var collection_end = rest.len;
    var limit_expr: ?Expression = null;
    var offset_expr: ?Expression = null;
    var reversed = false;

    // Find modifiers
    if (std.mem.indexOf(u8, rest, " limit:")) |pos| {
        if (pos < collection_end) collection_end = pos;
        const limit_start = pos + 7;
        var limit_end = limit_start;
        while (limit_end < rest.len and rest[limit_end] != ' ') : (limit_end += 1) {}
        const limit_str = rest[limit_start..limit_end];
        limit_expr = try parseExpression(allocator, limit_str);
    }

    if (std.mem.indexOf(u8, rest, " offset:")) |pos| {
        if (pos < collection_end) collection_end = pos;
        const offset_start = pos + 8;
        var offset_end = offset_start;
        while (offset_end < rest.len and rest[offset_end] != ' ') : (offset_end += 1) {}
        const offset_str = rest[offset_start..offset_end];
        if (!std.mem.eql(u8, offset_str, "continue")) {
            offset_expr = try parseExpression(allocator, offset_str);
        }
        // Note: offset:continue is not fully supported in liquid tag for loops
    }

    if (std.mem.indexOf(u8, rest, " reversed")) |pos| {
        if (pos < collection_end) collection_end = pos;
        reversed = true;
    }

    const collection_str = std.mem.trim(u8, rest[0..collection_end], " \t");

    // Create collection expression
    const collection_expr = try parseExpression(allocator, collection_str);

    // Create continue key from "item_name-collection_str"
    const continue_key = try std.fmt.allocPrint(allocator, "{s}-{s}", .{ item_name, collection_str });

    // Emit for_loop instruction (using existing ForLoop struct)
    try instructions.append(allocator, Instruction{
        .for_loop = .{
            .item_name = try allocator.dupe(u8, item_name),
            .collection_expr = collection_expr,
            .collection_name = continue_key,
            .end_label = end_label,
            .else_label = null, // No else support in liquid tag for loops
            .limit = limit_expr,
            .offset = offset_expr,
            .reversed = reversed,
            .offset_continue = false, // Not supported in liquid tag
        },
    });
    try instructions.append(allocator, Instruction{ .label = loop_label });
}

fn convertCaseToIR(allocator: std.mem.Allocator, tag: *const Tag, instructions: *std.ArrayList(Instruction)) (std.mem.Allocator.Error || error{ UnterminatedString, InvalidAssign, InvalidFor, InvalidCapture, InvalidIncrement, InvalidDecrement, InvalidCycle, InvalidInclude, InvalidRender })!void {
    // Parse: case variable
    // Ruby ignores trailing content after the first expression, so we extract just the first token
    const full_condition = std.mem.trim(u8, tag.content[4..], " \t\n\r"); // Skip "case"
    const condition_str = extractFirstExpression(full_condition);

    // Generate labels
    const end_label = generateLabelId();
    var next_branch_label = generateLabelId();

    // Generate IR for each when branch
    for (tag.when_branches) |*when_branch| {
        // Label for this branch
        try instructions.append(allocator, Instruction{ .label = next_branch_label });

        // Parse when values - can be comma-separated or "or"-separated: when 1, 2, 3 or when 1 or 2 or 3
        // We need to split by comma or " or " and check if ANY match
        var when_values: std.ArrayList([]const u8) = .{};
        defer when_values.deinit(allocator);

        // Split the when value by comma or " or "
        var remaining = when_branch.value;
        while (remaining.len > 0) {
            // Look for comma or " or " separator, respecting quotes
            var in_single_quote = false;
            var in_double_quote = false;
            var sep_pos: ?usize = null;
            var sep_len: usize = 1;

            var i: usize = 0;
            while (i < remaining.len) : (i += 1) {
                const c = remaining[i];
                if (c == '\'' and !in_double_quote) {
                    in_single_quote = !in_single_quote;
                } else if (c == '"' and !in_single_quote) {
                    in_double_quote = !in_double_quote;
                } else if (!in_single_quote and !in_double_quote) {
                    if (c == ',') {
                        sep_pos = i;
                        sep_len = 1;
                        break;
                    } else if (i + 4 <= remaining.len and std.mem.eql(u8, remaining[i .. i + 4], " or ")) {
                        sep_pos = i;
                        sep_len = 4;
                        break;
                    }
                }
            }

            if (sep_pos) |pos| {
                const value = std.mem.trim(u8, remaining[0..pos], " \t\n\r");
                if (value.len > 0) {
                    try when_values.append(allocator, value);
                }
                remaining = remaining[pos + sep_len ..];
            } else {
                const value = std.mem.trim(u8, remaining, " \t\n\r");
                if (value.len > 0) {
                    try when_values.append(allocator, value);
                }
                break;
            }
        }

        // Generate next branch label
        next_branch_label = generateLabelId();

        // If we have multiple when values, generate: if (x==1 or x==2 or x==3)
        // We'll do this by jumping to the block if ANY matches
        const execute_block_label = generateLabelId();

        for (when_values.items, 0..) |when_value, val_idx| {
            // Parse the case expression (fresh for each comparison)
            const case_expr = try parseExpression(allocator, condition_str);
            errdefer {
                var expr = case_expr;
                expr.deinit(allocator);
            }

            // Parse the when value as an expression
            const when_expr = try parseExpression(allocator, when_value);
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

            // If this is the last value, jump to next branch if false
            // Otherwise, jump to execute block if true
            if (val_idx == when_values.items.len - 1) {
                // Last value: if false, go to next branch
                try instructions.append(allocator, Instruction{
                    .jump_if_false = .{
                        .condition = comparison,
                        .target_label = next_branch_label,
                    },
                });
            } else {
                // Not last value: if true, go to execute block
                try instructions.append(allocator, Instruction{
                    .jump_if_true = .{
                        .condition = comparison,
                        .target_label = execute_block_label,
                    },
                });
            }
        }

        // Execute block label (for when any of the values matched)
        if (when_values.items.len > 1) {
            try instructions.append(allocator, Instruction{ .label = execute_block_label });
        }

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
    collection_name: []const u8, // For offset:continue tracking
    base_offset: usize, // Offset applied at start of loop
    end_label: usize,
    owned_array: ?[]json.Value, // Owned copy if needed
    parent_forloop: ?json.Value, // Snapshot of parent's forloop object
    previous_item_value: ?json.Value, // Saved value of shadowed variable (for scope restoration)
};

// Tablerow loop state for tracking tablerow blocks
const TableRowLoopState = struct {
    collection: []const json.Value, // Array elements (after offset/limit applied)
    current_index: usize, // Index in the filtered collection
    item_name: []const u8,
    end_label: usize,
    cols: usize, // Columns per row
    current_col: usize, // Current column in row (1-based)
    current_row: usize, // Current row (1-based)
    owned_array: ?[]json.Value, // Owned copy if needed
};

// Capture state for tracking capture blocks
const CaptureState = struct {
    variable_name: []const u8,
    captured_output: std.ArrayList(u8),
};

const IfChangedState = struct {
    captured_output: std.ArrayList(u8),
};

// VM executes IR instructions
const VM = struct {
    allocator: std.mem.Allocator,
    context: Context,
    output: std.ArrayList(u8),
    loop_stack: std.ArrayList(LoopState),
    tablerow_loop_stack: std.ArrayList(TableRowLoopState),
    capture_stack: std.ArrayList(CaptureState),
    ifchanged_stack: std.ArrayList(IfChangedState), // Stack for ifchanged blocks
    continue_offsets: std.StringHashMap(usize), // Tracks offset:continue positions
    filesystem: *const std.StringHashMap([]const u8), // Reference to template filesystem
    error_mode: ErrorMode,
    inside_render: bool, // True when executing inside a render partial (prohibits include)
    recursion_depth: u32, // Current nesting depth for include/render
    current_partial_name: ?[]const u8, // Name of current partial (for error messages)
    frozen_time: ?i64, // Frozen time for date filter (unix timestamp)
    ifchanged_last_value: ?[]const u8, // Last output from an ifchanged block (for comparison)

    const MAX_RECURSION_DEPTH: u32 = 100;

    pub fn init(allocator: std.mem.Allocator, environment: ?json.Value, filesystem: *const std.StringHashMap([]const u8), error_mode: ErrorMode, frozen_time: ?i64) VM {
        var ctx = Context.init(allocator, environment);
        ctx.frozen_time = frozen_time;
        return .{
            .allocator = allocator,
            .context = ctx,
            .output = .{},
            .loop_stack = .{},
            .tablerow_loop_stack = .{},
            .capture_stack = .{},
            .ifchanged_stack = .{},
            .continue_offsets = std.StringHashMap(usize).init(allocator),
            .filesystem = filesystem,
            .error_mode = error_mode,
            .inside_render = false,
            .recursion_depth = 0,
            .current_partial_name = null,
            .frozen_time = frozen_time,
            .ifchanged_last_value = null,
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
        // Clean up tablerow loop stack
        for (self.tablerow_loop_stack.items) |*loop_state| {
            if (loop_state.owned_array) |arr| {
                self.allocator.free(arr);
            }
        }
        self.tablerow_loop_stack.deinit(self.allocator);
        // Clean up capture stack
        for (self.capture_stack.items) |*capture_state| {
            capture_state.captured_output.deinit(self.allocator);
        }
        self.capture_stack.deinit(self.allocator);
        // Clean up ifchanged stack
        for (self.ifchanged_stack.items) |*ifchanged_state| {
            ifchanged_state.captured_output.deinit(self.allocator);
        }
        self.ifchanged_stack.deinit(self.allocator);
        if (self.ifchanged_last_value) |v| {
            self.allocator.free(v);
        }
        // Clean up continue_offsets
        self.continue_offsets.deinit();
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
            const next_pc = self.executeInstruction(inst, &label_map, ir, pc) catch |err| {
                // BreakOutsideLoop means break was called (possibly in an include)
                if (err == error.BreakOutsideLoop) {
                    // If we're in a loop, perform the break (exit the loop)
                    if (self.loop_stack.items.len > 0) {
                        const loop_state = &self.loop_stack.items[self.loop_stack.items.len - 1];
                        const end_label = loop_state.end_label;
                        const prev_value = loop_state.previous_item_value;

                        // Restore previous value of loop variable
                        if (prev_value) |prev| {
                            self.context.set(loop_state.item_name, prev) catch {};
                        } else {
                            _ = self.context.remove(loop_state.item_name);
                        }
                        _ = self.context.remove("forloop");

                        // Pop loop state and free resources
                        if (self.loop_stack.pop()) |popped_state| {
                            if (popped_state.owned_array) |arr| {
                                self.allocator.free(arr);
                            }
                        }

                        // Jump to the end label
                        pc = label_map.get(end_label) orelse break;
                        continue;
                    } else if (self.tablerow_loop_stack.items.len > 0) {
                        const loop_state = &self.tablerow_loop_stack.items[self.tablerow_loop_stack.items.len - 1];
                        const end_label = loop_state.end_label;
                        _ = self.tablerow_loop_stack.pop();
                        pc = label_map.get(end_label) orelse break;
                        continue;
                    }
                    // No loop on stack - stop rendering
                    break;
                }
                // ContinueOutsideLoop means continue was called (possibly in an include)
                if (err == error.ContinueOutsideLoop) {
                    // If we're in a loop, jump to end_for_loop to trigger next iteration
                    if (self.loop_stack.items.len > 0) {
                        // Search for end_for_loop in the IR
                        var search_pc = pc + 1;
                        var nesting_depth: i32 = 0;
                        while (search_pc < ir.instructions.len) : (search_pc += 1) {
                            const search_inst = &ir.instructions[search_pc];
                            switch (search_inst.*) {
                                .for_loop, .tablerow_loop => nesting_depth += 1,
                                .end_for_loop => {
                                    if (nesting_depth == 0) {
                                        pc = search_pc;
                                        break;
                                    }
                                    nesting_depth -= 1;
                                },
                                else => {},
                            }
                        } else {
                            // Couldn't find end_for_loop - stop
                            break;
                        }
                        continue;
                    } else if (self.tablerow_loop_stack.items.len > 0) {
                        // Search for end_tablerow_loop
                        var search_pc = pc + 1;
                        var nesting_depth: i32 = 0;
                        while (search_pc < ir.instructions.len) : (search_pc += 1) {
                            const search_inst = &ir.instructions[search_pc];
                            switch (search_inst.*) {
                                .for_loop, .tablerow_loop => nesting_depth += 1,
                                .end_tablerow_loop => {
                                    if (nesting_depth == 0) {
                                        pc = search_pc;
                                        break;
                                    }
                                    nesting_depth -= 1;
                                },
                                else => {},
                            }
                        } else {
                            break;
                        }
                        continue;
                    }
                    // No loop on stack - stop rendering
                    break;
                }
                return err;
            };
            if (next_pc) |new_pc| {
                pc = new_pc;
            } else {
                pc += 1;
            }
        }

        return try self.output.toOwnedSlice(self.allocator);
    }

    fn executeInstruction(self: *VM, inst: *const Instruction, label_map: *const std.AutoHashMap(usize, usize), ir: *const IR, pc: usize) anyerror!?usize {
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
            .output_range => |*range_inst| {
                // Evaluate start and end expressions
                var start_expr = range_inst.start;
                var end_expr = range_inst.end;
                const start_val = start_expr.evaluate(&self.context);
                const end_val = end_expr.evaluate(&self.context);

                // Get the start and end as integers for the string representation
                const start_int: i64 = if (start_val) |v| switch (v) {
                    .integer => |i| i,
                    .float => |f| @intFromFloat(f),
                    else => 0,
                } else 0;
                const end_int: i64 = if (end_val) |v| switch (v) {
                    .integer => |i| i,
                    .float => |f| @intFromFloat(f),
                    else => 0,
                } else 0;

                // Create the range string "start..end"
                const range_str = try std.fmt.allocPrint(self.allocator, "{d}..{d}", .{ start_int, end_int });
                defer self.allocator.free(range_str);

                // Apply filters if any
                var current_value: json.Value = .{ .string = range_str };
                var temp_strings: std.ArrayList([]u8) = .{};
                defer {
                    for (temp_strings.items) |s| {
                        self.allocator.free(s);
                    }
                    temp_strings.deinit(self.allocator);
                }

                for (range_inst.filters) |*filter| {
                    const result = try filter.applyValueWithContext(self.allocator, current_value, &self.context);
                    switch (result) {
                        .string => |s| {
                            try temp_strings.append(self.allocator, s);
                            current_value = json.Value{ .string = s };
                        },
                        .json_value => |v| {
                            current_value = v;
                        },
                        .error_message => |s| {
                            // Output error message and stop processing
                            try self.getActiveOutput().appendSlice(self.allocator, s);
                            self.allocator.free(s);
                            return null;
                        },
                    }
                }

                try renderValue(current_value, self.getActiveOutput(), self.allocator);
            },
            .output_variable => |*var_inst| {
                // Look up variable and apply filters
                const value = self.context.get(var_inst.path);

                // If variable not found and no filters, output nothing
                if (value == null and var_inst.filters.len == 0) {
                    return null;
                }

                // Use null value if variable not found (filters like default can handle it)
                var current_value = value orelse json.Value{ .null = {} };
                var temp_strings: std.ArrayList([]u8) = .{};
                defer {
                    for (temp_strings.items) |s| {
                        self.allocator.free(s);
                    }
                    temp_strings.deinit(self.allocator);
                }

                for (var_inst.filters) |*filter| {
                    const result = try filter.applyValueWithContext(self.allocator, current_value, &self.context);
                    switch (result) {
                        .string => |s| {
                            try temp_strings.append(self.allocator, s);
                            current_value = json.Value{ .string = s };
                        },
                        .json_value => |v| {
                            current_value = v;
                        },
                        .error_message => |s| {
                            // Output error message and stop processing
                            try self.getActiveOutput().appendSlice(self.allocator, s);
                            self.allocator.free(s);
                            return null;
                        },
                    }
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
                    const result = try filter.applyValueWithContext(self.allocator, current_value, &self.context);
                    switch (result) {
                        .string => |s| {
                            try temp_strings.append(self.allocator, s);
                            current_value = json.Value{ .string = s };
                        },
                        .json_value => |v| {
                            current_value = v;
                        },
                        .error_message => |s| {
                            // Output error message and stop processing
                            try self.getActiveOutput().appendSlice(self.allocator, s);
                            self.allocator.free(s);
                            return null;
                        },
                    }
                }

                try renderValue(current_value, self.getActiveOutput(), self.allocator);
            },
            .assign => |*assign_inst| {
                // Evaluate the expression
                var expr = assign_inst.expression;
                var current_value = expr.evaluate(&self.context) orelse json.Value{ .null = {} };

                // Apply filters if any
                var temp_strings: std.ArrayList([]u8) = .{};
                defer {
                    // Don't free the last string if it's being stored
                    const items = temp_strings.items;
                    if (items.len > 1) {
                        for (items[0 .. items.len - 1]) |s| {
                            self.allocator.free(s);
                        }
                    }
                    temp_strings.deinit(self.allocator);
                }

                for (assign_inst.filters) |*filter| {
                    const result = try filter.applyValueWithContext(self.allocator, current_value, &self.context);
                    switch (result) {
                        .string => |s| {
                            try temp_strings.append(self.allocator, s);
                            current_value = json.Value{ .string = s };
                        },
                        .json_value => |v| {
                            current_value = v;
                        },
                        .error_message => |s| {
                            // Output error message and stop processing
                            try self.getActiveOutput().appendSlice(self.allocator, s);
                            self.allocator.free(s);
                            return null;
                        },
                    }
                }

                // For string values, we need to dupe since the string might come from
                // a partial template that will be deinitialized
                if (current_value == .string) {
                    const duped = try self.allocator.dupe(u8, current_value.string);
                    try self.context.setOwnedString(assign_inst.variable_name, duped);
                } else {
                    try self.context.set(assign_inst.variable_name, current_value);
                }
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
                        var items = coll.array.items;
                        const original_len = items.len;

                        // Evaluate offset parameter
                        var offset: usize = 0;
                        if (for_inst.offset_continue) {
                            // Use stored continue offset
                            if (self.continue_offsets.get(for_inst.collection_name)) |stored_offset| {
                                offset = stored_offset;
                            }
                        } else if (for_inst.offset) |*offset_expr| {
                            var offset_expr_mut = offset_expr.*;
                            const offset_value = offset_expr_mut.evaluate(&self.context);
                            if (offset_value) |v| {
                                if (v == .integer) {
                                    offset = @intCast(@max(0, v.integer));
                                }
                            }
                        }

                        // Evaluate limit parameter
                        var limit: ?usize = null;
                        if (for_inst.limit) |*limit_expr| {
                            var limit_expr_mut = limit_expr.*;
                            const limit_value = limit_expr_mut.evaluate(&self.context);
                            if (limit_value) |v| {
                                if (v == .integer) {
                                    limit = @intCast(@max(0, v.integer));
                                }
                            }
                        }

                        // Apply offset
                        if (offset >= items.len) {
                            // Offset beyond array - jump to else block if exists, otherwise end
                            // Still store the offset for future offset:continue
                            try self.continue_offsets.put(for_inst.collection_name, original_len);
                            const target_label = for_inst.else_label orelse for_inst.end_label;
                            const target_pc = label_map.get(target_label) orelse return error.InvalidLabel;
                            return target_pc;
                        }
                        items = items[offset..];

                        // Apply limit
                        if (limit) |l| {
                            if (l < items.len) {
                                items = items[0..l];
                            }
                        }

                        if (items.len == 0) {
                            // Empty array - jump to else block if exists, otherwise end
                            const target_label = for_inst.else_label orelse for_inst.end_label;
                            const target_pc = label_map.get(target_label) orelse return error.InvalidLabel;
                            return target_pc;
                        }

                        // Handle reversed
                        var final_items: []const json.Value = undefined;
                        var owned_array: ?[]json.Value = null;

                        if (for_inst.reversed) {
                            // Create a reversed copy
                            var reversed_items = try self.allocator.alloc(json.Value, items.len);
                            for (items, 0..) |item, i| {
                                reversed_items[items.len - 1 - i] = item;
                            }
                            final_items = reversed_items;
                            owned_array = reversed_items;
                        } else {
                            final_items = items;
                        }

                        // Capture parent forloop before pushing new loop
                        const parent_forloop = self.context.get("forloop");

                        // Capture current value of loop variable for later restoration
                        const previous_item_value = self.context.get(for_inst.item_name);

                        // Calculate items processed (for offset:continue)
                        // items_processed = offset + final_items.len (limit applies)
                        const items_processed = offset + final_items.len;

                        // Push loop state onto stack
                        try self.loop_stack.append(self.allocator, LoopState{
                            .collection = final_items,
                            .current_index = 0,
                            .item_name = for_inst.item_name,
                            .collection_name = for_inst.collection_name,
                            .base_offset = items_processed, // Store where we'll end up
                            .end_label = for_inst.end_label,
                            .owned_array = owned_array,
                            .parent_forloop = parent_forloop,
                            .previous_item_value = previous_item_value,
                        });

                        // Set up the first iteration
                        try self.setupLoopIteration();
                    } else {
                        // Non-array - jump to else block if exists, otherwise end
                        const target_label = for_inst.else_label orelse for_inst.end_label;
                        const target_pc = label_map.get(target_label) orelse return error.InvalidLabel;
                        return target_pc;
                    }
                } else {
                    // Null collection - jump to else block if exists, otherwise end
                    const target_label = for_inst.else_label orelse for_inst.end_label;
                    const target_pc = label_map.get(target_label) orelse return error.InvalidLabel;
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
                        // Loop is done - restore previous value of loop variable
                        if (loop_state.previous_item_value) |prev| {
                            try self.context.set(loop_state.item_name, prev);
                        } else {
                            _ = self.context.remove(loop_state.item_name);
                        }
                        _ = self.context.remove("forloop");

                        // Store continue offset for offset:continue
                        try self.continue_offsets.put(loop_state.collection_name, loop_state.base_offset);

                        // Pop state and jump to end
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
            .tablerow_loop => |*tablerow_inst| {
                // Evaluate the collection expression
                var expr = tablerow_inst.collection_expr;
                const collection_value = expr.evaluate(&self.context);

                if (collection_value) |coll| {
                    // Handle array collections
                    if (coll == .array) {
                        var items = coll.array.items;

                        // Apply offset and limit
                        var offset: usize = 0;
                        if (tablerow_inst.offset) |*offset_expr| {
                            var oe = offset_expr.*;
                            if (oe.evaluate(&self.context)) |offset_val| {
                                if (offset_val == .integer) {
                                    offset = @intCast(@max(0, offset_val.integer));
                                }
                            }
                        }

                        var limit: ?usize = null;
                        if (tablerow_inst.limit) |*limit_expr| {
                            var le = limit_expr.*;
                            const limit_val = le.evaluate(&self.context);
                            if (limit_val) |lv| {
                                if (lv == .integer) {
                                    limit = @intCast(@max(0, lv.integer));
                                } else if (lv == .null) {
                                    // nil limit is treated as 0
                                    limit = 0;
                                }
                            } else {
                                // null/undefined limit is treated as 0
                                limit = 0;
                            }
                        }

                        // Apply offset
                        if (offset >= items.len) {
                            items = &[_]json.Value{};
                        } else {
                            items = items[offset..];
                        }

                        // Apply limit
                        if (limit) |lim| {
                            if (lim == 0) {
                                items = &[_]json.Value{};
                            } else if (lim < items.len) {
                                items = items[0..lim];
                            }
                        }

                        if (items.len == 0) {
                            // Empty collection - output empty row structure and jump to end
                            try self.getActiveOutput().appendSlice(self.allocator, "<tr class=\"row1\">\n</tr>\n");
                            const target_pc = label_map.get(tablerow_inst.end_label) orelse return error.InvalidLabel;
                            return target_pc;
                        }

                        // Evaluate cols parameter
                        var cols: usize = items.len; // Default: all items in one row
                        if (tablerow_inst.cols) |*cols_expr| {
                            var ce = cols_expr.*;
                            const cols_val = ce.evaluate(&self.context);
                            if (cols_val) |cv| {
                                if (cv == .integer) {
                                    cols = @intCast(@max(1, cv.integer));
                                } else if (cv == .null) {
                                    // nil cols means infinite columns (no row breaks)
                                    cols = std.math.maxInt(usize);
                                }
                            } else {
                                // undefined cols means infinite columns
                                cols = std.math.maxInt(usize);
                            }
                        }

                        // Push loop state onto stack
                        try self.tablerow_loop_stack.append(self.allocator, TableRowLoopState{
                            .collection = items,
                            .current_index = 0,
                            .item_name = tablerow_inst.item_name,
                            .end_label = tablerow_inst.end_label,
                            .cols = cols,
                            .current_col = 1,
                            .current_row = 1,
                            .owned_array = null,
                        });

                        // Output opening <tr> tag
                        try self.getActiveOutput().appendSlice(self.allocator, "<tr class=\"row1\">\n");

                        // Set up the first iteration
                        try self.setupTableRowIteration();
                    } else {
                        // Non-array - jump to end (no iteration)
                        const target_pc = label_map.get(tablerow_inst.end_label) orelse return error.InvalidLabel;
                        return target_pc;
                    }
                } else {
                    // Null collection - jump to end
                    const target_pc = label_map.get(tablerow_inst.end_label) orelse return error.InvalidLabel;
                    return target_pc;
                }
            },
            .end_tablerow_loop => {
                // Check if we have more iterations
                if (self.tablerow_loop_stack.items.len > 0) {
                    const loop_state = &self.tablerow_loop_stack.items[self.tablerow_loop_stack.items.len - 1];

                    // Close the current <td> tag
                    try self.getActiveOutput().appendSlice(self.allocator, "</td>");

                    loop_state.current_index += 1;

                    if (loop_state.current_index < loop_state.collection.len) {
                        // Check if we need to start a new row
                        if (loop_state.current_col >= loop_state.cols) {
                            // Close current row and start new row
                            try self.getActiveOutput().appendSlice(self.allocator, "</tr>\n");
                            loop_state.current_row += 1;
                            loop_state.current_col = 1;

                            // Output opening <tr> tag with row class
                            try self.getActiveOutput().writer(self.allocator).print("<tr class=\"row{d}\">", .{loop_state.current_row});
                        } else {
                            loop_state.current_col += 1;
                        }

                        // Set up next iteration
                        try self.setupTableRowIteration();
                        // Continue to next instruction (which will be the jump back)
                    } else {
                        // Loop is done - close final row and pop state
                        try self.getActiveOutput().appendSlice(self.allocator, "</tr>\n");

                        const end_label = loop_state.end_label;
                        const item_name = loop_state.item_name;
                        if (self.tablerow_loop_stack.pop()) |popped_state| {
                            if (popped_state.owned_array) |arr| {
                                self.allocator.free(arr);
                            }
                        }
                        // Remove the loop variable from scope (tablerow isolates its variable)
                        _ = self.context.remove(item_name);
                        _ = self.context.remove("tablerowloop");
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
            .start_ifchanged => {
                // Start capturing output into a new buffer
                try self.ifchanged_stack.append(self.allocator, IfChangedState{
                    .captured_output = .{},
                });
            },
            .end_ifchanged => {
                // End capturing and output only if different from last time
                if (self.ifchanged_stack.items.len > 0) {
                    const last_idx = self.ifchanged_stack.items.len - 1;
                    var ifchanged_state = self.ifchanged_stack.items[last_idx];
                    const captured_str = try ifchanged_state.captured_output.toOwnedSlice(self.allocator);

                    // Remove from stack BEFORE calling getActiveOutput so we write to parent
                    _ = self.ifchanged_stack.orderedRemove(last_idx);

                    // Compare with previous value
                    const should_output = if (self.ifchanged_last_value) |prev| blk: {
                        break :blk !std.mem.eql(u8, captured_str, prev);
                    } else true; // First time always outputs

                    if (should_output) {
                        // Output the captured content to parent output
                        try self.getActiveOutput().appendSlice(self.allocator, captured_str);
                        // Update last value
                        if (self.ifchanged_last_value) |prev| {
                            self.allocator.free(prev);
                        }
                        self.ifchanged_last_value = captured_str;
                    } else {
                        // Discard the captured content
                        self.allocator.free(captured_str);
                    }
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
            .cycle => |*cycle_inst| {
                // Determine the identity key
                var identity_key: []const u8 = undefined;
                var owned_key: ?[]const u8 = null;
                defer if (owned_key) |k| self.allocator.free(k);

                if (cycle_inst.name) |name_expr| {
                    // Named cycle - evaluate the name expression to get the key
                    var name_expr_mut = name_expr;
                    const name_value = name_expr_mut.evaluate(&self.context);
                    if (name_value) |v| {
                        owned_key = try valueToString(self.allocator, v);
                        identity_key = owned_key.?;
                    } else {
                        identity_key = "";
                    }
                } else if (cycle_inst.has_variable) {
                    // Unnamed cycle with variables - each occurrence has independent counter
                    // Use PC to differentiate between different cycle tag instances
                    owned_key = try std.fmt.allocPrint(self.allocator, "{s}@{d}", .{ cycle_inst.identity_key, pc });
                    identity_key = owned_key.?;
                } else {
                    // Unnamed cycle with only literals - use the pre-computed identity key
                    identity_key = cycle_inst.identity_key;
                }

                // Get the current index for this cycle
                const index = try self.context.cycle(identity_key, cycle_inst.values.len);

                // Get the value at the current index
                var value_expr = cycle_inst.values[index];
                const value = value_expr.evaluate(&self.context);

                // Output the value
                if (value) |v| {
                    const value_str = try valueToString(self.allocator, v);
                    defer self.allocator.free(value_str);
                    try self.getActiveOutput().appendSlice(self.allocator, value_str);
                }
            },
            .include => |*include_inst| {
                try self.executeInclude(include_inst);
            },
            .render => |*render_inst| {
                try self.executeRender(render_inst);
            },
            .loop_break => {
                // Break out of the current loop - pop the state and jump past end
                if (self.loop_stack.items.len > 0) {
                    const loop_state = &self.loop_stack.items[self.loop_stack.items.len - 1];
                    const end_label = loop_state.end_label;

                    // Check if the loop's end_label is in our label_map
                    // If not, we're in an include and need to propagate the break up
                    if (label_map.get(end_label)) |target_pc| {
                        // Before breaking, complete any pending capture blocks
                        while (self.capture_stack.items.len > 0) {
                            const last_idx = self.capture_stack.items.len - 1;
                            var capture_state = self.capture_stack.items[last_idx];
                            const captured_str = try capture_state.captured_output.toOwnedSlice(self.allocator);
                            const value = json.Value{ .string = captured_str };
                            try self.context.set(capture_state.variable_name, value);
                            _ = self.capture_stack.orderedRemove(last_idx);
                        }

                        const prev_value = loop_state.previous_item_value;

                        // Restore previous value of loop variable
                        if (prev_value) |prev| {
                            try self.context.set(loop_state.item_name, prev);
                        } else {
                            _ = self.context.remove(loop_state.item_name);
                        }
                        _ = self.context.remove("forloop");

                        // Store continue offset for offset:continue
                        try self.continue_offsets.put(loop_state.collection_name, loop_state.base_offset + loop_state.current_index);

                        // Pop loop state and free resources
                        if (self.loop_stack.pop()) |popped_state| {
                            if (popped_state.owned_array) |arr| {
                                self.allocator.free(arr);
                            }
                        }

                        // Jump to the end label (past end_for_loop)
                        return target_pc;
                    } else {
                        // Loop is in an outer template - propagate break
                        return error.BreakOutsideLoop;
                    }
                } else if (self.tablerow_loop_stack.items.len > 0) {
                    const loop_state = &self.tablerow_loop_stack.items[self.tablerow_loop_stack.items.len - 1];
                    const end_label = loop_state.end_label;
                    const item_name = loop_state.item_name;

                    // Check if the loop's end_label is in our label_map
                    if (label_map.get(end_label)) |target_pc| {
                        // Close the current cell and row properly
                        try self.getActiveOutput().appendSlice(self.allocator, "</td></tr>\n");

                        if (self.tablerow_loop_stack.pop()) |popped_state| {
                            if (popped_state.owned_array) |arr| {
                                self.allocator.free(arr);
                            }
                        }
                        // Remove loop variables from scope
                        _ = self.context.remove(item_name);
                        _ = self.context.remove("tablerowloop");
                        return target_pc;
                    } else {
                        // Loop is in an outer template - propagate break
                        return error.BreakOutsideLoop;
                    }
                }
                // If not in a loop, break stops template rendering entirely
                return error.BreakOutsideLoop;
            },
            .loop_continue => {
                // Continue to next iteration of the current loop
                // First, check if we're in a loop and the end label exists in our label_map
                if (self.loop_stack.items.len > 0) {
                    const loop_state = &self.loop_stack.items[self.loop_stack.items.len - 1];
                    // Search for end_for_loop in the current IR
                    var search_pc = pc + 1;
                    var nesting_depth: i32 = 0;
                    while (search_pc < ir.instructions.len) : (search_pc += 1) {
                        const search_inst = &ir.instructions[search_pc];
                        switch (search_inst.*) {
                            .for_loop, .tablerow_loop => nesting_depth += 1,
                            .end_for_loop => {
                                if (nesting_depth == 0) return search_pc;
                                nesting_depth -= 1;
                            },
                            else => {},
                        }
                    }
                    // end_for_loop not found in current IR - we're in an include
                    // Check if the loop's end_label is in our label_map
                    if (label_map.get(loop_state.end_label)) |_| {
                        // Loop is in this template - shouldn't happen, but handle it
                        return search_pc;
                    }
                    // Loop is in parent template - propagate continue
                    return error.ContinueOutsideLoop;
                } else if (self.tablerow_loop_stack.items.len > 0) {
                    // Search for end_tablerow_loop in the current IR
                    var search_pc = pc + 1;
                    var nesting_depth: i32 = 0;
                    while (search_pc < ir.instructions.len) : (search_pc += 1) {
                        const search_inst = &ir.instructions[search_pc];
                        switch (search_inst.*) {
                            .for_loop, .tablerow_loop => nesting_depth += 1,
                            .end_tablerow_loop => {
                                if (nesting_depth == 0) return search_pc;
                                nesting_depth -= 1;
                            },
                            else => {},
                        }
                    }
                    // Not found - propagate continue
                    return error.ContinueOutsideLoop;
                }
                // If not in a loop, continue stops template rendering entirely
                return error.BreakOutsideLoop;
            },
        }
        return null; // Continue to next instruction
    }

    fn getActiveOutput(self: *VM) *std.ArrayList(u8) {
        // If we're inside an ifchanged block, write to the ifchanged buffer
        if (self.ifchanged_stack.items.len > 0) {
            return &self.ifchanged_stack.items[self.ifchanged_stack.items.len - 1].captured_output;
        }
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
        try forloop_obj.put("rindex", json.Value{ .integer = @intCast(length - index) }); // 1-based from end
        try forloop_obj.put("rindex0", json.Value{ .integer = @intCast(length - index - 1) }); // 0-based from end
        try forloop_obj.put("first", json.Value{ .bool = index == 0 });
        try forloop_obj.put("last", json.Value{ .bool = index == length - 1 });
        try forloop_obj.put("length", json.Value{ .integer = @intCast(length) });

        // Set parentloop using the stored parent forloop (captured when entering the loop)
        if (loop_state.parent_forloop) |pf| {
            try forloop_obj.put("parentloop", pf);
        }

        try self.context.set("forloop", json.Value{ .object = forloop_obj });
    }

    fn setupTableRowIteration(self: *VM) !void {
        if (self.tablerow_loop_stack.items.len == 0) return;

        const loop_state = &self.tablerow_loop_stack.items[self.tablerow_loop_stack.items.len - 1];
        const item = loop_state.collection[loop_state.current_index];

        // Set the loop item variable
        try self.context.set(loop_state.item_name, item);

        // Set tablerowloop object with properties
        const index = loop_state.current_index;
        const length = loop_state.collection.len;
        const col = loop_state.current_col;
        const row = loop_state.current_row;

        // Create tablerowloop object
        var tablerowloop_obj = json.ObjectMap.init(self.allocator);
        try tablerowloop_obj.put("index", json.Value{ .integer = @intCast(index + 1) }); // 1-based
        try tablerowloop_obj.put("index0", json.Value{ .integer = @intCast(index) }); // 0-based
        try tablerowloop_obj.put("first", json.Value{ .bool = index == 0 });
        try tablerowloop_obj.put("last", json.Value{ .bool = index == length - 1 });
        try tablerowloop_obj.put("length", json.Value{ .integer = @intCast(length) });
        try tablerowloop_obj.put("col", json.Value{ .integer = @intCast(col) });
        try tablerowloop_obj.put("col0", json.Value{ .integer = @intCast(col - 1) });
        try tablerowloop_obj.put("row", json.Value{ .integer = @intCast(row) });
        try tablerowloop_obj.put("col_first", json.Value{ .bool = col == 1 });
        // col_last is true only when we're at the last column in the row
        // When cols is infinite (nil), col_last is always false
        const is_col_last = if (loop_state.cols == std.math.maxInt(usize))
            false
        else
            col == loop_state.cols or index == length - 1;
        try tablerowloop_obj.put("col_last", json.Value{ .bool = is_col_last });

        try self.context.set("tablerowloop", json.Value{ .object = tablerowloop_obj });

        // Output <td> tag with column class
        try self.getActiveOutput().writer(self.allocator).print("<td class=\"col{d}\">", .{col});
    }

    fn executeInclude(self: *VM, include_inst: *const Instruction.Include) !void {
        // Include is prohibited inside render partials
        if (self.inside_render) {
            try self.getActiveOutput().appendSlice(self.allocator, "Liquid error: include usage is not allowed in this context");
            return;
        }

        // Check recursion depth (include limit is 99, render limit is 100)
        if (self.recursion_depth >= MAX_RECURSION_DEPTH - 1) {
            // For include, emit inline error message with partial name
            if (self.current_partial_name) |name| {
                const err_msg = try std.fmt.allocPrint(self.allocator, "Liquid error ({s} line 2): Nesting too deep", .{name});
                defer self.allocator.free(err_msg);
                try self.getActiveOutput().appendSlice(self.allocator, err_msg);
            } else {
                try self.getActiveOutput().appendSlice(self.allocator, "Liquid error (line 2): Nesting too deep");
            }
            return;
        }

        // Evaluate the partial name expression
        var partial_name_expr = include_inst.partial_name;
        const partial_name_value = partial_name_expr.evaluate(&self.context);

        if (partial_name_value == null or partial_name_value.? != .string) {
            // If partial name can't be evaluated to a string, output nothing
            return;
        }

        const partial_name = partial_name_value.?.string;

        // Look up the partial in the filesystem
        // Try direct lookup first, then with .liquid extension
        const partial_source = self.filesystem.get(partial_name) orelse blk: {
            // Try with .liquid extension
            const with_ext = std.fmt.allocPrint(self.allocator, "{s}.liquid", .{partial_name}) catch return;
            defer self.allocator.free(with_ext);
            break :blk self.filesystem.get(with_ext) orelse {
                // Partial not found - in lax mode, just output nothing
                return;
            };
        };

        // Parse the partial template (without its own filesystem to prevent nested includes)
        var partial_template = Template.parse(self.allocator, partial_source, null, self.error_mode) catch {
            // Parse error - output nothing
            return;
        };
        defer partial_template.deinit();

        // Handle 'for' iteration if specified
        if (include_inst.for_collection) |*for_coll| {
            var coll_expr = for_coll.*;
            const coll_value = coll_expr.evaluate(&self.context);

            if (coll_value) |coll| {
                if (coll == .array) {
                    const items = coll.array.items;
                    const item_name = if (include_inst.item_name) |name| name else partial_name;

                    for (items) |item| {
                        // Set the item variable in the shared context
                        try self.context.set(item_name, item);

                        // Evaluate and set any additional variables
                        for (include_inst.variables) |*var_arg| {
                            var value_expr = var_arg.value;
                            const value = value_expr.evaluate(&self.context);
                            if (value) |v| {
                                try self.context.set(var_arg.name, v);
                            }
                        }

                        // Execute the partial
                        try self.executeIncludePartial(&partial_template, partial_name);
                    }
                    return;
                }
            }
        }

        // Standard include (no 'for' iteration)
        // Evaluate and set variables passed to the partial (they're set in the shared context)
        for (include_inst.variables) |*var_arg| {
            var value_expr = var_arg.value;
            const value = value_expr.evaluate(&self.context);
            if (value) |v| {
                try self.context.set(var_arg.name, v);
            }
        }

        try self.executeIncludePartial(&partial_template, partial_name);
    }

    fn executeIncludePartial(self: *VM, partial_template: *Template, partial_name: []const u8) !void {
        // Increment recursion depth and track partial name
        self.recursion_depth += 1;
        const old_partial_name = self.current_partial_name;
        self.current_partial_name = partial_name;
        defer {
            self.recursion_depth -= 1;
            self.current_partial_name = old_partial_name;
        }

        // Execute the partial's IR using the current VM state (shared scope)
        if (partial_template.ir) |*partial_ir| {
            // Build label map for the partial
            var label_map = std.AutoHashMap(usize, usize).init(self.allocator);
            defer label_map.deinit();

            for (partial_ir.instructions, 0..) |*inst, i| {
                if (inst.* == .label) {
                    try label_map.put(inst.label, i);
                }
            }

            // Execute partial instructions
            var partial_pc: usize = 0;
            while (partial_pc < partial_ir.instructions.len) {
                const inst = &partial_ir.instructions[partial_pc];
                const next_pc = self.executeInstruction(inst, &label_map, partial_ir, partial_pc) catch |err| {
                    // BreakOutsideLoop needs to propagate to break outer loop
                    if (err == error.BreakOutsideLoop) {
                        return error.BreakOutsideLoop;
                    }
                    // ContinueOutsideLoop needs to propagate to continue outer loop
                    if (err == error.ContinueOutsideLoop) {
                        return error.ContinueOutsideLoop;
                    }
                    // Other errors - stop executing partial and return silently
                    return;
                };
                if (next_pc) |new_pc| {
                    partial_pc = new_pc;
                } else {
                    partial_pc += 1;
                }
            }
        }
    }

    fn executeRender(self: *VM, render_inst: *const Instruction.Render) !void {
        // Check recursion depth (render counts as entering a new level)
        const partial_name = render_inst.partial_name;

        if (self.recursion_depth >= MAX_RECURSION_DEPTH) {
            // For render, emit inline error message with partial name
            const err_msg = try std.fmt.allocPrint(self.allocator, "Liquid error ({s} line 2): Nesting too deep", .{partial_name});
            defer self.allocator.free(err_msg);
            try self.getActiveOutput().appendSlice(self.allocator, err_msg);
            return;
        }

        // Look up the partial in the filesystem
        // Try direct lookup first, then with .liquid extension
        const partial_source = self.filesystem.get(partial_name) orelse blk: {
            // Try with .liquid extension
            const with_ext = std.fmt.allocPrint(self.allocator, "{s}.liquid", .{partial_name}) catch return;
            defer self.allocator.free(with_ext);
            break :blk self.filesystem.get(with_ext) orelse {
                // Partial not found - in lax mode, just output nothing
                return;
            };
        };

        // Parse the partial template
        var partial_template = Template.parse(self.allocator, partial_source, null, self.error_mode) catch {
            // Parse error - output nothing
            return;
        };
        defer partial_template.deinit();

        // Handle 'for' iteration
        if (render_inst.for_collection) |*for_coll| {
            var coll_expr = for_coll.*;
            const coll_value = coll_expr.evaluate(&self.context);

            if (coll_value) |coll| {
                if (coll == .array) {
                    const items = coll.array.items;
                    const item_name = if (render_inst.for_alias) |alias| alias else partial_name;

                    for (items, 0..) |item, idx| {
                        try self.executeRenderOnce(&partial_template, render_inst, item_name, item, idx, items.len);
                    }
                }
            }
            return;
        }

        // Handle 'with' syntax
        var with_item_name: ?[]const u8 = null;
        var with_item_value: ?json.Value = null;
        if (render_inst.with_value) |*with_val| {
            var wv = with_val.*;
            with_item_value = wv.evaluate(&self.context);
            with_item_name = if (render_inst.with_alias) |alias| alias else partial_name;
        }

        try self.executeRenderOnce(&partial_template, render_inst, with_item_name, with_item_value, null, null);
    }

    fn executeRenderOnce(
        self: *VM,
        partial_template: *Template,
        render_inst: *const Instruction.Render,
        item_name: ?[]const u8,
        item_value: ?json.Value,
        index: ?usize,
        total: ?usize,
    ) !void {
        // Create a fresh isolated context for render (no parent scope visibility,
        // but static environments/global data IS visible)
        var isolated_context = Context.init(self.allocator, self.context.environment);
        isolated_context.frozen_time = self.context.frozen_time; // Inherit frozen time
        defer isolated_context.deinit();

        // Set the item variable if provided
        if (item_name) |name| {
            if (item_value) |val| {
                try isolated_context.set(name, val);
            }
        }

        // Set forloop object for 'for' syntax
        if (index != null and total != null) {
            const idx = index.?;
            const len = total.?;

            var forloop_obj = json.ObjectMap.init(self.allocator);
            try forloop_obj.put("index", json.Value{ .integer = @intCast(idx + 1) });
            try forloop_obj.put("index0", json.Value{ .integer = @intCast(idx) });
            try forloop_obj.put("rindex", json.Value{ .integer = @intCast(len - idx) });
            try forloop_obj.put("rindex0", json.Value{ .integer = @intCast(len - idx - 1) });
            try forloop_obj.put("first", json.Value{ .bool = idx == 0 });
            try forloop_obj.put("last", json.Value{ .bool = idx == len - 1 });
            try forloop_obj.put("length", json.Value{ .integer = @intCast(len) });

            try isolated_context.set("forloop", json.Value{ .object = forloop_obj });
        }

        // Evaluate and set variables passed to the partial (evaluated in parent context)
        for (render_inst.variables) |*var_arg| {
            var value_expr = var_arg.value;
            const value = value_expr.evaluate(&self.context);
            if (value) |v| {
                try isolated_context.set(var_arg.name, v);
            }
        }

        // Create a new VM with isolated context but same output destination
        var isolated_vm = VM{
            .allocator = self.allocator,
            .context = isolated_context,
            .output = .{}, // Temporary output for the render
            .loop_stack = .{},
            .tablerow_loop_stack = .{},
            .capture_stack = .{},
            .ifchanged_stack = .{},
            .continue_offsets = std.StringHashMap(usize).init(self.allocator),
            .filesystem = self.filesystem,
            .error_mode = self.error_mode,
            .inside_render = true, // Include is prohibited inside render partials
            .recursion_depth = self.recursion_depth + 1, // Inherit and increment depth
            .current_partial_name = render_inst.partial_name, // Track partial name for error messages
            .frozen_time = self.frozen_time, // Inherit frozen time
            .ifchanged_last_value = null,
        };
        defer {
            // Clean up the isolated VM
            isolated_vm.loop_stack.deinit(self.allocator);
            isolated_vm.tablerow_loop_stack.deinit(self.allocator);
            isolated_vm.capture_stack.deinit(self.allocator);
            isolated_vm.ifchanged_stack.deinit(self.allocator);
            if (isolated_vm.ifchanged_last_value) |v| {
                self.allocator.free(v);
            }
            isolated_vm.continue_offsets.deinit();
        }

        // Execute the partial's IR
        if (partial_template.ir) |*partial_ir| {
            // Build label map for the partial
            var label_map = std.AutoHashMap(usize, usize).init(self.allocator);
            defer label_map.deinit();

            for (partial_ir.instructions, 0..) |*inst, i| {
                if (inst.* == .label) {
                    try label_map.put(inst.label, i);
                }
            }

            // Execute partial instructions
            var render_pc: usize = 0;
            while (render_pc < partial_ir.instructions.len) {
                const inst = &partial_ir.instructions[render_pc];
                const next_pc = isolated_vm.executeInstruction(inst, &label_map, partial_ir, render_pc) catch |err| {
                    // Handle break/continue outside loop gracefully in render
                    // (render is isolated, so these shouldn't propagate)
                    if (err == error.BreakOutsideLoop or err == error.ContinueOutsideLoop) {
                        // Just stop rendering this partial
                        break;
                    }
                    // Other errors - stop executing partial and return
                    isolated_vm.output.deinit(self.allocator);
                    return;
                };
                if (next_pc) |new_pc| {
                    render_pc = new_pc;
                } else {
                    render_pc += 1;
                }
            }
        }

        // Append isolated VM output to current output
        try self.getActiveOutput().appendSlice(self.allocator, isolated_vm.output.items);
        isolated_vm.output.deinit(self.allocator);
    }
};

const Context = struct {
    allocator: std.mem.Allocator,
    environment: ?json.Value,
    variables: std.StringHashMap(json.Value),
    counters: std.StringHashMap(i64), // Separate namespace for increment/decrement counters
    cycle_registers: std.StringHashMap(usize), // Cycle state: key -> current index
    owned_strings: std.ArrayList([]const u8), // Track strings we own for cleanup
    frozen_time: ?i64, // Frozen time for date filter (unix timestamp)

    pub fn init(allocator: std.mem.Allocator, environment: ?json.Value) Context {
        return .{
            .allocator = allocator,
            .environment = environment,
            .variables = std.StringHashMap(json.Value).init(allocator),
            .counters = std.StringHashMap(i64).init(allocator),
            .cycle_registers = std.StringHashMap(usize).init(allocator),
            .owned_strings = .{},
            .frozen_time = null,
        };
    }

    pub fn deinit(self: *Context) void {
        // Free owned keys in variables (we own all keys we store)
        var var_iter = self.variables.keyIterator();
        while (var_iter.next()) |key_ptr| {
            self.allocator.free(key_ptr.*);
        }
        self.variables.deinit();
        self.counters.deinit();

        // Free owned strings
        for (self.owned_strings.items) |str| {
            self.allocator.free(str);
        }
        self.owned_strings.deinit(self.allocator);

        // Free cycle register keys
        var iter = self.cycle_registers.keyIterator();
        while (iter.next()) |key_ptr| {
            self.allocator.free(key_ptr.*);
        }
        self.cycle_registers.deinit();
    }

    pub fn get(self: *Context, key: []const u8) ?json.Value {
        // Ruby Liquid supports => as an alternative to . for property access (e.g., foo=>bar)
        // Normalize => to . before processing
        var normalized_key: ?[]u8 = null;
        const actual_key: []const u8 = blk: {
            if (std.mem.indexOf(u8, key, "=>")) |_| {
                normalized_key = self.allocator.alloc(u8, key.len) catch return null;
                var i: usize = 0;
                var j: usize = 0;
                while (i < key.len) {
                    if (i + 1 < key.len and key[i] == '=' and key[i + 1] == '>') {
                        normalized_key.?[j] = '.';
                        j += 1;
                        i += 2;
                    } else {
                        normalized_key.?[j] = key[i];
                        j += 1;
                        i += 1;
                    }
                }
                break :blk normalized_key.?[0..j];
            }
            break :blk key;
        };
        defer {
            if (normalized_key) |k| self.allocator.free(k);
        }

        // Handle bracket notation first: colors[0] or colors[0].name or data.users[0].name
        if (std.mem.indexOfScalar(u8, actual_key, '[')) |bracket_index| {
            // Get the base key before the bracket
            const base_key = actual_key[0..bracket_index];
            const rest = actual_key[bracket_index..];

            // Get the base value - use recursive get to handle dot notation in base key
            var base_value: json.Value = undefined;
            if (std.mem.indexOfScalar(u8, base_key, '.')) |_| {
                // Base key has dot notation, use recursive get
                base_value = self.get(base_key) orelse return null;
            } else {
                base_value = self.getSimple(base_key) orelse return null;
            }

            // Navigate through bracket accesses
            var remaining = rest;
            while (remaining.len > 0 and remaining[0] == '[') {
                // Find closing bracket
                const close_bracket = std.mem.indexOfScalar(u8, remaining, ']') orelse return null;
                const index_expr = std.mem.trim(u8, remaining[1..close_bracket], " \t\n\r");

                // Check if it's a quoted string: ['key'] or ["key"]
                if ((index_expr.len >= 2 and index_expr[0] == '\'' and index_expr[index_expr.len - 1] == '\'') or
                    (index_expr.len >= 2 and index_expr[0] == '"' and index_expr[index_expr.len - 1] == '"'))
                {
                    // String key access for objects
                    const string_key = index_expr[1 .. index_expr.len - 1];
                    if (base_value == .object) {
                        base_value = base_value.object.get(string_key) orelse return null;
                    } else {
                        return null;
                    }
                } else if (std.fmt.parseInt(i64, index_expr, 10)) |index| {
                    // Integer index - for arrays or hash with numeric string keys
                    if (base_value == .array) {
                        const arr = base_value.array.items;
                        const actual_index: usize = if (index < 0) blk: {
                            if (@as(usize, @intCast(-index)) > arr.len) return null;
                            break :blk arr.len - @as(usize, @intCast(-index));
                        } else blk: {
                            if (@as(usize, @intCast(index)) >= arr.len) return null;
                            break :blk @intCast(index);
                        };
                        base_value = arr[actual_index];
                    } else if (base_value == .object) {
                        // Try to look up integer as string key in object
                        base_value = base_value.object.get(index_expr) orelse return null;
                    } else {
                        return null;
                    }
                } else |_| {
                    // Try to look up as variable
                    const var_value_raw = self.getSimple(index_expr) orelse return null;
                    // Unwrap drops to get their underlying value for indexing
                    const var_value = unwrapDrop(var_value_raw);
                    switch (var_value) {
                        .integer => |i| {
                            // Integer for array or object access
                            if (base_value == .array) {
                                const arr = base_value.array.items;
                                const actual_index: usize = if (i < 0) blk: {
                                    if (@as(usize, @intCast(-i)) > arr.len) return null;
                                    break :blk arr.len - @as(usize, @intCast(-i));
                                } else blk: {
                                    if (@as(usize, @intCast(i)) >= arr.len) return null;
                                    break :blk @intCast(i);
                                };
                                base_value = arr[actual_index];
                            } else if (base_value == .object) {
                                // Try to look up integer as string key in object
                                var int_key_buf: [32]u8 = undefined;
                                const int_key = std.fmt.bufPrint(&int_key_buf, "{d}", .{i}) catch return null;
                                base_value = base_value.object.get(int_key) orelse return null;
                            } else {
                                return null;
                            }
                        },
                        .string => |s| {
                            // String key for object access
                            if (base_value == .object) {
                                base_value = base_value.object.get(s) orelse return null;
                            } else {
                                // Also try to parse as integer for arrays
                                if (std.fmt.parseInt(i64, s, 10)) |i| {
                                    if (base_value == .array) {
                                        const arr = base_value.array.items;
                                        const actual_index: usize = if (i < 0) blk: {
                                            if (@as(usize, @intCast(-i)) > arr.len) return null;
                                            break :blk arr.len - @as(usize, @intCast(-i));
                                        } else blk: {
                                            if (@as(usize, @intCast(i)) >= arr.len) return null;
                                            break :blk @intCast(i);
                                        };
                                        base_value = arr[actual_index];
                                    } else {
                                        return null;
                                    }
                                } else |_| {
                                    return null;
                                }
                            }
                        },
                        else => return null,
                    }
                }

                remaining = remaining[close_bracket + 1 ..];

                // Handle dot after bracket: colors[0].name
                if (remaining.len > 0 and remaining[0] == '.') {
                    remaining = remaining[1..];
                    return self.getNestedValue(base_value, remaining);
                }
            }

            return base_value;
        }

        // Handle dot notation for nested property access
        if (std.mem.indexOfScalar(u8, actual_key, '.')) |dot_index| {
            const first_key = actual_key[0..dot_index];
            const rest = actual_key[dot_index + 1 ..];

            // Get the first part
            const first_value = self.getSimple(first_key) orelse return null;

            // Navigate through the rest of the path
            return self.getNestedValue(first_value, rest);
        }

        // Simple key lookup (no dots)
        return self.getSimple(actual_key);
    }

    fn getSimple(self: *Context, key: []const u8) ?json.Value {
        // Check local variables first
        if (self.variables.get(key)) |value| {
            return value;
        }

        // Check environment
        if (self.environment) |env| {
            if (env == .object) {
                if (env.object.get(key)) |val| {
                    return val;
                }
            }
        }

        // Check counters (increment/decrement namespace)
        // After {% increment foo %}, {{ foo }} should return the counter value
        if (self.counters.get(key)) |counter_value| {
            return json.Value{ .integer = counter_value };
        }

        return null;
    }

    fn getNestedValue(self: *Context, value: json.Value, path: []const u8) ?json.Value {
        if (path.len == 0) {
            return value;
        }

        // Check for bracket access first (handles items[0].value)
        const bracket_index = std.mem.indexOfScalar(u8, path, '[');
        const dot_index = std.mem.indexOfScalar(u8, path, '.');

        // Determine which comes first - dot or bracket
        const key_end = blk: {
            if (bracket_index) |bi| {
                if (dot_index) |di| {
                    break :blk @min(bi, di);
                } else {
                    break :blk bi;
                }
            } else if (dot_index) |di| {
                break :blk di;
            } else {
                break :blk path.len;
            }
        };

        const key = path[0..key_end];
        var current_value = value;

        // Get the property first if there's a key before bracket/dot
        if (key.len > 0) {
            // Handle special properties for arrays
            if (current_value == .array) {
                const items = current_value.array.items;
                if (std.mem.eql(u8, key, "size")) {
                    current_value = json.Value{ .integer = @intCast(items.len) };
                } else if (std.mem.eql(u8, key, "first")) {
                    if (items.len == 0) return json.Value{ .null = {} };
                    current_value = items[0];
                } else if (std.mem.eql(u8, key, "last")) {
                    if (items.len == 0) return json.Value{ .null = {} };
                    current_value = items[items.len - 1];
                } else {
                    return null;
                }
            } else if (current_value == .string) {
                if (std.mem.eql(u8, key, "size")) {
                    current_value = json.Value{ .integer = @intCast(current_value.string.len) };
                } else {
                    return null;
                }
            } else if (current_value == .object) {
                const obj = current_value.object;
                // First, try direct property lookup (e.g., forloop.first)
                if (obj.get(key)) |prop_value| {
                    current_value = prop_value;
                } else if (std.mem.eql(u8, key, "size")) {
                    // Handle special 'size' property for objects/hashes that don't have it
                    current_value = json.Value{ .integer = @intCast(obj.count()) };
                } else if (std.mem.eql(u8, key, "first")) {
                    // h.first returns first key-value pair as array [key, value]
                    var iter = obj.iterator();
                    if (iter.next()) |entry| {
                        // Create array with [key, value]
                        var arr = json.Array.init(self.allocator);
                        arr.append(json.Value{ .string = entry.key_ptr.* }) catch return null;
                        arr.append(entry.value_ptr.*) catch return null;
                        current_value = json.Value{ .array = arr };
                    } else {
                        return json.Value{ .null = {} };
                    }
                } else {
                    return null;
                }
            } else {
                return null;
            }
        }

        // Process remaining path
        var remaining = path[key_end..];

        while (remaining.len > 0) {
            if (remaining[0] == '.') {
                remaining = remaining[1..];
                // Continue to process next segment
            } else if (remaining[0] == '[') {
                // Find closing bracket
                const close_bracket = std.mem.indexOfScalar(u8, remaining, ']') orelse return null;
                const index_expr = std.mem.trim(u8, remaining[1..close_bracket], " \t\n\r");

                // Check if it's a quoted string
                if ((index_expr.len >= 2 and index_expr[0] == '\'' and index_expr[index_expr.len - 1] == '\'') or
                    (index_expr.len >= 2 and index_expr[0] == '"' and index_expr[index_expr.len - 1] == '"'))
                {
                    const string_key = index_expr[1 .. index_expr.len - 1];
                    if (current_value == .object) {
                        current_value = current_value.object.get(string_key) orelse return null;
                    } else {
                        return null;
                    }
                } else if (std.fmt.parseInt(i64, index_expr, 10)) |index| {
                    // Integer index - for arrays or hash with numeric string keys
                    if (current_value == .array) {
                        const arr = current_value.array.items;
                        const actual_index: usize = if (index < 0) blk2: {
                            if (@as(usize, @intCast(-index)) > arr.len) return null;
                            break :blk2 arr.len - @as(usize, @intCast(-index));
                        } else blk2: {
                            if (@as(usize, @intCast(index)) >= arr.len) return null;
                            break :blk2 @intCast(index);
                        };
                        current_value = arr[actual_index];
                    } else if (current_value == .object) {
                        // Try to look up integer as string key in object
                        current_value = current_value.object.get(index_expr) orelse return null;
                    } else {
                        return null;
                    }
                } else |_| {
                    // Try to look up as variable
                    const var_value_raw = self.getSimple(index_expr) orelse return null;
                    // Unwrap drops to get their underlying value for indexing
                    const var_value = unwrapDrop(var_value_raw);
                    switch (var_value) {
                        .integer => |i| {
                            // Integer for array or object access
                            if (current_value == .array) {
                                const arr = current_value.array.items;
                                const actual_index: usize = if (i < 0) blk2: {
                                    if (@as(usize, @intCast(-i)) > arr.len) return null;
                                    break :blk2 arr.len - @as(usize, @intCast(-i));
                                } else blk2: {
                                    if (@as(usize, @intCast(i)) >= arr.len) return null;
                                    break :blk2 @intCast(i);
                                };
                                current_value = arr[actual_index];
                            } else if (current_value == .object) {
                                // Try to look up integer as string key in object
                                var int_key_buf: [32]u8 = undefined;
                                const int_key = std.fmt.bufPrint(&int_key_buf, "{d}", .{i}) catch return null;
                                current_value = current_value.object.get(int_key) orelse return null;
                            } else {
                                return null;
                            }
                        },
                        .string => |s| {
                            if (current_value == .object) {
                                current_value = current_value.object.get(s) orelse return null;
                            } else {
                                return null;
                            }
                        },
                        else => return null,
                    }
                }

                remaining = remaining[close_bracket + 1 ..];
                continue;
            } else {
                // Find next separator
                var next_sep = remaining.len;
                for (remaining, 0..) |c, idx| {
                    if (c == '.' or c == '[') {
                        next_sep = idx;
                        break;
                    }
                }

                const next_key = remaining[0..next_sep];
                if (next_key.len > 0) {
                    // Handle special properties for arrays
                    if (current_value == .array) {
                        const items = current_value.array.items;
                        if (std.mem.eql(u8, next_key, "size")) {
                            current_value = json.Value{ .integer = @intCast(items.len) };
                        } else if (std.mem.eql(u8, next_key, "first")) {
                            if (items.len == 0) return json.Value{ .null = {} };
                            current_value = items[0];
                        } else if (std.mem.eql(u8, next_key, "last")) {
                            if (items.len == 0) return json.Value{ .null = {} };
                            current_value = items[items.len - 1];
                        } else {
                            return null;
                        }
                    } else if (current_value == .string) {
                        if (std.mem.eql(u8, next_key, "size")) {
                            current_value = json.Value{ .integer = @intCast(current_value.string.len) };
                        } else {
                            return null;
                        }
                    } else if (current_value == .object) {
                        current_value = current_value.object.get(next_key) orelse return null;
                    } else {
                        return null;
                    }
                }
                remaining = remaining[next_sep..];
            }
        }

        return current_value;
    }

    pub fn set(self: *Context, key: []const u8, value: json.Value) !void {
        // Check if key already exists - if not, we need to dupe it
        // because the key might come from a partial template that gets deinitialized
        if (self.variables.getKey(key)) |_| {
            // Key exists, just update the value
            try self.variables.put(key, value);
        } else {
            // Key doesn't exist, we need to dupe it to own the memory
            const owned_key = try self.allocator.dupe(u8, key);
            try self.variables.put(owned_key, value);
        }
    }

    // Set a value with an owned string that will be freed when context is deinitialized
    pub fn setOwnedString(self: *Context, key: []const u8, owned_string: []const u8) !void {
        // Track the string for cleanup
        try self.owned_strings.append(self.allocator, owned_string);
        // Store the value
        try self.set(key, json.Value{ .string = owned_string });
    }

    pub fn remove(self: *Context, key: []const u8) bool {
        return self.variables.remove(key);
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

    // Decrement counter and return the value AFTER decrement
    pub fn decrement(self: *Context, name: []const u8) !i64 {
        const current = self.getCounter(name);
        const new_value = current - 1;
        try self.counters.put(name, new_value);
        return new_value;
    }

    // Get current cycle index and advance it
    pub fn cycle(self: *Context, key: []const u8, num_values: usize) !usize {
        const current_index = self.cycle_registers.get(key) orelse 0;
        const next_index = (current_index + 1) % num_values;

        // We need to own the key, so dupe it if we're adding a new entry
        if (!self.cycle_registers.contains(key)) {
            const key_owned = try self.allocator.dupe(u8, key);
            try self.cycle_registers.put(key_owned, next_index);
        } else {
            try self.cycle_registers.put(key, next_index);
        }

        return current_index;
    }
};

// Parse a single node from token stream
fn parseNode(allocator: std.mem.Allocator, tokens: []Token, index: *usize, error_mode: ErrorMode) (std.mem.Allocator.Error || error{ UnterminatedString, UnknownTag, UnclosedTag })!Node {
    const token = tokens[index.*];
    index.* += 1;

    return switch (token.kind) {
        .text => Node{ .text = try allocator.dupe(u8, token.content) },
        .variable => Node{ .variable = try Variable.parse(allocator, token.content) },
        .tag => blk: {
            const trimmed = std.mem.trim(u8, token.content, " \t\n\r");

            // Check if it's an if tag
            if (std.mem.startsWith(u8, trimmed, "if ")) {
                break :blk try parseIfTag(allocator, tokens, index, trimmed, error_mode);
            } else if (std.mem.startsWith(u8, trimmed, "unless ")) {
                break :blk try parseUnlessTag(allocator, tokens, index, trimmed, error_mode);
            } else if (std.mem.startsWith(u8, trimmed, "for ")) {
                break :blk try parseForTag(allocator, tokens, index, trimmed, error_mode);
            } else if (std.mem.startsWith(u8, trimmed, "tablerow ")) {
                break :blk try parseTableRowTag(allocator, tokens, index, trimmed, error_mode);
            } else if (std.mem.startsWith(u8, trimmed, "capture ")) {
                break :blk try parseCaptureTag(allocator, tokens, index, trimmed, error_mode);
            } else if (std.mem.startsWith(u8, trimmed, "case ")) {
                break :blk try parseCaseTag(allocator, tokens, index, trimmed, error_mode);
            } else if (std.mem.eql(u8, trimmed, "comment")) {
                break :blk try parseCommentTag(allocator, tokens, index, trimmed, error_mode);
            } else if (std.mem.eql(u8, trimmed, "doc")) {
                break :blk try parseDocTag(allocator, tokens, index, trimmed, error_mode);
            } else if (std.mem.eql(u8, trimmed, "raw")) {
                break :blk try parseRawTag(allocator, tokens, index, trimmed, error_mode);
            } else if (std.mem.eql(u8, trimmed, "ifchanged")) {
                break :blk try parseIfChangedTag(allocator, tokens, index, trimmed, error_mode);
            } else if (std.mem.startsWith(u8, trimmed, "assign ")) {
                break :blk Node{ .tag = Tag{
                    .tag_type = .assign,
                    .content = try allocator.dupe(u8, trimmed),
                    .children = &.{},
                    .else_children = &.{},
                    .elsif_branches = &.{},
                } };
            } else if (std.mem.startsWith(u8, trimmed, "increment ") or std.mem.eql(u8, trimmed, "increment")) {
                break :blk Node{ .tag = Tag{
                    .tag_type = .increment,
                    .content = try allocator.dupe(u8, trimmed),
                    .children = &.{},
                    .else_children = &.{},
                    .elsif_branches = &.{},
                } };
            } else if (std.mem.startsWith(u8, trimmed, "decrement ") or std.mem.eql(u8, trimmed, "decrement")) {
                break :blk Node{ .tag = Tag{
                    .tag_type = .decrement,
                    .content = try allocator.dupe(u8, trimmed),
                    .children = &.{},
                    .else_children = &.{},
                    .elsif_branches = &.{},
                } };
            } else if (std.mem.startsWith(u8, trimmed, "cycle ")) {
                break :blk Node{ .tag = Tag{
                    .tag_type = .cycle,
                    .content = try allocator.dupe(u8, trimmed),
                    .children = &.{},
                    .else_children = &.{},
                    .elsif_branches = &.{},
                } };
            } else if (std.mem.startsWith(u8, trimmed, "include ")) {
                break :blk Node{ .tag = Tag{
                    .tag_type = .include,
                    .content = try allocator.dupe(u8, trimmed),
                    .children = &.{},
                    .else_children = &.{},
                    .elsif_branches = &.{},
                } };
            } else if (std.mem.startsWith(u8, trimmed, "render ")) {
                break :blk Node{ .tag = Tag{
                    .tag_type = .render,
                    .content = try allocator.dupe(u8, trimmed),
                    .children = &.{},
                    .else_children = &.{},
                    .elsif_branches = &.{},
                } };
            } else if (std.mem.eql(u8, trimmed, "break")) {
                break :blk Node{ .tag = Tag{
                    .tag_type = .loop_break,
                    .content = try allocator.dupe(u8, trimmed),
                    .children = &.{},
                    .else_children = &.{},
                    .elsif_branches = &.{},
                } };
            } else if (std.mem.eql(u8, trimmed, "continue")) {
                break :blk Node{ .tag = Tag{
                    .tag_type = .loop_continue,
                    .content = try allocator.dupe(u8, trimmed),
                    .children = &.{},
                    .else_children = &.{},
                    .elsif_branches = &.{},
                } };
            } else if (std.mem.startsWith(u8, trimmed, "echo ") or std.mem.eql(u8, trimmed, "echo")) {
                // {% echo expression %} - outputs the expression value
                break :blk Node{ .tag = Tag{
                    .tag_type = .echo,
                    .content = try allocator.dupe(u8, trimmed),
                    .children = &.{},
                    .else_children = &.{},
                    .elsif_branches = &.{},
                } };
            } else if (std.mem.startsWith(u8, trimmed, "liquid")) {
                // {% liquid ... %} - contains multiple statements
                const liquid_content = if (trimmed.len > 6) std.mem.trim(u8, trimmed[6..], " \t\n\r") else "";
                break :blk Node{ .tag = Tag{
                    .tag_type = .liquid_tag,
                    .content = try allocator.dupe(u8, liquid_content),
                    .children = &.{},
                    .else_children = &.{},
                    .elsif_branches = &.{},
                } };
            } else if (trimmed.len > 0 and trimmed[0] == '#') {
                // Inline comment: {% # comment %}
                // Just output an empty text node (comment produces no output)
                break :blk Node{ .text = try allocator.dupe(u8, "") };
            } else if (std.mem.eql(u8, trimmed, "endif") or
                std.mem.eql(u8, trimmed, "endfor") or
                std.mem.eql(u8, trimmed, "endunless") or
                std.mem.eql(u8, trimmed, "endcase") or
                std.mem.eql(u8, trimmed, "endtablerow") or
                std.mem.eql(u8, trimmed, "endcapture"))
            {
                // Orphan end tags (without matching open tag) should be silently ignored
                // This matches Ruby Liquid behavior
                break :blk Node{ .text = try allocator.dupe(u8, "") };
            } else {
                // Unknown tags always cause parse errors in liquid-ruby (both strict and lax modes)
                return error.UnknownTag;
            }
        },
    };
}

// Parse if/elsif/else/endif block
fn parseIfTag(allocator: std.mem.Allocator, tokens: []Token, index: *usize, initial_content: []const u8, error_mode: ErrorMode) (std.mem.Allocator.Error || error{ UnterminatedString, UnknownTag, UnclosedTag })!Node {
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
    var found_endif = false;

    while (index.* < tokens.len) {
        const token = tokens[index.*];

        if (token.kind == .tag) {
            const trimmed = std.mem.trim(u8, token.content, " \t\n\r");

            if (std.mem.eql(u8, trimmed, "endif")) {
                index.* += 1;
                found_endif = true;
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
        const child = try parseNode(allocator, tokens, index, error_mode);

        switch (current_section) {
            .if_block => try children.append(allocator, child),
            .elsif_block => try current_elsif_children.append(allocator, child),
            .else_block => try else_children.append(allocator, child),
        }
    }

    // Check if we found endif or reached the end without finding it
    if (!found_endif) {
        // Clean up current_elsif state (not covered by errdefer)
        for (current_elsif_children.items) |*child| {
            child.deinit(allocator);
        }
        current_elsif_children.deinit(allocator);
        if (current_elsif_condition) |c| allocator.free(c);
        // errdefer will handle children, elsif_branches, and else_children
        return error.UnclosedTag;
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
fn parseUnlessTag(allocator: std.mem.Allocator, tokens: []Token, index: *usize, initial_content: []const u8, error_mode: ErrorMode) (std.mem.Allocator.Error || error{ UnterminatedString, UnknownTag, UnclosedTag })!Node {
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
        const child = try parseNode(allocator, tokens, index, error_mode);

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
fn parseForTag(allocator: std.mem.Allocator, tokens: []Token, index: *usize, initial_content: []const u8, error_mode: ErrorMode) (std.mem.Allocator.Error || error{ UnterminatedString, UnknownTag, UnclosedTag })!Node {
    var children: std.ArrayList(Node) = .{};
    errdefer {
        for (children.items) |*child| {
            child.deinit(allocator);
        }
        children.deinit(allocator);
    }

    var else_children: std.ArrayList(Node) = .{};
    errdefer {
        for (else_children.items) |*child| {
            child.deinit(allocator);
        }
        else_children.deinit(allocator);
    }

    var in_else = false;

    while (index.* < tokens.len) {
        const token = tokens[index.*];

        if (token.kind == .tag) {
            const trimmed = std.mem.trim(u8, token.content, " \t\n\r");

            if (std.mem.eql(u8, trimmed, "endfor")) {
                index.* += 1;
                break;
            }

            if (std.mem.eql(u8, trimmed, "else")) {
                in_else = true;
                index.* += 1;
                continue;
            }
        }

        // Parse child node
        const child = try parseNode(allocator, tokens, index, error_mode);
        if (in_else) {
            try else_children.append(allocator, child);
        } else {
            try children.append(allocator, child);
        }
    }

    return Node{ .tag = Tag{
        .tag_type = .for_tag,
        .content = try allocator.dupe(u8, initial_content),
        .children = try children.toOwnedSlice(allocator),
        .else_children = try else_children.toOwnedSlice(allocator),
        .elsif_branches = &.{},
    } };
}

// Parse tablerow/endtablerow block
fn parseTableRowTag(allocator: std.mem.Allocator, tokens: []Token, index: *usize, initial_content: []const u8, error_mode: ErrorMode) (std.mem.Allocator.Error || error{ UnterminatedString, UnknownTag, UnclosedTag })!Node {
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

            if (std.mem.eql(u8, trimmed, "endtablerow")) {
                index.* += 1;
                break;
            }
        }

        // Parse child node
        const child = try parseNode(allocator, tokens, index, error_mode);
        try children.append(allocator, child);
    }

    return Node{ .tag = Tag{
        .tag_type = .tablerow,
        .content = try allocator.dupe(u8, initial_content),
        .children = try children.toOwnedSlice(allocator),
        .else_children = &.{},
        .elsif_branches = &.{},
    } };
}

// Parse capture/endcapture block
fn parseCaptureTag(allocator: std.mem.Allocator, tokens: []Token, index: *usize, initial_content: []const u8, error_mode: ErrorMode) (std.mem.Allocator.Error || error{ UnterminatedString, UnknownTag, UnclosedTag })!Node {
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
        const child = try parseNode(allocator, tokens, index, error_mode);
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

// Parse ifchanged/endifchanged block - only outputs if content changed from last time
fn parseIfChangedTag(allocator: std.mem.Allocator, tokens: []Token, index: *usize, initial_content: []const u8, error_mode: ErrorMode) (std.mem.Allocator.Error || error{ UnterminatedString, UnknownTag, UnclosedTag })!Node {
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

            if (std.mem.eql(u8, trimmed, "endifchanged")) {
                index.* += 1;
                break;
            }
        }

        // Parse child node
        const child = try parseNode(allocator, tokens, index, error_mode);
        try children.append(allocator, child);
    }

    return Node{ .tag = Tag{
        .tag_type = .ifchanged,
        .content = try allocator.dupe(u8, initial_content),
        .children = try children.toOwnedSlice(allocator),
        .else_children = &.{},
        .elsif_branches = &.{},
    } };
}

// Parse case/when/else/endcase block
fn parseCaseTag(allocator: std.mem.Allocator, tokens: []Token, index: *usize, initial_content: []const u8, error_mode: ErrorMode) (std.mem.Allocator.Error || error{ UnterminatedString, UnknownTag, UnclosedTag })!Node {
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
        var child = try parseNode(allocator, tokens, index, error_mode);

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
fn parseCommentTag(allocator: std.mem.Allocator, tokens: []Token, index: *usize, initial_content: []const u8, error_mode: ErrorMode) (std.mem.Allocator.Error || error{ UnterminatedString, UnknownTag, UnclosedTag })!Node {
    _ = error_mode; // Not used in comment tags
    // Comments consume all content until matching endcomment, tracking nesting depth
    var nesting_depth: usize = 1; // We start with one open comment
    while (index.* < tokens.len) {
        const token = tokens[index.*];
        index.* += 1;

        if (token.kind == .tag) {
            const trimmed = std.mem.trim(u8, token.content, " \t\n\r");
            if (std.mem.eql(u8, trimmed, "comment")) {
                // Nested comment - increase depth
                nesting_depth += 1;
            } else if (std.mem.eql(u8, trimmed, "endcomment")) {
                nesting_depth -= 1;
                if (nesting_depth == 0) {
                    break;
                }
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

// Parse doc/enddoc block (similar to comment but for documentation)
fn parseDocTag(allocator: std.mem.Allocator, tokens: []Token, index: *usize, initial_content: []const u8, error_mode: ErrorMode) (std.mem.Allocator.Error || error{ UnterminatedString, UnknownTag, UnclosedTag })!Node {
    _ = error_mode; // Not used in doc tags
    // Doc tags consume all content until enddoc, but don't parse it
    while (index.* < tokens.len) {
        const token = tokens[index.*];
        index.* += 1;

        if (token.kind == .tag) {
            const trimmed = std.mem.trim(u8, token.content, " \t\n\r");
            // Handle both "enddoc" and "enddoc xyz" (with trailing content)
            if (std.mem.eql(u8, trimmed, "enddoc") or std.mem.startsWith(u8, trimmed, "enddoc ") or std.mem.startsWith(u8, trimmed, "enddoc\t")) {
                break;
            }
        }
    }

    return Node{ .tag = Tag{
        .tag_type = .comment, // Reuse comment type since behavior is identical
        .content = try allocator.dupe(u8, initial_content),
        .children = &.{},
        .else_children = &.{},
        .elsif_branches = &.{},
    } };
}

// Parse raw/endraw block
fn parseRawTag(allocator: std.mem.Allocator, tokens: []Token, index: *usize, initial_content: []const u8, error_mode: ErrorMode) (std.mem.Allocator.Error || error{ UnterminatedString, UnknownTag, UnclosedTag })!Node {
    _ = error_mode; // Not used in raw tags
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
    empty: void, // Special empty literal for comparisons
    blank: void, // Special blank literal for comparisons
    binary_op: *BinaryOp,
    range: *Range, // Range like (1..5) or (a..b)

    const Range = struct {
        start: Expression,
        end: Expression,
    };

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
            .range => |r| {
                var start = r.*.start;
                start.deinit(allocator);
                var end = r.*.end;
                end.deinit(allocator);
                allocator.destroy(r);
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
            .empty => json.Value{ .string = "empty" }, // Special marker value
            .blank => json.Value{ .string = "blank" }, // Special marker value
            .binary_op => |op| evaluateBinaryOp(op, ctx),
            .range => |r| evaluateRange(r, ctx),
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
            // Equality check - handle empty/blank keywords specially
            if (right_expr == .empty) {
                const left = left_expr.evaluate(ctx);
                return json.Value{ .bool = isEmptyValue(left) };
            }
            if (right_expr == .blank) {
                const left = left_expr.evaluate(ctx);
                return json.Value{ .bool = isBlankValue(left) };
            }
            if (left_expr == .empty) {
                const right = right_expr.evaluate(ctx);
                return json.Value{ .bool = isEmptyValue(right) };
            }
            if (left_expr == .blank) {
                const right = right_expr.evaluate(ctx);
                return json.Value{ .bool = isBlankValue(right) };
            }
            const left = left_expr.evaluate(ctx);
            const right = right_expr.evaluate(ctx);
            return json.Value{ .bool = compareValues(left, right, .eq) };
        },
        .ne => {
            // Inequality check - handle empty/blank keywords specially
            if (right_expr == .empty) {
                const left = left_expr.evaluate(ctx);
                return json.Value{ .bool = !isEmptyValue(left) };
            }
            if (right_expr == .blank) {
                const left = left_expr.evaluate(ctx);
                return json.Value{ .bool = !isBlankValue(left) };
            }
            if (left_expr == .empty) {
                const right = right_expr.evaluate(ctx);
                return json.Value{ .bool = !isEmptyValue(right) };
            }
            if (left_expr == .blank) {
                const right = right_expr.evaluate(ctx);
                return json.Value{ .bool = !isBlankValue(right) };
            }
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

fn evaluateRange(r: *const Expression.Range, ctx: *Context) ?json.Value {
    var start_expr = r.*.start;
    var end_expr = r.*.end;

    const start_val = start_expr.evaluate(ctx);
    const end_val = end_expr.evaluate(ctx);

    // Both must be integers
    const start: i64 = if (start_val) |v|
        (if (v == .integer) v.integer else return null)
    else
        return null;

    const end: i64 = if (end_val) |v|
        (if (v == .integer) v.integer else return null)
    else
        return null;

    // Create an array from start to end (inclusive)
    var arr = json.Array.init(ctx.allocator);

    if (start <= end) {
        var i = start;
        while (i <= end) : (i += 1) {
            arr.append(json.Value{ .integer = i }) catch return null;
        }
    } else {
        // Descending range
        var i = start;
        while (i >= end) : (i -= 1) {
            arr.append(json.Value{ .integer = i }) catch return null;
        }
    }

    return json.Value{ .array = arr };
}

fn compareValues(left: ?json.Value, right: ?json.Value, op: Expression.BinaryOp.Operator) bool {
    // Treat Zig null (undefined variable) as json.Value.null for comparison
    const l_raw: json.Value = left orelse json.Value{ .null = {} };
    const r_raw: json.Value = right orelse json.Value{ .null = {} };

    // Unwrap drops to get their underlying values for comparison
    const l = unwrapDrop(l_raw);
    const r = unwrapDrop(r_raw);

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

    // In Liquid, contains cannot find nil values in arrays
    if (n == .null) return false;

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

/// Apply upcase or downcase transformation to a JSON value and return Ruby inspect format string
fn applyCaseToValueRubyFormat(allocator: std.mem.Allocator, value: json.Value, is_upcase: bool) ![]u8 {
    return switch (value) {
        .string => |s| {
            const transformed = if (is_upcase)
                try std.ascii.allocUpperString(allocator, s)
            else
                try std.ascii.allocLowerString(allocator, s);
            return transformed;
        },
        .array => |arr| {
            var result: std.ArrayList(u8) = .{};
            try result.append(allocator, '[');
            for (arr.items, 0..) |item, i| {
                if (i > 0) try result.appendSlice(allocator, ", ");
                const transformed = try applyCaseToValueRubyFormat(allocator, item, is_upcase);
                defer allocator.free(transformed);
                // Wrap strings in quotes for Ruby format
                if (item == .string) {
                    try result.append(allocator, '"');
                    try result.appendSlice(allocator, transformed);
                    try result.append(allocator, '"');
                } else {
                    try result.appendSlice(allocator, transformed);
                }
            }
            try result.append(allocator, ']');
            return try result.toOwnedSlice(allocator);
        },
        .object => |obj| {
            var result: std.ArrayList(u8) = .{};
            try result.append(allocator, '{');
            var first = true;
            var iter = obj.iterator();
            while (iter.next()) |entry| {
                if (!first) try result.appendSlice(allocator, ", ");
                first = false;

                // Key in quotes
                try result.append(allocator, '"');
                const new_key = if (is_upcase)
                    try std.ascii.allocUpperString(allocator, entry.key_ptr.*)
                else
                    try std.ascii.allocLowerString(allocator, entry.key_ptr.*);
                defer allocator.free(new_key);
                try result.appendSlice(allocator, new_key);
                try result.appendSlice(allocator, "\"=>");

                // Value
                const transformed_value = try applyCaseToValueRubyFormat(allocator, entry.value_ptr.*, is_upcase);
                defer allocator.free(transformed_value);
                if (entry.value_ptr.* == .string) {
                    try result.append(allocator, '"');
                    try result.appendSlice(allocator, transformed_value);
                    try result.append(allocator, '"');
                } else {
                    try result.appendSlice(allocator, transformed_value);
                }
            }
            try result.append(allocator, '}');
            return try result.toOwnedSlice(allocator);
        },
        .integer => |i| try std.fmt.allocPrint(allocator, "{d}", .{i}),
        .float => |f| try std.fmt.allocPrint(allocator, "{d}", .{f}),
        .bool => |b| {
            const str = if (is_upcase)
                (if (b) "TRUE" else "FALSE")
            else
                (if (b) "true" else "false");
            return try allocator.dupe(u8, str);
        },
        .null => {
            const str = if (is_upcase) "NIL" else "nil";
            return try allocator.dupe(u8, str);
        },
        else => try allocator.dupe(u8, ""),
    };
}

/// Convert a JSON value to Ruby inspect format string (no transformation)
fn valueToRubyInspectString(allocator: std.mem.Allocator, value: json.Value) ![]u8 {
    return switch (value) {
        .string => |s| try allocator.dupe(u8, s),
        .array => |arr| {
            var result: std.ArrayList(u8) = .{};
            try result.append(allocator, '[');
            for (arr.items, 0..) |item, i| {
                if (i > 0) try result.appendSlice(allocator, ", ");
                const item_str = try valueToRubyInspectString(allocator, item);
                defer allocator.free(item_str);
                if (item == .string) {
                    try result.append(allocator, '"');
                    try result.appendSlice(allocator, item_str);
                    try result.append(allocator, '"');
                } else {
                    try result.appendSlice(allocator, item_str);
                }
            }
            try result.append(allocator, ']');
            return try result.toOwnedSlice(allocator);
        },
        .object => |obj| {
            var result: std.ArrayList(u8) = .{};
            try result.append(allocator, '{');
            var first = true;
            var iter = obj.iterator();
            while (iter.next()) |entry| {
                if (!first) try result.appendSlice(allocator, ", ");
                first = false;
                try result.append(allocator, '"');
                try result.appendSlice(allocator, entry.key_ptr.*);
                try result.appendSlice(allocator, "\"=>");
                const val_str = try valueToRubyInspectString(allocator, entry.value_ptr.*);
                defer allocator.free(val_str);
                if (entry.value_ptr.* == .string) {
                    try result.append(allocator, '"');
                    try result.appendSlice(allocator, val_str);
                    try result.append(allocator, '"');
                } else {
                    try result.appendSlice(allocator, val_str);
                }
            }
            try result.append(allocator, '}');
            return try result.toOwnedSlice(allocator);
        },
        .integer => |i| try std.fmt.allocPrint(allocator, "{d}", .{i}),
        .float => |f| try std.fmt.allocPrint(allocator, "{d}", .{f}),
        .bool => |b| try allocator.dupe(u8, if (b) "true" else "false"),
        .null => try allocator.dupe(u8, "nil"),
        else => try allocator.dupe(u8, ""),
    };
}

/// Escape a string for HTML
fn escapeHtml(allocator: std.mem.Allocator, s: []const u8) ![]u8 {
    var result: std.ArrayList(u8) = .{};
    for (s) |c| {
        switch (c) {
            '&' => try result.appendSlice(allocator, "&amp;"),
            '<' => try result.appendSlice(allocator, "&lt;"),
            '>' => try result.appendSlice(allocator, "&gt;"),
            '"' => try result.appendSlice(allocator, "&quot;"),
            else => try result.append(allocator, c),
        }
    }
    return try result.toOwnedSlice(allocator);
}

/// Apply escape transformation to a JSON value and return Ruby inspect format string with HTML escaping
fn applyEscapeToValueRubyFormat(allocator: std.mem.Allocator, value: json.Value) ![]u8 {
    return switch (value) {
        .string => |s| try escapeHtml(allocator, s),
        .array => |arr| {
            var result: std.ArrayList(u8) = .{};
            try result.appendSlice(allocator, "[");
            for (arr.items, 0..) |item, i| {
                if (i > 0) try result.appendSlice(allocator, ", ");
                const transformed = try applyEscapeToValueRubyFormat(allocator, item);
                defer allocator.free(transformed);
                // Wrap strings in escaped quotes
                if (item == .string) {
                    try result.appendSlice(allocator, "&quot;");
                    try result.appendSlice(allocator, transformed);
                    try result.appendSlice(allocator, "&quot;");
                } else {
                    try result.appendSlice(allocator, transformed);
                }
            }
            try result.appendSlice(allocator, "]");
            return try result.toOwnedSlice(allocator);
        },
        .object => |obj| {
            var result: std.ArrayList(u8) = .{};
            try result.appendSlice(allocator, "{");
            var first = true;
            var iter = obj.iterator();
            while (iter.next()) |entry| {
                if (!first) try result.appendSlice(allocator, ", ");
                first = false;

                // Key in escaped quotes
                try result.appendSlice(allocator, "&quot;");
                const escaped_key = try escapeHtml(allocator, entry.key_ptr.*);
                defer allocator.free(escaped_key);
                try result.appendSlice(allocator, escaped_key);
                try result.appendSlice(allocator, "&quot;=&gt;");

                // Value
                const transformed_value = try applyEscapeToValueRubyFormat(allocator, entry.value_ptr.*);
                defer allocator.free(transformed_value);
                if (entry.value_ptr.* == .string) {
                    try result.appendSlice(allocator, "&quot;");
                    try result.appendSlice(allocator, transformed_value);
                    try result.appendSlice(allocator, "&quot;");
                } else {
                    try result.appendSlice(allocator, transformed_value);
                }
            }
            try result.appendSlice(allocator, "}");
            return try result.toOwnedSlice(allocator);
        },
        .integer => |i| try std.fmt.allocPrint(allocator, "{d}", .{i}),
        .float => |f| try std.fmt.allocPrint(allocator, "{d}", .{f}),
        .bool => |b| try allocator.dupe(u8, if (b) "true" else "false"),
        .null => try allocator.dupe(u8, "nil"),
        else => try allocator.dupe(u8, ""),
    };
}

const Variable = struct {
    expression: Expression,
    filters: []Filter,

    pub fn parse(allocator: std.mem.Allocator, content: []const u8) !Variable {
        const trimmed = std.mem.trim(u8, content, " \t\n\r");

        // Split by pipe outside of quoted strings to handle pipes in string literals
        var parts = try splitByPipeOutsideStrings(allocator, trimmed);
        defer parts.deinit(allocator);

        if (parts.items.len == 0) {
            return Variable{
                .expression = Expression{ .nil = {} },
                .filters = &.{},
            };
        }

        const expr_str = std.mem.trim(u8, parts.items[0], " \t\n\r");

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

        for (parts.items[1..]) |filter_str| {
            const trimmed_filter = std.mem.trim(u8, filter_str, " \t\n\r");
            // Skip empty filter strings (handles trailing pipe and double pipes)
            if (trimmed_filter.len == 0) continue;
            const filter = try Filter.parse(allocator, trimmed_filter);
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

    // Look for " or " operator (word boundaries), respecting strings
    var pos: usize = 0;
    var in_single_quote = false;
    var in_double_quote = false;
    while (pos + 4 <= trimmed.len) : (pos += 1) {
        const c = trimmed[pos];
        if (c == '\'' and !in_double_quote) {
            in_single_quote = !in_single_quote;
            continue;
        } else if (c == '"' and !in_single_quote) {
            in_double_quote = !in_double_quote;
            continue;
        }
        // Skip if inside a string
        if (in_single_quote or in_double_quote) continue;
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

    // Look for " and " operator (word boundaries), respecting strings
    var pos: usize = 0;
    var in_single_quote = false;
    var in_double_quote = false;
    while (pos + 5 <= trimmed.len) : (pos += 1) {
        const c = trimmed[pos];
        if (c == '\'' and !in_double_quote) {
            in_single_quote = !in_single_quote;
            continue;
        } else if (c == '"' and !in_single_quote) {
            in_double_quote = !in_double_quote;
            continue;
        }
        // Skip if inside a string
        if (in_single_quote or in_double_quote) continue;
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

    // Look for comparison operators (skip characters inside quotes)
    // Check for two-character operators first (==, !=, <=, >=)
    var i: usize = 0;
    var in_single_quote = false;
    var in_double_quote = false;
    while (i + 1 < trimmed.len) : (i += 1) {
        const c = trimmed[i];
        if (c == '\'' and !in_double_quote) {
            in_single_quote = !in_single_quote;
            continue;
        } else if (c == '"' and !in_single_quote) {
            in_double_quote = !in_double_quote;
            continue;
        }
        if (in_single_quote or in_double_quote) continue;

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

    // Check for single-character operators (<, >) - skip characters inside quotes
    i = 0;
    in_single_quote = false;
    in_double_quote = false;
    while (i < trimmed.len) : (i += 1) {
        const c = trimmed[i];
        if (c == '\'' and !in_double_quote) {
            in_single_quote = !in_single_quote;
            continue;
        } else if (c == '"' and !in_single_quote) {
            in_double_quote = !in_double_quote;
            continue;
        }
        if (in_single_quote or in_double_quote) continue;

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

    // Check for range syntax: (start..end)
    if (trimmed.len >= 5 and trimmed[0] == '(' and trimmed[trimmed.len - 1] == ')') {
        const inner = trimmed[1 .. trimmed.len - 1];
        // Look for ".." separator
        if (std.mem.indexOf(u8, inner, "..")) |dot_idx| {
            const start_str = std.mem.trim(u8, inner[0..dot_idx], " \t\n\r");
            const end_str = std.mem.trim(u8, inner[dot_idx + 2 ..], " \t\n\r");

            const start_expr = try parsePrimaryExpression(allocator, start_str);
            errdefer {
                var s = start_expr;
                s.deinit(allocator);
            }

            const end_expr = try parsePrimaryExpression(allocator, end_str);
            errdefer {
                var e = end_expr;
                e.deinit(allocator);
            }

            const range = try allocator.create(Expression.Range);
            range.* = .{
                .start = start_expr,
                .end = end_expr,
            };

            return Expression{ .range = range };
        }
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

    // Check for empty literal (special keyword for comparisons)
    if (std.mem.eql(u8, trimmed, "empty")) {
        return Expression{ .empty = {} };
    }

    // Check for blank literal (special keyword for comparisons)
    if (std.mem.eql(u8, trimmed, "blank")) {
        return Expression{ .blank = {} };
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

// Filter struct uses FilterKind and filter_map imported from filters_mod
const Filter = struct {
    name: []const u8,
    kind: FilterKind, // Comptime-resolved filter type for fast dispatch
    args: [][]const u8,
    args_are_literals: []bool, // true if arg was quoted (literal string), false if variable reference

    pub fn parse(allocator: std.mem.Allocator, content: []const u8) !Filter {
        const trimmed = std.mem.trim(u8, content, " \t\n\r");

        // Find the colon separating filter name from args
        // But don't split inside quotes
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
            // Split by comma but respect quoted strings
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
            // Add the last arg
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

    // Remove surrounding quotes from a string, preserving content inside
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

    // Get the resolved value of an argument, looking up variables in context if needed
    pub fn getResolvedArg(self: *const Filter, index: usize, ctx: *Context) ?[]const u8 {
        if (index >= self.args.len) return null;
        const arg = self.args[index];
        const is_literal = if (index < self.args_are_literals.len) self.args_are_literals[index] else true;

        if (is_literal) {
            return arg;
        } else {
            // Look up variable in context
            if (ctx.get(arg)) |val| {
                switch (val) {
                    .string => |s| return s,
                    .null => return "",
                    else => return arg, // Return the string representation for non-strings
                }
            } else {
                return ""; // Undefined variable = empty string
            }
        }
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
                // Optional property argument: sort by object property
                const items = value.array.items;
                const sorted = try allocator.alloc(json.Value, items.len);
                defer allocator.free(sorted);
                @memcpy(sorted, items);

                if (self.args.len > 0) {
                    // Sort by property
                    const property = self.args[0];
                    std.mem.sort(json.Value, sorted, PropertySortContext{ .property = property }, compareJsonValuesByProperty);
                } else {
                    std.mem.sort(json.Value, sorted, {}, compareJsonValues);
                }

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
                    const item_str = try valueToJsonString(allocator, item);
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
        defer allocator.free(str);

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
        } else if (std.mem.eql(u8, self.name, "date")) {
            // date filter: parse input as date and format with strftime-style specifiers
            const format_arg = if (self.args.len > 0) self.args[0] else "%Y-%m-%d %H:%M:%S";
            // Empty or nil format string returns original input as string
            if (format_arg.len == 0) {
                return try allocator.dupe(u8, str);
            }
            const format = format_arg;

            // Parse the input value to get a timestamp
            const timestamp: i64 = blk: {
                // Handle special keywords
                if (std.ascii.eqlIgnoreCase(str, "now")) {
                    break :blk @divTrunc(std.time.timestamp(), 1);
                } else if (std.ascii.eqlIgnoreCase(str, "today")) {
                    break :blk @divTrunc(std.time.timestamp(), 1);
                }

                // Try to parse as integer (unix timestamp)
                if (value == .integer) {
                    break :blk value.integer;
                }

                // Try numeric string as unix timestamp
                if (std.fmt.parseInt(i64, str, 10)) |ts| {
                    break :blk ts;
                } else |_| {}

                // Try to parse ISO 8601 style dates: YYYY-MM-DD or YYYY-MM-DD HH:MM:SS
                if (parseIsoDate(str)) |parsed| {
                    break :blk parsed.timestamp;
                }

                // If we can't parse, return input unchanged
                return try allocator.dupe(u8, str);
            };

            // Format the date
            return try formatDate(allocator, timestamp, format);
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
            // If either operand is a float, preserve float formatting
            const input_is_float = valueIsFloat(value);
            const arg_is_float = stringIsFloat(self.args[0]);
            if (input_is_float or arg_is_float) {
                return try numberToStringForceFloat(allocator, result);
            }
            return try numberToString(allocator, result);
        } else if (std.mem.eql(u8, self.name, "minus")) {
            // minus: subtract numbers
            const num = try valueToNumber(value);
            if (self.args.len == 0) {
                return try allocator.dupe(u8, str);
            }
            const arg_num = try stringToNumber(self.args[0]);
            const result = num - arg_num;
            // If either operand is a float, preserve float formatting
            const input_is_float = valueIsFloat(value);
            const arg_is_float = stringIsFloat(self.args[0]);
            if (input_is_float or arg_is_float) {
                return try numberToStringForceFloat(allocator, result);
            }
            return try numberToString(allocator, result);
        } else if (std.mem.eql(u8, self.name, "times")) {
            // times: multiply numbers
            const num = try valueToNumber(value);
            if (self.args.len == 0) {
                return try allocator.dupe(u8, str);
            }
            const arg_num = try stringToNumber(self.args[0]);
            const result = num * arg_num;
            // If either operand is a float, preserve float formatting
            const input_is_float = valueIsFloat(value);
            const arg_is_float = stringIsFloat(self.args[0]);
            if (input_is_float or arg_is_float) {
                return try numberToStringForceFloat(allocator, result);
            }
            return try numberToString(allocator, result);
        } else if (std.mem.eql(u8, self.name, "divided_by")) {
            // divided_by: divide numbers
            // In Liquid, integer / integer = integer (truncated), otherwise float
            if (self.args.len == 0) {
                return try allocator.dupe(u8, str);
            }

            // Check if both operands are integers
            const value_is_int = value == .integer;
            const arg_is_int = if (self.args.len > 0) isIntegerString(self.args[0]) else false;

            if (value_is_int and arg_is_int) {
                // Integer division (truncation)
                const num_int = value.integer;
                const arg_int = std.fmt.parseInt(i64, self.args[0], 10) catch 0;
                if (arg_int == 0) {
                    // Division by zero - return error message (lax mode)
                    return try allocator.dupe(u8, "Liquid error (line 1): divided by 0");
                }
                const result = @divTrunc(num_int, arg_int);
                return try std.fmt.allocPrint(allocator, "{d}", .{result});
            } else {
                // Float division - follows IEEE 754 floating-point standards
                const num = try valueToNumber(value);
                const arg_num = try stringToNumber(self.args[0]);
                if (arg_num == 0) {
                    // Float division by zero returns Infinity (IEEE 754 behavior)
                    if (num > 0) {
                        return try allocator.dupe(u8, "Infinity");
                    } else if (num < 0) {
                        return try allocator.dupe(u8, "-Infinity");
                    } else {
                        return try allocator.dupe(u8, "NaN");
                    }
                }
                const result = num / arg_num;
                return try numberToString(allocator, result);
            }
        } else if (std.mem.eql(u8, self.name, "modulo")) {
            // modulo: compute remainder
            const num = try valueToNumber(value);
            if (self.args.len == 0) {
                return try allocator.dupe(u8, str);
            }
            const arg_num = try stringToNumber(self.args[0]);
            if (arg_num == 0) {
                // Modulo by zero - return error message (same as divided_by)
                return try allocator.dupe(u8, "Liquid error (line 1): divided by 0");
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
        } else if (std.mem.eql(u8, self.name, "at_least")) {
            // at_least: ensures minimum value
            const num = try valueToNumber(value);
            if (self.args.len == 0) {
                return try allocator.dupe(u8, str);
            }
            const min_val = try stringToNumber(self.args[0]);
            const result = @max(num, min_val);
            return try numberToString(allocator, result);
        } else if (std.mem.eql(u8, self.name, "at_most")) {
            // at_most: ensures maximum value
            const num = try valueToNumber(value);
            if (self.args.len == 0) {
                return try allocator.dupe(u8, str);
            }
            const max_val = try stringToNumber(self.args[0]);
            const result = @min(num, max_val);
            return try numberToString(allocator, result);
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
                // Empty search string inserts replacement between every character
                // Ruby: "hi".gsub("", "x") => "xhxix"
                var result: std.ArrayList(u8) = .{};
                try result.appendSlice(allocator, replace_with); // Before first char
                for (str) |c| {
                    try result.append(allocator, c);
                    try result.appendSlice(allocator, replace_with); // After each char
                }
                return try result.toOwnedSlice(allocator);
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
        } else if (std.mem.eql(u8, self.name, "remove_first")) {
            // remove_first: remove first occurrence of substring
            if (self.args.len == 0) {
                return try allocator.dupe(u8, str);
            }
            const search = self.args[0];
            if (search.len == 0) {
                return try allocator.dupe(u8, str);
            }
            if (std.mem.indexOf(u8, str, search)) |pos| {
                var result: std.ArrayList(u8) = .{};
                try result.appendSlice(allocator, str[0..pos]);
                try result.appendSlice(allocator, str[pos + search.len ..]);
                return try result.toOwnedSlice(allocator);
            }
            return try allocator.dupe(u8, str);
        } else if (std.mem.eql(u8, self.name, "remove_last")) {
            // remove_last: remove last occurrence of substring
            if (self.args.len == 0) {
                return try allocator.dupe(u8, str);
            }
            const search = self.args[0];
            if (search.len == 0) {
                return try allocator.dupe(u8, str);
            }
            if (std.mem.lastIndexOf(u8, str, search)) |pos| {
                var result: std.ArrayList(u8) = .{};
                try result.appendSlice(allocator, str[0..pos]);
                try result.appendSlice(allocator, str[pos + search.len ..]);
                return try result.toOwnedSlice(allocator);
            }
            return try allocator.dupe(u8, str);
        } else if (std.mem.eql(u8, self.name, "replace_first")) {
            // replace_first: replace first occurrence of substring
            if (self.args.len < 2) {
                return try allocator.dupe(u8, str);
            }
            const search = self.args[0];
            const replace_with = self.args[1];
            if (search.len == 0) {
                return try allocator.dupe(u8, str);
            }
            if (std.mem.indexOf(u8, str, search)) |pos| {
                var result: std.ArrayList(u8) = .{};
                try result.appendSlice(allocator, str[0..pos]);
                try result.appendSlice(allocator, replace_with);
                try result.appendSlice(allocator, str[pos + search.len ..]);
                return try result.toOwnedSlice(allocator);
            }
            return try allocator.dupe(u8, str);
        } else if (std.mem.eql(u8, self.name, "replace_last")) {
            // replace_last: replace last occurrence of substring
            if (self.args.len < 2) {
                return try allocator.dupe(u8, str);
            }
            const search = self.args[0];
            const replace_with = self.args[1];
            if (search.len == 0) {
                return try allocator.dupe(u8, str);
            }
            if (std.mem.lastIndexOf(u8, str, search)) |pos| {
                var result: std.ArrayList(u8) = .{};
                try result.appendSlice(allocator, str[0..pos]);
                try result.appendSlice(allocator, replace_with);
                try result.appendSlice(allocator, str[pos + search.len ..]);
                return try result.toOwnedSlice(allocator);
            }
            return try allocator.dupe(u8, str);
        } else if (std.mem.eql(u8, self.name, "slice")) {
            // slice: extract substring by position
            if (self.args.len == 0) {
                return try allocator.dupe(u8, str);
            }
            const start_int = std.fmt.parseInt(i64, self.args[0], 10) catch {
                return try std.fmt.allocPrint(allocator, "Liquid error (line 1): invalid integer", .{});
            };
            const start: usize = if (start_int < 0)
                if (@as(i64, @intCast(str.len)) + start_int < 0) 0 else @intCast(@as(i64, @intCast(str.len)) + start_int)
            else
                @intCast(@min(start_int, @as(i64, @intCast(str.len))));

            if (start >= str.len) {
                return try allocator.dupe(u8, "");
            }

            if (self.args.len > 1) {
                const len_i64 = std.fmt.parseInt(i64, self.args[1], 10) catch {
                    return try std.fmt.allocPrint(allocator, "Liquid error (line 1): invalid integer", .{});
                };
                // Negative length returns empty string
                if (len_i64 < 0) {
                    return try allocator.dupe(u8, "");
                }
                const len: usize = @intCast(len_i64);
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
            const ellipsis = if (self.args.len > 1) self.args[1] else "...";

            // Parse max_len, handling overflow and negative numbers gracefully
            const max_len: usize = blk: {
                // First try parsing as float to handle scientific notation
                if (std.fmt.parseFloat(f64, self.args[0])) |f| {
                    if (f < 0) {
                        // Negative length - return just ellipsis
                        break :blk 0;
                    }
                    // Very large numbers - return string as-is
                    if (f > @as(f64, @floatFromInt(std.math.maxInt(usize)))) {
                        return try allocator.dupe(u8, str);
                    }
                    break :blk @intFromFloat(f);
                } else |_| {
                    // Not a valid number - return string as-is
                    return try allocator.dupe(u8, str);
                }
            };

            if (str.len <= max_len) {
                return try allocator.dupe(u8, str);
            }

            // When max_len <= ellipsis.len, return just the ellipsis
            if (max_len <= ellipsis.len) {
                return try allocator.dupe(u8, ellipsis);
            }

            const text_len = max_len - ellipsis.len;
            return try std.mem.concat(allocator, u8, &[_][]const u8{ str[0..text_len], ellipsis });
        } else if (std.mem.eql(u8, self.name, "truncatewords")) {
            // truncatewords: limit word count
            if (self.args.len == 0) {
                return try allocator.dupe(u8, str);
            }

            const ellipsis = if (self.args.len > 1) self.args[1] else "...";

            // Parse max_words, handling overflow and negative numbers gracefully
            const max_words: usize = blk: {
                // First try parsing as float to handle scientific notation
                if (std.fmt.parseFloat(f64, self.args[0])) |f| {
                    if (f < 0) {
                        // Negative word count - use 1 word
                        break :blk 1;
                    }
                    // Very large numbers - return string as-is
                    if (f > @as(f64, @floatFromInt(std.math.maxInt(usize)))) {
                        return try allocator.dupe(u8, str);
                    }
                    break :blk @intFromFloat(f);
                } else |_| {
                    // Not a valid number - return string as-is
                    return try allocator.dupe(u8, str);
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
            // newline_to_br: insert <br /> BEFORE each newline (preserving the newline)
            var result: std.ArrayList(u8) = .{};
            for (str) |c| {
                if (c == '\n') {
                    try result.appendSlice(allocator, "<br />\n");
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
        } else if (std.mem.eql(u8, self.name, "strip_newlines")) {
            // strip_newlines: remove all newline characters
            var result: std.ArrayList(u8) = .{};
            for (str) |c| {
                if (c != '\n' and c != '\r') {
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

    // Apply filter with context for resolving variable args
    pub fn applyValueWithContext(self: *Filter, allocator: std.mem.Allocator, value: json.Value, ctx: *Context) !FilterResult {
        // Special handling for concat filter - needs array value, not string
        if (std.mem.eql(u8, self.name, "concat") and value == .array) {
            var arr = json.Array.init(allocator);
            // Add all items from first array
            for (value.array.items) |item| {
                try arr.append(item);
            }
            // Get the second array from context
            if (self.args.len > 0) {
                const arg = self.args[0];
                const is_literal = if (self.args_are_literals.len > 0) self.args_are_literals[0] else true;
                if (!is_literal) {
                    // Look up variable in context
                    if (ctx.get(arg)) |val| {
                        if (val == .array) {
                            for (val.array.items) |item| {
                                try arr.append(item);
                            }
                        }
                    }
                }
            }
            return FilterResult{ .json_value = json.Value{ .array = arr } };
        }

        // Special handling for where filter with boolean value
        if (std.mem.eql(u8, self.name, "where") and value == .array) {
            if (self.args.len >= 2) {
                const property = self.args[0];
                const target_arg = self.args[1];
                const is_literal = if (self.args_are_literals.len > 1) self.args_are_literals[1] else true;

                // Get the actual value (handle true/false as booleans)
                var target_value: ?json.Value = null;
                if (is_literal) {
                    if (std.mem.eql(u8, target_arg, "true")) {
                        target_value = json.Value{ .bool = true };
                    } else if (std.mem.eql(u8, target_arg, "false")) {
                        target_value = json.Value{ .bool = false };
                    } else {
                        target_value = json.Value{ .string = target_arg };
                    }
                } else {
                    target_value = ctx.get(target_arg);
                }

                var arr = json.Array.init(allocator);
                for (value.array.items) |item| {
                    if (item == .object) {
                        if (item.object.get(property)) |prop_value| {
                            const matches = if (target_value) |tv| blk: {
                                // Compare values directly for type-aware comparison
                                if (tv == .bool and prop_value == .bool) {
                                    break :blk tv.bool == prop_value.bool;
                                } else if (tv == .string and prop_value == .string) {
                                    break :blk std.mem.eql(u8, tv.string, prop_value.string);
                                } else if (tv == .integer and prop_value == .integer) {
                                    break :blk tv.integer == prop_value.integer;
                                } else {
                                    // Cross-type comparison - try string conversion
                                    const prop_str = try valueToString(allocator, prop_value);
                                    defer allocator.free(prop_str);
                                    const tv_str = try valueToString(allocator, tv);
                                    defer allocator.free(tv_str);
                                    break :blk std.mem.eql(u8, prop_str, tv_str);
                                }
                            } else !isFalsy(prop_value);
                            if (matches) {
                                try arr.append(item);
                            }
                        }
                    }
                }
                return FilterResult{ .json_value = json.Value{ .array = arr } };
            }
        }

        // Resolve args that are variable references
        var resolved_args: std.ArrayList([]const u8) = .{};
        defer {
            for (resolved_args.items) |arg| {
                allocator.free(arg);
            }
            resolved_args.deinit(allocator);
        }

        for (self.args, 0..) |arg, i| {
            const is_literal = if (i < self.args_are_literals.len) self.args_are_literals[i] else true;
            if (is_literal) {
                try resolved_args.append(allocator, try allocator.dupe(u8, arg));
            } else {
                // Look up variable in context
                if (ctx.get(arg)) |val| {
                    const val_str = try valueToString(allocator, val);
                    // valueToString returns owned string for integer/float
                    if (val == .integer or val == .float) {
                        try resolved_args.append(allocator, val_str);
                    } else {
                        try resolved_args.append(allocator, try allocator.dupe(u8, val_str));
                    }
                } else {
                    // Undefined variable = empty string
                    try resolved_args.append(allocator, try allocator.dupe(u8, ""));
                }
            }
        }

        // Handle date filter with context (for frozen_time support)
        if (std.mem.eql(u8, self.name, "date")) {
            const str = try valueToString(allocator, value);
            defer allocator.free(str);

            const format_arg = if (resolved_args.items.len > 0) resolved_args.items[0] else "%Y-%m-%d %H:%M:%S";
            // Empty or nil format string returns original input as string
            if (format_arg.len == 0) {
                return FilterResult{ .string = try allocator.dupe(u8, str) };
            }
            const format = format_arg;

            // Parse the input value to get a timestamp and timezone
            const parsed: ParsedDate = blk: {
                // Handle special keywords - use frozen_time if available
                if (std.ascii.eqlIgnoreCase(str, "now") or std.ascii.eqlIgnoreCase(str, "today")) {
                    if (ctx.frozen_time) |ft| {
                        break :blk .{ .timestamp = ft, .tz_offset_minutes = 0 };
                    }
                    break :blk .{ .timestamp = @divTrunc(std.time.timestamp(), 1), .tz_offset_minutes = 0 };
                }

                // Try to parse as integer (unix timestamp)
                if (value == .integer) {
                    break :blk .{ .timestamp = value.integer, .tz_offset_minutes = 0 };
                }

                // Try numeric string as unix timestamp
                if (std.fmt.parseInt(i64, str, 10)) |ts| {
                    break :blk .{ .timestamp = ts, .tz_offset_minutes = 0 };
                } else |_| {}

                // Try to parse date string (ISO or verbose format)
                if (parseDate(str)) |pd| {
                    break :blk pd;
                }

                // If we can't parse, return input unchanged
                return FilterResult{ .string = try allocator.dupe(u8, str) };
            };

            // Format the date with timezone
            const result = try formatDateWithTz(allocator, parsed.timestamp, format, parsed.tz_offset_minutes);
            return FilterResult{ .string = result };
        }

        // Create a temporary filter with resolved args
        const original_args = self.args;
        self.args = resolved_args.items;
        defer self.args = original_args;

        return try self.applyValue(allocator, value);
    }

    // Apply filter and return json.Value to preserve arrays for chaining
    pub fn applyValue(self: *Filter, allocator: std.mem.Allocator, value: json.Value) !FilterResult {
        // Handle default filter - check for falsy values (nil, false, empty string/array)
        if (std.mem.eql(u8, self.name, "default")) {
            const is_falsy = switch (value) {
                .null => true,
                .bool => |b| !b,
                .string => |s| s.len == 0,
                .array => |arr| arr.items.len == 0,
                else => false,
            };
            if (is_falsy and self.args.len > 0) {
                // Return the default value as a string
                return FilterResult{ .string = try allocator.dupe(u8, self.args[0]) };
            }
            // Otherwise, return the original value as a string
            const str = try valueToString(allocator, value);
            return FilterResult{ .string = str };
        }

        // Handle array-specific filters that should return arrays
        if (value == .array) {
            const items = value.array.items;

            if (std.mem.eql(u8, self.name, "reverse")) {
                // reverse: reverse array order - return array
                if (items.len == 0) {
                    // Return empty array
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
                // sort: sort array - return array
                // Optional property argument: sort by object property
                if (items.len == 0) {
                    const arr = json.Array.init(allocator);
                    return FilterResult{ .json_value = json.Value{ .array = arr } };
                }

                const sorted = try allocator.alloc(json.Value, items.len);
                defer allocator.free(sorted);
                @memcpy(sorted, items);

                // Check if we have a property argument for sorting objects
                if (self.args.len > 0) {
                    // Sort by property
                    const property = self.args[0];
                    std.mem.sort(json.Value, sorted, PropertySortContext{ .property = property }, compareJsonValuesByProperty);
                } else {
                    // Check for incompatible types (mixed types cannot be sorted)
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
                    std.mem.sort(json.Value, sorted, {}, compareJsonValues);
                }

                var arr = json.Array.init(allocator);
                for (sorted) |item| {
                    try arr.append(item);
                }
                return FilterResult{ .json_value = json.Value{ .array = arr } };
            } else if (std.mem.eql(u8, self.name, "sort_natural")) {
                // sort_natural: sort array case-insensitively - return array
                if (items.len == 0) {
                    const arr = json.Array.init(allocator);
                    return FilterResult{ .json_value = json.Value{ .array = arr } };
                }
                const sorted = try allocator.alloc(json.Value, items.len);
                defer allocator.free(sorted);
                @memcpy(sorted, items);
                std.mem.sort(json.Value, sorted, {}, compareJsonValuesNatural);
                var arr = json.Array.init(allocator);
                for (sorted) |item| {
                    try arr.append(item);
                }
                return FilterResult{ .json_value = json.Value{ .array = arr } };
            } else if (std.mem.eql(u8, self.name, "uniq")) {
                // uniq: remove duplicates - return array
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
                    const item_str = try valueToJsonString(allocator, item);
                    if (!seen.contains(item_str)) {
                        try seen.put(item_str, {});
                        try arr.append(item);
                    } else {
                        allocator.free(item_str);
                    }
                }
                return FilterResult{ .json_value = json.Value{ .array = arr } };
            } else if (std.mem.eql(u8, self.name, "compact")) {
                // compact: remove nil values - return array
                var arr = json.Array.init(allocator);
                for (items) |item| {
                    if (item != .null) {
                        try arr.append(item);
                    }
                }
                return FilterResult{ .json_value = json.Value{ .array = arr } };
            } else if (std.mem.eql(u8, self.name, "map")) {
                // map: extract property - return array
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
                // where: filter by property - return array
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
                                const prop_str = try valueToString(allocator, prop_value);
                                defer allocator.free(prop_str);
                                break :blk std.mem.eql(u8, prop_str, tv);
                            } else !isFalsy(prop_value);
                            if (matches) {
                                try arr.append(item);
                            }
                        }
                    }
                }
                return FilterResult{ .json_value = json.Value{ .array = arr } };
            } else if (std.mem.eql(u8, self.name, "concat")) {
                // concat: concatenate arrays - return array
                var arr = json.Array.init(allocator);
                // Add all items from first array
                for (items) |item| {
                    try arr.append(item);
                }
                // If we have an arg that should be another array, we can't easily parse it
                // This is a limitation - concat works best in applyValueWithContext
                return FilterResult{ .json_value = json.Value{ .array = arr } };
            } else if (std.mem.eql(u8, self.name, "join")) {
                // join: convert array to string with separator
                const separator = if (self.args.len > 0) self.args[0] else ", ";
                var result: std.ArrayList(u8) = .{};
                for (items, 0..) |item, i| {
                    if (i > 0) {
                        try result.appendSlice(allocator, separator);
                    }
                    const item_str = try valueToString(allocator, item);
                    try result.appendSlice(allocator, item_str);
                    if (item == .integer or item == .float) {
                        allocator.free(item_str);
                    }
                }
                return FilterResult{ .string = try result.toOwnedSlice(allocator) };
            } else if (std.mem.eql(u8, self.name, "first")) {
                // first: get first element
                if (items.len == 0) {
                    return FilterResult{ .json_value = json.Value{ .null = {} } };
                }
                return FilterResult{ .json_value = items[0] };
            } else if (std.mem.eql(u8, self.name, "last")) {
                // last: get last element
                if (items.len == 0) {
                    return FilterResult{ .json_value = json.Value{ .null = {} } };
                }
                return FilterResult{ .json_value = items[items.len - 1] };
            } else if (std.mem.eql(u8, self.name, "size")) {
                // size: return array length
                return FilterResult{ .json_value = json.Value{ .integer = @intCast(items.len) } };
            } else if (std.mem.eql(u8, self.name, "upcase") or std.mem.eql(u8, self.name, "downcase")) {
                // upcase/downcase on array: Ruby returns the Ruby inspect format string
                const is_upcase = std.mem.eql(u8, self.name, "upcase");
                const result = try applyCaseToValueRubyFormat(allocator, value, is_upcase);
                return FilterResult{ .string = result };
            } else if (std.mem.eql(u8, self.name, "escape") or std.mem.eql(u8, self.name, "escape_once")) {
                // escape/escape_once on array: Ruby returns escaped Ruby inspect format
                const result = try applyEscapeToValueRubyFormat(allocator, value);
                return FilterResult{ .string = result };
            } else if (std.mem.eql(u8, self.name, "capitalize")) {
                // capitalize on array: Ruby returns Ruby inspect format (no transformation)
                const result = try valueToRubyInspectString(allocator, value);
                return FilterResult{ .string = result };
            }
        }

        // Handle first/last for hashes (objects)
        if (value == .object) {
            const obj = value.object;
            if (std.mem.eql(u8, self.name, "first")) {
                // hash | first returns [key, value] array for first entry
                var iter = obj.iterator();
                if (iter.next()) |entry| {
                    var arr = json.Array.init(allocator);
                    try arr.append(json.Value{ .string = entry.key_ptr.* });
                    try arr.append(entry.value_ptr.*);
                    return FilterResult{ .json_value = json.Value{ .array = arr } };
                }
                return FilterResult{ .json_value = json.Value{ .null = {} } };
            } else if (std.mem.eql(u8, self.name, "last")) {
                // Ruby quirk: hash | last always returns empty string (nil)
                // Hashes don't have a meaningful "last" in Ruby
                return FilterResult{ .json_value = json.Value{ .null = {} } };
            } else if (std.mem.eql(u8, self.name, "size")) {
                return FilterResult{ .json_value = json.Value{ .integer = @intCast(obj.count()) } };
            } else if (std.mem.eql(u8, self.name, "upcase") or std.mem.eql(u8, self.name, "downcase")) {
                // upcase/downcase on hash/object: Ruby returns Ruby inspect format string
                const is_upcase = std.mem.eql(u8, self.name, "upcase");
                const result = try applyCaseToValueRubyFormat(allocator, value, is_upcase);
                return FilterResult{ .string = result };
            } else if (std.mem.eql(u8, self.name, "escape") or std.mem.eql(u8, self.name, "escape_once")) {
                // escape/escape_once on hash/object: Ruby returns escaped Ruby inspect format
                const result = try applyEscapeToValueRubyFormat(allocator, value);
                return FilterResult{ .string = result };
            } else if (std.mem.eql(u8, self.name, "capitalize")) {
                // capitalize on hash/object: Ruby returns Ruby inspect format (no transformation)
                const result = try valueToRubyInspectString(allocator, value);
                return FilterResult{ .string = result };
            }
        }

        // Handle first/last/size for strings (also handle bools/numbers/null specially)
        if (std.mem.eql(u8, self.name, "first") or std.mem.eql(u8, self.name, "last")) {
            if (value == .string) {
                const str = value.string;
                if (std.mem.eql(u8, self.name, "first")) {
                    if (str.len == 0) {
                        return FilterResult{ .string = try allocator.dupe(u8, "") };
                    }
                    return FilterResult{ .string = try allocator.dupe(u8, str[0..1]) };
                } else {
                    if (str.len == 0) {
                        return FilterResult{ .string = try allocator.dupe(u8, "") };
                    }
                    return FilterResult{ .string = try allocator.dupe(u8, str[str.len - 1 .. str.len]) };
                }
            } else if (value == .bool or value == .integer or value == .float or value == .null) {
                // Ruby quirk: first/last on non-string/non-array returns empty string
                return FilterResult{ .string = try allocator.dupe(u8, "") };
            }
        }

        // Handle size for non-collection types (Ruby quirks)
        if (std.mem.eql(u8, self.name, "size")) {
            if (value == .bool or value == .null or value == .float) {
                // Ruby quirk: bool, nil, and float | size returns 0
                return FilterResult{ .json_value = json.Value{ .integer = 0 } };
            } else if (value == .integer) {
                // Ruby quirk: integer.size returns 8 (bytes of internal representation)
                return FilterResult{ .json_value = json.Value{ .integer = 8 } };
            } else if (value == .string) {
                return FilterResult{ .json_value = json.Value{ .integer = @intCast(value.string.len) } };
            }
        }

        // Handle split specially - it returns an array
        if (std.mem.eql(u8, self.name, "split")) {
            const str = try valueToString(allocator, value);
            defer allocator.free(str);

            if (str.len == 0) {
                // Empty string returns empty array
                const arr = json.Array.init(allocator);
                return FilterResult{ .json_value = json.Value{ .array = arr } };
            }

            const separator = if (self.args.len > 0) self.args[0] else " ";
            var arr = json.Array.init(allocator);

            if (separator.len == 0) {
                // Empty separator - split into characters
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

        // Handle math filters with proper integer type preservation
        if (std.mem.eql(u8, self.name, "times") or
            std.mem.eql(u8, self.name, "plus") or
            std.mem.eql(u8, self.name, "minus"))
        {
            if (self.args.len == 0) {
                return FilterResult{ .json_value = value };
            }
            // Check if operands are integers:
            // - integer type is integer
            // - null is treated as integer 0
            // - string is integer if it parses as integer OR is non-numeric (becomes 0)
            const value_is_int = blk: {
                if (value == .integer or value == .null) break :blk true;
                if (value == .string) {
                    // Integer string like "5" or non-numeric like "hello" both treated as int
                    break :blk isIntegerString(value.string) or !stringIsFloat(value.string);
                }
                break :blk false;
            };
            const arg_is_int = isIntegerString(self.args[0]);

            if (value_is_int and arg_is_int) {
                const num_int: i64 = blk: {
                    if (value == .integer) break :blk value.integer;
                    if (value == .null) break :blk 0;
                    if (value == .string) break :blk std.fmt.parseInt(i64, value.string, 10) catch 0;
                    break :blk 0;
                };
                const arg_int = std.fmt.parseInt(i64, self.args[0], 10) catch 0;
                const result_int: i64 = if (std.mem.eql(u8, self.name, "times"))
                    num_int * arg_int
                else if (std.mem.eql(u8, self.name, "plus"))
                    num_int + arg_int
                else
                    num_int - arg_int;
                return FilterResult{ .json_value = json.Value{ .integer = result_int } };
            } else {
                // Float arithmetic
                const num = try valueToNumber(value);
                const arg_num = try stringToNumber(self.args[0]);
                const result_float: f64 = if (std.mem.eql(u8, self.name, "times"))
                    num * arg_num
                else if (std.mem.eql(u8, self.name, "plus"))
                    num + arg_num
                else
                    num - arg_num;
                return FilterResult{ .json_value = json.Value{ .float = result_float } };
            }
        } else if (std.mem.eql(u8, self.name, "divided_by")) {
            if (self.args.len == 0) {
                return FilterResult{ .json_value = value };
            }
            // Check if both operands are integers
            const value_is_int = blk: {
                if (value == .integer or value == .null) break :blk true;
                if (value == .string) {
                    break :blk isIntegerString(value.string) or !stringIsFloat(value.string);
                }
                break :blk false;
            };
            const arg_is_int = isIntegerString(self.args[0]);

            if (value_is_int and arg_is_int) {
                const num_int: i64 = blk: {
                    if (value == .integer) break :blk value.integer;
                    if (value == .null) break :blk 0;
                    if (value == .string) break :blk std.fmt.parseInt(i64, value.string, 10) catch 0;
                    break :blk 0;
                };
                const arg_int = std.fmt.parseInt(i64, self.args[0], 10) catch 0;
                if (arg_int == 0) {
                    return FilterResult{ .string = try allocator.dupe(u8, "Liquid error (line 1): divided by 0") };
                }
                const result_int = @divTrunc(num_int, arg_int);
                return FilterResult{ .json_value = json.Value{ .integer = result_int } };
            } else {
                // Float division
                const num = try valueToNumber(value);
                const arg_num = try stringToNumber(self.args[0]);
                if (arg_num == 0) {
                    if (num > 0) return FilterResult{ .string = try allocator.dupe(u8, "Infinity") };
                    if (num < 0) return FilterResult{ .string = try allocator.dupe(u8, "-Infinity") };
                    return FilterResult{ .string = try allocator.dupe(u8, "NaN") };
                }
                return FilterResult{ .json_value = json.Value{ .float = num / arg_num } };
            }
        } else if (std.mem.eql(u8, self.name, "modulo")) {
            if (self.args.len == 0) {
                return FilterResult{ .json_value = value };
            }
            // Check if both operands are integers
            const value_is_int = blk: {
                if (value == .integer or value == .null) break :blk true;
                if (value == .string) {
                    break :blk isIntegerString(value.string) or !stringIsFloat(value.string);
                }
                break :blk false;
            };
            const arg_is_int = isIntegerString(self.args[0]);

            if (value_is_int and arg_is_int) {
                const num_int: i64 = blk: {
                    if (value == .integer) break :blk value.integer;
                    if (value == .null) break :blk 0;
                    if (value == .string) break :blk std.fmt.parseInt(i64, value.string, 10) catch 0;
                    break :blk 0;
                };
                const arg_int = std.fmt.parseInt(i64, self.args[0], 10) catch 0;
                if (arg_int == 0) {
                    return FilterResult{ .string = try allocator.dupe(u8, "Liquid error (line 1): divided by 0") };
                }
                const result_int = @mod(num_int, arg_int);
                return FilterResult{ .json_value = json.Value{ .integer = result_int } };
            } else {
                const num = try valueToNumber(value);
                const arg_num = try stringToNumber(self.args[0]);
                if (arg_num == 0) {
                    return FilterResult{ .string = try allocator.dupe(u8, "Liquid error (line 1): divided by 0") };
                }
                return FilterResult{ .json_value = json.Value{ .float = @rem(num, arg_num) } };
            }
        }

        // Fall back to string-based apply for all other filters
        const result = try self.apply(allocator, value);
        return FilterResult{ .string = result };
    }

};

// Helper to clone an array of filters with proper deep copy
fn cloneFilters(allocator: std.mem.Allocator, filters: []const Filter) ![]Filter {
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

// Date parsing and formatting helpers for the date filter

const ParsedDate = struct {
    timestamp: i64,
    tz_offset_minutes: i16,
};

fn parseDate(str: []const u8) ?ParsedDate {
    // Try ISO format first: YYYY-MM-DD or YYYY-MM-DD HH:MM:SS [+-]HHMM
    if (parseIsoDate(str)) |result| return result;

    // Try verbose format: "Fri Jul 16 01:00:00 2004"
    if (parseVerboseDate(str)) |result| return result;

    return null;
}

fn parseIsoDate(str: []const u8) ?ParsedDate {
    // Try to parse YYYY-MM-DD or YYYY-MM-DD HH:MM:SS [+-]HH:MM
    if (str.len < 10) return null;

    // Parse year
    const year = std.fmt.parseInt(i32, str[0..4], 10) catch return null;
    if (str[4] != '-') return null;

    // Parse month
    const month = std.fmt.parseInt(u8, str[5..7], 10) catch return null;
    if (month < 1 or month > 12) return null;
    if (str[7] != '-') return null;

    // Parse day
    const day = std.fmt.parseInt(u8, str[8..10], 10) catch return null;
    if (day < 1 or day > 31) return null;

    var hour: u8 = 0;
    var minute: u8 = 0;
    var second: u8 = 0;
    var tz_offset_minutes: i16 = 0;

    // Check for time part
    if (str.len >= 19 and (str[10] == ' ' or str[10] == 'T')) {
        hour = std.fmt.parseInt(u8, str[11..13], 10) catch 0;
        if (str[13] == ':') {
            minute = std.fmt.parseInt(u8, str[14..16], 10) catch 0;
        }
        if (str.len >= 19 and str[16] == ':') {
            second = std.fmt.parseInt(u8, str[17..19], 10) catch 0;
        }

        // Check for timezone offset after time (position 19 or 20)
        var tz_start: usize = 19;
        if (str.len > 19 and str[19] == ' ') tz_start = 20;

        if (str.len > tz_start) {
            tz_offset_minutes = parseTimezoneOffset(str[tz_start..]) orelse 0;
        }
    }

    // Convert to unix timestamp (in UTC)
    const epoch_days = epochDaysFromYmd(year, month, day);
    var epoch_seconds = epoch_days * 86400 + @as(i64, hour) * 3600 + @as(i64, minute) * 60 + @as(i64, second);
    // Adjust for timezone (convert local time to UTC)
    epoch_seconds -= @as(i64, tz_offset_minutes) * 60;

    return .{ .timestamp = epoch_seconds, .tz_offset_minutes = tz_offset_minutes };
}

fn parseVerboseDate(str: []const u8) ?ParsedDate {
    // Parse "Fri Jul 16 01:00:00 2004" format
    // Format: DOW MON DD HH:MM:SS YYYY
    if (str.len < 24) return null;

    // Skip weekday (first 4 chars including space)
    var pos: usize = 0;
    while (pos < str.len and str[pos] != ' ') pos += 1;
    if (pos >= str.len) return null;
    pos += 1; // skip space

    // Parse month abbreviation
    if (pos + 3 >= str.len) return null;
    const month = monthFromAbbr(str[pos .. pos + 3]) orelse return null;
    pos += 3;
    if (pos >= str.len or str[pos] != ' ') return null;
    pos += 1;

    // Parse day (may be space-padded)
    while (pos < str.len and str[pos] == ' ') pos += 1;
    const day_start = pos;
    while (pos < str.len and str[pos] >= '0' and str[pos] <= '9') pos += 1;
    if (pos == day_start) return null;
    const day = std.fmt.parseInt(u8, str[day_start..pos], 10) catch return null;
    if (pos >= str.len or str[pos] != ' ') return null;
    pos += 1;

    // Parse time HH:MM:SS
    if (pos + 8 > str.len) return null;
    const hour = std.fmt.parseInt(u8, str[pos .. pos + 2], 10) catch return null;
    if (str[pos + 2] != ':') return null;
    const minute = std.fmt.parseInt(u8, str[pos + 3 .. pos + 5], 10) catch return null;
    if (str[pos + 5] != ':') return null;
    const second = std.fmt.parseInt(u8, str[pos + 6 .. pos + 8], 10) catch return null;
    pos += 8;

    if (pos >= str.len or str[pos] != ' ') return null;
    pos += 1;

    // Parse year
    const year_start = pos;
    while (pos < str.len and str[pos] >= '0' and str[pos] <= '9') pos += 1;
    const year = std.fmt.parseInt(i32, str[year_start..pos], 10) catch return null;

    // Convert to unix timestamp
    const epoch_days = epochDaysFromYmd(year, month, day);
    const epoch_seconds = epoch_days * 86400 + @as(i64, hour) * 3600 + @as(i64, minute) * 60 + @as(i64, second);

    return .{ .timestamp = epoch_seconds, .tz_offset_minutes = 0 };
}

fn parseTimezoneOffset(str: []const u8) ?i16 {
    // Parse timezone offset: +HHMM, -HHMM, +HH:MM, -HH:MM
    if (str.len < 5) return null;

    const sign: i16 = if (str[0] == '-') -1 else if (str[0] == '+') 1 else return null;

    // Check if format is +HH:MM or +HHMM
    if (str.len >= 6 and str[3] == ':') {
        // +HH:MM format
        const hours = std.fmt.parseInt(i16, str[1..3], 10) catch return null;
        const minutes = std.fmt.parseInt(i16, str[4..6], 10) catch return null;
        return sign * (hours * 60 + minutes);
    } else if (str.len >= 5) {
        // +HHMM format
        const hours = std.fmt.parseInt(i16, str[1..3], 10) catch return null;
        const minutes = std.fmt.parseInt(i16, str[3..5], 10) catch return null;
        return sign * (hours * 60 + minutes);
    }

    return null;
}

fn monthFromAbbr(abbr: []const u8) ?u8 {
    const months = [_][]const u8{ "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" };
    for (months, 0..) |m, i| {
        if (std.ascii.eqlIgnoreCase(abbr, m)) return @intCast(i + 1);
    }
    return null;
}

fn epochDaysFromYmd(year: i32, month: u8, day: u8) i64 {
    // Convert year/month/day to days since Unix epoch (1970-01-01)
    var y = @as(i64, year);
    var m = @as(i64, month);

    // Adjust for months before March
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

fn formatDate(allocator: std.mem.Allocator, timestamp: i64, format: []const u8) ![]u8 {
    return formatDateWithTz(allocator, timestamp, format, 0);
}

fn formatDateWithTz(allocator: std.mem.Allocator, timestamp: i64, format: []const u8, tz_offset_minutes: i16) ![]u8 {
    // Adjust timestamp to local time for formatting
    const local_timestamp = timestamp + @as(i64, tz_offset_minutes) * 60;
    var dt = strftime.DateTime.fromTimestamp(local_timestamp);
    dt.tz_offset_minutes = tz_offset_minutes;
    return strftime.strftime(allocator, dt, format);
}

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
        tablerow,
        comment,
        raw,
        capture,
        case_tag,
        increment,
        decrement,
        cycle,
        include,
        render,
        loop_break,
        loop_continue,
        echo, // {% echo expression %}
        liquid_tag, // {% liquid ... %}
        ifchanged, // {% ifchanged %}...{% endifchanged %}
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
