const std = @import("std");
const utils = @import("utils.zig");

pub const SourceLocation = struct {
    line: usize,
    column: usize,
    offset: usize,
};

pub const Token = struct {
    kind: TokenKind,
    content: []const u8,
    location: SourceLocation,
    strip_left: bool,
    strip_right: bool,

    pub const TokenKind = enum {
        text,
        variable,
        tag,
    };

    pub fn deinit(self: Token, allocator: std.mem.Allocator) void {
        allocator.free(self.content);
    }
};

/// Apply whitespace control based on strip_left and strip_right flags
pub fn applyWhitespaceControl(allocator: std.mem.Allocator, tokens: *std.ArrayList(Token)) !void {
    var i: usize = 0;
    while (i < tokens.items.len) : (i += 1) {
        const token = &tokens.items[i];

        // If this token has strip_left=true, strip whitespace from the end of the previous text token
        if (token.strip_left and i > 0) {
            var prev_token = &tokens.items[i - 1];
            if (prev_token.kind == .text) {
                const trimmed = utils.trimRight(prev_token.content);
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
                const trimmed = utils.trimLeft(next_token.content);
                if (trimmed.len != next_token.content.len) {
                    const new_content = try allocator.dupe(u8, trimmed);
                    allocator.free(next_token.content);
                    next_token.content = new_content;
                }
            }
        }
    }
}

pub const Lexer = struct {
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
                            } else if (j == content_start and strip_left) {
                                // Empty content with strip_left: {{-}} means strip both sides
                                strip_right = true;
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
                            } else if (j == content_start and strip_left) {
                                // Empty content with strip_left: {%-} means strip both sides
                                strip_right = true;
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
