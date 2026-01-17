//! liquid-zig: A high-performance Liquid template engine implementation in Zig
//!
//! This library provides a complete implementation of the Liquid template language,
//! designed to pass the liquid-spec test suite. It can be used either as a library
//! for embedding in other Zig applications, or via the JSON-RPC executable for
//! integration with external systems.
//!
//! ## Quick Start
//!
//! ```zig
//! const std = @import("std");
//! const liquid = @import("liquid");
//!
//! pub fn main() !void {
//!     var gpa = std.heap.GeneralPurposeAllocator(.{}){};
//!     defer _ = gpa.deinit();
//!     const allocator = gpa.allocator();
//!
//!     var engine = liquid.Engine.init(allocator);
//!     defer engine.deinit();
//!
//!     const template_id = try engine.compile("Hello, {{ name }}!", null, .lax);
//!     const output = try engine.render(template_id, .{ .object = ... });
//!     defer allocator.free(output);
//!
//!     std.debug.print("{s}\n", .{output});
//! }
//! ```
//!
//! ## Features
//!
//! - Full Liquid template syntax support
//! - Variables with dot notation: `{{ user.name }}`
//! - Filters: `{{ text | upcase | truncate: 10 }}`
//! - Control flow: `if`, `elsif`, `else`, `unless`, `case/when`
//! - Loops: `for`, `tablerow` with limit, offset, reversed
//! - Tags: `assign`, `capture`, `increment`, `decrement`, `cycle`
//! - Partials: `include`, `render`
//! - Whitespace control: `{{-`, `-}}`, `{%-`, `-%}`
//!

const liquid = @import("liquid.zig");
pub const jsonrpc = @import("jsonrpc.zig");

// Re-export main types from the liquid module
pub const Engine = liquid.Engine;
pub const ErrorMode = liquid.ErrorMode;

test {
    @import("std").testing.refAllDecls(@This());
}
