# liquid-zig

A high-performance [Liquid](https://shopify.github.io/liquid/) template engine written in Zig.

## Warning

**This is highly experimental software.** It was built in a weekend as a proof-of-concept. The API will change. There are bugs. Do not use this in production. You have been warned.

## Features

- Full Liquid template syntax
- Variables with dot notation: `{{ user.name }}`
- Filters: `{{ text | upcase | truncate: 10 }}`
- Control flow: `if`, `elsif`, `else`, `unless`, `case/when`
- Loops: `for`, `tablerow` with `limit`, `offset`, `reversed`
- Tags: `assign`, `capture`, `increment`, `decrement`, `cycle`
- Partials: `include`, `render`
- Whitespace control: `{{-`, `-}}`, `{%-`, `-%}`

Passes 554 tests from the [liquid-spec](https://github.com/pjuu/liquid-spec) basics suite.

## Usage

```zig
const std = @import("std");
const liquid = @import("liquid-zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Initialize engine
    var engine = liquid.Engine.init(allocator);
    defer engine.deinit();

    // Compile template (no partials needed)
    const template_id = try engine.compile(
        "Hello, {{ name | upcase }}!",
        null,  // filesystem (for partials)
        .lax,  // error mode
    );

    // Build environment as JSON
    var env = std.json.ObjectMap.init(allocator);
    defer env.deinit();
    try env.put("name", .{ .string = "world" });

    // Render
    const output = try engine.render(template_id, .{ .object = env });
    defer allocator.free(output);

    std.debug.print("{s}\n", .{output}); // "Hello, WORLD!"
}
```

## Partials (include/render)

If your templates use `{% include %}` or `{% render %}`, you must provide **all partial sources upfront** when compiling. The filesystem is a string map passed at compile time:

```zig
const std = @import("std");
const liquid = @import("liquid-zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var engine = liquid.Engine.init(allocator);
    defer engine.deinit();

    // Build filesystem with all partials
    var filesystem = std.json.ObjectMap.init(allocator);
    defer filesystem.deinit();
    try filesystem.put("header", .{ .string = "<h1>{{ title }}</h1>" });
    try filesystem.put("footer", .{ .string = "<footer>{{ year }}</footer>" });

    // Compile main template with filesystem
    const template_id = try engine.compile(
        \\{% include 'header' %}
        \\<p>Content here</p>
        \\{% include 'footer' %}
    ,
        filesystem,
        .lax,
    );

    // Environment
    var env = std.json.ObjectMap.init(allocator);
    defer env.deinit();
    try env.put("title", .{ .string = "Welcome" });
    try env.put("year", .{ .string = "2026" });

    const output = try engine.render(template_id, .{ .object = env });
    defer allocator.free(output);

    std.debug.print("{s}\n", .{output});
}
```

**Important:** All partials must be present at compile time. There is no lazy loading or filesystem access at render time. This is by design—templates are fully resolved and optimized once during compilation.

## Error Modes

- `.lax` (default): Unknown tags render as text, undefined variables return empty string
- `.strict`: Parse errors on unknown tags

## Building

```bash
zig build                        # debug build
zig build -Doptimize=ReleaseFast # optimized build
```

## License

MIT
