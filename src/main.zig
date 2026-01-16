const std = @import("std");
const json = std.json;

const JsonRpc = @import("jsonrpc.zig");
const Liquid = @import("liquid.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var liquid_engine = Liquid.Engine.init(allocator);
    defer liquid_engine.deinit();

    var rpc_handler = JsonRpc.Handler.init(allocator, &liquid_engine);
    defer rpc_handler.deinit();

    const stdin = std.posix.STDIN_FILENO;
    const stdout = std.posix.STDOUT_FILENO;
    const stderr = std.posix.STDERR_FILENO;

    const stdin_file = std.fs.File{ .handle = stdin };
    const stdout_file = std.fs.File{ .handle = stdout };
    _ = stderr;

    var read_buffer: [1]u8 = undefined;
    while (true) {
        var line_buffer: std.ArrayList(u8) = .{};
        defer line_buffer.deinit(allocator);

        // Read until newline
        while (true) {
            const n = stdin_file.read(&read_buffer) catch |err| {
                if (err == error.EndOfStream and line_buffer.items.len > 0) break;
                if (err == error.EndOfStream) return;
                std.debug.print("[liquid-zig] Error reading: {}\n", .{err});
                continue;
            };
            if (n == 0) {
                if (line_buffer.items.len > 0) break;
                return;
            }
            if (read_buffer[0] == '\n') break;
            line_buffer.append(allocator, read_buffer[0]) catch |err| {
                std.debug.print("[liquid-zig] Error appending: {}\n", .{err});
                break;
            };
        }

        const line = line_buffer.items;
        if (line.len == 0) continue;

        const response = rpc_handler.handleRequest(line) catch |err| {
            std.debug.print("[liquid-zig] Error handling request: {}\n", .{err});
            continue;
        };

        if (response) |resp| {
            defer allocator.free(resp);
            // Write response + newline
            _ = stdout_file.write(resp) catch continue;
            _ = stdout_file.write("\n") catch continue;
        }
    }
}
