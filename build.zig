const std = @import("std");

pub fn build(b: *std.Build) void {
    // Standard build options
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // ============================================================
    // Library Module
    // ============================================================
    // This module can be used as a dependency by other Zig projects
    const lib_mod = b.addModule("liquid", .{
        .root_source_file = b.path("src/root.zig"),
        .target = target,
        .optimize = optimize,
    });

    // ============================================================
    // Executable
    // ============================================================
    // The JSON-RPC server executable for liquid-spec integration
    const exe = b.addExecutable(.{
        .name = "liquid-zig",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });

    b.installArtifact(exe);

    // Run command
    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the JSON-RPC server");
    run_step.dependOn(&run_cmd.step);

    // ============================================================
    // Unit Tests
    // ============================================================
    // Test the library
    const lib_unit_tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/root.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });

    const run_lib_unit_tests = b.addRunArtifact(lib_unit_tests);

    // Test the main executable
    const exe_unit_tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });

    const run_exe_unit_tests = b.addRunArtifact(exe_unit_tests);

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_lib_unit_tests.step);
    test_step.dependOn(&run_exe_unit_tests.step);

    // ============================================================
    // Documentation
    // ============================================================
    const docs = b.addInstallDirectory(.{
        .source_dir = lib_mod.owner.addObject(.{
            .name = "liquid",
            .root_module = lib_mod,
        }).getEmittedDocs(),
        .install_dir = .prefix,
        .install_subdir = "docs",
    });

    const docs_step = b.step("docs", "Generate documentation");
    docs_step.dependOn(&docs.step);

    // ============================================================
    // Formatting Check
    // ============================================================
    const fmt_step = b.step("fmt", "Check source formatting");
    const fmt = b.addFmt(.{
        .paths = &.{"src"},
        .check = true,
    });
    fmt_step.dependOn(&fmt.step);

    // ============================================================
    // Clean Step
    // ============================================================
    const clean_step = b.step("clean", "Remove build artifacts");
    clean_step.dependOn(&b.addRemoveDirTree(b.path(".zig-cache")).step);
    clean_step.dependOn(&b.addRemoveDirTree(b.path("zig-out")).step);
}
