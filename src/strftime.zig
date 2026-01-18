// strftime.zig - POSIX strftime implementation
// Ported from public-domain strftime.c by Arnold Robbins
// Supports SYSV_EXT, SUNOS_EXT, POSIX2_DATE, VMS_EXT, MAILHEADER_EXT, ISO_DATE_EXT

const std = @import("std");

pub const DateTime = struct {
    year: i32, // Full year (e.g., 2024)
    month: u8, // 1-12
    day: u8, // 1-31
    hour: u8, // 0-23
    minute: u8, // 0-59
    second: u8, // 0-59
    weekday: u8, // 0=Sunday, 6=Saturday
    yday: u16, // 1-366
    subsec_nanos: u32 = 0, // Nanoseconds (for %L, %N)

    pub fn fromTimestamp(timestamp: i64) DateTime {
        const days = @divFloor(timestamp, 86400);
        const day_seconds = @mod(timestamp, 86400);

        const ymd = ymdFromEpochDays(days);
        const yday = dayOfYear(ymd.year, ymd.month, ymd.day);
        const weekday: u8 = @intCast(@mod(days + 4, 7)); // Jan 1, 1970 was Thursday

        return .{
            .year = ymd.year,
            .month = ymd.month,
            .day = ymd.day,
            .hour = @intCast(@divFloor(day_seconds, 3600)),
            .minute = @intCast(@mod(@divFloor(day_seconds, 60), 60)),
            .second = @intCast(@mod(day_seconds, 60)),
            .weekday = weekday,
            .yday = yday,
        };
    }
};

const Flags = struct {
    left: bool = false, // '-' no padding
    upper: bool = false, // '^' uppercase
    chcase: bool = false, // '#' change case
    space_pad: bool = false, // '_' space padding
    zero_pad: bool = false, // '0' zero padding
    colons: u8 = 0, // Number of colons for %z
    width: u8 = 0, // Explicit width
};

pub fn strftime(allocator: std.mem.Allocator, dt: DateTime, format: []const u8) ![]u8 {
    var result: std.ArrayList(u8) = .{};

    var i: usize = 0;
    while (i < format.len) {
        if (format[i] == '%' and i + 1 < format.len) {
            i += 1;

            // Parse flags
            var flags = Flags{};
            while (i < format.len) {
                switch (format[i]) {
                    '-' => {
                        flags.left = true;
                        flags.zero_pad = false;
                        flags.space_pad = false;
                        i += 1;
                    },
                    '^' => {
                        flags.upper = true;
                        i += 1;
                    },
                    '#' => {
                        flags.chcase = true;
                        i += 1;
                    },
                    '_' => {
                        flags.space_pad = true;
                        flags.zero_pad = false;
                        i += 1;
                    },
                    '0' => {
                        if (!flags.left and !flags.space_pad) {
                            flags.zero_pad = true;
                        }
                        i += 1;
                    },
                    ':' => {
                        // Count colons for %z variants
                        var colon_count: u8 = 0;
                        var j = i;
                        while (j < format.len and format[j] == ':' and colon_count < 3) {
                            colon_count += 1;
                            j += 1;
                        }
                        if (j < format.len and format[j] == 'z') {
                            flags.colons = colon_count;
                            i = j;
                        }
                        break;
                    },
                    '1'...'9' => {
                        // Parse width
                        var width: u8 = 0;
                        while (i < format.len and format[i] >= '0' and format[i] <= '9') {
                            width = width * 10 + (format[i] - '0');
                            i += 1;
                        }
                        flags.width = width;
                        continue; // Don't increment i again
                    },
                    else => break,
                }
            }

            if (i >= format.len) break;

            const spec = format[i];
            try formatSpec(allocator, &result, dt, spec, flags);
        } else {
            try result.append(allocator, format[i]);
        }
        i += 1;
    }

    return try result.toOwnedSlice(allocator);
}

fn formatSpec(allocator: std.mem.Allocator, result: *std.ArrayList(u8), dt: DateTime, spec: u8, flags: Flags) !void {
    const writer = result.writer(allocator);

    switch (spec) {
        // Literal
        '%' => try result.append(allocator, '%'),
        'n' => try result.append(allocator, '\n'),
        't' => try result.append(allocator, '\t'),

        // Year
        'Y' => try formatNum(writer, dt.year, 4, flags), // 4-digit year
        'y' => try formatNum(writer, @mod(dt.year, 100), 2, flags), // 2-digit year
        'C' => try formatNum(writer, @divFloor(dt.year, 100), 2, flags), // Century

        // Month
        'm' => try formatNum(writer, dt.month, 2, flags),
        'B' => try formatStr(result, allocator, monthName(dt.month), blk: {
            var f = flags;
            if (f.chcase) {
                f.chcase = false;
                f.upper = true;
            }
            break :blk f;
        }),
        'b', 'h' => try formatStr(result, allocator, monthAbbr(dt.month), blk: {
            var f = flags;
            if (f.chcase) {
                f.chcase = false;
                f.upper = true;
            }
            break :blk f;
        }),

        // Day
        'd' => try formatNum(writer, dt.day, 2, flags),
        'e' => try formatNum(writer, dt.day, 2, blk: {
            var f = flags;
            if (!f.zero_pad and !f.left) f.space_pad = true;
            break :blk f;
        }),
        'j' => try formatNum(writer, dt.yday, 3, flags), // Day of year

        // Hour
        'H' => try formatNum(writer, dt.hour, 2, flags), // 24-hour
        'I' => try formatNum(writer, hour12(dt.hour), 2, flags), // 12-hour
        'k' => try formatNum(writer, dt.hour, 2, blk: {
            var f = flags;
            if (!f.zero_pad and !f.left) f.space_pad = true;
            break :blk f;
        }),
        'l' => try formatNum(writer, hour12(dt.hour), 2, blk: {
            var f = flags;
            if (!f.zero_pad and !f.left) f.space_pad = true;
            break :blk f;
        }),

        // Minute/Second
        'M' => try formatNum(writer, dt.minute, 2, flags),
        'S' => try formatNum(writer, dt.second, 2, flags),

        // AM/PM
        'p' => try formatStr(result, allocator, if (dt.hour < 12) "AM" else "PM", blk: {
            var f = flags;
            if (f.chcase) {
                f.chcase = false;
                f.upper = false;
            }
            break :blk f;
        }),
        'P' => try formatStr(result, allocator, if (dt.hour < 12) "am" else "pm", flags),

        // Weekday
        'A' => try formatStr(result, allocator, weekdayName(dt.weekday), blk: {
            var f = flags;
            if (f.chcase) {
                f.chcase = false;
                f.upper = true;
            }
            break :blk f;
        }),
        'a' => try formatStr(result, allocator, weekdayAbbr(dt.weekday), blk: {
            var f = flags;
            if (f.chcase) {
                f.chcase = false;
                f.upper = true;
            }
            break :blk f;
        }),
        'w' => try formatNum(writer, dt.weekday, 1, flags), // 0=Sunday
        'u' => try formatNum(writer, if (dt.weekday == 0) @as(u8, 7) else dt.weekday, 1, flags), // 1=Monday

        // Week number
        'U' => try formatNum(writer, weekNumber(dt.yday, dt.weekday, 0), 2, flags), // Sunday first
        'W' => try formatNum(writer, weekNumber(dt.yday, dt.weekday, 1), 2, flags), // Monday first
        'V' => try formatNum(writer, iso8601WeekNum(dt.year, dt.yday, dt.weekday), 2, flags), // ISO week

        // ISO week year
        'G' => try formatNum(writer, iso8601WeekYear(dt.year, dt.month, dt.yday, dt.weekday), 4, flags),
        'g' => try formatNum(writer, @mod(iso8601WeekYear(dt.year, dt.month, dt.yday, dt.weekday), 100), 2, flags),

        // Unix timestamp
        's' => {
            const days = epochDaysFromYmd(dt.year, dt.month, dt.day);
            const ts = days * 86400 + @as(i64, dt.hour) * 3600 + @as(i64, dt.minute) * 60 + @as(i64, dt.second);
            try writer.print("{d}", .{ts});
        },

        // Timezone
        'z' => try formatTimezone(result, allocator, flags.colons),
        'Z' => try result.appendSlice(allocator, "UTC"),

        // Subseconds
        'L' => { // Milliseconds (3 digits)
            const ms = dt.subsec_nanos / 1_000_000;
            try writer.print("{d:0>3}", .{ms});
        },
        'N' => { // Nanoseconds (9 digits by default, or width-specified)
            const width: u8 = if (flags.width > 0) flags.width else 9;
            try formatSubsec(writer, dt.subsec_nanos, width);
        },

        // Composite formats
        'c' => { // %a %b %e %H:%M:%S %Y
            try formatStr(result, allocator, weekdayAbbr(dt.weekday), flags);
            try result.append(allocator, ' ');
            try formatStr(result, allocator, monthAbbr(dt.month), flags);
            try result.append(allocator, ' ');
            try formatNum(writer, dt.day, 2, .{ .space_pad = true });
            try result.append(allocator, ' ');
            try formatNum(writer, dt.hour, 2, .{});
            try result.append(allocator, ':');
            try formatNum(writer, dt.minute, 2, .{});
            try result.append(allocator, ':');
            try formatNum(writer, dt.second, 2, .{});
            try result.append(allocator, ' ');
            try formatNum(writer, dt.year, 4, .{});
        },
        'D', 'x' => { // %m/%d/%y
            try formatNum(writer, dt.month, 2, .{});
            try result.append(allocator, '/');
            try formatNum(writer, dt.day, 2, .{});
            try result.append(allocator, '/');
            try formatNum(writer, @as(i32, @intCast(@mod(dt.year, 100))), 2, .{});
        },
        'F' => { // %Y-%m-%d
            try formatNum(writer, dt.year, 4, .{});
            try result.append(allocator, '-');
            try formatNum(writer, dt.month, 2, .{});
            try result.append(allocator, '-');
            try formatNum(writer, dt.day, 2, .{});
        },
        'T', 'X' => { // %H:%M:%S
            try formatNum(writer, dt.hour, 2, .{});
            try result.append(allocator, ':');
            try formatNum(writer, dt.minute, 2, .{});
            try result.append(allocator, ':');
            try formatNum(writer, dt.second, 2, .{});
        },
        'R' => { // %H:%M
            try formatNum(writer, dt.hour, 2, .{});
            try result.append(allocator, ':');
            try formatNum(writer, dt.minute, 2, .{});
        },
        'r' => { // %I:%M:%S %p
            try formatNum(writer, hour12(dt.hour), 2, .{});
            try result.append(allocator, ':');
            try formatNum(writer, dt.minute, 2, .{});
            try result.append(allocator, ':');
            try formatNum(writer, dt.second, 2, .{});
            try result.append(allocator, ' ');
            try result.appendSlice(allocator, if (dt.hour < 12) "AM" else "PM");
        },
        'v' => { // VMS date: %e-%^b-%4Y
            try formatNum(writer, dt.day, 2, .{ .space_pad = true });
            try result.append(allocator, '-');
            try formatStr(result, allocator, monthAbbr(dt.month), .{ .upper = true });
            try result.append(allocator, '-');
            try formatNum(writer, dt.year, 4, .{});
        },

        // POSIX locale extensions (ignored, pass through to next char)
        'E', 'O' => {},

        // Unknown - output literally
        else => {
            try result.append(allocator, '%');
            try result.append(allocator, spec);
        },
    }
}

fn formatNum(writer: anytype, value: anytype, default_width: u8, flags: Flags) !void {
    const width = if (flags.width > 0) flags.width else default_width;

    // Convert value to string
    var buf: [32]u8 = undefined;
    const num_str = std.fmt.bufPrint(&buf, "{d}", .{value}) catch return;

    if (flags.left or num_str.len >= width) {
        // No padding needed
        try writer.writeAll(num_str);
    } else {
        // Add padding
        const pad_char: u8 = if (flags.space_pad) ' ' else '0';
        const pad_count = width - @as(u8, @intCast(num_str.len));
        var i: u8 = 0;
        while (i < pad_count) : (i += 1) {
            try writer.writeByte(pad_char);
        }
        try writer.writeAll(num_str);
    }
}

fn formatStr(result: *std.ArrayList(u8), allocator: std.mem.Allocator, str: []const u8, flags: Flags) !void {
    if (flags.upper) {
        for (str) |c| {
            try result.append(allocator, std.ascii.toUpper(c));
        }
    } else if (flags.chcase) {
        // Swap case
        for (str) |c| {
            if (std.ascii.isUpper(c)) {
                try result.append(allocator, std.ascii.toLower(c));
            } else if (std.ascii.isLower(c)) {
                try result.append(allocator, std.ascii.toUpper(c));
            } else {
                try result.append(allocator, c);
            }
        }
    } else {
        try result.appendSlice(allocator, str);
    }
}

fn formatTimezone(result: *std.ArrayList(u8), allocator: std.mem.Allocator, colons: u8) !void {
    // We always assume UTC (offset 0)
    switch (colons) {
        0 => try result.appendSlice(allocator, "+0000"), // %z
        1 => try result.appendSlice(allocator, "+00:00"), // %:z
        2 => try result.appendSlice(allocator, "+00:00:00"), // %::z
        3 => try result.appendSlice(allocator, "+00"), // %:::z (shortest)
        else => try result.appendSlice(allocator, "+0000"),
    }
}

fn formatSubsec(writer: anytype, nanos: u32, width: u8) !void {
    if (width >= 9) {
        try writer.print("{d:0>9}", .{nanos});
        // Pad with zeros if width > 9
        var i: u8 = 9;
        while (i < width) : (i += 1) {
            try writer.writeByte('0');
        }
    } else {
        // Truncate to requested precision
        var divisor: u32 = 1;
        var j: u8 = 0;
        while (j < 9 - width) : (j += 1) {
            divisor *= 10;
        }
        const truncated = nanos / divisor;
        try writer.print("{d:0>[1]}", .{ truncated, width });
    }
}

fn hour12(hour: u8) u8 {
    if (hour == 0) return 12;
    if (hour > 12) return hour - 12;
    return hour;
}

// ============ Date calculation helpers ============

fn ymdFromEpochDays(days: i64) struct { year: i32, month: u8, day: u8 } {
    const z = days + 719468;
    const era: i64 = @divFloor(if (z >= 0) z else z - 146096, 146097);
    const doe: i64 = z - era * 146097;
    const yoe: i64 = @divFloor(doe - @divFloor(doe, 1460) + @divFloor(doe, 36524) - @divFloor(doe, 146096), 365);
    const y = yoe + era * 400;
    const doy = doe - (365 * yoe + @divFloor(yoe, 4) - @divFloor(yoe, 100));
    const mp = @divFloor(5 * doy + 2, 153);
    const d: u8 = @intCast(doy - @divFloor(153 * mp + 2, 5) + 1);
    const m: u8 = @intCast(if (mp < 10) mp + 3 else mp - 9);
    const year: i32 = @intCast(if (m <= 2) y + 1 else y);
    return .{ .year = year, .month = m, .day = d };
}

fn epochDaysFromYmd(year: i32, month: u8, day: u8) i64 {
    var y = @as(i64, year);
    var m = @as(i64, month);
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

fn dayOfYear(year: i32, month: u8, day: u8) u16 {
    const days_before_month = [_]u16{ 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334 };
    var doy = days_before_month[month - 1] + @as(u16, day);
    if (month > 2 and isLeapYear(year)) {
        doy += 1;
    }
    return doy;
}

fn isLeapYear(year: i32) bool {
    return (@mod(year, 4) == 0 and @mod(year, 100) != 0) or @mod(year, 400) == 0;
}

/// Week number (0-53), first_weekday: 0=Sunday, 1=Monday
fn weekNumber(yday: u16, wday: u8, first_weekday: u8) u8 {
    var weekday = wday;
    if (first_weekday == 1) {
        weekday = if (weekday == 0) 6 else weekday - 1;
    }
    const result = @divFloor(@as(i32, yday) + 6 - @as(i32, weekday), 7);
    return @intCast(if (result < 0) 0 else result);
}

/// ISO 8601 week number (1-53)
fn iso8601WeekNum(year: i32, yday: u16, wday: u8) u8 {
    var weeknum = weekNumber(yday, wday, 1);

    // What day is Jan 1?
    const yday_i32: i32 = @intCast(yday);
    const wday_i32: i32 = @intCast(wday);
    var jan1day = @mod(wday_i32 - @mod(yday_i32 - 1, 7), 7);
    if (jan1day < 0) jan1day += 7;

    switch (jan1day) {
        1 => {}, // Monday
        2, 3, 4 => weeknum += 1, // Tue-Thu
        0, 5, 6 => { // Sun, Fri, Sat
            if (weeknum == 0) {
                const prev_year = year - 1;
                const prev_leap = isLeapYear(prev_year);
                const dec31_yday: u16 = if (prev_leap) 366 else 365;
                const dec31_wday: u8 = @intCast(if (jan1day == 0) 6 else @as(u8, @intCast(jan1day)) - 1);
                weeknum = iso8601WeekNum(prev_year, dec31_yday, dec31_wday);
            }
        },
        else => {},
    }

    // Check if late December is week 1 of next year
    if (yday >= 360) {
        const month = monthFromYday(year, yday);
        if (month == 12) {
            const mday = mdayFromYday(year, yday);
            if ((wday == 1 and mday >= 29 and mday <= 31) or
                (wday == 2 and (mday == 30 or mday == 31)) or
                (wday == 3 and mday == 31))
            {
                weeknum = 1;
            }
        }
    }

    return weeknum;
}

fn monthFromYday(year: i32, yday: u16) u8 {
    const leap = isLeapYear(year);
    const days = if (leap)
        [_]u16{ 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366 }
    else
        [_]u16{ 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365 };
    for (days, 0..) |d, m| {
        if (yday <= d) return @intCast(m + 1);
    }
    return 12;
}

fn mdayFromYday(year: i32, yday: u16) u8 {
    const leap = isLeapYear(year);
    const days_before = if (leap)
        [_]u16{ 0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335 }
    else
        [_]u16{ 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334 };
    const month = monthFromYday(year, yday);
    return @intCast(yday - days_before[month - 1]);
}

fn iso8601WeekYear(year: i32, month: u8, yday: u16, wday: u8) i32 {
    const w = iso8601WeekNum(year, yday, wday);
    if (month == 12 and w == 1) return year + 1;
    if (month == 1 and w >= 52) return year - 1;
    return year;
}

fn weekdayName(d: u8) []const u8 {
    const names = [_][]const u8{ "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday" };
    return names[d];
}

fn weekdayAbbr(d: u8) []const u8 {
    const names = [_][]const u8{ "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat" };
    return names[d];
}

fn monthName(m: u8) []const u8 {
    const names = [_][]const u8{ "", "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December" };
    return names[m];
}

fn monthAbbr(m: u8) []const u8 {
    const names = [_][]const u8{ "", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" };
    return names[m];
}

// ============ Tests ============

test "basic format" {
    const allocator = std.testing.allocator;
    const dt = DateTime.fromTimestamp(0); // 1970-01-01 00:00:00 UTC

    const result = try strftime(allocator, dt, "%Y-%m-%d");
    defer allocator.free(result);
    try std.testing.expectEqualStrings("1970-01-01", result);
}

test "composite formats" {
    const allocator = std.testing.allocator;
    const dt = DateTime.fromTimestamp(1234567890); // 2009-02-13 23:31:30 UTC

    const f = try strftime(allocator, dt, "%F %T");
    defer allocator.free(f);
    try std.testing.expectEqualStrings("2009-02-13 23:31:30", f);
}

test "flags" {
    const allocator = std.testing.allocator;
    const dt = DateTime{
        .year = 2024,
        .month = 1,
        .day = 5,
        .hour = 9,
        .minute = 5,
        .second = 3,
        .weekday = 5, // Friday
        .yday = 5,
    };

    // No padding
    const np = try strftime(allocator, dt, "%-d %-m");
    defer allocator.free(np);
    try std.testing.expectEqualStrings("5 1", np);

    // Uppercase
    const up = try strftime(allocator, dt, "%^a %^b");
    defer allocator.free(up);
    try std.testing.expectEqualStrings("FRI JAN", up);
}
