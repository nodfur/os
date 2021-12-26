const std = @import("std");
const epd = @import("./epd.zig");

const c_allocator = std.heap.c_allocator;

pub fn main() !void {
    try epd.initializeBroadcomChip();
    defer {
        epd.finalizeBroadcomChip() catch |err| std.log.err("BCM2835 exit failed", .{});
    }

    var info = try epd.initializeDisplay(-1.73);

    std.log.info("allocating full-screen bitmap", .{});

    var frame: []u8 =
        try std.heap.c_allocator.alloc(u8, info.panelHeight * @as(u32, info.panelWidth) / 8);

    defer std.heap.c_allocator.free(frame);

    std.log.info("setting bitmap to 0xf", .{});

    std.mem.set(u8, frame, 0xff);
    
    try epd.clearScreen(info, 0xff, 0);
    epd.delayMs(200);

    try epd.sleep();
}
