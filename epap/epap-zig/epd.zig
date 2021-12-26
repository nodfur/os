const std = @import("std");
const c = @import("./c.zig");

const c_allocator = std.heap.c_allocator;

const Pin = enum(u8) {
    rst = 17,
    cs = 8,
    busy = 24,
};

const PacketType = enum(u16) {
    command = 0x6000,
    write = 0x0000,
    read = 0x1000,
};

pub const SystemInfo = struct {
    panelWidth: u16,
    panelHeight: u16,
    memoryAddress: u32,
    fwVersion: [16]u8,
    lutVersion: [16]u8,
};

const Commands = enum(u16) {
    run = 0x1,
    standby = 0x2,
    sleep = 0x03,
    read_register = 0x10,
    write_register = 0x11,
    vcom = 0x39,
    dev_info = 0x302,
    load_img_area_start = 0x21,
    load_img_end = 0x22,
    display_area = 0x34,
    display_area_buf = 0x37,
};

const mcsr_base_address: u16 = 0x200;
const display_reg_base: u16 = 0x1000;

const Registers = enum(u16) {
    i80cpcr = 0x04,
    lisar0 = mcsr_base_address + 0x8,
    lisar2 = mcsr_base_address + 0x8 + 2,
    lutafsr = display_reg_base + 0x224,
    up1sr_2 = display_reg_base + 0x138 + 2,
    bgvr = display_reg_base + 0x250,
};

const Endianness = enum(u1) {
    little = 0,
    big = 1,
};

const PixelFormat = enum(u2) {
    bpp2 = 0,
    bpp3 = 1,
    bpp4 = 2,
    bpp8 = 3,

    pub fn bits(self: PixelFormat) u8 {
        return switch (self) {
            PixelFormat.bpp2 => 2,
            PixelFormat.bpp3 => 3,
            PixelFormat.bpp4 => 4,
            PixelFormat.bpp8 => 8,
        };
    }
};

const Rotation = enum(u2) {
    normal = 0,
    rotate_90 = 1,
    rotate_180 = 2,
    rotate_270 = 3,
};

pub const Rectangle = struct {
    x: u16,
    y: u16,
    w: u16,
    h: u16,
};

pub const PixelArea = struct {
    rectangle: Rectangle,
    bitsPerPixel: PixelFormat,

    pub fn byteSize(self: PixelArea) usize {
        var w = @as(usize, self.rectangle.w);
        var h = @as(usize, self.rectangle.h);
        var bpp = @as(usize, self.bitsPerPixel.bits());
        return (w * h * bpp) / 8;
    }
};

pub const Image = struct {
    area: PixelArea,
    data: [*]const u8,
    endianness: Endianness,
    rotation: Rotation,
};

pub fn initializeBroadcomChip() !void {
    std.log.info("starting BCM2835", .{});

    if (c.bcm2835_init() == 0) {
        return error.bcm2835_init_failed;
    }

    if (c.bcm2835_spi_begin() == 0) {
        return error.bcm2835_spi_failed;
    }

    c.bcm2835_spi_setBitOrder(c.BCM2835_SPI_BIT_ORDER_MSBFIRST);
    c.bcm2835_spi_setDataMode(c.BCM2835_SPI_MODE0);
    c.bcm2835_spi_setClockDivider(c.BCM2835_SPI_CLOCK_DIVIDER_32);

    gpioInit();
}

pub fn finalizeBroadcomChip() !void {
    std.log.info("closing BCM2835", .{});

    gpioWriteBit(Pin.cs, c.LOW);
    gpioWriteBit(Pin.rst, c.LOW);

    c.bcm2835_spi_end();

    if (c.bcm2835_close() == 0) {
        return error.bcm2835_exit_failed;
    }
}

fn gpioMode(pin: Pin, mode: u16) void {
    c.bcm2835_gpio_fsel(@enumToInt(pin), if (mode == 0 or mode == c.BCM2835_GPIO_FSEL_INPT)
        c.BCM2835_GPIO_FSEL_INPT
    else
        c.BCM2835_GPIO_FSEL_OUTP);
}

fn gpioInit() void {
    std.log.info("starting GPIO", .{});
    gpioMode(Pin.rst, c.BCM2835_GPIO_FSEL_OUTP);
    gpioMode(Pin.cs, c.BCM2835_GPIO_FSEL_OUTP);
    gpioMode(Pin.busy, c.BCM2835_GPIO_FSEL_INPT);

    gpioWriteBit(Pin.cs, c.HIGH);
}

fn gpioWriteBit(pin: Pin, value: u8) void {
    c.bcm2835_gpio_write(@enumToInt(pin), value);
}

fn gpioReadBit(pin: Pin) u8 {
    return c.bcm2835_gpio_lev(@enumToInt(pin));
}

pub fn delayMs(ms: c_uint) void {
    c.bcm2835_delay(ms);
}

fn delayUs(us: u64) void {
    c.bcm2835_delayMicroseconds(us);
}

pub fn epdReset() void {
    std.log.info("resetting EPD", .{});
    gpioWriteBit(Pin.rst, c.HIGH);
    delayMs(200);
    gpioWriteBit(Pin.rst, c.LOW);
    delayMs(10);
    gpioWriteBit(Pin.rst, c.HIGH);
    delayMs(200);
}

fn spiWriteByte(byte: u8) void {
    _ = c.bcm2835_spi_transfer(byte);
    // std.io.getStdOut().writer().print("{x:0>2}", .{byte}) catch |err|
    //     std.log.err("spiWriteByte failed {}", .{err});
}

fn spiWriteWord(word: u16) !void {
    spiWriteByte(@truncate(u8, word >> 8));
    spiWriteByte(@truncate(u8, word));
}

fn gpioLow(pin: Pin) void {
    gpioWriteBit(pin, c.LOW);
}

fn gpioHigh(pin: Pin) void {
    gpioWriteBit(pin, c.HIGH);
}

fn csLow() void {
    gpioLow(Pin.cs);
}

fn csHigh() void {
    gpioHigh(Pin.cs);
}

fn wait() void {
    while (true) {
        if (gpioReadBit(Pin.busy) == 1) {
            return;
        }
    }
}

fn epdStartPacket(kind: PacketType) !void {
    wait();
    csLow();
    try spiWriteWord(@enumToInt(kind));
    wait();
}

fn epdWriteCommand(command: Commands) !void {
    // std.log.debug("cmd {x} {x}", .{ @enumToInt(command), command });
    try epdStartPacket(PacketType.command);
    defer csHigh();

    try spiWriteWord(@enumToInt(command));
}

fn epdWriteU16(data: u16) !void {
    try epdStartPacket(PacketType.write);
    defer csHigh();

    try spiWriteWord(data);
}

fn epdWriteMultiData(data: []u16) !void {
    std.log.info("writing {d} data words", .{data.len});
    try epdStartPacket(PacketType.write);
    defer csHigh();

    for (data) |x| {
        try spiWriteWord(x);
    }
}

fn epdWriteMultiArg(data: []u16) !void {
    for (data) |x| {
        try epdWriteU16(x);
    }
}

fn spiReadByte() u8 {
    return c.bcm2835_spi_transfer(0x00);
}

fn spiReadWord() u16 {
    var hi: u8 = spiReadByte();
    var lo: u8 = spiReadByte();
    var word: u16 = (@as(u16, hi) << 8) | @as(u16, lo);

    // std.log.debug("read word 0x{x}", .{word});

    return word;
}

fn spiReadU32() u32 {
    // this is kinda backwards...
    var lo: u16 = spiReadWord();
    var hi: u16 = spiReadWord();
    return (@as(u32, hi) << 16) | @as(u32, lo);
}

fn spiReadBytes(comptime n: usize) [n]u8 {
    var buffer = [_]u8{0} ** n;
    for (buffer) |_, i| {
        buffer[i] = spiReadByte();
    }
    return buffer;
}

fn epdReadWord() !u16 {
    try epdStartReading();
    defer csHigh();

    return spiReadWord();
}

fn epdStartReading() !void {
    try epdStartPacket(PacketType.read);

    _ = spiReadWord(); // read a dummy word

    wait();
}

fn epdReadWords(slice: []u16) void {
    epdStartReading();
    defer csHigh();

    for (slice) |_, i| {
        slice[i] = spiReadWord();
    }
}

pub fn epdGetSystemInfo() !SystemInfo {
    std.log.info("reading EPD system info", .{});

    try epdWriteCommand(Commands.dev_info);
    try epdStartReading();
    defer csHigh();

    return SystemInfo{
        .panelWidth = spiReadWord(),
        .panelHeight = spiReadWord(),
        .memoryAddress = spiReadU32(),
        .fwVersion = spiReadBytes(16),
        .lutVersion = spiReadBytes(16),
    };
}

pub fn initializeDisplay(vcom: f64) !SystemInfo {
    epdReset();

    std.log.info("starting EPD", .{});
    try epdWriteCommand(Commands.run);

    var info = try epdGetSystemInfo();

    std.log.info("panel width: {d}", .{info.panelWidth});
    std.log.info("panel height: {d}", .{info.panelHeight});
    std.log.info("memory address: 0x{x}", .{info.memoryAddress});
    std.log.info("firmware version: {s}", .{info.fwVersion});
    std.log.info("LUT version: {s}", .{info.lutVersion});

    // enable pack write
    std.log.info("enabling pack write", .{});
    try epdWriteRegister(Registers.i80cpcr, 1);

    try epdSetVcom(vcom);

    return info;
}

pub fn sleep() !void {
    std.log.info("going to sleep", .{});
    try epdWriteCommand(Commands.sleep);
}

pub fn epdSetVcom(vcom: f64) !void {
    var vcom_word: u16 =
        @floatToInt(u16, @fabs(vcom) * 1000.0);

    try epdWriteCommand(Commands.vcom);
    try epdWriteU16(0);
    var read_vcom: u16 = try epdReadWord();

    std.log.info("setting vcom from 0x{x} to {d} (0x{x})", .{ read_vcom, vcom, vcom_word });

    try epdWriteCommand(Commands.vcom);
    try epdWriteU16(1);
    try epdWriteU16(vcom_word);
}

fn epdWriteRegister(r: Registers, value: u16) !void {
    try epdWriteCommand(Commands.write_register);
    try epdWriteU16(@enumToInt(r));
    try epdWriteU16(value);
}

fn epdReadRegister(r: Registers) !u16 {
    try epdWriteCommand(Commands.read_register);
    try epdWriteU16(@enumToInt(r));
    return epdReadWord();
}

pub fn epdWaitForDisplay() !void {
    var i: u32 = 0;
    while (i < 10000) {
        if ((try epdReadRegister(Registers.lutafsr)) == 0) {
            return;
        }

        i += 1;
    }

    std.log.err("timeout waiting for display", .{});
    return error.epd_timeout;
}

pub fn fullScreenRectangle(info: SystemInfo) Rectangle {
    return Rectangle{
        .x = 0,
        .y = 0,
        .w = info.panelWidth,
        .h = info.panelHeight,
    };
}

pub fn drawBitmap(rectangle: Rectangle, frame: [*]const u8, base: u32) !void {
    var area = PixelArea{
        .rectangle = rectangle,
        .bitsPerPixel = PixelFormat.bpp8,
    };

    var area2 = area;
    area2.rectangle.w /= 8;

    var image = Image{
        .area = area2,
        .data = @ptrCast([*]const u8, frame),
        .endianness = Endianness.little,
        .rotation = Rotation.normal,
    };

    std.log.info("waiting for display", .{});
    try epdWaitForDisplay();

    std.log.info("writing image", .{});
    try epdWriteImage(image, base, 6);

    std.log.info("displaying area", .{});
    try epdDisplayArea(area.rectangle, 6, base);
}

pub fn clearScreen(info: SystemInfo, byte: u8, mode: u8) !void {
    const area = PixelArea{
        .rectangle = fullScreenRectangle(info),
        .bitsPerPixel = PixelFormat.bpp4,
    };

    var data = try c_allocator.alloc(u8, area.byteSize());
    defer c_allocator.free(data);

    std.mem.set(u8, data, byte);

    var image = Image{
        .area = area,
        .data = @ptrCast([*]const u8, data),
        .endianness = Endianness.little,
        .rotation = Rotation.normal,
    };

    try epdWaitForDisplay();
    try epdWriteImage(image, info.memoryAddress, mode);
    try epdDisplayArea(area.rectangle, mode, info.memoryAddress);
}

pub fn epdWriteImage(image: Image, address: u32, mode: u8) !void {
    std.log.info("writing {} image {}", .{ image.area.bitsPerPixel, image.area.rectangle });

    try epdSetTargetAddress(address);
    try epdLoadImgAreaStart(image);

    var i: usize = 0;

    var length = image.area.byteSize();

    std.log.info("writing {d} individual bytes", .{length});

    while (i * 2 < length) {
        try epdWriteU16(@as(u16, image.data[i * 2 + 0]) | (@as(u16, image.data[i * 2 + 1]) << 8));
        i += 1;
    }

    try epdWriteCommand(Commands.load_img_end);
}

fn dumpMessage(msg: []const u8) void {
    writeStdout(msg);
}

fn writeStdout(msg: []const u8) void {
    _ = std.io.getStdOut().writer().writeAll(msg) catch |err| {
        std.log.err("error writing to stdout: {s}", .{err});
    };
}

fn epdSetTargetAddress(address: u32) !void {
    var addressHigh: u16 = @truncate(u16, address >> 16);
    var addressLow: u16 = @truncate(u16, address & 0xFFFF);
    try epdWriteRegister(Registers.lisar2, addressHigh);
    try epdWriteRegister(Registers.lisar0, addressLow);
}

fn epdLoadImgAreaStart(image: Image) !void {
    var format: u16 =
        (@as(u16, @enumToInt(image.endianness)) << 8) |
        (@as(u16, @enumToInt(image.area.bitsPerPixel)) << 4) |
        @as(u16, @enumToInt(image.rotation));

    var args = [_]u16{
        format,
        image.area.rectangle.x,
        image.area.rectangle.y,
        image.area.rectangle.w,
        image.area.rectangle.h,
    };

    try epdWriteCommand(Commands.load_img_area_start);
    try epdWriteMultiArg(&args);
}

pub fn epdDisplayArea(rect: Rectangle, mode: u8, address: u32) !void {
    std.log.info("displaying area with mode {d}", .{mode});

    if (mode == 6) {
        try epdWriteRegister(.up1sr_2, (try epdReadRegister(.up1sr_2)) | (1 << 2));
        try epdWriteRegister(.bgvr, 0xf0);

        var args = [_]u16{
            rect.x,
            rect.y,
            rect.w,
            rect.h,
            mode,
            @truncate(u16, address & 0xFFFF),
            @truncate(u16, address >> 16),
        };

        std.log.info("display area args {any}", .{args});

        try epdWriteCommand(Commands.display_area_buf);
        try epdWriteMultiArg(&args);

        try epdWaitForDisplay();
        try epdWriteRegister(.up1sr_2, (try epdReadRegister(.up1sr_2)) & ~(@as(u16, 1 << 2)));
    } else {
        var args = [_]u16{
            rect.x,
            rect.y,
            rect.w,
            rect.h,
            mode,
        };

        try epdWriteCommand(Commands.display_area);
        try epdWriteMultiArg(&args);
    }
}
