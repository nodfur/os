const std = @import("std");
const c = @import("./c.zig");

var library: c.FT_Library = null;

fn checkFreetypeError(err: c_int) !void {
    if (err != 0) {
        std.log.err("freetype: {s}", .{c.FT_Error_String(err)});
        return error.freetype_error;
    }
}

pub fn init() !void {
    try checkFreetypeError(c.FT_Init_FreeType(&library));
    std.log.debug("freetype: initialized library", .{});
}

pub fn done() !void {
    try checkFreetypeError(c.FT_Done_FreeType(library));
    std.log.debug("freetype: closed library", .{});
}

pub fn loadFreetypeFace(path: [*:0]const u8) !c.FT_Face {
    var face: c.FT_Face = null;
    try checkFreetypeError(c.FT_New_Face(library, path, 0, &face));

    std.log.debug("freetype: loaded {s}", .{path});

    return face;
}

pub fn setPixelSizes(face: c.FT_Face, height: u32) !void {
    try checkFreetypeError(c.FT_Set_Pixel_Sizes(face, 0, height));
    std.log.debug("freetype: set pixel size to {d}", .{height});
}

pub const Font = struct {
    freetype: c.FT_Face,
    harfbuzz: *c.hb_font_t,
    height: u32,
};

pub fn loadFont(path: [*:0]const u8, height: u32) !Font {
    var freetype = try loadFreetypeFace(path);
    try setPixelSizes(freetype, height);

    var harfbuzz =
        @ptrCast(*c.hb_font_t, c.hb_ft_font_create_referenced(freetype));

    c.hb_ft_font_set_funcs(harfbuzz);

    std.log.debug("harfbuzz: created font from freetype", .{});

    return Font{
        .freetype = freetype,
        .harfbuzz = harfbuzz,
        .height = height,
    };
}

pub fn demo() !void {
    var screenWidth: u32 = 800;
    var screenHeight: u32 = 600;

    var fontPath = "fonts/DMMono-Regular.ttf";
    var fontHeight: u32 = 24;

    try init();

    var frame: []u8 =
        try std.heap.c_allocator.alloc(u8, screenHeight * screenWidth / 8);

    defer std.heap.c_allocator.free(frame);

    std.mem.set(u8, frame, 0);

    var font = try loadFont(fontPath, fontHeight);

    try renderText(1, font, "foo bar (void &*[]~) { 1 + 2 + 3 = 6; }", @ptrCast([*]u8, frame), screenWidth, 40, 40);
    try bitmapToPBM(frame, screenWidth, screenHeight, "frame.pbm");
    try done();
}

// extern fn epap_render_glyph_to_bitmap(
//     ft_face: c.FT_Face,

//     bitmap: [*]u8,
//     width: u32,
//     x: i32,
//     y: i32,
// ) u32 {

// }

pub fn renderText(
    black: u1,
    font: Font,
    text: [*:0]const u8,
    frame: [*]u8,
    screenWidth: u32,
    x0: i32,
    y0: i32,
) !void {
    var buffer: *c.hb_buffer_t =
        @ptrCast(*c.hb_buffer_t, c.hb_buffer_create());

    std.log.debug("harfbuzz: created buffer", .{});

    c.hb_buffer_set_direction(buffer, .HB_DIRECTION_LTR);
    c.hb_buffer_set_script(buffer, .HB_SCRIPT_LATIN);
    c.hb_buffer_set_language(buffer, c.hb_language_from_string("en", -1));
    c.hb_buffer_add_utf8(buffer, text, -1, 0, -1);

    std.log.debug("harfbuzz: added text", .{});

    c.hb_shape(font.harfbuzz, buffer, null, 0);

    std.log.debug("harfbuzz: shaped text", .{});

    var glyph_count: u32 = 0;

    var glyph_info =
        @ptrCast(
        [*]c.hb_glyph_info_t,
        c.hb_buffer_get_glyph_infos(buffer, &glyph_count),
    );

    var glyph_pos =
        @ptrCast(
        [*]c.hb_glyph_position_t,
        c.hb_buffer_get_glyph_positions(buffer, &glyph_count),
    );

    var x: i32 = x0;
    var y: i32 = y0;

    var i: u32 = 0;
    while (i < glyph_count) : (i += 1) {
        var glyph_id = glyph_info[i].codepoint;
        var x_advance = glyph_pos[i].x_advance;
        var y_advance = glyph_pos[i].y_advance;
        var x_offset = glyph_pos[i].x_offset;
        var y_offset = glyph_pos[i].y_offset;

        // std.log.debug("harfbuzz: x {d} advance {d} offset {d}", .{x, x_advance, x_offset});

        try checkFreetypeError(c.FT_Load_Glyph(
            font.freetype,
            glyph_id,
            c.FT_LOAD_RENDER | c.FT_LOAD_TARGET_MONO | c.FT_LOAD_FORCE_AUTOHINT,
        ));

        // try printGlyph(font.freetype.*.glyph.*.bitmap);

        var extents: c.hb_glyph_extents_t = undefined;

        if (1 == c.hb_font_get_glyph_extents(font.harfbuzz, glyph_id, &extents)) {
            // std.log.debug("harfbuzz: glyph extents {any}", .{extents});
        } else {
            std.log.err("harfbuzz: failed to get glyph extents", .{});
            return error.harfbuzz_error;
        }

        try drawGlyph(
            black,
            frame,
            screenWidth,
            @intCast(u32, x + @divTrunc(x_offset, 64)),
            @intCast(u32, y + @divTrunc(y_offset, 64)),
            font.freetype.*.glyph.*.bitmap,
            font.height + font.height / 2,
            extents,
        );

        x += @divTrunc(x_advance, 64);
        y += @divTrunc(y_advance, 64);
    }
}

fn getBit(bytes: []u8, bit: u32) u1 {
    var mask = (@as(u8, 1) << @intCast(u3, bit % 8));
    if (0 != bytes[bit / 8] & mask) {
        return 1;
    } else {
        return 0;
    }
}

pub fn bitmapToPBM(
    bytes: []u8,
    width: u32,
    height: u32,
    path: []const u8,
) !void {
    var file = try std.fs.cwd().createFile(path, .{});
    defer file.close();

    var writer = file.writer();

    try writer.print("P1\n{d} {d}\n", .{ width, height });

    var i: u32 = 0;
    while (i < width * height) : (i += 1) {
        var bit = getBit(bytes, i);
        try writer.print("{d}", .{bit});
    }
}

fn setBitInArray(bytes: [*]u8, index: u32, bit: u1) void {
    var byteIndex = @divTrunc(index, 8);
    var bitIndex = @intCast(u3, index % 8);
    bytes[byteIndex] = (bytes[byteIndex] & ~(@as(u8, 1) << bitIndex)) | (@as(u8, bit) << bitIndex);
}

pub fn drawGlyph(black: u1, frame: [*]u8, screenWidth: u32, x: u32, y: u32, bitmap: c.FT_Bitmap, lineHeight: u32, extents: c.hb_glyph_extents_t) !void {
    var width = bitmap.width;
    var height = bitmap.rows;
    var pitch: u32 = @intCast(u32, bitmap.pitch);
    var buffer = bitmap.buffer;

    var i: u32 = 0;
    while (i < height) : (i += 1) {
        var j: u32 = 0;
        while (j < width) : (j += 1) {
            var pixel = buffer[i * pitch + @divTrunc(j, 8)];
            var bit: u32 = @as(u32, 1) << (7 - @truncate(u4, (j % 8)));
            var yOrigin = @intCast(u32, @divTrunc(extents.y_bearing, 64));
            var xOrigin = @intCast(u32, @divTrunc(extents.x_bearing, 64));
            if (pixel & bit != 0) {
                var pixel_index = ((lineHeight - yOrigin) + y + i) * screenWidth + x + xOrigin + j;
                setBitInArray(frame, pixel_index, black);
            }
        }
    }
}

pub fn printGlyph(glyph: c.FT_Bitmap) !void {
    var width = glyph.width;
    var height = glyph.rows;
    var pitch: u32 = @intCast(u32, glyph.pitch);
    var buffer = glyph.buffer;

    var writer = std.io.getStdOut().writer();

    var i: u32 = 0;
    while (i < height) : (i += 1) {
        var j: u32 = 0;
        while (j < width) : (j += 1) {
            var pixel = buffer[i * pitch + @divTrunc(j, 8)];
            var bit: u32 = @as(u32, 1) << (7 - @truncate(u4, (j % 8)));
            if (pixel & bit != 0) {
                try writer.writeByte('#');
            } else {
                try writer.writeByte(' ');
            }
        }
        try writer.writeByte('\n');
    }

    try writer.writeByte('\n');
}
