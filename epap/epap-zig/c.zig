pub usingnamespace @cImport({
    @cInclude("termios.h");
    @cInclude("unistd.h");
    @cInclude("stdlib.h");

    @cInclude("sys/capability.h");

    // GPIO/SPI for Raspberry Pi
    @cInclude("bcm2835.h");

    // FreeType
    @cInclude("ft2build.h");
    @cInclude("freetype/freetype.h");
    @cInclude("freetype/ftglyph.h");

    // Harfbuzz
    @cInclude("hb-ft.h");
});
