const std = @import("std");

pub fn build(b: *std.build.Builder) void {
    const target = b.standardTargetOptions(.{});
    const mode = b.standardReleaseOptions();

    const text = b.addSharedLibrary("epapi-text", null, .unversioned);
    text.setTarget(target);
    text.setBuildMode(mode);
    text.linkSystemLibrary("c");
    text.linkSystemLibrary("c++");
    text.addIncludeDir(".");
    text.addIncludeDir("../vendor/bcm2835-1.70/src");
    text.addIncludeDir("../vendor/freetype/include");
    text.addIncludeDir("../vendor/freetype/src");
    text.addIncludeDir("../vendor/harfbuzz/src");

    text.addCSourceFile(
        "freetype.c",
        &.{"-fno-sanitize=undefined"},
    );

    text.addCSourceFile(
        "../vendor/harfbuzz/src/harfbuzz.cc",
        &.{ "-DHAVE_FREETYPE", "-fno-sanitize=undefined" },
    );

    const epapi = b.addSharedLibrary("epapi", null, .unversioned);

    epapi.setTarget(target);
    epapi.setBuildMode(mode);
    epapi.linkSystemLibrary("c");
    epapi.linkSystemLibrary("c++");

    epapi.linkLibrary(text);

    epapi.addIncludeDir(".");
    epapi.addIncludeDir("../vendor/bcm2835-1.70/src");
    epapi.addIncludeDir("../vendor/freetype/include");
    epapi.addIncludeDir("../vendor/harfbuzz/src");

    if (target.isLinux()) {
        epapi.addCSourceFile(
            "../vendor/bcm2835-1.70/src/bcm2835.c",
            &.{"-fno-sanitize=undefined"},
        );
    }

    text.install();
    epapi.install();
}
