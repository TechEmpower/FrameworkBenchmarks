// File upload example using zig-http multipart/form-data support
//
// Run with: zig build && ./zig-out/bin/file_upload
// Test with:
//   Small files (buffered): curl -F "name=John" -F "file=@test.txt" http://localhost:8080/upload
//   Large files (streaming): curl -F "file=@large_file.bin" http://localhost:8080/upload
//   Images (restricted): curl -F "avatar=@photo.jpg" http://localhost:8080/images

const std = @import("std");
const http = @import("http");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var router = http.router(allocator);
    defer router.deinit();

    _ = router
        .get("/", handleHome)
        .post("/upload", handleUpload)
        .post("/images", handleImageUpload);

    // Server with streaming multipart configuration
    // Files larger than streaming_threshold (1MB) will use streaming mode
    // where data is written to temp files instead of memory
    var server = http.Server.init(allocator, &router, .{
        .threads = 4,
        // Streaming multipart settings
        .streaming_threshold = 1024 * 1024, // 1MB - files larger than this stream to disk
        .max_upload_size = 100 * 1024 * 1024, // 100MB max upload
        .temp_dir = null, // Uses /tmp/zig-http-uploads by default
        .enable_streaming_multipart = true,
    });
    defer server.deinit();

    std.log.info("File upload server running on http://localhost:8080", .{});
    std.log.info("Test with: curl -F \"name=John\" -F \"file=@test.txt\" http://localhost:8080/upload", .{});
    std.log.info("Large files (>1MB) automatically use streaming mode", .{});
    try server.run("0.0.0.0", 8080);
}

fn handleHome(ctx: *http.Context) !void {
    try ctx.sendHtml(
        \\<!DOCTYPE html>
        \\<html>
        \\<head><title>File Upload</title></head>
        \\<body>
        \\  <h1>File Upload Demo</h1>
        \\  <h2>General Upload</h2>
        \\  <form action="/upload" method="post" enctype="multipart/form-data">
        \\    <p><label>Name: <input type="text" name="name"></label></p>
        \\    <p><label>File: <input type="file" name="file"></label></p>
        \\    <p><button type="submit">Upload</button></p>
        \\  </form>
        \\  <h2>Image Upload (restricted types)</h2>
        \\  <form action="/images" method="post" enctype="multipart/form-data">
        \\    <p><label>Avatar: <input type="file" name="avatar" accept="image/*"></label></p>
        \\    <p><button type="submit">Upload Image</button></p>
        \\  </form>
        \\</body>
        \\</html>
    );
}

fn handleUpload(ctx: *http.Context) !void {
    // Check if this is a multipart request
    if (!ctx.request.isMultipart()) {
        _ = ctx.badRequest();
        try ctx.sendJson(.{ .@"error" = "Expected multipart/form-data" });
        return;
    }

    // Check if this is a streaming multipart request (large file upload)
    // The server automatically switches to streaming mode for uploads > 1MB
    if (ctx.request.streamingParts()) |parser| {
        return handleStreamingUpload(ctx, parser);
    }

    // Small file upload - use buffered mode
    var iter = ctx.request.multipartParts(.{
        .max_file_size = 5 * 1024 * 1024, // 5MB limit
        .max_part_count = 10,
    }) orelse {
        _ = ctx.badRequest();
        try ctx.sendJson(.{ .@"error" = "Invalid multipart request" });
        return;
    };

    var files_received: usize = 0;
    var fields_received: usize = 0;
    var total_bytes: usize = 0;

    // Iterate through all parts
    while (iter.next()) |part| {
        if (part.isFile()) {
            files_received += 1;
            total_bytes += part.data.len;
            std.log.info("Received file (buffered): {s} ({s}, {} bytes)", .{
                part.filename.?,
                part.content_type orelse "unknown",
                part.data.len,
            });
        } else {
            fields_received += 1;
            std.log.info("Received field: {s} = {s}", .{ part.name, part.data });
        }
    }

    // Check for parsing errors
    if (iter.getError()) |err| {
        _ = ctx.badRequest();
        const message = switch (err) {
            error.FileTooLarge => "File exceeds 5MB limit",
            error.TooManyParts => "Too many form fields",
            error.InvalidFormat => "Malformed multipart data",
            else => "Upload error",
        };
        try ctx.sendJson(.{ .@"error" = message });
        return;
    }

    try ctx.sendJson(.{
        .success = true,
        .mode = "buffered",
        .files_received = files_received,
        .fields_received = fields_received,
        .total_bytes = total_bytes,
    });
}

fn handleStreamingUpload(ctx: *http.Context, parser: *http.StreamingMultipartParser) !void {
    var files_received: usize = 0;
    var fields_received: usize = 0;
    var total_bytes: usize = 0;

    // Iterate through streamed parts
    // Large files are stored in temp files, small parts stay in memory
    while (parser.nextPart()) |*part| {
        defer part.deinit();

        if (part.isFile()) {
            files_received += 1;
            total_bytes += part.size;

            if (part.isInMemory()) {
                // Small file - data is in memory
                std.log.info("Received file (memory): {s} ({s}, {} bytes)", .{
                    part.filename orelse "unnamed",
                    part.content_type orelse "unknown",
                    part.size,
                });
            } else {
                // Large file - data is in temp file
                std.log.info("Received file (temp): {s} ({s}, {} bytes, path: {s})", .{
                    part.filename orelse "unnamed",
                    part.content_type orelse "unknown",
                    part.size,
                    part.filePath() orelse "unknown",
                });

                // Example: Move temp file to permanent storage
                // try std.fs.renameAbsolute(part.filePath().?, "/uploads/myfile.bin");
            }
        } else {
            fields_received += 1;
            std.log.info("Received field: {s} = {s}", .{
                part.name,
                part.data() orelse "(binary)",
            });
        }
    }

    // Check for parsing errors
    if (parser.getError()) |err| {
        _ = ctx.badRequest();
        const message = switch (err) {
            http.StreamingError.PartTooLarge => "File exceeds size limit",
            http.StreamingError.TooManyParts => "Too many form fields",
            http.StreamingError.InvalidFormat => "Malformed multipart data",
            http.StreamingError.UploadTooLarge => "Upload exceeds maximum size",
            else => "Upload error",
        };
        try ctx.sendJson(.{ .@"error" = message });
        return;
    }

    // Temp files are automatically cleaned up after handler returns
    try ctx.sendJson(.{
        .success = true,
        .mode = "streaming",
        .files_received = files_received,
        .fields_received = fields_received,
        .total_bytes = total_bytes,
    });
}

fn handleImageUpload(ctx: *http.Context) !void {
    if (!ctx.request.isMultipart()) {
        _ = ctx.badRequest();
        try ctx.sendJson(.{ .@"error" = "Expected multipart/form-data" });
        return;
    }

    // For streaming requests, we'd need to validate content types differently
    // For simplicity, this example uses buffered mode for image validation
    if (ctx.request.isStreamingMultipart()) {
        _ = ctx.badRequest();
        try ctx.sendJson(.{ .@"error" = "Image uploads must be under 1MB" });
        return;
    }

    // Restrict to image types only
    const allowed_types = [_][]const u8{
        "image/jpeg",
        "image/png",
        "image/gif",
        "image/webp",
    };

    var iter = ctx.request.multipartParts(.{
        .max_file_size = 10 * 1024 * 1024, // 10MB for images
        .max_part_count = 5,
        .allowed_content_types = &allowed_types,
    }) orelse {
        _ = ctx.badRequest();
        try ctx.sendJson(.{ .@"error" = "Invalid multipart request" });
        return;
    };

    var images: usize = 0;

    while (iter.next()) |part| {
        if (part.isFile()) {
            images += 1;
            std.log.info("Received image: {s} ({s}, {} bytes)", .{
                part.filename.?,
                part.content_type.?,
                part.data.len,
            });
        }
    }

    if (iter.getError()) |err| {
        _ = ctx.badRequest();
        const message = switch (err) {
            error.DisallowedContentType => "Only JPEG, PNG, GIF, and WebP images are allowed",
            error.FileTooLarge => "Image exceeds 10MB limit",
            else => "Upload error",
        };
        try ctx.sendJson(.{ .@"error" = message });
        return;
    }

    try ctx.sendJson(.{
        .success = true,
        .images_received = images,
    });
}
