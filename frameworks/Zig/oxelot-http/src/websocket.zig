// WebSocket Protocol Implementation (RFC 6455)
//
// Features:
// - Frame parsing and serialization
// - HTTP upgrade handshake
// - Masking/unmasking (client-to-server)
// - Ping/pong keepalive
// - Message fragmentation support
// - Close handshake

const std = @import("std");
const Sha1 = std.crypto.hash.Sha1;

/// WebSocket frame opcodes
pub const Opcode = enum(u4) {
    continuation = 0x0,
    text = 0x1,
    binary = 0x2,
    // 0x3-0x7 reserved for future non-control frames
    close = 0x8,
    ping = 0x9,
    pong = 0xA,
    // 0xB-0xF reserved for future control frames

    pub fn isControl(self: Opcode) bool {
        return @intFromEnum(self) >= 0x8;
    }

    pub fn isData(self: Opcode) bool {
        return @intFromEnum(self) <= 0x2;
    }

    pub fn fromInt(value: u4) ?Opcode {
        return switch (value) {
            0x0 => .continuation,
            0x1 => .text,
            0x2 => .binary,
            0x8 => .close,
            0x9 => .ping,
            0xA => .pong,
            else => null,
        };
    }
};

/// WebSocket close status codes
pub const CloseCode = enum(u16) {
    normal = 1000,
    going_away = 1001,
    protocol_error = 1002,
    unsupported_data = 1003,
    // 1004 reserved
    no_status = 1005, // Must not be sent in close frame
    abnormal = 1006, // Must not be sent in close frame
    invalid_data = 1007,
    policy_violation = 1008,
    message_too_big = 1009,
    missing_extension = 1010,
    internal_error = 1011,
    // 1015 = TLS handshake failure (not sent)

    pub fn isValid(code: u16) bool {
        // Valid close codes per RFC 6455
        return (code >= 1000 and code <= 1003) or
            (code >= 1007 and code <= 1011) or
            (code >= 3000 and code <= 4999);
    }
};

/// Parsed WebSocket frame header
pub const FrameHeader = struct {
    fin: bool,
    rsv1: bool = false,
    rsv2: bool = false,
    rsv3: bool = false,
    opcode: Opcode,
    masked: bool,
    payload_len: u64,
    mask_key: ?[4]u8,

    /// Total header size in bytes
    pub fn headerSize(self: FrameHeader) usize {
        var size: usize = 2; // Base header
        if (self.payload_len > 125) {
            if (self.payload_len <= 65535) {
                size += 2; // 16-bit extended length
            } else {
                size += 8; // 64-bit extended length
            }
        }
        if (self.masked) {
            size += 4; // Mask key
        }
        return size;
    }
};

/// WebSocket frame (header + payload)
pub const Frame = struct {
    header: FrameHeader,
    payload: []u8,

    /// Check if this is a control frame
    pub fn isControl(self: Frame) bool {
        return self.header.opcode.isControl();
    }

    /// Get close code from close frame payload
    pub fn closeCode(self: Frame) ?CloseCode {
        if (self.header.opcode != .close) return null;
        if (self.payload.len < 2) return null;
        const code = std.mem.readInt(u16, self.payload[0..2], .big);
        return @enumFromInt(code);
    }

    /// Get close reason from close frame payload
    pub fn closeReason(self: Frame) ?[]const u8 {
        if (self.header.opcode != .close) return null;
        if (self.payload.len <= 2) return null;
        return self.payload[2..];
    }
};

/// WebSocket message (reassembled from potentially multiple frames)
pub const Message = struct {
    opcode: Opcode,
    data: std.ArrayListUnmanaged(u8),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, opcode: Opcode) Message {
        return .{
            .opcode = opcode,
            .data = .empty,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Message) void {
        self.data.deinit(self.allocator);
    }

    pub fn append(self: *Message, payload: []const u8) !void {
        try self.data.appendSlice(self.allocator, payload);
    }

    pub fn getPayload(self: *const Message) []const u8 {
        return self.data.items;
    }
};

/// WebSocket frame parser/writer
pub const Codec = struct {
    allocator: std.mem.Allocator,
    max_frame_size: usize = 16 * 1024 * 1024, // 16MB default max
    current_message: ?Message = null,

    pub fn init(allocator: std.mem.Allocator) Codec {
        return .{ .allocator = allocator };
    }

    pub fn deinit(self: *Codec) void {
        if (self.current_message) |*msg| {
            msg.deinit();
        }
    }

    /// Parse a frame from a buffer, returns null if incomplete
    pub fn parseFrame(self: *Codec, buffer: []const u8) !?struct { frame: Frame, consumed: usize } {
        if (buffer.len < 2) return null;

        // Parse first two bytes
        const byte0 = buffer[0];
        const byte1 = buffer[1];

        const fin = (byte0 & 0x80) != 0;
        const rsv1 = (byte0 & 0x40) != 0;
        const rsv2 = (byte0 & 0x20) != 0;
        const rsv3 = (byte0 & 0x10) != 0;
        const opcode_raw: u4 = @truncate(byte0 & 0x0F);
        const masked = (byte1 & 0x80) != 0;
        const len0: u7 = @truncate(byte1 & 0x7F);

        // Validate opcode
        const opcode = Opcode.fromInt(opcode_raw) orelse return error.InvalidOpcode;

        // RSV bits must be 0 unless extension negotiated
        if (rsv1 or rsv2 or rsv3) return error.InvalidRsvBits;

        // Control frames must not be fragmented and must have payload <= 125
        if (opcode.isControl()) {
            if (!fin) return error.FragmentedControlFrame;
            if (len0 > 125) return error.ControlFrameTooLarge;
        }

        // Determine payload length
        var offset: usize = 2;
        var payload_len: u64 = len0;

        if (len0 == 126) {
            if (buffer.len < offset + 2) return null;
            payload_len = std.mem.readInt(u16, buffer[offset..][0..2], .big);
            offset += 2;
        } else if (len0 == 127) {
            if (buffer.len < offset + 8) return null;
            payload_len = std.mem.readInt(u64, buffer[offset..][0..8], .big);
            offset += 8;
            // MSB must be 0 per RFC
            if (payload_len & 0x8000000000000000 != 0) return error.InvalidPayloadLength;
        }

        // Check max frame size
        if (payload_len > self.max_frame_size) return error.FrameTooLarge;

        // Read mask key if present
        var mask_key: ?[4]u8 = null;
        if (masked) {
            if (buffer.len < offset + 4) return null;
            mask_key = buffer[offset..][0..4].*;
            offset += 4;
        }

        // Check if we have complete payload
        const payload_len_usize: usize = @intCast(payload_len);
        if (buffer.len < offset + payload_len_usize) return null;

        // Extract and unmask payload
        const payload = try self.allocator.alloc(u8, payload_len_usize);
        errdefer self.allocator.free(payload);
        @memcpy(payload, buffer[offset..][0..payload_len_usize]);

        if (mask_key) |key| {
            unmaskPayload(payload, key);
        }

        const header = FrameHeader{
            .fin = fin,
            .rsv1 = rsv1,
            .rsv2 = rsv2,
            .rsv3 = rsv3,
            .opcode = opcode,
            .masked = masked,
            .payload_len = payload_len,
            .mask_key = mask_key,
        };

        return .{
            .frame = Frame{ .header = header, .payload = payload },
            .consumed = offset + payload_len_usize,
        };
    }

    /// Free a frame's payload
    pub fn freeFrame(self: *Codec, frame: *Frame) void {
        self.allocator.free(frame.payload);
    }

    /// Process a frame and return a complete message if available
    pub fn processFrame(self: *Codec, frame: Frame) !?Message {
        // Control frames are returned immediately
        if (frame.header.opcode.isControl()) {
            var msg = Message.init(self.allocator, frame.header.opcode);
            try msg.append(frame.payload);
            return msg;
        }

        // Data frame handling
        if (frame.header.opcode == .continuation) {
            // Must have a message in progress
            if (self.current_message == null) return error.UnexpectedContinuation;
        } else {
            // Starting new message - must not have one in progress
            if (self.current_message != null) return error.MessageInProgress;
            self.current_message = Message.init(self.allocator, frame.header.opcode);
        }

        // Append payload
        try self.current_message.?.append(frame.payload);

        // Check if message is complete
        if (frame.header.fin) {
            const msg = self.current_message.?;
            self.current_message = null;
            return msg;
        }

        return null;
    }

    /// Write a frame to a buffer
    pub fn writeFrame(
        self: *Codec,
        buffer: []u8,
        opcode: Opcode,
        payload: []const u8,
        fin: bool,
        mask: bool,
    ) !usize {
        _ = self;
        var offset: usize = 0;

        // Byte 0: FIN, RSV, Opcode
        buffer[0] = @as(u8, if (fin) 0x80 else 0) | @intFromEnum(opcode);
        offset += 1;

        // Byte 1: MASK, Payload length
        const mask_bit: u8 = if (mask) 0x80 else 0;
        if (payload.len <= 125) {
            buffer[1] = mask_bit | @as(u8, @intCast(payload.len));
            offset += 1;
        } else if (payload.len <= 65535) {
            buffer[1] = mask_bit | 126;
            std.mem.writeInt(u16, buffer[2..4], @intCast(payload.len), .big);
            offset += 3;
        } else {
            buffer[1] = mask_bit | 127;
            std.mem.writeInt(u64, buffer[2..10], @intCast(payload.len), .big);
            offset += 9;
        }

        // Mask key (if masking)
        var mask_key: [4]u8 = undefined;
        if (mask) {
            std.crypto.random.bytes(&mask_key);
            @memcpy(buffer[offset..][0..4], &mask_key);
            offset += 4;
        }

        // Payload (masked if required)
        if (mask) {
            for (payload, 0..) |byte, i| {
                buffer[offset + i] = byte ^ mask_key[i % 4];
            }
        } else {
            @memcpy(buffer[offset..][0..payload.len], payload);
        }
        offset += payload.len;

        return offset;
    }

    /// Calculate required buffer size for a frame
    pub fn frameSize(payload_len: usize, masked: bool) usize {
        var size: usize = 2; // Base header
        if (payload_len > 125) {
            if (payload_len <= 65535) {
                size += 2;
            } else {
                size += 8;
            }
        }
        if (masked) {
            size += 4;
        }
        return size + payload_len;
    }
};

/// Unmask payload data in-place using XOR with mask key
pub fn unmaskPayload(payload: []u8, mask_key: [4]u8) void {
    // SIMD-friendly unmasking for larger payloads
    if (payload.len >= 8) {
        const mask_u32: u32 = @bitCast(mask_key);
        const mask_u64: u64 = @as(u64, mask_u32) | (@as(u64, mask_u32) << 32);

        var i: usize = 0;
        // Process 8 bytes at a time
        while (i + 8 <= payload.len) : (i += 8) {
            const chunk = std.mem.readInt(u64, payload[i..][0..8], .little);
            std.mem.writeInt(u64, payload[i..][0..8], chunk ^ mask_u64, .little);
        }
        // Handle remaining bytes
        for (payload[i..], i..) |*byte, j| {
            byte.* ^= mask_key[j % 4];
        }
    } else {
        for (payload, 0..) |*byte, i| {
            byte.* ^= mask_key[i % 4];
        }
    }
}

/// Mask payload data in-place (same operation as unmask)
pub const maskPayload = unmaskPayload;

// ============================================================================
// HTTP Upgrade Handshake
// ============================================================================

/// WebSocket GUID for handshake (RFC 6455)
const WS_GUID = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11";

/// Compute Sec-WebSocket-Accept value from client key
pub fn computeAcceptKey(client_key: []const u8, out: *[28]u8) void {
    var hasher = Sha1.init(.{});
    hasher.update(client_key);
    hasher.update(WS_GUID);
    const hash = hasher.finalResult();

    // Base64 encode the SHA-1 hash
    _ = std.base64.standard.Encoder.encode(out, &hash);
}

/// Validate WebSocket upgrade request headers
/// headers_get should be a struct with a `get(name: []const u8) ?[]const u8` method
pub fn validateUpgradeRequest(
    headers_get: anytype,
) ![]const u8 {
    // Check Upgrade header
    const upgrade = headers_get.get("Upgrade") orelse return error.MissingUpgradeHeader;
    if (!std.ascii.eqlIgnoreCase(upgrade, "websocket")) {
        return error.InvalidUpgradeHeader;
    }

    // Check Connection header contains "Upgrade"
    const connection = headers_get.get("Connection") orelse return error.MissingConnectionHeader;
    var found_upgrade = false;
    var iter = std.mem.tokenizeAny(u8, connection, ", ");
    while (iter.next()) |token| {
        if (std.ascii.eqlIgnoreCase(token, "upgrade")) {
            found_upgrade = true;
            break;
        }
    }
    if (!found_upgrade) return error.InvalidConnectionHeader;

    // Check Sec-WebSocket-Version
    const version = headers_get.get("Sec-WebSocket-Version") orelse return error.MissingVersionHeader;
    if (!std.mem.eql(u8, version, "13")) {
        return error.UnsupportedVersion;
    }

    // Get and validate Sec-WebSocket-Key
    const key = headers_get.get("Sec-WebSocket-Key") orelse return error.MissingKeyHeader;
    if (key.len != 24) return error.InvalidKeyLength; // Base64 of 16 bytes = 24 chars

    return key;
}

/// Generate WebSocket upgrade response
pub fn generateUpgradeResponse(client_key: []const u8, buffer: []u8) usize {
    var accept_key: [28]u8 = undefined;
    computeAcceptKey(client_key, &accept_key);

    const response =
        "HTTP/1.1 101 Switching Protocols\r\n" ++
        "Upgrade: websocket\r\n" ++
        "Connection: Upgrade\r\n" ++
        "Sec-WebSocket-Accept: ";
    const suffix = "\r\n\r\n";

    var offset: usize = 0;
    @memcpy(buffer[offset..][0..response.len], response);
    offset += response.len;
    @memcpy(buffer[offset..][0..28], &accept_key);
    offset += 28;
    @memcpy(buffer[offset..][0..suffix.len], suffix);
    offset += suffix.len;

    return offset;
}

// ============================================================================
// Convenience Functions
// ============================================================================

/// Create a close frame payload with status code and optional reason
pub fn makeClosePayload(allocator: std.mem.Allocator, code: CloseCode, reason: ?[]const u8) ![]u8 {
    const reason_len = if (reason) |r| r.len else 0;
    const payload = try allocator.alloc(u8, 2 + reason_len);

    std.mem.writeInt(u16, payload[0..2], @intFromEnum(code), .big);
    if (reason) |r| {
        @memcpy(payload[2..], r);
    }

    return payload;
}

/// Create a text frame
pub fn createTextFrame(codec: *Codec, buffer: []u8, text: []const u8) !usize {
    return codec.writeFrame(buffer, .text, text, true, false);
}

/// Create a binary frame
pub fn createBinaryFrame(codec: *Codec, buffer: []u8, data: []const u8) !usize {
    return codec.writeFrame(buffer, .binary, data, true, false);
}

/// Create a ping frame
pub fn createPingFrame(codec: *Codec, buffer: []u8, payload: []const u8) !usize {
    return codec.writeFrame(buffer, .ping, payload, true, false);
}

/// Create a pong frame (response to ping)
pub fn createPongFrame(codec: *Codec, buffer: []u8, payload: []const u8) !usize {
    return codec.writeFrame(buffer, .pong, payload, true, false);
}

/// Create a close frame
pub fn createCloseFrame(codec: *Codec, buffer: []u8, code: CloseCode, reason: ?[]const u8) !usize {
    var payload_buf: [127]u8 = undefined;
    std.mem.writeInt(u16, payload_buf[0..2], @intFromEnum(code), .big);
    var payload_len: usize = 2;

    if (reason) |r| {
        const copy_len = @min(r.len, 123); // Max 125 - 2 for code
        @memcpy(payload_buf[2..][0..copy_len], r[0..copy_len]);
        payload_len += copy_len;
    }

    return codec.writeFrame(buffer, .close, payload_buf[0..payload_len], true, false);
}

// ============================================================================
// WebSocket Connection Handler
// ============================================================================

/// WebSocket connection for use in handlers
pub const Connection = struct {
    socket: std.posix.socket_t,
    codec: Codec,
    allocator: std.mem.Allocator,
    closed: bool = false,

    pub fn init(allocator: std.mem.Allocator, socket: std.posix.socket_t) Connection {
        return .{
            .socket = socket,
            .codec = Codec.init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Connection) void {
        self.codec.deinit();
    }

    /// Send a text message
    pub fn sendText(self: *Connection, text: []const u8) !void {
        var buffer: [16384]u8 = undefined;
        const frame_size = Codec.frameSize(text.len, false);
        if (frame_size > buffer.len) return error.MessageTooLarge;

        const len = try self.codec.writeFrame(&buffer, .text, text, true, false);
        _ = try std.posix.write(self.socket, buffer[0..len]);
    }

    /// Send a binary message
    pub fn sendBinary(self: *Connection, data: []const u8) !void {
        var buffer: [16384]u8 = undefined;
        const frame_size = Codec.frameSize(data.len, false);
        if (frame_size > buffer.len) return error.MessageTooLarge;

        const len = try self.codec.writeFrame(&buffer, .binary, data, true, false);
        _ = try std.posix.write(self.socket, buffer[0..len]);
    }

    /// Send a ping
    pub fn sendPing(self: *Connection, payload: []const u8) !void {
        var buffer: [131]u8 = undefined; // Max control frame size
        const len = try self.codec.writeFrame(&buffer, .ping, payload, true, false);
        _ = try std.posix.write(self.socket, buffer[0..len]);
    }

    /// Send a pong (response to ping)
    pub fn sendPong(self: *Connection, payload: []const u8) !void {
        var buffer: [131]u8 = undefined;
        const len = try self.codec.writeFrame(&buffer, .pong, payload, true, false);
        _ = try std.posix.write(self.socket, buffer[0..len]);
    }

    /// Send close frame and mark connection as closed
    pub fn close(self: *Connection, code: CloseCode, reason: ?[]const u8) !void {
        if (self.closed) return;
        self.closed = true;

        var buffer: [131]u8 = undefined;
        const len = try createCloseFrame(&self.codec, &buffer, code, reason);
        _ = try std.posix.write(self.socket, buffer[0..len]);
    }

    /// Check if connection is still open
    pub fn isOpen(self: *const Connection) bool {
        return !self.closed;
    }
};

/// WebSocket event handler callbacks
pub const Handler = struct {
    /// Called when a text message is received
    onMessage: ?*const fn (*Connection, []const u8, Opcode) void = null,

    /// Called when connection is closed
    onClose: ?*const fn (*Connection, ?CloseCode, ?[]const u8) void = null,

    /// Called on error
    onError: ?*const fn (*Connection, anyerror) void = null,

    /// User data pointer
    user_data: ?*anyopaque = null,
};

/// Result of checking if a request is a WebSocket upgrade
pub const UpgradeCheck = struct {
    is_upgrade: bool,
    client_key: ?[]const u8,
};

/// Check if an HTTP request is a WebSocket upgrade request
pub fn isUpgradeRequest(headers_get: anytype) UpgradeCheck {
    const key = validateUpgradeRequest(headers_get) catch {
        return .{ .is_upgrade = false, .client_key = null };
    };
    return .{ .is_upgrade = true, .client_key = key };
}

// ============================================================================
// Tests
// ============================================================================

test "accept key computation" {
    // Test vector from RFC 6455
    var accept: [28]u8 = undefined;
    computeAcceptKey("dGhlIHNhbXBsZSBub25jZQ==", &accept);
    try std.testing.expectEqualStrings("s3pPLMBiTxaQ9kYGzzhZRbK+xOo=", &accept);
}

test "unmask payload" {
    var payload = [_]u8{ 0x7f, 0x9f, 0x4d, 0x51, 0x58 };
    const mask_key = [_]u8{ 0x37, 0xfa, 0x21, 0x3d };
    unmaskPayload(&payload, mask_key);
    try std.testing.expectEqualStrings("Hello", &payload);
}

test "frame size calculation" {
    // Small payload, no mask
    try std.testing.expectEqual(@as(usize, 2 + 10), Codec.frameSize(10, false));
    // Small payload, masked
    try std.testing.expectEqual(@as(usize, 2 + 4 + 10), Codec.frameSize(10, true));
    // Medium payload (126-65535)
    try std.testing.expectEqual(@as(usize, 4 + 1000), Codec.frameSize(1000, false));
    // Large payload (>65535)
    try std.testing.expectEqual(@as(usize, 10 + 100000), Codec.frameSize(100000, false));
}

test "parse simple frame" {
    const allocator = std.testing.allocator;
    var codec = Codec.init(allocator);
    defer codec.deinit();

    // Simple text frame: FIN=1, opcode=1 (text), no mask, payload="Hi"
    const frame_bytes = [_]u8{ 0x81, 0x02, 'H', 'i' };

    const result = try codec.parseFrame(&frame_bytes);
    try std.testing.expect(result != null);

    var frame = result.?.frame;
    defer codec.freeFrame(&frame);

    try std.testing.expect(frame.header.fin);
    try std.testing.expectEqual(Opcode.text, frame.header.opcode);
    try std.testing.expect(!frame.header.masked);
    try std.testing.expectEqual(@as(u64, 2), frame.header.payload_len);
    try std.testing.expectEqualStrings("Hi", frame.payload);
    try std.testing.expectEqual(@as(usize, 4), result.?.consumed);
}

test "parse masked frame" {
    const allocator = std.testing.allocator;
    var codec = Codec.init(allocator);
    defer codec.deinit();

    // Masked text frame with "Hello"
    // Mask key: 0x37, 0xfa, 0x21, 0x3d
    // "Hello" XOR mask = 0x7f, 0x9f, 0x4d, 0x51, 0x58
    const frame_bytes = [_]u8{
        0x81, 0x85, // FIN=1, text, MASK=1, len=5
        0x37, 0xfa, 0x21, 0x3d, // mask key
        0x7f, 0x9f, 0x4d, 0x51, 0x58, // masked payload
    };

    const result = try codec.parseFrame(&frame_bytes);
    try std.testing.expect(result != null);

    var frame = result.?.frame;
    defer codec.freeFrame(&frame);

    try std.testing.expectEqualStrings("Hello", frame.payload);
}

test "write text frame" {
    const allocator = std.testing.allocator;
    var codec = Codec.init(allocator);
    defer codec.deinit();

    var buffer: [100]u8 = undefined;
    const len = try codec.writeFrame(&buffer, .text, "Hello", true, false);

    try std.testing.expectEqual(@as(usize, 7), len);
    try std.testing.expectEqual(@as(u8, 0x81), buffer[0]); // FIN=1, text
    try std.testing.expectEqual(@as(u8, 0x05), buffer[1]); // len=5, no mask
    try std.testing.expectEqualStrings("Hello", buffer[2..7]);
}

test "upgrade response generation" {
    var buffer: [200]u8 = undefined;
    const len = generateUpgradeResponse("dGhlIHNhbXBsZSBub25jZQ==", &buffer);

    const response = buffer[0..len];
    try std.testing.expect(std.mem.indexOf(u8, response, "101 Switching Protocols") != null);
    try std.testing.expect(std.mem.indexOf(u8, response, "s3pPLMBiTxaQ9kYGzzhZRbK+xOo=") != null);
}
