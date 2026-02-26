const std = @import("std");
const builtin = @import("builtin");
const assert = std.debug.assert;
const mem = std.mem;
const Io = std.Io;
const proto = @import("protocol.zig");
const max_ciphertext_len = @import("cipher.zig").max_ciphertext_len;

const log = std.log.scoped(.tls);

pub const header_len = 5;

pub const Record = struct {
    content_type: proto.ContentType,
    protocol_version: proto.Version = .tls_1_2,
    buffer: []const u8,
    header: []const u8,
    payload: []const u8,

    pub fn init(buffer: []const u8) Record {
        return .{
            .content_type = @enumFromInt(buffer[0]),
            .protocol_version = @enumFromInt(mem.readInt(u16, buffer[1..3], .big)),
            .buffer = buffer,
            .header = buffer[0..header_len],
            .payload = buffer[header_len..],
        };
    }

    const Error = error{
        EndOfStream, // clean close of the stream
        ReadFailed, // all other stream close
        InputBufferUndersize, // input can't fit tls record
        TlsRecordOverflow, // incorrect tls record
    };

    pub fn read(rdr: *Io.Reader) Error!Record {
        if (header_len > rdr.buffer.len) {
            @branchHint(.cold);
            return error.InputBufferUndersize;
        }
        const hdr = try rdr.peek(header_len);
        const payload_len = mem.readInt(u16, hdr[3..5], .big);
        if (payload_len > max_ciphertext_len) {
            @branchHint(.cold);
            return error.TlsRecordOverflow;
        }
        const record_len = header_len + payload_len;
        if (record_len > rdr.buffer.len) {
            @branchHint(.cold);
            return error.InputBufferUndersize;
        }
        return .init(try rdr.take(record_len));
    }

    pub fn decoder(rdr: *Io.Reader) !Decoder {
        const rec = try Record.read(rdr);
        if (@intFromEnum(rec.protocol_version) != 0x0300 and
            @intFromEnum(rec.protocol_version) != 0x0301 and
            rec.protocol_version != .tls_1_2)
            return error.TlsBadVersion;
        return .{
            .content_type = rec.content_type,
            .payload = rec.payload,
        };
    }
};

pub const Decoder = struct {
    content_type: proto.ContentType,
    payload: []const u8,
    idx: usize = 0,

    pub fn init(content_type: proto.ContentType, payload: []const u8) Decoder {
        return .{
            .content_type = content_type,
            .payload = payload,
        };
    }

    pub fn decode(d: *Decoder, comptime T: type) !T {
        switch (@typeInfo(T)) {
            .int => |info| switch (info.bits) {
                8 => {
                    try skip(d, 1);
                    return d.payload[d.idx - 1];
                },
                16 => {
                    try skip(d, 2);
                    const b0: u16 = d.payload[d.idx - 2];
                    const b1: u16 = d.payload[d.idx - 1];
                    return (b0 << 8) | b1;
                },
                24 => {
                    try skip(d, 3);
                    const b0: u24 = d.payload[d.idx - 3];
                    const b1: u24 = d.payload[d.idx - 2];
                    const b2: u24 = d.payload[d.idx - 1];
                    return (b0 << 16) | (b1 << 8) | b2;
                },
                32 => {
                    try skip(d, 4);
                    const b0: u32 = d.payload[d.idx - 4];
                    const b1: u32 = d.payload[d.idx - 3];
                    const b2: u32 = d.payload[d.idx - 2];
                    const b3: u32 = d.payload[d.idx - 1];
                    return (b0 << 24) | (b1 << 16) | (b2 << 8) | b3;
                },
                else => @compileError("unsupported int type: " ++ @typeName(T)),
            },
            .@"enum" => |info| {
                const int = try d.decode(info.tag_type);
                if (info.is_exhaustive) @compileError("exhaustive enum cannot be used");
                return @as(T, @enumFromInt(int));
            },
            else => @compileError("unsupported type: " ++ @typeName(T)),
        }
    }

    pub fn array(d: *Decoder, comptime len: usize) ![len]u8 {
        try d.skip(len);
        return d.payload[d.idx - len ..][0..len].*;
    }

    pub fn slice(d: *Decoder, len: usize) ![]const u8 {
        try d.skip(len);
        return d.payload[d.idx - len ..][0..len];
    }

    pub fn skip(d: *Decoder, amt: usize) !void {
        if (d.idx + amt > d.payload.len) return error.TlsDecodeError;
        d.idx += amt;
    }

    pub fn rest(d: Decoder) []const u8 {
        return d.payload[d.idx..];
    }

    pub fn eof(d: Decoder) bool {
        return d.idx == d.payload.len;
    }

    pub fn expectContentType(d: *Decoder, content_type: proto.ContentType) !void {
        if (d.content_type == content_type) return;

        switch (d.content_type) {
            .alert => try d.raiseAlert(),
            else => return error.TlsUnexpectedMessage,
        }
    }

    pub fn raiseAlert(d: *Decoder) !void {
        if (d.payload.len < 2) return error.TlsUnexpectedMessage;
        try proto.Alert.parse(try d.array(2)).toError();
        return error.TlsAlertCloseNotify;
    }
};

const testing = std.testing;
const data12 = @import("testdata/tls12.zig");
const testu = @import("testu.zig");
const CipherSuite = @import("cipher.zig").CipherSuite;

test "record.read" {
    // expected records in the data12.server_responses
    const expected = [_]struct {
        content_type: proto.ContentType,
        payload_len: usize,
    }{
        .{ .content_type = .handshake, .payload_len = 49 },
        .{ .content_type = .handshake, .payload_len = 815 },
        .{ .content_type = .handshake, .payload_len = 300 },
        .{ .content_type = .handshake, .payload_len = 4 },
        .{ .content_type = .change_cipher_spec, .payload_len = 1 },
        .{ .content_type = .handshake, .payload_len = 64 },
    };
    var reader: Io.Reader = .fixed(&data12.server_responses);

    {
        var n: usize = 0;
        for (expected) |e| {
            const rec = try Record.read(&reader);
            try testing.expectEqual(e.content_type, rec.content_type);
            try testing.expectEqual(e.payload_len, rec.payload.len);
            try testing.expectEqual(.tls_1_2, rec.protocol_version);
            try testing.expectEqual(e.payload_len + header_len, rec.buffer.len);

            n += rec.payload.len + header_len;
            try testing.expectEqual(n, reader.seek);
        }
        try testing.expectEqual(data12.server_responses.len, reader.seek);
        try testing.expectError(error.EndOfStream, Record.read(&reader));
    }

    reader.seek = 0; // rewind
    { // use smaller buffer to force fill, buffer must fit largest record 815 + 5 (header)
        var buffer: [820]u8 = undefined;
        var limited = reader.limited(.unlimited, &buffer);
        for (expected) |e| {
            const rec = try Record.read(&limited.interface);
            try testing.expectEqual(e.content_type, rec.content_type);
            try testing.expectEqual(e.payload_len, rec.payload.len);
            try testing.expectEqual(.tls_1_2, rec.protocol_version);
        }
        try testing.expectError(error.EndOfStream, Record.read(&limited.interface));
    }
}

test Decoder {
    var reader: Io.Reader = .fixed(&data12.server_responses);
    var d = (try Record.decoder(&reader));
    try testing.expectEqual(.handshake, d.content_type);

    try testing.expectEqual(.server_hello, try d.decode(proto.Handshake));
    try testing.expectEqual(45, try d.decode(u24)); // length
    try testing.expectEqual(.tls_1_2, try d.decode(proto.Version));
    try testing.expectEqualStrings(
        &testu.hexToBytes("707172737475767778797a7b7c7d7e7f808182838485868788898a8b8c8d8e8f"),
        try d.slice(32),
    ); // server random
    try testing.expectEqual(0, try d.decode(u8)); // session id len
    try testing.expectEqual(.ECDHE_RSA_WITH_AES_128_CBC_SHA, try d.decode(CipherSuite));
    try testing.expectEqual(0, try d.decode(u8)); // compression method
    try testing.expectEqual(5, try d.decode(u16)); // extension length
    try testing.expectEqual(5, d.rest().len);
    try d.skip(5);
    try testing.expect(d.eof());
}

pub const Writer = struct {
    inner: Io.Writer,

    pub fn initFromIo(io_w: *Io.Writer) Writer {
        return .{ .inner = Io.Writer.fixed(io_w.unusedCapacitySlice()) };
    }

    pub fn init(buffer: []u8) Writer {
        return .{ .inner = Io.Writer.fixed(buffer) };
    }

    fn ensureCapacity(w: Writer, n: usize) !void {
        if (w.inner.unusedCapacityLen() < n)
            return error.OutputBufferUndersize;
    }

    pub fn buffered(w: Writer) []const u8 {
        return w.inner.buffered();
    }

    pub fn bytesWritten(w: Writer) usize {
        return w.inner.end;
    }

    pub fn pos(w: Writer) usize {
        return w.inner.end;
    }

    pub fn byte(w: *Writer, b: u8) !void {
        try w.ensureCapacity(1);
        try w.inner.writeByte(b);
    }

    pub fn slice(w: *Writer, bytes: []const u8) !void {
        try w.ensureCapacity(bytes.len);
        try w.inner.writeAll(bytes);
    }

    pub fn int(w: *Writer, comptime T: type, value: anytype) !void {
        try w.ensureCapacity(@divExact(@typeInfo(T).int.bits, 8));
        try w.inner.writeInt(T, @intCast(value), .big);
    }

    pub fn writableArray(w: *Writer, comptime len: usize) !*[len]u8 {
        try w.ensureCapacity(len);
        return try w.inner.writableArray(len);
    }

    pub fn enumValue(w: *Writer, value: anytype) !void {
        const i = @intFromEnum(value);
        try w.int(@TypeOf(i), i);
    }

    pub fn enumList(w: *Writer, comptime E: type, tags: []const E) !void {
        assert(@sizeOf(E) == 2);
        try w.int(u16, tags.len * 2);
        for (tags) |t| {
            try w.enumValue(t);
        }
    }

    /// Default extension writer, writes extension type and list of tags
    pub fn extension(w: *Writer, ex: proto.Extension, tags: anytype) !void {
        try w.enumValue(ex);
        if (ex == .supported_versions) {
            try w.int(u16, tags.len * 2 + 1);
            try w.int(u8, tags.len * 2);
        } else if (ex == .psk_key_exchange_modes) {
            try w.int(u16, tags.len + 1);
            try w.int(u8, tags.len);
        } else {
            try w.int(u16, tags.len * 2 + 2);
            try w.int(u16, tags.len * 2);
        }
        for (tags) |t| {
            try w.enumValue(t);
        }
    }

    /// Key share extension
    pub fn keyShare(w: *Writer, named_groups: []const proto.NamedGroup, keys: []const []const u8) !void {
        if (keys.len == 0) return;
        assert(named_groups.len == keys.len);
        try w.enumValue(proto.Extension.key_share);
        var l: usize = 0;
        for (keys) |key| {
            l += key.len + 4;
        }
        try w.int(u16, l + 2);
        try w.int(u16, l);
        for (named_groups, 0..) |ng, i| {
            const key = keys[i];
            try w.enumValue(ng);
            try w.int(u16, key.len);
            try w.inner.writeAll(key);
        }
    }

    /// Server name extension
    pub fn serverName(w: *Writer, host: []const u8) !void {
        const host_len: u16 = @intCast(host.len);
        try w.enumValue(proto.Extension.server_name);
        try w.int(u16, host_len + 5); // byte length of extension payload
        try w.int(u16, host_len + 3); // server_name_list byte count
        try w.inner.writeByte(0); // name type
        try w.int(u16, host_len);
        try w.inner.writeAll(host);
    }

    /// Writes header of the pre shared key extension, without binders
    pub fn preSharedKey(
        w: *Writer,
        identity: []const u8,
        obfuscated_age: u32,
        binder_len: u8,
    ) !void {
        try w.enumValue(proto.Extension.pre_shared_key);
        try w.int(u16, identity.len + binder_len + 4 + 2 + 2);
        try w.int(u16, identity.len + 4 + 2);
        try w.int(u16, identity.len);
        try w.inner.writeAll(identity);
        try w.int(u32, obfuscated_age);
    }

    /// Writes the rest of the pre shared keys extension
    pub fn preSharedKeyBinder(w: *Writer, binder: []const u8) !void {
        try w.int(u16, binder.len + 1);
        try w.int(u8, binder.len);
        try w.inner.writeAll(binder);
    }

    /// tls record
    pub fn record(w: *Writer, content_type: proto.ContentType, payload: []const u8) !void {
        try w.enumValue(content_type);
        try w.enumValue(proto.Version.tls_1_2);
        try w.int(u16, payload.len);
        if (w.unused().ptr == payload.ptr) {
            w.inner.advance(payload.len);
        } else {
            try w.slice(payload);
        }
    }

    pub fn recordHeader(w: *Writer, content_type: proto.ContentType, payload_len: usize) !void {
        try w.enumValue(content_type);
        try w.enumValue(proto.Version.tls_1_2);
        try w.int(u16, payload_len);
    }

    /// tls handshake record
    pub fn handshakeRecord(w: *Writer, handshake_type: proto.Handshake, payload: []const u8) !void {
        try w.enumValue(handshake_type);
        try w.int(u24, payload.len);
        if (w.unused().ptr == payload.ptr) {
            w.inner.advance(payload.len);
        } else {
            try w.slice(payload);
        }
    }

    pub fn handshakeRecordHeader(w: *Writer, handshake_type: proto.Handshake, payload_len: usize) !void {
        try w.enumValue(handshake_type);
        try w.int(u24, payload_len);
    }

    pub fn unused(w: *Writer) []u8 {
        return w.inner.unusedCapacitySlice();
    }

    pub fn advance(w: *Writer, n: usize) void {
        w.inner.advance(n);
    }

    /// Skip `n` bytes from current position and return position before skip.
    /// Used with writerAt to later return to that point and write skipped
    /// bytes.
    pub fn skip(w: *Writer, n: usize) !usize {
        try w.ensureCapacity(n);
        const p = w.pos();
        w.inner.advance(n);
        return p;
    }

    /// Returns writer at some previous position
    /// Parent buffer position is not changed.
    pub fn writerAt(w: *Writer, p: usize) Writer {
        assert(p < w.inner.end);
        return Writer{ .inner = Io.Writer.fixed(w.inner.buffered()[p..]) };
    }

    /// Returns new writer in unused buffer part advancing `n` bytes.
    /// Parent buffer position is not changed.
    pub fn writerAdvance(w: *Writer, n: usize) !Writer {
        try w.ensureCapacity(n);
        return Writer{ .inner = Io.Writer.fixed(w.inner.unusedCapacitySlice()[n..]) };
    }
};

test "Writer" {
    var buf: [16]u8 = undefined;
    var w = Writer{ .inner = Io.Writer.fixed(&buf) };

    try w.slice("ab");
    try w.enumValue(proto.Curve.named_curve);
    try w.enumValue(proto.NamedGroup.x25519);
    try w.int(u16, 0x1234);
    try testing.expectEqualSlices(u8, &[_]u8{ 'a', 'b', 0x03, 0x00, 0x1d, 0x12, 0x34 }, w.buffered());
}

fn int2(int: u16) [2]u8 {
    var arr: [2]u8 = undefined;
    std.mem.writeInt(u16, &arr, int, .big);
    return arr;
}

fn int3(int: u24) [3]u8 {
    var arr: [3]u8 = undefined;
    std.mem.writeInt(u24, &arr, int, .big);
    return arr;
}

pub fn header(content_type: proto.ContentType, payload_len: usize) [header_len]u8 {
    return [1]u8{@intFromEnum(content_type)} ++
        int2(@intFromEnum(proto.Version.tls_1_2)) ++
        int2(@intCast(payload_len));
}

pub fn handshakeHeader(handshake_type: proto.Handshake, payload_len: usize) [4]u8 {
    var ret: [4]u8 = undefined;
    ret[0] = @intFromEnum(handshake_type);
    std.mem.writeInt(u24, ret[1..4], @intCast(payload_len), .big);
    return ret;
}
