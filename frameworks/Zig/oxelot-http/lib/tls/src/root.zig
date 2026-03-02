const std = @import("std");
const assert = std.debug.assert;
const Io = std.Io;

pub const max_ciphertext_record_len = @import("cipher.zig").max_ciphertext_record_len;

/// Buffer of this size will fit any tls ciphertext record sent by other side.
/// To decrytp we need full record, smalled buffer will not work in general
/// case. Bigger can be used for performance reason.
pub const input_buffer_len = max_ciphertext_record_len; // 16645 bytes

/// Needed output buffer during handshake is the size of the tls hello message,
/// which is (when client authentication is not used) ~1600 bytes. After
/// handshake it limits how big tls record can be produced. This suggested value
/// can hold max ciphertext record produced with this implementation.
pub const output_buffer_len = @import("cipher.zig").max_encrypted_record_len; // 16469 bytes

pub const Connection = @import("connection.zig").Connection;

const handshake = struct {
    const Client = @import("handshake_client.zig").Handshake;
    const Server = @import("handshake_server.zig").Handshake;
};

//TODO: io first
/// Upgrades existing stream to the tls connection by the client tls handshake.
pub inline fn clientFromStream(io: std.Io, stream: anytype, opt: config.Client) !Connection {
    const input, const output = streamToRaderWriter(io, stream);
    return try client(input, output, opt);
}

pub fn client(input: *Io.Reader, output: *Io.Writer, opt: config.Client) !Connection {
    assert(input.buffer.len >= input_buffer_len);
    assert(output.buffer.len >= 2048); // client hello requires: 1572 + opt.host.len

    var hc: handshake.Client = .{ .input = input, .output = output };
    const cipher, const session_resumption_secret_idx = try hc.handshake(opt);
    return .{
        .cipher = cipher,
        .input = input,
        .output = output,
        .session_resumption_secret_idx = session_resumption_secret_idx,
        .session_resumption = opt.session_resumption,
    };
}

/// Upgrades existing stream to the tls connection by the server side tls handshake.
pub inline fn serverFromStream(io: Io, stream: anytype, opt: config.Server) !Connection {
    const input, const output = streamToRaderWriter(io, stream);
    return try server(input, output, opt);
}

pub fn server(input: *Io.Reader, output: *Io.Writer, opt: config.Server) !Connection {
    var hs: handshake.Server = .{ .input = input, .output = output };
    const cipher = try hs.handshake(opt);
    return .{
        .cipher = cipher,
        .input = input,
        .output = output,
    };
}

/// With default buffer sizes
inline fn streamToRaderWriter(io: std.Io, stream: anytype) struct { *Io.Reader, *Io.Writer } {
    var input_buf: [input_buffer_len]u8 = undefined;
    var output_buf: [output_buffer_len]u8 = undefined;
    var reader = stream.reader(io, &input_buf);
    var writer = stream.writer(io, &output_buf);
    const input = if (@hasField(@TypeOf(reader), "interface")) &reader.interface else reader.interface();
    const output = &writer.interface;
    return .{ input, output };
}

pub const Cipher = @import("cipher.zig").Cipher;
pub const config = struct {
    const proto = @import("protocol.zig");
    const common = @import("handshake_common.zig");

    pub const CipherSuite = @import("cipher.zig").CipherSuite;
    pub const PrivateKey = @import("PrivateKey.zig");
    pub const NamedGroup = proto.NamedGroup;
    pub const Version = proto.Version;
    pub const cert = common.cert;
    pub const CertKeyPair = common.CertKeyPair;

    pub const cipher_suites = @import("cipher.zig").cipher_suites;
    pub const key_log = @import("key_log.zig");

    pub const Client = @import("handshake_client.zig").Options;
    pub const Server = @import("handshake_server.zig").Options;
};

/// Non-blocking client/server handshake and connection. Handshake produces
/// cipher used in connection to encrypt data for sending and decrypt received
/// data.
pub const nonblock = struct {
    pub const Client = @import("handshake_client.zig").NonBlock;
    pub const Server = @import("handshake_server.zig").NonBlock;
    pub const Connection = @import("connection.zig").NonBlock;
};

pub const Ktls = @import("Ktls.zig");

test "nonblock handshake and connection" {
    const testing = @import("std").testing;

    // data from server to the client
    var sc_buf: [max_ciphertext_record_len]u8 = undefined;
    // data from client to the server
    var cs_buf: [max_ciphertext_record_len]u8 = undefined;

    // client/server handshake produces ciphers
    const cli_cipher, const srv_cipher = brk: {
        var cli = nonblock.Client.init(.{
            .root_ca = .{},
            .host = &.{},
            .insecure_skip_verify = true,
            .now = .zero,
        });
        var srv = nonblock.Server.init(.{ .auth = null, .now = .zero });

        // client flight1; client hello is in buf1
        var cr = try cli.run(&sc_buf, &cs_buf);
        try testing.expectEqual(0, cr.recv_pos);
        try testing.expect(cr.send.len > 0);
        try testing.expect(!cli.done());

        { // short read, partial buffer received
            for (0..cr.send_pos) |i| {
                const sr = try srv.run(cs_buf[0..i], &sc_buf);
                try testing.expectEqual(0, sr.recv_pos);
                try testing.expectEqual(0, sr.send_pos);
            }
        }

        // server flight 1; server parses client hello from buf2 and writes server hello into buf1
        var sr = try srv.run(&cs_buf, &sc_buf);
        try testing.expectEqual(sr.recv_pos, cr.send_pos);
        try testing.expect(sr.send.len > 0);
        try testing.expect(!srv.done());

        { // short read, partial buffer received
            for (0..sr.send_pos) |i| {
                cr = try cli.run(sc_buf[0..i], &cs_buf);
                try testing.expectEqual(0, cr.recv_pos);
                try testing.expectEqual(0, cr.send_pos);
            }
        }

        // client flight 2; client parses server hello from buf1 and writes finished into buf2
        cr = try cli.run(&sc_buf, &cs_buf);
        try testing.expectEqual(sr.send_pos, cr.recv_pos);
        try testing.expect(cr.send.len > 0);
        try testing.expect(cli.done()); // client is done
        try testing.expect(cli.cipher() != null);

        // server parses client finished
        sr = try srv.run(&cs_buf, &sc_buf);
        try testing.expectEqual(sr.recv_pos, cr.send_pos);
        try testing.expectEqual(0, sr.send.len);
        try testing.expect(srv.done()); // server is done
        try testing.expect(srv.cipher() != null);

        break :brk .{ cli.cipher().?, srv.cipher().? };
    };
    { // use ciphers in connection
        var cli = nonblock.Connection.init(cli_cipher);
        var srv = nonblock.Connection.init(srv_cipher);

        const cleartext = "Lorem ipsum dolor sit amet";
        { // client to server
            const e = try cli.encrypt(cleartext, &cs_buf);
            try testing.expectEqual(cleartext.len, e.cleartext_pos);
            try testing.expect(e.ciphertext.len > cleartext.len);
            try testing.expect(e.unused_cleartext.len == 0);

            const d = try srv.decrypt(e.ciphertext, &sc_buf);
            try testing.expectEqualSlices(u8, cleartext, d.cleartext);
            try testing.expectEqual(e.ciphertext.len, d.ciphertext_pos);
            try testing.expectEqual(0, d.unused_ciphertext.len);
        }
        { // server to client
            const e = try srv.encrypt(cleartext, &sc_buf);
            const d = try cli.decrypt(e.ciphertext, &cs_buf);
            try testing.expectEqualSlices(u8, cleartext, d.cleartext);
        }
        { // server sends close
            const close_buf = try srv.close(&sc_buf);
            const d = try cli.decrypt(close_buf, &cs_buf);
            try testing.expectEqual(close_buf.len, d.ciphertext_pos);
            try testing.expectEqual(0, d.unused_ciphertext.len);
            try testing.expect(d.closed);
        }
    }
}

test {
    _ = @import("handshake_common.zig");
    _ = @import("handshake_server.zig");
    _ = @import("handshake_client.zig");

    _ = @import("connection.zig");
    _ = @import("cipher.zig");
    _ = @import("record.zig");
    _ = @import("transcript.zig");
    _ = @import("PrivateKey.zig");
}
