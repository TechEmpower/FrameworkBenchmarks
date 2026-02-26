const std = @import("std");
const assert = std.debug.assert;
const crypto = std.crypto;
const mem = std.mem;
const Io = std.Io;
const Certificate = crypto.Certificate;

const Cipher = @import("cipher.zig").Cipher;
const CipherSuite = @import("cipher.zig").CipherSuite;
const cipher_suites = @import("cipher.zig").cipher_suites;
const max_cleartext_len = @import("cipher.zig").max_cleartext_len;

const Transcript = @import("transcript.zig").Transcript;
const record = @import("record.zig");
const Record = record.Record;
const rsa = @import("rsa/rsa.zig");
const key_log = @import("key_log.zig");
const PrivateKey = @import("PrivateKey.zig");
const proto = @import("protocol.zig");

const common = @import("handshake_common.zig");
const CertificateBuilder = common.CertificateBuilder;
const CertificateParser = common.CertificateParser;
const DhKeyPair = common.DhKeyPair;
const CertKeyPair = common.CertKeyPair;
const cert = common.cert;

const log = std.log.scoped(.tls);

pub const Options = struct {
    now: Io.Timestamp,

    host: []const u8,
    /// Set of root certificate authorities that clients use when verifying
    /// server certificates.
    root_ca: cert.Bundle,

    /// Controls whether a client verifies the server's certificate chain and
    /// host name.
    insecure_skip_verify: bool = false,

    /// List of cipher suites to use.
    /// To use just tls 1.3 cipher suites:
    ///   .cipher_suites = &tls.CipherSuite.tls13,
    /// To select particular cipher suite:
    ///   .cipher_suites = &[_]tls.CipherSuite{tls.CipherSuite.CHACHA20_POLY1305_SHA256},
    cipher_suites: []const CipherSuite = cipher_suites.all,

    /// List of named groups to use.
    /// To use specific named group:
    ///   .named_groups = &[_]tls.NamedGroup{.secp384r1},
    named_groups: []const proto.NamedGroup = &[_]proto.NamedGroup{
        .x25519,
        .secp256r1,
        .secp384r1,
        .x25519_ml_kem768,
    },

    /// Client authentication certificates and private key.
    auth: ?*CertKeyPair = null,

    /// If this structure is provided it will be filled with handshake attributes
    /// at the end of the handshake process.
    diagnostic: ?*Diagnostic = null,

    /// For logging current connection tls keys, so we can share them with
    /// Wireshark and analyze decrypted traffic there.
    key_log_callback: ?key_log.Callback = null,

    session_resumption: ?*SessionResumption = null,

    pub const Diagnostic = struct {
        tls_version: proto.Version = @enumFromInt(0),
        cipher_suite_tag: CipherSuite = @enumFromInt(0),
        named_group: proto.NamedGroup = @enumFromInt(0),
        signature_scheme: proto.SignatureScheme = @enumFromInt(0),
        client_signature_scheme: proto.SignatureScheme = @enumFromInt(0),
        is_session_resumption: bool = false,
        max_server_record_len: usize = 0,
        max_server_cleartext_len: usize = 0,
        max_client_record_len: usize = 0,
    };

    /// Collects and stores session resumption tickets.
    pub const SessionResumption = struct {
        const Self = @This();

        pub const Ticket = struct {
            // in seconds
            lifetime: u32,
            // in milliseconds
            created_at: i64,
            // value to add to the milliseconds age
            age_add: u32,
            // nonce and identity are pointes to the part of the payload
            nonce: []const u8, // <0..255>
            identity: []const u8, // <1..2^16-1>
            // payload is allocated and should be freed
            payload: []const u8,
            // pointer to the externaly allocated secret
            secret: []const u8,

            pub fn init(payload: []const u8, secret: []const u8, now: Io.Timestamp) !Ticket {
                var d = record.Decoder.init(.handshake, payload);
                const handshake_type: proto.Handshake = @enumFromInt(try d.decode(u8));
                if (handshake_type != proto.Handshake.new_session_ticket) return error.InvalidType;
                const length = try d.decode(u24);
                if (d.rest().len != length) return error.InvalidLength;

                return .{
                    .lifetime = try d.decode(u32),
                    .age_add = try d.decode(u32),
                    .nonce = try d.slice(try d.decode(u8)),
                    .identity = try d.slice(try d.decode(u16)),
                    .payload = payload,
                    .secret = secret,
                    .created_at = @intCast(@divTrunc(now.nanoseconds, std.time.ns_per_ms)),
                };
            }

            /// Obfuscated age for pre shared extension in client hello message when
            /// using this ticket.
            pub fn obfuscatedAge(t: Ticket, now: Io.Timestamp) u32 {
                const now_ms: i64 = @intCast(@divTrunc(now.nanoseconds, std.time.ns_per_ms));
                const age: u32 = @intCast(now_ms - t.created_at);
                return t.age_add +% age;
            }
        };

        allocator: std.mem.Allocator,
        now: Io.Timestamp,
        hash_tag: CipherSuite.HashTag = .sha256,
        tickets: std.ArrayListUnmanaged(Ticket) = .empty,
        secrets: std.ArrayListUnmanaged([]const u8) = .empty,
        used_tickets: usize = 0,

        pub fn init(allocator: mem.Allocator, now: Io.Timestamp) Self {
            return .{
                .allocator = allocator,
                .now = now,
            };
        }

        /// Set resumption master secret (known at then end of the handshake) to
        /// be used by new session ticket's sent on that connection. Returns
        /// index of the appended secret in the secrets list. Connection should
        /// use that index in pushTicket to connect ticket and the secret.
        pub fn appendSecret(self: *Self, tag: CipherSuite.HashTag, secret: []const u8) !usize {
            if (self.secrets.items.len == 0) self.hash_tag = tag;
            if (tag != self.hash_tag) return error.InvalidHash;
            try self.secrets.ensureUnusedCapacity(self.allocator, 1);
            self.secrets.appendAssumeCapacity(try self.allocator.dupe(u8, secret));
            return self.secrets.items.len - 1;
        }

        /// When new session ticket message is recived connection should push ticket.
        pub fn pushTicket(self: *Self, data: []const u8, secret_idx: usize) !void {
            assert(self.secrets.items.len > secret_idx);
            try self.tickets.ensureUnusedCapacity(self.allocator, 1);
            const payload = try self.allocator.dupe(u8, data);
            errdefer self.allocator.free(payload);
            const ticket = try Ticket.init(payload, self.secrets.items[secret_idx], self.now);
            self.tickets.appendAssumeCapacity(ticket);
        }

        pub fn popTicket(self: *Self) ?Ticket {
            if (self.used_tickets >= self.tickets.items.len) return null;
            defer self.used_tickets += 1;
            return self.tickets.items[self.used_tickets];
        }

        pub fn deinit(self: *Self) void {
            for (self.tickets.items) |ticket|
                self.allocator.free(ticket.payload);
            self.tickets.deinit(self.allocator);
            for (self.secrets.items) |secret|
                self.allocator.free(secret);
            self.secrets.deinit(self.allocator);
        }

        pub fn print(self: Self) !void {
            std.debug.print(
                "resumption tag: {}, tickets: {} used_tickets: {}\n",
                .{ self.hash_tag, self.tickets.items.len, self.used_tickets },
            );
            for (self.tickets.items) |ticket| {
                std.debug.print(
                    "  lifetime: {}, ticket age add: {}, nonce: {x}\n  ticket: {x}\n  secret: {x}\n\n",
                    .{ ticket.lifetime, ticket.age_add, ticket.nonce, ticket.identity, ticket.secret },
                );
            }
        }
    };
};

const ResumptionTicket = Options.SessionResumption.Ticket;

const supported_named_groups = &[_]proto.NamedGroup{
    .x25519,
    .secp256r1,
    .secp384r1,
    .x25519_ml_kem768,
};

/// Handshake parses tls server message and creates client messages. Collects
/// tls attributes: server random, cipher suite and so on. Client messages are
/// created using provided buffer. Provided record reader is used to get tls
/// record when needed.
pub const Handshake = struct {
    /// Underlying network connection stream reader/writer pair.
    input: *Io.Reader,
    output: *Io.Writer,

    client_random: [32]u8 = undefined,
    server_random: [32]u8 = undefined,
    master_secret: [48]u8 = undefined,
    key_material: [48 * 4]u8 = undefined, // for sha256 32 * 4 is filled, for sha384 48 * 4

    transcript: Transcript = .{},
    cipher_suite: CipherSuite = @enumFromInt(0),
    named_group: ?proto.NamedGroup = null,
    dh_kp: DhKeyPair = undefined,
    rsa_secret: RsaSecret = undefined,
    tls_version: proto.Version = .tls_1_2,
    cipher: Cipher = undefined,
    cert: CertificateParser = undefined,
    client_certificate_requested: bool = false,
    // public key len: x25519 = 32, secp256r1 = 65, secp384r1 = 97, x25519_ml_kem768 = 1120
    server_pub_key_buf: [1120]u8 = undefined,
    server_pub_key: []const u8 = undefined,
    pre_shared_selected_identity: ?u16 = null,
    // statistics
    max_server_record_len: usize = 0,
    max_server_cleartext_len: usize = 0,
    max_client_record_len: usize = 0,

    const Self = @This();

    fn initKeys(h: *Self, opt: Options) !void {
        const init_keys_buf_len = 32 + 46 + DhKeyPair.seed_len;
        var buf: [init_keys_buf_len]u8 = undefined;
        crypto.random.bytes(&buf);

        h.client_random = buf[0..32].*;
        h.rsa_secret = RsaSecret.init(buf[32..][0..46].*);
        h.dh_kp = try DhKeyPair.init(buf[32 + 46 ..][0..DhKeyPair.seed_len].*, opt.named_groups);

        h.cert = .{
            .host = opt.host,
            .root_ca = opt.root_ca,
            .skip_verify = opt.insecure_skip_verify,
            .now_sec = opt.now.toSeconds(),
        };
    }

    /// Handshake exchanges messages with server to get agreement about
    /// cryptographic parameters. That upgrades existing client-server
    /// connection to TLS connection. Returns cipher used in application for
    /// encrypted message exchange.
    ///
    /// Handles TLS 1.2 and TLS 1.3 connections. After initial client hello
    /// server chooses in its server hello which TLS version will be used.
    ///
    /// TLS 1.2 handshake messages exchange:
    ///    Client                                               Server
    /// --------------------------------------------------------------
    ///    ClientHello        client flight 1 --->
    ///                                                    ServerHello
    ///                                                    Certificate
    ///                                              ServerKeyExchange
    ///                                            CertificateRequest*
    ///                     <--- server flight 1      ServerHelloDone
    ///    Certificate*
    ///    ClientKeyExchange
    ///    CertificateVerify*
    ///    ChangeCipherSpec
    ///    Finished            client flight 2 --->
    ///                                               ChangeCipherSpec
    ///                       <--- server flight 2           Finished
    ///
    /// TLS 1.3 handshake messages exchange:
    ///    Client                                               Server
    /// --------------------------------------------------------------
    ///    ClientHello       client flight 1 --->
    ///                                                    ServerHello
    ///                                          {EncryptedExtensions}
    ///                                          {CertificateRequest*}
    ///                                                  {Certificate}
    ///                                            {CertificateVerify}
    ///                     <--- server flight 1           {Finished}
    ///    ChangeCipherSpec
    ///    {Certificate*}
    ///    {CertificateVerify*}
    ///    Finished           client flight 2 --->
    ///
    ///  *  - optional
    ///  {} - encrypted
    ///
    /// References:
    /// https://datatracker.ietf.org/doc/html/rfc5246#section-7.3
    /// https://datatracker.ietf.org/doc/html/rfc8446#section-2
    ///
    pub fn handshake(h: *Self, opt: Options) !struct { Cipher, ?usize } {
        var resumption_ticket: ?ResumptionTicket =
            if (opt.session_resumption) |r|
                r.popTicket()
            else
                null;

        defer h.updateDiagnostic(opt, resumption_ticket != null);
        try h.initKeys(opt);

        if (resumption_ticket) |ticket| {
            h.transcript.use(opt.session_resumption.?.hash_tag);
            h.transcript.setPreSharedSecret(ticket.secret, ticket.nonce);
        }

        try h.makeClientHello(opt, resumption_ticket); // client flight 1
        h.max_client_record_len = h.output.end;
        try h.output.flush();

        try h.readServerFlight1(); // server flight 1

        if (resumption_ticket != null and h.pre_shared_selected_identity == null) {
            // server didn't accept resuption
            resumption_ticket = null;
            h.transcript.clearPreSharedSecret();
        }

        if (resumption_ticket != null and h.cipher_suite.hash() != h.transcript.tag)
            return error.TlsAlertIllegalParameter; // session resumption must use same hash as previous session
        h.transcript.use(h.cipher_suite.hash());

        // tls 1.3 specific handshake part
        if (h.tls_version == .tls_1_3) {
            try h.generateHandshakeCipher(opt.key_log_callback);
            try h.readEncryptedServerFlight1(resumption_ticket != null); // server flight 1
            const app_cipher = try h.generateApplicationCipher(opt.key_log_callback);
            try h.makeClientFlight2Tls13(opt.auth); // client flight 2
            h.max_client_record_len = @max(h.max_client_record_len, h.output.end);
            try h.output.flush();

            const secret_idx: ?usize = if (opt.session_resumption) |r|
                try r.appendSecret(h.transcript.tag, h.transcript.resumptionSecret())
            else
                null;

            return .{ app_cipher, secret_idx };
        }

        // tls 1.2 specific handshake part
        try h.generateCipher(opt.key_log_callback);
        try h.makeClientFlight2Tls12(opt.auth); // client flight 2
        h.max_client_record_len = @max(h.max_client_record_len, h.output.end);
        try h.output.flush();
        try h.readServerFlight2(); // server flight 2
        return .{ h.cipher, null };
    }

    fn clientFlight1(h: *Self, opt: Options) !void {
        try h.makeClientHello(opt, null);
    }

    fn serverFlight1(h: *Self, opt: Options) !void {
        try h.readServerFlight1();
        h.transcript.use(h.cipher_suite.hash());
        if (h.tls_version == .tls_1_3) {
            try h.generateHandshakeCipher(opt.key_log_callback);
            try h.readEncryptedServerFlight1(false);
        }
    }

    fn clientFlight2(h: *Self, opt: Options) !void {
        if (h.tls_version == .tls_1_3) {
            const app_cipher = try h.generateApplicationCipher(opt.key_log_callback);
            try h.makeClientFlight2Tls13(opt.auth);
            h.cipher = app_cipher;
        } else {
            // tls 1.2 specific handshake part
            try h.generateCipher(opt.key_log_callback);
            try h.makeClientFlight2Tls12(opt.auth);
        }
    }

    fn serverFlight2(h: *Self, _: Options) !void {
        if (h.tls_version == .tls_1_3) return;
        try h.readServerFlight2();
    }

    /// Prepare key material and generate cipher for TLS 1.2
    fn generateCipher(h: *Self, key_log_callback: ?key_log.Callback) !void {
        try h.verifyCertificateSignatureTls12();
        try h.generateKeyMaterial(key_log_callback);
        h.cipher = try Cipher.initTls12(h.cipher_suite, &h.key_material, .client);
    }

    /// Generate TLS 1.2 pre master secret, master secret and key material.
    fn generateKeyMaterial(h: *Self, key_log_callback: ?key_log.Callback) !void {
        const pre_master_secret = if (h.named_group) |named_group|
            try h.dh_kp.sharedKey(named_group, h.server_pub_key)
        else
            &h.rsa_secret.secret;

        h.transcript.masterSecret(&h.master_secret, pre_master_secret, h.client_random, h.server_random);
        h.transcript.keyMaterial(&h.key_material, &h.master_secret, h.client_random, h.server_random);

        if (key_log_callback) |cb| {
            cb(key_log.label.client_random, &h.client_random, &h.master_secret);
        }
    }

    /// TLS 1.3 cipher used during handshake
    fn generateHandshakeCipher(h: *Self, key_log_callback: ?key_log.Callback) !void {
        const shared_key = try h.dh_kp.sharedKey(h.named_group.?, h.server_pub_key);
        const handshake_secret = h.transcript.handshakeSecret(shared_key);
        if (key_log_callback) |cb| {
            cb(key_log.label.server_handshake_traffic_secret, &h.client_random, handshake_secret.server);
            cb(key_log.label.client_handshake_traffic_secret, &h.client_random, handshake_secret.client);
        }
        h.cipher = try Cipher.initTls13(h.cipher_suite, handshake_secret, .client);
    }

    /// TLS 1.3 application (client) cipher
    fn generateApplicationCipher(h: *Self, key_log_callback: ?key_log.Callback) !Cipher {
        const application_secret = h.transcript.applicationSecret();
        if (key_log_callback) |cb| {
            cb(key_log.label.server_traffic_secret_0, &h.client_random, application_secret.server);
            cb(key_log.label.client_traffic_secret_0, &h.client_random, application_secret.client);
        }
        return try Cipher.initTls13(h.cipher_suite, application_secret, .client);
    }

    fn makeClientHello(h: *Self, opt: Options, resumption_ticket: ?ResumptionTicket) !void {
        // Prepare data
        const tls_versions = try CipherSuite.versions(opt.cipher_suites);
        const shared_keys: []const []const u8 = if (tls_versions != .tls_1_2) brk: {
            var keys: [supported_named_groups.len][]const u8 = undefined;
            for (opt.named_groups, 0..) |ng, i| {
                keys[i] = try h.dh_kp.publicKey(ng);
            }
            break :brk keys[0..opt.named_groups.len];
        } else &.{};
        const supported_versions = switch (tls_versions) {
            .both => &[_]proto.Version{ .tls_1_3, .tls_1_2 },
            .tls_1_3 => &[_]proto.Version{.tls_1_3},
            .tls_1_2 => &[_]proto.Version{.tls_1_2},
        };
        const psk_binder_len = if (resumption_ticket == null) 0 else h.transcript.hashLength() + 3;

        var w: record.Writer = .initFromIo(h.output);

        // Placeholder for tls record header (5 bytes) and handshake header (4 bytes).
        // We will write it when we know payload length.
        const header_pos = try w.skip(9);

        try w.enumValue(proto.Version.tls_1_2);
        try w.slice(&h.client_random);
        try w.byte(0); // no session id
        try w.enumList(CipherSuite, opt.cipher_suites);
        try w.slice(&[_]u8{ 0x01, 0x00 }); // no compression
        // Remeber position to write extensions length
        const ext_len_pos = try w.skip(2);
        try w.extension(.supported_versions, supported_versions);
        try w.extension(.signature_algorithms, common.supported_signature_algorithms);
        try w.extension(.supported_groups, opt.named_groups);
        try w.keyShare(opt.named_groups, shared_keys);
        try w.serverName(opt.host);
        // binder key placeholder
        const binder_pos: ?usize = if (resumption_ticket) |ticket| brk: {
            // Add pre shared key extension if this is session resumption. Must be last extension.
            try w.extension(.psk_key_exchange_modes, &[_]proto.KeyExchangeModes{.psk_dhe_ke});
            try w.preSharedKey(ticket.identity, ticket.obfuscatedAge(opt.now), psk_binder_len);
            // Retruns placeholder for binder, we can write binder when we have
            // hash up to thise point with valid extnsions length and
            // record header written.
            break :brk try w.skip(psk_binder_len);
        } else null;

        // Write extensions length to the placeholder
        var ew = w.writerAt(ext_len_pos);
        try ew.int(u16, w.pos() - ext_len_pos - 2);
        // Writer header to the placeholder
        var hw = w.writerAt(header_pos);
        try hw.recordHeader(.handshake, w.pos() - 5);
        try hw.handshakeRecordHeader(.client_hello, w.pos() - 9);

        const pre_binder_hash_buf = w.buffered()[record.header_len .. w.pos() - psk_binder_len];
        h.transcript.update(pre_binder_hash_buf);

        if (binder_pos) |pos| {
            // Write binder to the placeholder and update hash with binder extenstion
            const binder = h.transcript.pskBinder();
            var bw = w.writerAt(pos);
            try bw.preSharedKeyBinder(binder);
            h.transcript.update(w.buffered()[w.buffered().len - psk_binder_len ..]);
        }

        h.output.advance(w.buffered().len);
    }

    /// Process first flight of the messages from the server.
    /// Read server hello message. If TLS 1.3 is chosen in server hello
    /// return. For TLS 1.2 continue and read certificate, key_exchange
    /// eventual certificate request and hello done messages.
    fn readServerFlight1(h: *Self) !void {
        var handshake_states: []const proto.Handshake = &.{.server_hello};

        while (true) {
            var d = try Record.decoder(h.input);
            try d.expectContentType(.handshake);
            h.transcript.update(d.payload);
            h.max_server_record_len = @max(h.max_server_record_len, d.payload.len + record.header_len);

            // Multiple handshake messages can be packed in single tls record.
            while (!d.eof()) {
                const handshake_type = try d.decode(proto.Handshake);

                const length = try d.decode(u24);
                if (length > max_cleartext_len)
                    return error.TlsUnsupportedFragmentedHandshakeMessage;

                brk: {
                    for (handshake_states) |state|
                        if (state == handshake_type) break :brk;
                    return error.TlsUnexpectedMessage;
                }
                switch (handshake_type) {
                    .server_hello => { // server hello, ref: https://datatracker.ietf.org/doc/html/rfc5246#section-7.4.1.3
                        try h.parseServerHello(&d, length);
                        if (h.tls_version == .tls_1_3) {
                            if (!d.eof()) return error.TlsIllegalParameter;
                            return; // end of tls 1.3 server flight 1
                        }
                        handshake_states = if (h.cert.skip_verify)
                            &.{ .certificate, .server_key_exchange, .server_hello_done }
                        else
                            &.{.certificate};
                    },
                    .certificate => {
                        try h.cert.parseCertificate(&d, h.tls_version);
                        handshake_states = if (h.cipher_suite.keyExchange() == .rsa)
                            &.{.server_hello_done}
                        else
                            &.{.server_key_exchange};
                    },
                    .server_key_exchange => {
                        try h.parseServerKeyExchange(&d);
                        handshake_states = &.{ .certificate_request, .server_hello_done };
                    },
                    .certificate_request => {
                        h.client_certificate_requested = true;
                        try d.skip(length);
                        handshake_states = &.{.server_hello_done};
                    },
                    .server_hello_done => {
                        if (length != 0) return error.TlsIllegalParameter;
                        return;
                    },
                    else => return error.TlsUnexpectedMessage,
                }
            }
        }
    }

    /// Parse server hello message.
    fn parseServerHello(h: *Self, d: *record.Decoder, length: u24) !void {
        if (try d.decode(proto.Version) != proto.Version.tls_1_2)
            return error.TlsBadVersion;
        h.server_random = try d.array(32);
        if (isServerHelloRetryRequest(&h.server_random))
            return error.TlsServerHelloRetryRequest;

        const session_id_len = try d.decode(u8);
        if (session_id_len > 32) return error.TlsIllegalParameter;
        try d.skip(session_id_len);

        h.cipher_suite = try d.decode(CipherSuite);
        try h.cipher_suite.validate();
        try d.skip(1); // skip compression method

        const extensions_present = length > 2 + 32 + 1 + session_id_len + 2 + 1;
        if (extensions_present) {
            const exs_len = try d.decode(u16);
            var l: usize = 0;
            while (l < exs_len) {
                const typ = try d.decode(proto.Extension);
                const len = try d.decode(u16);
                defer l += len + 4;

                switch (typ) {
                    .supported_versions => {
                        switch (try d.decode(proto.Version)) {
                            .tls_1_2, .tls_1_3 => |v| h.tls_version = v,
                            else => return error.TlsIllegalParameter,
                        }
                        if (len != 2) return error.TlsIllegalParameter;
                    },
                    .key_share => {
                        h.named_group = try d.decode(proto.NamedGroup);
                        h.server_pub_key = try common.dupe(&h.server_pub_key_buf, try d.slice(try d.decode(u16)));
                        if (len != h.server_pub_key.len + 4) return error.TlsIllegalParameter;
                    },
                    .pre_shared_key => {
                        h.pre_shared_selected_identity = try d.decode(u16);
                    },
                    else => {
                        try d.skip(len);
                    },
                }
            }
        }
    }

    fn isServerHelloRetryRequest(server_random: []const u8) bool {
        // Ref: https://datatracker.ietf.org/doc/html/rfc8446#section-4.1.3
        const hello_retry_request_magic = [32]u8{
            0xCF, 0x21, 0xAD, 0x74, 0xE5, 0x9A, 0x61, 0x11, 0xBE, 0x1D, 0x8C, 0x02, 0x1E, 0x65, 0xB8, 0x91,
            0xC2, 0xA2, 0x11, 0x16, 0x7A, 0xBB, 0x8C, 0x5E, 0x07, 0x9E, 0x09, 0xE2, 0xC8, 0xA8, 0x33, 0x9C,
        };
        return mem.eql(u8, server_random, &hello_retry_request_magic);
    }

    fn parseServerKeyExchange(h: *Self, d: *record.Decoder) !void {
        const curve_type = try d.decode(proto.Curve);
        h.named_group = try d.decode(proto.NamedGroup);
        h.server_pub_key = try common.dupe(&h.server_pub_key_buf, try d.slice(try d.decode(u8)));
        h.cert.signature_scheme = try d.decode(proto.SignatureScheme);
        h.cert.signature = try common.dupe(&h.cert.signature_buf, try d.slice(try d.decode(u16)));
        if (curve_type != .named_curve) return error.TlsIllegalParameter;
    }

    /// Read encrypted part (after server hello) of the server first flight
    /// for TLS 1.3: change cipher spec, eventual certificate request,
    /// certificate, certificate verify and handshake finished messages.
    fn readEncryptedServerFlight1(h: *Self, is_session_resumption: bool) !void {
        // buffer for decrypted handshake records
        var cleartext_buffer: [max_cleartext_len]u8 = undefined;
        // cleartext writer
        var cw = Io.Writer.fixed(&cleartext_buffer);
        // valid handshake types for the next record
        var handshake_states: []const proto.Handshake = &.{.encrypted_extensions};

        outer: while (true) {
            // wrapped record decoder
            const rec = try Record.read(h.input);
            if (rec.protocol_version != .tls_1_2) return error.TlsBadVersion;
            h.max_server_record_len = @max(h.max_server_record_len, rec.buffer.len);
            switch (rec.content_type) {
                .change_cipher_spec => {},
                .application_data => {
                    // `decrypt` will return TlsCipherNoSpaceLeft
                    // unusedCapacitySlice is too small to hold cleartext
                    const content_type, const cleartext = try h.cipher.decrypt(cw.unusedCapacitySlice(), rec);
                    cw.advance(cleartext.len);
                    h.max_server_cleartext_len = @max(h.max_server_cleartext_len, cw.end);

                    var d = record.Decoder.init(content_type, cw.buffered());
                    try d.expectContentType(.handshake);
                    while (!d.eof()) {
                        const handshake_type = try d.decode(proto.Handshake);
                        const length = try d.decode(u24);

                        if (length > max_cleartext_len)
                            return error.TlsUnsupportedFragmentedHandshakeMessage;
                        if (length > d.rest().len)
                            continue :outer; // handshake fragmented into multiple records
                        // jump to outer must be before this defer to presrve buffers
                        defer {
                            // 'd.idx' data is consumed
                            h.transcript.update(d.payload[0..d.idx]);
                            _ = cw.consume(d.idx);
                            d = record.Decoder.init(content_type, cw.buffered());
                        }
                        // check valid handshake types in this state
                        brk: {
                            for (handshake_states) |state|
                                if (state == handshake_type) break :brk;
                            return error.TlsUnexpectedMessage;
                        }
                        switch (handshake_type) {
                            .encrypted_extensions => {
                                try d.skip(length);
                                handshake_states = if (is_session_resumption)
                                    &.{ .certificate_request, .certificate, .finished }
                                else if (h.cert.skip_verify)
                                    &.{ .certificate_request, .certificate, .finished }
                                else
                                    &.{ .certificate_request, .certificate };
                            },
                            .certificate_request => {
                                h.client_certificate_requested = true;
                                try d.skip(length);
                                handshake_states = if (h.cert.skip_verify)
                                    &.{ .certificate, .finished }
                                else
                                    &.{.certificate};
                            },
                            .certificate => {
                                try h.cert.parseCertificate(&d, h.tls_version);
                                handshake_states = &.{.certificate_verify};
                            },
                            .certificate_verify => {
                                try h.cert.parseCertificateVerify(&d);
                                try h.cert.verifySignature(h.transcript.serverCertificateVerify());
                                handshake_states = &.{.finished};
                            },
                            .finished => {
                                const actual = try d.slice(length);
                                const expected = h.transcript.serverFinishedTls13();
                                if (!mem.eql(u8, expected, actual))
                                    return error.TlsDecryptError;
                                return;
                            },
                            else => return error.TlsUnexpectedMessage,
                        }
                    }
                },
                else => return error.TlsUnexpectedMessage,
            }
        }
    }

    fn verifyCertificateSignatureTls12(h: *Self) !void {
        if (h.cipher_suite.keyExchange() != .ecdhe) return;
        const verify_bytes = brk: {
            var buffer: [h.server_pub_key_buf.len + 128]u8 = undefined;
            var w = Io.Writer.fixed(&buffer);
            try w.writeAll(&h.client_random);
            try w.writeAll(&h.server_random);
            try w.writeByte(@intFromEnum(proto.Curve.named_curve));
            try w.writeInt(u16, @intFromEnum(h.named_group.?), .big);
            try w.writeByte(@intCast(h.server_pub_key.len));
            try w.writeAll(h.server_pub_key);
            break :brk w.buffered();
        };
        try h.cert.verifySignature(verify_bytes);
    }

    /// Create client key exchange, change cipher spec and handshake
    /// finished messages for tls 1.2.
    /// If client certificate is requested also adds client certificate and
    /// certificate verify messages.
    fn makeClientFlight2Tls12(h: *Self, auth: ?*CertKeyPair) !void {
        var w: record.Writer = .initFromIo(h.output);
        var cert_builder: ?CertificateBuilder = null;

        // Client certificate
        if (h.client_certificate_requested) {
            if (auth) |a| {
                const cb = h.certificateBuilder(a);
                cert_builder = cb;
                var hw = try w.writerAdvance(record.header_len);
                try cb.makeCertificate(&hw);
                h.transcript.update(hw.buffered());
                try w.record(.handshake, hw.buffered());
            } else {
                var hw = try w.writerAdvance(record.header_len);
                try hw.handshakeRecord(.certificate, &[_]u8{ 0, 0, 0 });
                h.transcript.update(hw.buffered());
                try w.record(.handshake, hw.buffered());
            }
        }

        { // Client key exchange
            var hw = try w.writerAdvance(record.header_len);
            if (h.named_group) |named_group| {
                const key = try h.dh_kp.publicKey(named_group);
                try hw.handshakeRecordHeader(.client_key_exchange, 1 + key.len);
                try hw.int(u8, key.len);
                try hw.slice(key);
            } else {
                var key_buf: [512]u8 = undefined;
                const key = try h.rsa_secret.encrypted(&key_buf, h.cert.pub_key_algo, h.cert.pub_key);
                try hw.handshakeRecordHeader(.client_key_exchange, 2 + key.len);
                try hw.int(u16, key.len);
                try hw.slice(key);
            }
            h.transcript.update(hw.buffered());
            try w.record(.handshake, hw.buffered());
        }

        // Client certificate verify
        if (cert_builder) |cb| {
            var hw = try w.writerAdvance(record.header_len);
            try cb.makeCertificateVerify(&hw);
            h.transcript.update(hw.buffered());
            try w.record(.handshake, hw.buffered());
        }

        // Client change cipher spec
        try w.record(.change_cipher_spec, &[_]u8{1});

        { // Client handshake finished
            const client_finished = &record.handshakeHeader(.finished, 12) ++
                h.transcript.clientFinishedTls12(&h.master_secret);
            h.transcript.update(client_finished);
            try h.writeEncrypted(&w, client_finished);
        }

        h.output.advance(w.buffered().len);
    }

    /// Create client change cipher spec and handshake finished messages for
    /// tls 1.3.
    /// If the client certificate is requested by the server and client is
    /// configured with certificates and private key then client certificate
    /// and client certificate verify messages are also created. If the
    /// server has requested certificate but the client is not configured
    /// empty certificate message is sent, as is required by rfc.
    fn makeClientFlight2Tls13(h: *Self, auth: ?*CertKeyPair) !void {
        var w: record.Writer = .initFromIo(h.output);

        // Client change cipher spec
        try w.record(.change_cipher_spec, &[_]u8{1});

        if (h.client_certificate_requested) {
            if (auth) |a| {
                const cb = h.certificateBuilder(a);
                { // Certificate
                    var hw = try w.writerAdvance(record.header_len);
                    try cb.makeCertificate(&hw);
                    h.transcript.update(hw.buffered());
                    try h.writeEncrypted(&w, hw.buffered());
                }
                { // Client certificate
                    var hw = try w.writerAdvance(record.header_len);
                    try cb.makeCertificateVerify(&hw);
                    h.transcript.update(hw.buffered());
                    try h.writeEncrypted(&w, hw.buffered());
                }
            } else {
                // Empty certificate message and no certificate verify message
                var hw = try w.writerAdvance(record.header_len);
                try hw.handshakeRecord(.certificate, &[_]u8{ 0, 0, 0, 0 });
                h.transcript.update(hw.buffered());
                try h.writeEncrypted(&w, hw.buffered());
            }
        }

        { // Finished
            var hw = try w.writerAdvance(record.header_len);
            try hw.handshakeRecord(.finished, h.transcript.clientFinishedTls13());
            h.transcript.update(hw.buffered());
            try h.writeEncrypted(&w, hw.buffered());
        }

        h.output.advance(w.buffered().len);
    }

    fn certificateBuilder(h: *Self, auth: *CertKeyPair) CertificateBuilder {
        return .{
            .cert_key_pair = auth,
            .transcript = &h.transcript,
            .tls_version = h.tls_version,
            .side = .client,
        };
    }

    fn readServerFlight2(h: *Self) !void {
        // Read server change cipher spec message.
        {
            var d = try Record.decoder(h.input);
            try d.expectContentType(.change_cipher_spec);
            h.max_server_record_len = @max(h.max_server_record_len, d.payload.len + record.header_len);
        }
        // Read encrypted server handshake finished message. Verify that
        // content of the server finished message is based on transcript
        // hash and master secret.
        {
            const rec = try Record.read(h.input);
            h.max_server_record_len = @max(h.max_server_record_len, rec.buffer.len);
            const content_type, const server_finished = try h.cipher.decrypt(@constCast(rec.buffer), rec);
            if (content_type != .handshake)
                return error.TlsUnexpectedMessage;
            const expected = record.handshakeHeader(.finished, 12) ++ h.transcript.serverFinishedTls12(&h.master_secret);
            if (!mem.eql(u8, server_finished, &expected))
                return error.TlsBadRecordMac;
        }
    }

    /// Write encrypted handshake message into `w`
    fn writeEncrypted(h: *Self, w: *record.Writer, cleartext: []const u8) !void {
        const ciphertext = try h.cipher.encrypt(w.unused(), .handshake, cleartext);
        w.advance(ciphertext.len);
    }

    // Copy handshake parameters to opt.diagnostic
    fn updateDiagnostic(h: *Self, opt: Options, is_session_resumption: bool) void {
        if (opt.diagnostic) |d| {
            d.tls_version = h.tls_version;
            d.cipher_suite_tag = h.cipher_suite;
            d.named_group = h.named_group orelse @as(proto.NamedGroup, @enumFromInt(0x0000));
            d.signature_scheme = h.cert.signature_scheme;
            d.is_session_resumption = is_session_resumption;
            d.max_server_record_len = h.max_server_record_len;
            d.max_server_cleartext_len = h.max_server_cleartext_len;
            d.max_client_record_len = h.max_client_record_len;
            if (opt.auth) |a|
                d.client_signature_scheme = a.key.signature_scheme;
        }
    }
};

const RsaSecret = struct {
    secret: [48]u8,

    fn init(rand: [46]u8) RsaSecret {
        return .{ .secret = [_]u8{ 0x03, 0x03 } ++ rand };
    }

    // Pre master secret encrypted with certificate public key.
    fn encrypted(
        self: RsaSecret,
        out: *[512]u8,
        cert_pub_key_algo: Certificate.Parsed.PubKeyAlgo,
        cert_pub_key: []const u8,
    ) ![]const u8 {
        if (cert_pub_key_algo != .rsaEncryption) return error.TlsBadSignatureScheme;
        const pk = try rsa.PublicKey.fromDer(cert_pub_key);
        return try pk.encryptPkcsv1_5(&self.secret, out);
    }
};

const testing = std.testing;
const data12 = @import("testdata/tls12.zig");
const data13 = @import("testdata/tls13.zig");
const testu = @import("testu.zig");

test "parse tls 1.2 server hello" {
    var buffer: [1024]u8 = undefined;
    var writer: Io.Writer = .fixed(&buffer);
    var reader: Io.Reader = .fixed(&data12.server_hello_responses);
    var h: Handshake = .{
        .output = &writer,
        .input = &reader,
        .client_random = data12.client_random,
    };
    // Set to known instead of random
    h.dh_kp.x25519_kp.secret_key = data12.client_secret;

    // Parse server hello, certificate and key exchange messages.
    // Read cipher suite, named group, signature scheme, server random certificate public key
    // Verify host name, signature
    // Calculate key material
    h.cert = .{ .host = "example.ulfheim.net", .skip_verify = true, .root_ca = .{}, .now_sec = 0 };
    try h.readServerFlight1();
    try testing.expectEqual(.ECDHE_RSA_WITH_AES_128_CBC_SHA, h.cipher_suite);
    try testing.expectEqual(.x25519, h.named_group.?);
    try testing.expectEqual(.rsa_pkcs1_sha256, h.cert.signature_scheme);
    try testing.expectEqualSlices(u8, &data12.server_random, &h.server_random);
    try testing.expectEqualSlices(u8, &data12.server_pub_key, h.server_pub_key);
    try testing.expectEqualSlices(u8, &data12.signature, h.cert.signature);
    try testing.expectEqualSlices(u8, &data12.cert_pub_key, h.cert.pub_key);

    try h.verifyCertificateSignatureTls12();
    try h.generateKeyMaterial(null);

    try testing.expectEqualSlices(u8, &data12.key_material, h.key_material[0..data12.key_material.len]);
}

test "verify google.com certificate" {
    var buffer: [1024]u8 = undefined;
    var writer: Io.Writer = .fixed(&buffer);
    var reader: Io.Reader = .fixed(@embedFile("testdata/google.com/server_hello"));
    var h: Handshake = .{
        .output = &writer,
        .input = &reader,
        .client_random = @embedFile("testdata/google.com/client_random").*,
    };

    var threaded: std.Io.Threaded = .init(testing.allocator);
    defer threaded.deinit();
    const io = threaded.io();

    var ca_bundle: Certificate.Bundle = .{};
    try ca_bundle.rescan(testing.allocator, io, try std.Io.Clock.real.now(io));
    defer ca_bundle.deinit(testing.allocator);

    h.cert = .{ .host = "google.com", .skip_verify = true, .root_ca = .{}, .now_sec = 1714846451 };
    try h.readServerFlight1();
    try h.verifyCertificateSignatureTls12();
}

test "parse tls 1.3 server hello" {
    var reader: Io.Reader = .fixed(&data13.server_hello);
    var d = try Record.decoder(&reader);

    const handshake_type = try d.decode(proto.Handshake);
    const length = try d.decode(u24);
    try testing.expectEqual(0x000076, length);
    try testing.expectEqual(.server_hello, handshake_type);

    var h: Handshake = undefined;
    try h.parseServerHello(&d, length);

    try testing.expectEqual(.AES_256_GCM_SHA384, h.cipher_suite);
    try testing.expectEqualSlices(u8, &data13.server_random, &h.server_random);
    try testing.expectEqual(.tls_1_3, h.tls_version);
    try testing.expectEqual(.x25519, h.named_group);
    try testing.expectEqualSlices(u8, &data13.server_pub_key, h.server_pub_key);
}

test "init tls 1.3 handshake cipher" {
    const cipher_suite_tag: CipherSuite = .AES_256_GCM_SHA384;

    var transcript = Transcript{};
    transcript.use(cipher_suite_tag.hash());
    transcript.update(data13.client_hello[record.header_len..]);
    transcript.update(data13.server_hello[record.header_len..]);

    var dh_kp = DhKeyPair{
        .x25519_kp = .{
            .public_key = data13.client_public_key,
            .secret_key = data13.client_private_key,
        },
    };
    const shared_key = try dh_kp.sharedKey(.x25519, &data13.server_pub_key);
    try testing.expectEqualSlices(u8, &data13.shared_key, shared_key);

    const cipher = try Cipher.initTls13(cipher_suite_tag, transcript.handshakeSecret(shared_key), .client);

    const c = &cipher.AES_256_GCM_SHA384;
    try testing.expectEqualSlices(u8, &data13.server_handshake_key, &c.decrypt_key);
    try testing.expectEqualSlices(u8, &data13.client_handshake_key, &c.encrypt_key);
    try testing.expectEqualSlices(u8, &data13.server_handshake_iv, &c.decrypt_iv);
    try testing.expectEqualSlices(u8, &data13.client_handshake_iv, &c.encrypt_iv);
}

fn initExampleHandshake(h: *Handshake) !void {
    h.cipher_suite = .AES_256_GCM_SHA384;
    h.transcript.use(h.cipher_suite.hash());
    h.transcript.update(data13.client_hello[record.header_len..]);
    h.transcript.update(data13.server_hello[record.header_len..]);
    h.cipher = try Cipher.initTls13(h.cipher_suite, h.transcript.handshakeSecret(&data13.shared_key), .client);
    h.tls_version = .tls_1_3;
    h.cert.now_sec = 1714846451;
    h.server_pub_key = &data13.server_pub_key;
}

test "tls 1.3 decrypt wrapped record" {
    var cipher = brk: {
        var h: Handshake = .{ .input = undefined, .output = undefined };
        try initExampleHandshake(&h);
        break :brk h.cipher;
    };

    var cleartext_buf: [1024]u8 = undefined;
    {
        const rec = record.Record.init(&data13.server_encrypted_extensions_wrapped);

        const content_type, const cleartext = try cipher.decrypt(&cleartext_buf, rec);
        try testing.expectEqual(.handshake, content_type);
        try testing.expectEqualSlices(u8, &data13.server_encrypted_extensions, cleartext);
    }
    {
        const rec = record.Record.init(&data13.server_certificate_wrapped);

        const content_type, const cleartext = try cipher.decrypt(&cleartext_buf, rec);
        try testing.expectEqual(.handshake, content_type);
        try testing.expectEqualSlices(u8, &data13.server_certificate, cleartext);
    }
}

test "tls 1.3 process server flight" {
    var h: Handshake = brk: {
        var buffer: [1024]u8 = undefined;
        var writer: Io.Writer = .fixed(&buffer);
        var reader: Io.Reader = .fixed(&data13.server_flight);
        break :brk .{ .input = &reader, .output = &writer };
    };
    try initExampleHandshake(&h);

    h.cert = .{ .host = "example.ulfheim.net", .skip_verify = true, .root_ca = .{}, .now_sec = 0 };
    try h.readEncryptedServerFlight1(false);

    { // application cipher keys calculation
        try testing.expectEqualSlices(u8, &data13.handshake_hash, &h.transcript.sha384.hash.peek());

        var cipher = try Cipher.initTls13(h.cipher_suite, h.transcript.applicationSecret(), .client);
        const c = &cipher.AES_256_GCM_SHA384;
        try testing.expectEqualSlices(u8, &data13.server_application_key, &c.decrypt_key);
        try testing.expectEqualSlices(u8, &data13.client_application_key, &c.encrypt_key);
        try testing.expectEqualSlices(u8, &data13.server_application_iv, &c.decrypt_iv);
        try testing.expectEqualSlices(u8, &data13.client_application_iv, &c.encrypt_iv);

        var buffer: [128]u8 = undefined;
        const encrypted = try cipher.encrypt(&buffer, .application_data, "ping");
        try testing.expectEqualSlices(u8, &data13.client_ping_wrapped, encrypted);
    }
    { // client finished message
        const verify_data = h.transcript.clientFinishedTls13();
        try testing.expectEqualSlices(u8, &data13.client_finished_verify_data, verify_data);

        var buffer: [128]u8 = undefined;
        var w: Io.Writer = .fixed(&buffer);
        h.output = &w;
        try h.makeClientFlight2Tls13(null);
        try testing.expectEqualSlices(u8, &testu.hexToBytes("140303000101"), w.buffered()[0..6]);
        try testing.expectEqualSlices(u8, &data13.client_finished_wrapped, w.buffered()[6..]);
    }
}

test "create client hello" {
    const expected = testu.hexToBytes(
        "16 03 03 00 6d " ++ // record header
            "01 00 00 69 " ++ // handshake header
            "03 03 " ++ // protocol version
            "00 01 02 03 04 05 06 07 08 09 0a 0b 0c 0d 0e 0f 10 11 12 13 14 15 16 17 18 19 1a 1b 1c 1d 1e 1f " ++ // client random
            "00 " ++ // no session id
            "00 02 c0 2b " ++ // cipher suites
            "01 00 " ++ // compression methods
            "00 3e " ++ // extensions length
            "00 2b 00 03 02 03 03 " ++ // supported versions extension
            "00 0d 00 14 00 12 04 03 05 03 08 04 08 05 08 06 08 07 02 01 04 01 05 01 " ++ // signature algorithms extension
            "00 0a 00 08 00 06 00 1d 00 17 00 18 " ++ // named groups extension
            "00 00 00 0f 00 0d 00 00 0a 67 6f 6f 67 6c 65 2e 63 6f 6d ", // server name extension
    );

    var h = brk: {
        var buffer: [256 + 10]u8 = undefined; // reserved + host name len
        var stream_writer: Io.Writer = .fixed(&buffer);
        break :brk Handshake{
            .output = &stream_writer,
            .input = undefined,
            .client_random = testu.hexToBytes(
                \\ 00 01 02 03 04 05 06 07 08 09 0a 0b 0c 0d 0e 0f 10 11 12 13 14 15 16 17 18 19 1a 1b 1c 1d 1e 1f
            ),
        };
    };

    try h.makeClientHello(.{
        .host = "google.com",
        .root_ca = .{},
        .cipher_suites = &[_]CipherSuite{CipherSuite.ECDHE_ECDSA_WITH_AES_128_GCM_SHA256},
        .named_groups = &[_]proto.NamedGroup{ .x25519, .secp256r1, .secp384r1 },
        .now = .zero,
    }, null);

    const actual = h.output.buffered();
    try testing.expectEqualSlices(u8, &expected, actual);
    try testing.expectEqual(152, h.output.unusedCapacityLen());
    try testing.expectEqual(114, expected.len);
}

test "client hello size" {
    const opt: Options = .{
        .host = "some-host-name.net",
        .root_ca = .{},
        .cipher_suites = cipher_suites.all,
        .now = .zero,
        //.named_groups = supported_named_groups,
    };

    var buffer: [max_cleartext_len]u8 = undefined;
    var stream_writer: Io.Writer = .fixed(&buffer);
    var h = Handshake{
        .output = &stream_writer,
        .input = undefined,
    };
    try h.initKeys(opt);
    try h.makeClientHello(opt, null);
    try testing.expectEqual(1572 + opt.host.len, h.output.end);
    //try testing.expectEqual(2794 + opt.host.len, h.stream_writer.end);
}

test "handshake verify server finished message" {
    var h: Handshake = brk: {
        var buffer: [1024]u8 = undefined;
        var writer: Io.Writer = .fixed(&buffer);
        var reader: Io.Reader = .fixed(&data12.server_handshake_finished_msgs);
        break :brk .{ .input = &reader, .output = &writer };
    };

    h.cipher_suite = .ECDHE_ECDSA_WITH_AES_128_CBC_SHA;
    h.master_secret = data12.master_secret;

    // add handshake messages to the transcript
    for (data12.handshake_messages) |msg| {
        h.transcript.update(msg[record.header_len..]);
    }

    // expect verify data
    const client_finished = h.transcript.clientFinishedTls12(&h.master_secret);
    try testing.expectEqualSlices(u8, &data12.client_finished, &record.handshakeHeader(.finished, 12) ++ client_finished);

    // init client with prepared key_material
    h.cipher = try Cipher.initTls12(.ECDHE_RSA_WITH_AES_128_CBC_SHA, &data12.key_material, .client);

    // check that server verify data matches calculates from hashes of all handshake messages
    h.transcript.update(&data12.client_finished);
    try h.readServerFlight2();
}

pub const NonBlock = struct {
    const Self = @This();

    // inner sync handshake
    inner: Handshake = undefined,
    opt: Options = undefined,
    state: State = .none,

    const State = enum {
        none,
        init,
        client_flight_1,
        server_flight_1,
        client_flight_2,
        server_flight_2,

        fn next(self: *State) void {
            self.* = @enumFromInt(@intFromEnum(self.*) + 1);
        }
    };

    pub fn init(opt: Options) Self {
        var inner = Handshake{
            .input = undefined,
            .output = undefined,
        };
        inner.initKeys(opt) catch unreachable;
        return .{
            .inner = inner,
            .opt = opt,
            .state = .init,
        };
    }

    fn send(self: *Self) !void {
        switch (self.state) {
            .init => {
                try self.inner.clientFlight1(self.opt);
                self.state.next();
            },
            .server_flight_1 => {
                try self.inner.clientFlight2(self.opt);
                self.state.next();
                if (self.done()) self.inner.updateDiagnostic(self.opt, false);
            },
            else => return,
        }
    }

    fn recv(self: *Self) !void {
        const prev: Transcript = self.inner.transcript;
        errdefer self.inner.transcript = prev;

        switch (self.state) {
            .client_flight_1 => {
                try self.inner.serverFlight1(self.opt);
                self.state.next();
            },
            .client_flight_2 => {
                try self.inner.serverFlight2(self.opt);
                self.state.next();
                if (self.done()) self.inner.updateDiagnostic(self.opt, false);
            },
            else => return,
        }
    }

    /// True when handshake is successfully finished
    pub fn done(self: Self) bool {
        return self.state == .server_flight_2 or
            (self.inner.tls_version == .tls_1_3 and self.state == .client_flight_2);
    }

    /// Runs next handshake step.
    pub fn run(
        self: *Self,
        /// Data received from the peer
        recv_buf: []const u8,
        /// Scratch buffer where data to be sent to the peer will be prepared
        send_buf: []u8,
    ) !struct {
        /// Number of bytes consumed from recv_buf
        recv_pos: usize,
        /// Number of bytes prepared in send_buf
        send_pos: usize,
        /// Unused part of the recv_buf,
        unused_recv: []const u8,
        /// Part of the send_buf that should be sent to the peer
        send: []const u8,
    } {
        if (self.done()) return .{
            .recv_pos = 0,
            .send_pos = 0,
            .unused_recv = recv_buf,
            .send = &.{},
        };

        var input: Io.Reader = .fixed(recv_buf);
        var output: Io.Writer = .fixed(send_buf);
        self.inner.input = &input;
        self.inner.output = &output;

        const recv_pos: usize = if (recv_buf.len > 0) brk: {
            self.recv() catch |err| switch (err) {
                error.EndOfStream, error.InputBufferUndersize => break :brk 0,
                else => return err,
            };
            break :brk input.seek;
        } else 0;

        try self.send();

        return .{
            .recv_pos = recv_pos,
            .send_pos = output.end,
            .unused_recv = recv_buf[recv_pos..],
            .send = output.buffered(),
        };
    }

    /// Cipher produced in handshake, null until successful handshake.
    pub fn cipher(self: Self) ?Cipher {
        return if (self.done()) self.inner.cipher else null;
    }
};

test "nonblock handshake" {
    var buffer: [1024]u8 = undefined;
    var h = NonBlock.init(.{
        .host = "example.ulfheim.net",
        .insecure_skip_verify = true,
        .root_ca = .{},
        .cipher_suites = &[_]CipherSuite{CipherSuite.AES_256_GCM_SHA384},
        .named_groups = &[_]proto.NamedGroup{.x25519},
        .now = .zero,
    });
    { // update secrets to well known from example
        h.inner.client_random = data13.client_random;
        h.inner.dh_kp.x25519_kp = .{
            .public_key = data13.client_public_key,
            .secret_key = data13.client_private_key,
        };
    }

    const expected_client_flight_1 = testu.hexToBytes(
        \\ 16 03 03 00 9c
        \\ 01 00 00 98
        \\ 03 03
        \\ 00 01 02 03 04 05 06 07 08 09 0a 0b 0c 0d 0e 0f 10 11 12 13 14 15 16 17 18 19 1a 1b 1c 1d 1e 1f
        \\ 00
        \\ 00 02 13 02
        \\ 01 00
        \\ 00 6d
        \\ 00 2b 00 03 02 03 04
        \\ 00 0d 00 14 00 12 04 03 05 03 08 04 08 05 08 06 08 07 02 01 04 01 05 01
        \\ 00 0a 00 04 00 02 00 1d 00 33 00 26 00 24 00 1d 00 20
        \\ 35 80 72 d6 36 58 80 d1 ae ea 32 9a df 91 21 38 38 51 ed 21 a2 8e 3b 75 e9 65 d0 d2 cd 16 62 54
        \\ 00 00 00 18 00 16 00 00 13 65 78 61 6d 70 6c 65 2e 75 6c 66 68 65 69 6d 2e 6e 65 74
    );

    var res = try h.run(&.{}, &buffer);
    try testing.expectEqualSlices(u8, &expected_client_flight_1, res.send);

    { // update transcript to well known from example
        h.inner.transcript = .{};
        h.inner.transcript.update(data13.client_hello[record.header_len..]);
    }

    // Parsing partial server flight consumes nothing
    var i: usize = 1;
    while (i < data13.server_flight_1.len) : (i += 16) {
        //for (1..data13.server_flight_1.len - 1) |i| {
        res = try h.run(data13.server_flight_1[0..i], &.{});
        try testing.expectEqual(0, res.recv_pos);
        try testing.expectEqual(i, res.unused_recv.len);
    }

    res = try h.run(&(data13.server_flight_1 ++ "dummy footer".*), &buffer);
    {
        try testing.expectEqual(data13.server_flight_1.len, res.recv_pos); // footer is not touched
        try testing.expectEqual(.tls_1_3, h.inner.tls_version);
        try testing.expectEqual(.x25519, h.inner.named_group);
        try testing.expectEqualSlices(u8, &data13.server_random, &h.inner.server_random);
        try testing.expectEqual(.AES_256_GCM_SHA384, h.inner.cipher_suite);
        try testing.expectEqualSlices(u8, &data13.server_pub_key, h.inner.server_pub_key);
        try testing.expect(h.done());
        try testing.expectEqualSlices(u8, &data13.client_flight_2, res.send);
    }

    // When done pushing more data is noop
    res = try h.run(&("dummy footer".*), &buffer);
    try testing.expectEqual(0, res.recv_pos);
    try testing.expectEqual(0, res.send_pos);
    try testing.expect(h.done());
}

test "note about sizes" {
    // values valid only for 64 bit platform, so skip in ci
    if (true) return error.SkipZigTest;

    try testing.expectEqual(18928, @sizeOf(Handshake));
    try testing.expectEqual(19072, @sizeOf(NonBlock));
    try testing.expectEqual(14384, @sizeOf(DhKeyPair));
    try testing.expectEqual(136, @sizeOf(Options));
    try testing.expectEqual(2792, @sizeOf(CertKeyPair));
    try testing.expectEqual(1736, @sizeOf(CertificateParser));
    try testing.expectEqual(48, @sizeOf(cert.Bundle));
    try testing.expectEqual(208, @sizeOf(Cipher));
}
