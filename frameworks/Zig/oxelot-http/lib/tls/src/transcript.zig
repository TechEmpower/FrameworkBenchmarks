const std = @import("std");
const crypto = std.crypto;
const tls = crypto.tls;
const hkdfExpandLabel = tls.hkdfExpandLabel;

const Sha256 = crypto.hash.sha2.Sha256;
const Sha384 = crypto.hash.sha2.Sha384;
const Sha512 = crypto.hash.sha2.Sha512;

const HashTag = @import("cipher.zig").CipherSuite.HashTag;

// Transcript holds hash of all handshake message.
//
// Until the server hello is parsed we don't know which hash (sha256, sha384,
// sha512) will be used so we update all of them. Handshake process will set
// `selected` field once cipher suite is known. Other function will use that
// selected hash. We continue to calculate all hashes because client certificate
// message could use different hash than the other part of the handshake.
// Handshake hash is dictated by the server selected cipher. Client certificate
// hash is dictated by the private key used.
//
// Most of the functions are inlined because they are returning pointers.
//
pub const Transcript = struct {
    sha256: Type(.sha256) = .{ .hash = Sha256.init(.{}) },
    sha384: Type(.sha384) = .{ .hash = Sha384.init(.{}) },
    sha512: Type(.sha512) = .{ .hash = Sha512.init(.{}) },

    tag: HashTag = .sha256,

    // Transcript Type from hash tag
    fn Type(h: HashTag) type {
        return switch (h) {
            .sha256 => TranscriptT(Sha256),
            .sha384 => TranscriptT(Sha384),
            .sha512 => TranscriptT(Sha512),
        };
    }

    /// Set hash to use in all following function calls.
    pub fn use(t: *Transcript, tag: HashTag) void {
        t.tag = tag;
    }

    pub fn update(t: *Transcript, buf: []const u8) void {
        t.sha256.hash.update(buf);
        t.sha384.hash.update(buf);
        t.sha512.hash.update(buf);
    }

    // tls 1.2 handshake specific

    pub fn masterSecret(
        t: *Transcript,
        out: []u8,
        pre_master_secret: []const u8,
        client_random: [32]u8,
        server_random: [32]u8,
    ) void {
        switch (t.tag) {
            inline else => |h| @field(t, @tagName(h)).masterSecret(
                out,
                pre_master_secret,
                client_random,
                server_random,
            ),
        }
    }

    pub fn keyMaterial(
        t: *Transcript,
        out: []u8,
        master_secret: []const u8,
        client_random: [32]u8,
        server_random: [32]u8,
    ) void {
        switch (t.tag) {
            inline else => |h| @field(t, @tagName(h)).keyExpansion(
                out,
                master_secret,
                client_random,
                server_random,
            ),
        }
    }

    pub fn clientFinishedTls12(t: *Transcript, master_secret: []const u8) [12]u8 {
        return switch (t.tag) {
            inline else => |h| @field(t, @tagName(h)).clientFinishedTls12(master_secret),
        };
    }

    pub fn serverFinishedTls12(t: *Transcript, master_secret: []const u8) [12]u8 {
        return switch (t.tag) {
            inline else => |h| @field(t, @tagName(h)).serverFinishedTls12(master_secret),
        };
    }

    // tls 1.3 handshake specific

    pub fn setPreSharedSecret(
        t: *Transcript,
        resumption_master_secret: []const u8,
        ticket_nonce: []const u8,
    ) void {
        switch (t.tag) {
            inline else => |h| @field(t, @tagName(h)).setPreSharedSecret(resumption_master_secret, ticket_nonce),
        }
    }

    pub fn clearPreSharedSecret(t: *Transcript) void {
        switch (t.tag) {
            inline else => |h| @field(t, @tagName(h)).clearPreSharedSecret(),
        }
    }

    pub fn serverCertificateVerify(t: *Transcript) []const u8 {
        return switch (t.tag) {
            inline else => |h| @field(t, @tagName(h)).serverCertificateVerify(),
        };
    }

    pub fn clientCertificateVerify(t: *Transcript) []const u8 {
        return switch (t.tag) {
            inline else => |h| @field(t, @tagName(h)).clientCertificateVerify(),
        };
    }

    pub fn serverFinishedTls13(t: *Transcript) []const u8 {
        return switch (t.tag) {
            inline else => |h| @field(t, @tagName(h)).serverFinishedTls13(),
        };
    }

    pub fn clientFinishedTls13(t: *Transcript) []const u8 {
        return switch (t.tag) {
            inline else => |h| @field(t, @tagName(h)).clientFinishedTls13(),
        };
    }

    pub const Secret = struct {
        client: []const u8,
        server: []const u8,
    };

    pub fn handshakeSecret(t: *Transcript, shared_key: []const u8) Secret {
        return switch (t.tag) {
            inline else => |h| @field(t, @tagName(h)).handshakeSecret(shared_key),
        };
    }

    pub fn applicationSecret(t: *Transcript) Secret {
        return switch (t.tag) {
            inline else => |h| @field(t, @tagName(h)).applicationSecret(),
        };
    }

    pub fn resumptionSecret(t: *Transcript) []const u8 {
        return switch (t.tag) {
            inline else => |h| @field(t, @tagName(h)).resumptionSecret(),
        };
    }

    pub fn pskBinder(t: *Transcript) []const u8 {
        return switch (t.tag) {
            inline else => |h| @field(t, @tagName(h)).pskBinder(),
        };
    }

    pub fn hashLength(t: *Transcript) u8 {
        return switch (t.tag) {
            inline else => |h| @TypeOf(@field(t, @tagName(h))).hash_length,
        };
    }

    // other

    pub fn Hkdf(h: HashTag) type {
        return Type(h).Hkdf;
    }

    /// Copy of the current hash value
    pub fn hash(t: *Transcript, comptime Hash: type) Hash {
        return switch (Hash) {
            Sha256 => t.sha256.hash,
            Sha384 => t.sha384.hash,
            Sha512 => t.sha512.hash,
            else => @compileError("unimplemented"),
        };
    }
};

fn TranscriptT(comptime Hash: type) type {
    return struct {
        const Hmac = crypto.auth.hmac.Hmac(Hash);
        const Hkdf = crypto.kdf.hkdf.Hkdf(Hmac);
        const hash_length = Hash.digest_length; // Hmac.mac_length == Hmac.key_length == Hash.digest_length

        hash: Hash,
        handshake_secret: ?[hash_length]u8 = null,
        server_finished_key: [hash_length]u8 = undefined,
        client_finished_key: [hash_length]u8 = undefined,
        buffer: [hash_length + 64 + 34]u8 = undefined,

        const Self = @This();

        fn init(transcript: Hash) Self {
            return .{ .transcript = transcript };
        }

        fn serverCertificateVerify(self: *Self) []const u8 {
            self.buffer = ([1]u8{0x20} ** 64) ++
                "TLS 1.3, server CertificateVerify\x00".* ++
                self.hash.peek();
            return &self.buffer;
        }

        // ref: https://www.rfc-editor.org/rfc/rfc8446#section-4.4.3
        fn clientCertificateVerify(self: *Self) []u8 {
            self.buffer = ([1]u8{0x20} ** 64) ++
                "TLS 1.3, client CertificateVerify\x00".* ++
                self.hash.peek();
            return &self.buffer;
        }

        fn masterSecret(
            _: *Self,
            out: []u8,
            pre_master_secret: []const u8,
            client_random: [32]u8,
            server_random: [32]u8,
        ) void {
            const seed = "master secret" ++ client_random ++ server_random;

            var a1: [hash_length]u8 = undefined;
            var a2: [hash_length]u8 = undefined;
            Hmac.create(&a1, seed, pre_master_secret);
            Hmac.create(&a2, &a1, pre_master_secret);

            var p1: [hash_length]u8 = undefined;
            var p2: [hash_length]u8 = undefined;
            Hmac.create(&p1, a1 ++ seed, pre_master_secret);
            Hmac.create(&p2, a2 ++ seed, pre_master_secret);

            const master_secret = p1 ++ p2;
            const len = @min(out.len, master_secret.len);
            @memcpy(out[0..len], master_secret[0..len]);
        }

        fn keyExpansion(
            _: *Self,
            out: []u8,
            master_secret: []const u8,
            client_random: [32]u8,
            server_random: [32]u8,
        ) void {
            const seed = "key expansion" ++ server_random ++ client_random;

            const a0 = seed;
            var a1: [hash_length]u8 = undefined;
            var a2: [hash_length]u8 = undefined;
            var a3: [hash_length]u8 = undefined;
            var a4: [hash_length]u8 = undefined;
            Hmac.create(&a1, a0, master_secret);
            Hmac.create(&a2, &a1, master_secret);
            Hmac.create(&a3, &a2, master_secret);
            Hmac.create(&a4, &a3, master_secret);

            var key_material: [hash_length * 4]u8 = undefined;
            Hmac.create(key_material[0..hash_length], a1 ++ seed, master_secret);
            Hmac.create(key_material[hash_length .. hash_length * 2], a2 ++ seed, master_secret);
            Hmac.create(key_material[hash_length * 2 .. hash_length * 3], a3 ++ seed, master_secret);
            Hmac.create(key_material[hash_length * 3 ..], a4 ++ seed, master_secret);

            const len = @min(out.len, key_material.len);
            @memcpy(out[0..len], key_material[0..len]);
        }

        fn clientFinishedTls12(self: *Self, master_secret: []const u8) [12]u8 {
            const seed = "client finished" ++ self.hash.peek();
            var a1: [hash_length]u8 = undefined;
            var p1: [hash_length]u8 = undefined;
            Hmac.create(&a1, seed, master_secret);
            Hmac.create(&p1, a1 ++ seed, master_secret);
            return p1[0..12].*;
        }

        fn serverFinishedTls12(self: *Self, master_secret: []const u8) [12]u8 {
            const seed = "server finished" ++ self.hash.peek();
            var a1: [hash_length]u8 = undefined;
            var p1: [hash_length]u8 = undefined;
            Hmac.create(&a1, seed, master_secret);
            Hmac.create(&p1, a1 ++ seed, master_secret);
            return p1[0..12].*;
        }

        // tls 1.3

        fn setPreSharedSecret(
            self: *Self,
            resumption_secret: []const u8,
            ticket_nonce: []const u8,
        ) void {
            const ikm = hkdfExpandLabel(
                Hkdf,
                resumption_secret[0..hash_length].*,
                "resumption",
                ticket_nonce,
                hash_length,
            );
            self.handshake_secret = Hkdf.extract(&[1]u8{0}, &ikm);
        }

        fn clearPreSharedSecret(self: *Self) void {
            self.handshake_secret = null;
        }

        fn handshakeSecret(self: *Self, shared_key: []const u8) Transcript.Secret {
            const hello_hash = self.hash.peek();

            const empty_hash = tls.emptyHash(Hash);
            const zeroes = [1]u8{0} ** hash_length;
            const early_secret = if (self.handshake_secret) |hs| hs else Hkdf.extract(&[1]u8{0}, &zeroes);
            const hs_derived_secret = hkdfExpandLabel(Hkdf, early_secret, "derived", &empty_hash, hash_length);

            const secret = Hkdf.extract(&hs_derived_secret, shared_key);
            self.handshake_secret = secret;
            const client_secret = hkdfExpandLabel(Hkdf, secret, "c hs traffic", &hello_hash, hash_length);
            const server_secret = hkdfExpandLabel(Hkdf, secret, "s hs traffic", &hello_hash, hash_length);

            self.server_finished_key = hkdfExpandLabel(Hkdf, server_secret, "finished", "", hash_length);
            self.client_finished_key = hkdfExpandLabel(Hkdf, client_secret, "finished", "", hash_length);

            self.buffer[0..hash_length].* = client_secret;
            self.buffer[hash_length .. 2 * hash_length].* = server_secret;
            return .{
                .client = self.buffer[0..hash_length],
                .server = self.buffer[hash_length .. 2 * hash_length],
            };
        }

        fn applicationSecret(self: *Self) Transcript.Secret {
            const handshake_hash = self.hash.peek();

            const empty_hash = tls.emptyHash(Hash);
            const zeroes = [1]u8{0} ** hash_length;
            const ap_derived_secret = hkdfExpandLabel(Hkdf, self.handshake_secret.?, "derived", &empty_hash, hash_length);
            const master_secret = Hkdf.extract(&ap_derived_secret, &zeroes);

            self.buffer[0..hash_length].* = hkdfExpandLabel(Hkdf, master_secret, "c ap traffic", &handshake_hash, hash_length);
            self.buffer[hash_length .. 2 * hash_length].* = hkdfExpandLabel(Hkdf, master_secret, "s ap traffic", &handshake_hash, hash_length);
            return .{
                .client = self.buffer[0..hash_length],
                .server = self.buffer[hash_length .. 2 * hash_length],
            };
        }

        fn resumptionSecret(self: *Self) []const u8 {
            const handshake_hash = self.hash.peek();
            const empty_hash = tls.emptyHash(Hash);
            const zeroes = [1]u8{0} ** hash_length;
            const ap_derived_secret = hkdfExpandLabel(Hkdf, self.handshake_secret.?, "derived", &empty_hash, hash_length);
            const master_secret = Hkdf.extract(&ap_derived_secret, &zeroes);
            self.buffer[0..hash_length].* = hkdfExpandLabel(Hkdf, master_secret, "res master", &handshake_hash, hash_length);
            return self.buffer[0..hash_length];
        }

        fn pskBinder(self: *Self) []const u8 {
            const secret = self.handshake_secret.?;
            const prk = hkdfExpandLabel(Hkdf, secret, "res binder", &tls.emptyHash(Hash), hash_length);
            const expanded = hkdfExpandLabel(Hkdf, prk, "finished", "", hash_length);
            Hmac.create(self.buffer[0..hash_length], &self.hash.peek(), &expanded);
            return self.buffer[0..hash_length];
        }

        fn serverFinishedTls13(self: *Self) []const u8 {
            Hmac.create(self.buffer[0..hash_length], &self.hash.peek(), &self.server_finished_key);
            return self.buffer[0..hash_length];
        }

        // client finished message with header
        fn clientFinishedTls13(self: *Self) []const u8 {
            Hmac.create(self.buffer[0..hash_length], &self.hash.peek(), &self.client_finished_key);
            return self.buffer[0..hash_length];
        }
    };
}

const hexToBytes = @import("testu.zig").hexToBytes;
const testing = std.testing;

fn pskBinder_(
    comptime Hash: type,
    resumption_master_secret: [Hash.digest_length]u8,
    binder: *[Hash.digest_length]u8,
    binder_hash: [Hash.digest_length]u8,
    ticket_nonce: []const u8,
) void {
    const Hmac = crypto.auth.hmac.Hmac(Hash);
    const Hkdf = crypto.kdf.hkdf.Hkdf(Hmac);

    const ikm = hkdfExpandLabel(Hkdf, resumption_master_secret, "resumption", ticket_nonce, Hash.digest_length);
    const secret = Hkdf.extract(&[1]u8{0}, &ikm);
    const prk = hkdfExpandLabel(Hkdf, secret, "res binder", &tls.emptyHash(Hash), Hash.digest_length);
    const expanded = hkdfExpandLabel(Hkdf, prk, "finished", "", Hash.digest_length);
    Hmac.create(binder, &binder_hash, &expanded);
}

// Example from: https://datatracker.ietf.org/doc/html/rfc8448#autoid-4
test pskBinder_ {
    // input values from example
    const resumption_master_secret = hexToBytes("7d f2 35 f2 03 1d 2a 05 12 87 d0 2b 02 41 b0 bf da f8 6c c8 56 23 1f 2d 5a ba 46 c4 34 ec 19 6c");
    const binder_hash = hexToBytes("63 22 4b 2e 45 73 f2 d3 45 4c a8 4b 9d 00 9a 04 f6 be 9e 05 71 1a 83 96 47 3a ef a0 1e 92 4a 14");
    const ticket_nonce = hexToBytes("00 00");
    // expected intermediate and resulting finished from example
    const expected_ikm = hexToBytes("4e cd 0e b6 ec 3b 4d 87 f5 d6 02 8f 92 2c a4 c5 85 1a 27 7f d4 13 11 c9 e6 2d 2c 94 92 e1 c4 f3");
    const expected_secret = hexToBytes("9b 21 88 e9 b2 fc 6d 64 d7 1d c3 29 90 0e 20 bb 41 91 50 00 f6 78 aa 83 9c bb 79 7c b7 d8 33 2c");
    const expected_prk = hexToBytes("69 fe 13 1a 3b ba d5 d6 3c 64 ee bc c3 0e 39 5b 9d 81 07 72 6a 13 d0 74 e3 89 db c8 a4 e4 72 56");
    const expected_expanded = hexToBytes("55 88 67 3e 72 cb 59 c8 7d 22 0c af fe 94 f2 de a9 a3 b1 60 9f 7d 50 e9 0a 48 22 7d b9 ed 7e aa");
    const expected_binder = hexToBytes("3a dd 4f b2 d8 fd f8 22 a0 ca 3c f7 67 8e f5 e8 8d ae 99 01 41 c5 92 4d 57 bb 6f a3 1b 9e 5f 9d");

    const Hash = Sha256;
    const Hmac = crypto.auth.hmac.Hmac(Hash);
    const Hkdf = crypto.kdf.hkdf.Hkdf(Hmac);

    const ikm = hkdfExpandLabel(Hkdf, resumption_master_secret, "resumption", &ticket_nonce, Hash.digest_length);
    const secret = Hkdf.extract(&[1]u8{0}, &ikm);
    const prk = hkdfExpandLabel(Hkdf, secret, "res binder", &tls.emptyHash(Hash), Hash.digest_length);
    const expanded = hkdfExpandLabel(Hkdf, prk, "finished", "", Hash.digest_length);
    var binder: [Hash.digest_length]u8 = undefined;
    Hmac.create(&binder, &binder_hash, &expanded);

    if (false) {
        std.debug.print("ikm         : {x}\n", .{ikm});
        std.debug.print("secret      : {x}\n", .{secret});
        std.debug.print("prk         : {x}\n", .{prk});
        std.debug.print("expanded    : {x}\n", .{expanded});
        std.debug.print("binder      : {x}\n", .{binder});
    }

    try testing.expectEqualSlices(u8, &expected_ikm, &ikm);
    try testing.expectEqualSlices(u8, &expected_secret, &secret);
    try testing.expectEqualSlices(u8, &expected_prk, &prk);
    try testing.expectEqualSlices(u8, &expected_expanded, &expanded);
    try testing.expectEqualSlices(u8, &expected_binder, &binder);

    pskBinder_(Hash, resumption_master_secret, &binder, binder_hash, &ticket_nonce);
    // test pskBinder function
    try testing.expectEqualSlices(
        u8,
        &expected_binder,
        &binder,
    );
}
