// TLS support for HTTP client
//
// Uses tls.zig's nonblock API for TLS 1.2/1.3 support.
// This wrapper provides a simple interface for HTTPS connections.

const std = @import("std");
const posix = std.posix;
const linux = std.os.linux;
const Allocator = std.mem.Allocator;
const tls = @import("tls");
const Certificate = std.crypto.Certificate;
const base64_decoder = std.base64.standard.decoderWithIgnore("\n\r \t");

// Zig 0.16 compatibility: POSIX-based file handle
const PosixFile = struct {
    fd: posix.fd_t,

    pub fn close(self: PosixFile) void {
        posix.close(self.fd);
    }

    pub fn read(self: PosixFile, buffer: []u8) !usize {
        return posix.read(self.fd, buffer);
    }

    pub fn stat(self: PosixFile) !struct { size: u64 } {
        // Get file size using lseek
        const SEEK_END = 2;
        const SEEK_SET = 0;
        const end_pos = linux.lseek(self.fd, 0, SEEK_END);
        if (@as(isize, @bitCast(end_pos)) < 0) return error.StatFailed;
        _ = linux.lseek(self.fd, 0, SEEK_SET);
        return .{ .size = end_pos };
    }
};

fn openFileAbsolute(path: []const u8) !PosixFile {
    const path_z = try posix.toPosixPath(path);
    const fd = try posix.openZ(&path_z, .{ .ACCMODE = .RDONLY }, 0);
    return .{ .fd = fd };
}

/// TLS connection errors
pub const TlsError = error{
    HandshakeFailed,
    CertificateError,
    DecryptError,
    EncryptError,
    ConnectionClosed,
    BufferTooSmall,
    CaBundleLoadFailed,
    ClientCertLoadFailed,
    PrivateKeyLoadFailed,
};

/// Client certificate for mutual TLS authentication
pub const ClientCertificate = tls.config.CertKeyPair;

/// System CA certificate paths for different Linux distributions
const linux_ca_paths = [_][]const u8{
    "/etc/ssl/certs/ca-certificates.crt", // Debian/Ubuntu/Gentoo
    "/etc/pki/tls/certs/ca-bundle.crt", // Fedora/RHEL 6
    "/etc/ssl/ca-bundle.pem", // OpenSUSE
    "/etc/pki/tls/cacert.pem", // OpenELEC
    "/etc/pki/ca-trust/extracted/pem/tls-ca-bundle.pem", // CentOS/RHEL 7
    "/etc/ssl/cert.pem", // Alpine Linux
};

/// Cached system CA bundle (loaded once, reused)
var cached_ca_bundle: ?Certificate.Bundle = null;
var ca_bundle_mutex: std.Thread.Mutex = .{};
var ca_bundle_allocator: ?Allocator = null;

/// Deinitialize the cached CA bundle (for testing/cleanup)
/// This frees the memory used by the cached certificate bundle.
/// After calling this, the next call to loadSystemCaBundle will reload certificates.
pub fn deinitCaBundle() void {
    ca_bundle_mutex.lock();
    defer ca_bundle_mutex.unlock();

    if (cached_ca_bundle) |*bundle| {
        if (ca_bundle_allocator) |alloc| {
            bundle.map.deinit(alloc);
            bundle.bytes.deinit(alloc);
        }
        cached_ca_bundle = null;
        ca_bundle_allocator = null;
    }
}

/// Load system CA certificates into a bundle
pub fn loadSystemCaBundle(allocator: Allocator) !Certificate.Bundle {
    // Check cache first
    ca_bundle_mutex.lock();
    defer ca_bundle_mutex.unlock();

    if (cached_ca_bundle) |bundle| {
        return bundle;
    }

    // Try to load from system paths
    var bundle: Certificate.Bundle = .{};

    const now_sec = getTimestampSeconds();

    for (linux_ca_paths) |path| {
        if (loadCertsFromPath(allocator, &bundle, path, now_sec)) {
            cached_ca_bundle = bundle;
            ca_bundle_allocator = allocator;
            return bundle;
        } else |_| {
            continue;
        }
    }

    return error.CaBundleLoadFailed;
}

/// Load client certificate and private key for mutual TLS authentication
pub fn loadClientCertificate(
    allocator: Allocator,
    cert_path: []const u8,
    key_path: []const u8,
) !ClientCertificate {
    // Load certificate bundle
    var bundle: Certificate.Bundle = .{};
    errdefer {
        bundle.map.deinit(allocator);
        bundle.bytes.deinit(allocator);
    }

    const now_sec = getTimestampSeconds();
    loadCertsFromPath(allocator, &bundle, cert_path, now_sec) catch
        return error.ClientCertLoadFailed;

    // Load private key
    const key_file = openFileAbsolute(key_path) catch
        return error.PrivateKeyLoadFailed;
    defer key_file.close();

    const stat = key_file.stat() catch return error.PrivateKeyLoadFailed;
    if (stat.size > 1024 * 1024) { // 1MB limit for key file
        return error.PrivateKeyLoadFailed;
    }

    const key_data = allocator.alloc(u8, stat.size) catch return error.PrivateKeyLoadFailed;
    defer allocator.free(key_data);

    var total_read: usize = 0;
    while (total_read < stat.size) {
        const bytes_read = key_file.read(key_data[total_read..]) catch return error.PrivateKeyLoadFailed;
        if (bytes_read == 0) break;
        total_read += bytes_read;
    }

    const key = tls.config.PrivateKey.parsePem(key_data[0..total_read]) catch
        return error.PrivateKeyLoadFailed;

    return .{
        .bundle = bundle,
        .key = key,
        .ecdsa_key_pair = null, // Will be computed lazily if needed
    };
}

/// Free a client certificate
pub fn deinitClientCertificate(cert: *ClientCertificate, allocator: Allocator) void {
    cert.bundle.map.deinit(allocator);
    cert.bundle.bytes.deinit(allocator);
}

/// Load certificates from a PEM file path
fn loadCertsFromPath(
    allocator: Allocator,
    bundle: *Certificate.Bundle,
    path: []const u8,
    now_sec: i64,
) !void {
    const file = openFileAbsolute(path) catch return error.CaBundleLoadFailed;
    defer file.close();

    // Get file size
    const stat = file.stat() catch return error.CaBundleLoadFailed;
    const size = stat.size;

    if (size > 10 * 1024 * 1024) { // 10MB limit
        return error.CaBundleLoadFailed;
    }

    // Read entire file
    const file_data = allocator.alloc(u8, size) catch return error.CaBundleLoadFailed;
    defer allocator.free(file_data);

    var total_read: usize = 0;
    while (total_read < size) {
        const bytes_read = file.read(file_data[total_read..]) catch return error.CaBundleLoadFailed;
        if (bytes_read == 0) break;
        total_read += bytes_read;
    }
    const pem_data = file_data[0..total_read];

    // Parse PEM certificates
    try parsePemCertificates(allocator, bundle, pem_data, now_sec);
}

/// Parse PEM-encoded certificates and add to bundle
fn parsePemCertificates(
    allocator: Allocator,
    bundle: *Certificate.Bundle,
    pem_data: []const u8,
    now_sec: i64,
) !void {
    const begin_marker = "-----BEGIN CERTIFICATE-----";
    const end_marker = "-----END CERTIFICATE-----";

    var start_index: usize = 0;
    while (std.mem.indexOfPos(u8, pem_data, start_index, begin_marker)) |begin_marker_start| {
        const cert_start = begin_marker_start + begin_marker.len;
        const cert_end = std.mem.indexOfPos(u8, pem_data, cert_start, end_marker) orelse break;
        start_index = cert_end + end_marker.len;

        // Extract and decode base64 certificate
        const encoded_cert = std.mem.trim(u8, pem_data[cert_start..cert_end], " \t\r\n");

        // Calculate max decoded size (upper bound since we ignore whitespace)
        const decoded_size_upper = base64_decoder.calcSizeUpperBound(encoded_cert.len);

        // Allocate space in bundle
        const decoded_start: u32 = @intCast(bundle.bytes.items.len);
        bundle.bytes.ensureUnusedCapacity(allocator, decoded_size_upper) catch continue;

        const dest_buf = bundle.bytes.allocatedSlice()[decoded_start..];
        const actual_decoded_len = base64_decoder.decode(dest_buf, encoded_cert) catch continue;
        bundle.bytes.items.len += actual_decoded_len;

        // Parse and add certificate
        parseCertInternal(bundle, allocator, decoded_start, now_sec) catch {
            // Revert if parsing fails
            bundle.bytes.items.len = decoded_start;
            continue;
        };
    }
}

/// Parse a single DER-encoded certificate and add to bundle
fn parseCertInternal(
    bundle: *Certificate.Bundle,
    allocator: Allocator,
    decoded_start: u32,
    now_sec: i64,
) !void {
    const parsed_cert = Certificate.parse(.{
        .buffer = bundle.bytes.items,
        .index = decoded_start,
    }) catch |err| switch (err) {
        error.CertificateHasUnrecognizedObjectId => return error.CertificateError,
        else => return error.CertificateError,
    };

    // Skip expired certificates
    if (now_sec > parsed_cert.validity.not_after) {
        return error.CertificateError;
    }

    // Add to map
    const gop = bundle.map.getOrPutContext(allocator, parsed_cert.subject_slice, .{ .cb = bundle }) catch
        return error.CertificateError;

    if (gop.found_existing) {
        // Duplicate - revert
        return error.CertificateError;
    } else {
        gop.value_ptr.* = decoded_start;
    }
}

/// Get current timestamp in seconds
fn getTimestampSeconds() i64 {
    const ts = posix.clock_gettime(.REALTIME) catch return 0;
    return ts.sec;
}

/// A TLS-wrapped connection
pub const TlsConnection = struct {
    socket: posix.socket_t,
    conn: tls.nonblock.Connection,
    // Buffers for TLS records
    recv_buf: []u8,
    send_buf: []u8,
    // Decrypted data buffer
    cleartext_buf: []u8,
    cleartext_start: usize,
    cleartext_end: usize,
    allocator: Allocator,
    closed: bool,

    const BUFFER_SIZE = tls.input_buffer_len; // ~16KB for TLS records

    /// TLS connection options
    pub const Options = struct {
        insecure: bool = false,
        client_cert: ?*ClientCertificate = null,
    };

    pub fn init(allocator: Allocator, socket: posix.socket_t, host: []const u8, options: Options) !TlsConnection {
        // Allocate buffers
        const recv_buf = try allocator.alloc(u8, BUFFER_SIZE);
        errdefer allocator.free(recv_buf);

        const send_buf = try allocator.alloc(u8, tls.output_buffer_len);
        errdefer allocator.free(send_buf);

        const cleartext_buf = try allocator.alloc(u8, BUFFER_SIZE);
        errdefer allocator.free(cleartext_buf);

        // Perform TLS handshake
        const cipher = try doHandshake(allocator, socket, recv_buf, send_buf, host, options);

        return .{
            .socket = socket,
            .conn = tls.nonblock.Connection.init(cipher),
            .recv_buf = recv_buf,
            .send_buf = send_buf,
            .cleartext_buf = cleartext_buf,
            .cleartext_start = 0,
            .cleartext_end = 0,
            .allocator = allocator,
            .closed = false,
        };
    }

    pub fn deinit(self: *TlsConnection) void {
        // Send close_notify if not already closed
        if (!self.closed) {
            if (self.conn.close(self.send_buf)) |close_data| {
                _ = posix.write(self.socket, close_data) catch {};
            } else |_| {}
        }
        self.allocator.free(self.recv_buf);
        self.allocator.free(self.send_buf);
        self.allocator.free(self.cleartext_buf);
    }

    /// Write data through TLS connection
    pub fn write(self: *TlsConnection, data: []const u8) !usize {
        if (self.closed) return error.ConnectionClosed;

        const result = self.conn.encrypt(data, self.send_buf) catch
            return error.EncryptError;

        if (result.ciphertext.len > 0) {
            _ = posix.write(self.socket, result.ciphertext) catch
                return error.EncryptError;
        }

        return result.cleartext_pos;
    }

    /// Write all data through TLS connection
    pub fn writeAll(self: *TlsConnection, data: []const u8) !void {
        var remaining = data;
        while (remaining.len > 0) {
            const written = try self.write(remaining);
            remaining = remaining[written..];
        }
    }

    /// Read data from TLS connection
    pub fn read(self: *TlsConnection, buffer: []u8) !usize {
        if (self.closed) return 0;

        // Return any buffered cleartext first
        if (self.cleartext_end > self.cleartext_start) {
            const available = self.cleartext_end - self.cleartext_start;
            const to_copy = @min(available, buffer.len);
            @memcpy(buffer[0..to_copy], self.cleartext_buf[self.cleartext_start..][0..to_copy]);
            self.cleartext_start += to_copy;
            if (self.cleartext_start == self.cleartext_end) {
                self.cleartext_start = 0;
                self.cleartext_end = 0;
            }
            return to_copy;
        }

        // Read more ciphertext from socket
        const n = posix.read(self.socket, self.recv_buf) catch |err| {
            if (err == error.WouldBlock) return 0;
            return error.DecryptError;
        };

        if (n == 0) {
            self.closed = true;
            return 0;
        }

        // Decrypt
        const result = self.conn.decrypt(self.recv_buf[0..n], self.cleartext_buf) catch
            return error.DecryptError;

        if (result.closed) {
            self.closed = true;
        }

        if (result.cleartext.len == 0) {
            return 0;
        }

        // Copy to user buffer
        const to_copy = @min(result.cleartext.len, buffer.len);
        @memcpy(buffer[0..to_copy], result.cleartext[0..to_copy]);

        // Buffer any remainder
        if (result.cleartext.len > to_copy) {
            const remainder = result.cleartext.len - to_copy;
            @memcpy(self.cleartext_buf[0..remainder], result.cleartext[to_copy..]);
            self.cleartext_start = 0;
            self.cleartext_end = remainder;
        }

        return to_copy;
    }
};

/// Perform TLS handshake on socket
fn doHandshake(
    allocator: Allocator,
    socket: posix.socket_t,
    recv_buf: []u8,
    send_buf: []u8,
    host: []const u8,
    options: TlsConnection.Options,
) !tls.Cipher {
    // Get current timestamp
    const now = getTimestamp();

    // Load CA bundle for certificate verification (unless insecure mode)
    const root_ca: Certificate.Bundle = if (options.insecure)
        .{}
    else
        loadSystemCaBundle(allocator) catch .{};

    // Initialize handshake
    var hs = tls.nonblock.Client.init(.{
        .host = host,
        .root_ca = root_ca,
        .insecure_skip_verify = options.insecure,
        .now = now,
        .auth = options.client_cert,
    });

    var recv_pos: usize = 0;

    while (!hs.done()) {
        // Run handshake step
        const result = hs.run(recv_buf[0..recv_pos], send_buf) catch
            return error.HandshakeFailed;

        // Send any handshake data
        if (result.send.len > 0) {
            _ = posix.write(socket, result.send) catch
                return error.HandshakeFailed;
        }

        // Shift consumed data
        if (result.recv_pos > 0 and result.recv_pos < recv_pos) {
            std.mem.copyForwards(u8, recv_buf, recv_buf[result.recv_pos..recv_pos]);
            recv_pos -= result.recv_pos;
        } else if (result.recv_pos == recv_pos) {
            recv_pos = 0;
        }

        if (hs.done()) break;

        // Read more data from server
        const n = posix.read(socket, recv_buf[recv_pos..]) catch
            return error.HandshakeFailed;

        if (n == 0) return error.HandshakeFailed;
        recv_pos += n;
    }

    return hs.cipher() orelse error.HandshakeFailed;
}

/// Get current timestamp for certificate validation
fn getTimestamp() std.Io.Timestamp {
    const ts = posix.clock_gettime(.REALTIME) catch return .zero;
    // Convert to nanoseconds (i96)
    const sec_ns: i96 = @as(i96, ts.sec) * 1_000_000_000;
    const nsec: i96 = @intCast(ts.nsec);
    return .{ .nanoseconds = sec_ns + nsec };
}

test "tls module compiles" {
    // Basic compilation test
    _ = TlsConnection;
    _ = TlsError;
}
