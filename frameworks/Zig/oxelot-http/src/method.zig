/// HTTP methods
pub const Method = enum {
    GET,
    POST,
    PUT,
    DELETE,
    PATCH,
    HEAD,
    OPTIONS,
    CONNECT,
    TRACE,

    pub fn fromString(str: []const u8) ?Method {
        if (str.len < 3 or str.len > 7) return null;

        return switch (str.len) {
            3 => switch (str[0]) {
                'G' => if (eql(str, "GET")) .GET else null,
                'P' => if (eql(str, "PUT")) .PUT else null,
                else => null,
            },
            4 => switch (str[0]) {
                'P' => if (eql(str, "POST")) .POST else null,
                'H' => if (eql(str, "HEAD")) .HEAD else null,
                else => null,
            },
            5 => switch (str[0]) {
                'P' => if (eql(str, "PATCH")) .PATCH else null,
                'T' => if (eql(str, "TRACE")) .TRACE else null,
                else => null,
            },
            6 => if (eql(str, "DELETE")) .DELETE else null,
            7 => switch (str[0]) {
                'O' => if (eql(str, "OPTIONS")) .OPTIONS else null,
                'C' => if (eql(str, "CONNECT")) .CONNECT else null,
                else => null,
            },
            else => null,
        };
    }

    pub fn toString(self: Method) []const u8 {
        return switch (self) {
            .GET => "GET",
            .POST => "POST",
            .PUT => "PUT",
            .DELETE => "DELETE",
            .PATCH => "PATCH",
            .HEAD => "HEAD",
            .OPTIONS => "OPTIONS",
            .CONNECT => "CONNECT",
            .TRACE => "TRACE",
        };
    }

    fn eql(a: []const u8, b: []const u8) bool {
        if (a.len != b.len) return false;
        for (a, b) |c1, c2| {
            if (c1 != c2) return false;
        }
        return true;
    }
};

test "method parsing" {
    const std = @import("std");
    try std.testing.expectEqual(Method.GET, Method.fromString("GET"));
    try std.testing.expectEqual(Method.POST, Method.fromString("POST"));
    try std.testing.expectEqual(Method.DELETE, Method.fromString("DELETE"));
    try std.testing.expectEqual(@as(?Method, null), Method.fromString("INVALID"));
}
