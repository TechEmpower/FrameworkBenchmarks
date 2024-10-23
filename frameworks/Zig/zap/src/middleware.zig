const std = @import("std");
const zap = @import("zap");
const pg = @import("pg");

// just a way to share our allocator via callback
pub const SharedAllocator = struct {
    // static
    var allocator: std.mem.Allocator = undefined;

    const Self = @This();

    // just a convenience function
    pub fn init(a: std.mem.Allocator) void {
        allocator = a;
    }

    // static function we can pass to the listener later
    pub fn getAllocator() std.mem.Allocator {
        return allocator;
    }
};

// create a combined context struct
pub const Context = struct {
    prng: ?RandomMiddleWare.Prng = null,
    pg: ?PgMiddleWare.Pg = null,
};

pub const Handler = zap.Middleware.Handler(Context);

pub const HeaderMiddleWare = struct {
    handler: Handler,

    const Self = @This();

    pub fn init(other: ?*Handler) Self {
        return .{
            .handler = Handler.init(onRequest, other),
        };
    }

    // we need the handler as a common interface to chain stuff
    pub fn getHandler(self: *Self) *Handler {
        return &self.handler;
    }

    // note that the first parameter is of type *Handler, not *Self !!!
    pub fn onRequest(handler: *Handler, req: zap.Request, context: *Context) bool {
        // this is how we would get our self pointer
        const self: *Self = @fieldParentPtr("handler", handler);
        _ = self;

        req.setHeader("Server", "Zap") catch return false;

        // continue in the chain
        return handler.handleOther(req, context);
    }
};

pub const RandomMiddleWare = struct {
    handler: Handler,
    rnd: *std.rand.DefaultPrng,

    const Self = @This();

    const Prng = struct {
        rnd: *std.rand.DefaultPrng = undefined,
    };

    pub fn init(other: ?*Handler, rnd: *std.rand.DefaultPrng) Self {
        return .{
            .handler = Handler.init(onRequest, other),
            .rnd = rnd,
        };
    }

    // we need the handler as a common interface to chain stuff
    pub fn getHandler(self: *Self) *Handler {
        return &self.handler;
    }

    // note that the first parameter is of type *Handler, not *Self !!!
    pub fn onRequest(handler: *Handler, req: zap.Request, context: *Context) bool {

        // this is how we would get our self pointer
        const self: *RandomMiddleWare = @fieldParentPtr("handler", handler);

        context.prng = Prng{ .rnd = self.rnd };

        // continue in the chain
        return handler.handleOther(req, context);
    }
};

pub const PgMiddleWare = struct {
    handler: Handler,
    pool: *pg.Pool,

    const Self = @This();

    const Pg = struct {
        pool: *pg.Pool = undefined,
    };

    pub fn init(other: ?*Handler, pool: *pg.Pool) Self {
        return .{
            .handler = Handler.init(onRequest, other),
            .pool = pool,
        };
    }

    // we need the handler as a common interface to chain stuff
    pub fn getHandler(self: *Self) *Handler {
        return &self.handler;
    }

    // note that the first parameter is of type *Handler, not *Self !!!
    pub fn onRequest(handler: *Handler, req: zap.Request, context: *Context) bool {

        // this is how we would get our self pointer
        const self: *Self = @fieldParentPtr("handler", handler);

        // do our work: fill in the user field of the context
        context.pg = Pg{ .pool = self.pool };

        // continue in the chain
        return handler.handleOther(req, context);
    }
};
