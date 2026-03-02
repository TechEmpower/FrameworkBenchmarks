// Middleware system for oxelot-http
//
// Provides composable middleware with support for:
// - Global middleware (runs on all routes)
// - Per-route middleware (via chain builder)
// - Pre and post-handler processing

const std = @import("std");

// Forward declare Context - will be the same type from router.zig
// We use an opaque pointer to avoid circular imports
pub const Context = opaque {};

/// Handler function signature (matches router.Handler)
pub const Handler = *const fn (*Context) anyerror!void;

/// Middleware function signature
/// Takes context and a "next" function to call for continuing the chain
/// The next function wraps chain.execute(ctx, next_index)
pub const Middleware = *const fn (*Context, Next) anyerror!void;

/// Next continuation - wraps the chain execution
/// This is a simple struct with a function pointer and context
pub const Next = struct {
    ptr: *const anyopaque,
    call_fn: *const fn (*const anyopaque, *Context) anyerror!void,

    /// Call the next middleware/handler in the chain
    pub fn call(self: Next, ctx: *Context) anyerror!void {
        return self.call_fn(self.ptr, ctx);
    }
};

/// Chain of middleware + final handler
pub const MiddlewareChain = struct {
    middlewares: []const Middleware,
    handler: Handler,

    /// Execute middleware chain starting at given index
    pub fn execute(self: *const MiddlewareChain, ctx: *Context, index: usize) anyerror!void {
        if (index < self.middlewares.len) {
            // Create next continuation that will execute from index+1
            const next = Next{
                .ptr = @ptrCast(&ExecuteContext{ .chain = self, .next_index = index + 1 }),
                .call_fn = executeNext,
            };
            return self.middlewares[index](ctx, next);
        } else {
            // End of chain - call the actual handler
            return self.handler(ctx);
        }
    }

    /// Start execution from the beginning of the chain
    pub fn run(self: *const MiddlewareChain, ctx: *Context) anyerror!void {
        return self.execute(ctx, 0);
    }
};

/// Helper struct for capturing chain execution context
const ExecuteContext = struct {
    chain: *const MiddlewareChain,
    next_index: usize,
};

/// Function that executes the next step in the chain
fn executeNext(ptr: *const anyopaque, ctx: *Context) anyerror!void {
    const exec_ctx: *const ExecuteContext = @ptrCast(@alignCast(ptr));
    return exec_ctx.chain.execute(ctx, exec_ctx.next_index);
}

// Note: Tests for middleware chain are in router.zig where we have access
// to the real Context type. The opaque Context type prevents standalone
// testing here.
