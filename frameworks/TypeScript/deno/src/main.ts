const options = {
  // Date and Content-Type headers are automatically set.
  headers: {
    "Server": "Deno",
  },
};

type HandlerFn = (req: Request) => Promise<Response> | Response;

const handlers: Record<string, HandlerFn> = {
  "/json": () => Response.json({ message: "Hello, World!" }, options),
  "/plaintext": () => new Response("Hello, World!", options),
};

Deno.serve({
  handler: (req: Request) => {
    const path = req.url.slice(req.url.indexOf("/", 8));
    const fn = handlers[path];
    return fn
      ? fn(req)
      : new Response("404 Not Found", { status: 404, ...options });
  },
  onError(err) {
    console.error(err);
    Deno.exit(9);
  },
  port: 8080,
  hostname: "0.0.0.0",
});
