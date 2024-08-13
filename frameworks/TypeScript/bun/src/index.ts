const plainOptions: ResponseInit = { headers: { "Server": "Bun" } };
const jsonOptions: ResponseInit = { headers: { "Server": "Bun", "Content-Type": "application/json" } };

const server = Bun.serve({
  port: 8080,
  reusePort: true,
  fetch(req: Request) {
    const pathname = req.url.slice(req.url.indexOf("/", 8));

    if (pathname == "/json") {
      return new Response(JSON.stringify({ message: "Hello, World!" }), jsonOptions);
    }

    if (pathname == "/plaintext") {
      return new Response("Hello, World!", plainOptions);
    }

    return new Response("", { status: 404 })
  },
});

console.log(`Listening on ${server.url}\n`);
