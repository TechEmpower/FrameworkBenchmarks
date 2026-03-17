const plainOptions: ResponseInit = { headers: { "Server": "x-gnosis" } };
const jsonOptions: ResponseInit = { headers: { "Server": "x-gnosis", "Content-Type": "application/json" } };

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

console.log(`x-gnosis listening on ${server.url}\n`);
