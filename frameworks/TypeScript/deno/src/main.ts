const HELLO_WORLD_STR = "Hello, World!";
const options: ResponseInit = { headers: { "Server": "Deno" } };

export default {
  fetch: (req: Request) => {
    const path = req.url.slice(req.url.indexOf("/", 8));
    if (path == "/plaintext") {
      return new Response(HELLO_WORLD_STR, options);
    } else if (path == "/json") {
      return Response.json({ message: HELLO_WORLD_STR }, options);
    } else {
      return new Response("404 Not Found", { status: 404, ...options });
    }
  },
};
