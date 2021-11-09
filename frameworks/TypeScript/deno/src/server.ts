import { Handler } from "./handlers.ts";

export async function runServer(
  handlers: { [k: string]: Handler },
  config: { port: number; hostname: string },
) {
  const listener = Deno.listen(config);
  while (true) {
    try {
      const conn = await listener.accept();
      const httpConn = Deno.serveHttp(conn);
      setTimeout(() => handleHttp(httpConn), 0);
    } catch (err) {
      console.log("connection failed for reasons:", err);
    }
  }
  async function handleHttp(httpConn: Deno.HttpConn) {
    while (true) {
      try {
        const event = await httpConn.nextRequest();
        if (event) setTimeout(() => handleRequestEvent(event), 0);
      } catch (err) {
        // the connection has finished
        break;
      }
    }
  }

  async function handleRequestEvent(event: Deno.RequestEvent) {
    const req = event.request;
    const url = new URL(req.url)
    const handler = handlers[url.pathname];
    if (!handler) {
      event.respondWith(new Response("not found", { status: 404 }));
      return
    }

    try {
      const response = await handler(event.request);
      event.respondWith(response);
    } catch (e) {
      event.respondWith(new Response("server error", { status: 500 }));
    }
  }
}
