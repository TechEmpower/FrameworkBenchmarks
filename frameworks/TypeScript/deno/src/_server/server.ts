import { Url, parse } from "./url.ts";

export interface HttpHandler {
  (conn: Deno.RequestEvent, url: Url): Promise<void>;
}

const errorText = new TextEncoder().encode("Server Error");
const errorHandle = async(ev: Deno.RequestEvent) => {
  await ev.respondWith(new Response(errorText, { status: 500 }))
}

export class HttpServer {
  #handlers: Map<string, HttpHandler> = new Map()
  #opts: Deno.ListenOptions

  constructor(opts: Deno.ListenOptions) {
    // this.#socket = Deno.listen(opts);
    this.#opts = opts
  }

  add(path: string, handler: HttpHandler): this {
    this.#handlers.set(path, handler);
    return this
  }

  async #handleHttpRequest(conn: Deno.HttpConn) {
    while(true) {
      try {
        const e = await conn.nextRequest()
        if (e) {
          const u = parse(e.request.url);
          const h = this.#handlers.get(u.pathname!);
          if (h) h(e, u)
          else errorHandle(e)
        }
      } catch {
        break;
      }
    }
  }

  async #pull(socket: Deno.Listener) {
    for await (const conn of socket) this.#handleHttpRequest(Deno.serveHttp(conn));
  }

  async listen() {
    await this.#pull(Deno.listen(this.#opts));
  }
}