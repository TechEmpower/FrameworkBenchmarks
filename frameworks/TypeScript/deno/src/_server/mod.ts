// deno-lint-ignore-file require-await
import { Server, ConnInfo } from 'https://deno.land/std@0.119.0/http/server.ts'
// import { pathToRegexp } from 'https://deno.land/x/path_to_regexp@v6.2.0/index.ts'
import { parse, Url } from './_url.ts'

const ERROR_TEXT = null

export interface Handler {
  (req: Request, url:Url, conn: ConnInfo): Promise<Response>
}

export class HttpServer extends Server {
  #routes = new Map<string, Handler>();

  constructor() {
    super({
      handler: (req, conn) => this.#routeHandler(req, conn),
      onError: (err) => this.#errHandler(err),
    });
  }

  async #errHandler(_: unknown): Promise<Response> {
    return new Response(ERROR_TEXT, {
      status: 500
    });
  }

  async #routeHandler(req: Request, conn: ConnInfo): Promise<Response> {
    const url = parse(req.url, true);
    const h = this.#routes.get(url.pathname! ?? "/")
    // for (const [pat, handler] of this.#routes) {
    //   if (pat.exec(url.path ?? "/")) return await handler(req, url, conn);
    // }
    if (h) return await h(req, url, conn);
    return new Response(ERROR_TEXT, {
      status: 404
    });
  }

  add(path: string, handler: Handler): this {
    this.#routes.set((path), handler);
    return this;
  }
}
