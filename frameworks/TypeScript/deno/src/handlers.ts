import { ServerRequest } from "https://deno.land/std@v0.41.0/http/server.ts";

interface IRequestHandler {
  (request: ServerRequest): void
}

const HELLO_WORLD = new TextEncoder().encode("Hello, World!");

const json: IRequestHandler = (req: ServerRequest) => {
  const headers = new Headers();
  headers.set("server", "Deno");
  headers.set("content-type", "application/json");
  headers.set("date", (new Date()).toUTCString());

  req.respond({
    headers,
    body: JSON.stringify({ message: "Hello, World!" })
  });
};

const plaintext: IRequestHandler = (req: ServerRequest) => {
  const headers = new Headers();
  headers.set("server", "Deno");
  headers.set("content-type", "text/plain; charset=UTF-8");
  headers.set("date", (new Date()).toUTCString());

  req.respond({
    headers,
    body: HELLO_WORLD
  });
};

const handlers: { [Key: string]: IRequestHandler } = {};

handlers["/json"] = json;
handlers["/plaintext"] = plaintext;

export {
  handlers
}
