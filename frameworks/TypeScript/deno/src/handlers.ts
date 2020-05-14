import {
  ServerRequest,
  Response
} from "https://deno.land/std@v0.50.0/http/server.ts";

interface IRequestHandler {
  (request: ServerRequest): Promise<void>
}

const SERVER: string = "Deno";
const HELLO_WORLD: Uint8Array = new TextEncoder().encode("Hello, World!");

let date: string = new Date().toUTCString();
setInterval(() => { date = new Date().toUTCString(); }, 1000);

const json: IRequestHandler = async (req: ServerRequest): Promise<void> => {
  const headers = new Headers([
    ["server", SERVER],
    ["content-type", "application/json"],
    ["date", date]
  ]);

  req.respond({
    headers,
    body: JSON.stringify({ message: "Hello, World!" })
  } as Response);
};

const plaintext: IRequestHandler = async (req: ServerRequest): Promise<void> => {
  const headers = new Headers([
    ["server", SERVER],
    ["content-type", "text/plain; charset=UTF-8"],
    ["date", date]
  ]);

  req.respond({ headers, body: HELLO_WORLD } as Response);
};

const handlers: { [Key: string]: IRequestHandler } = {};

handlers["/json"] = json;
handlers["/plaintext"] = plaintext;

export {
  handlers
};
