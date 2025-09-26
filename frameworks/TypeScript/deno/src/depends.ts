export type { Response } from "https://deno.land/std@0.96.0/http/server.ts";

export {
  ServerRequest,
  serve,
} from "https://deno.land/std@0.96.0/http/server.ts";

export { MongoClient, Bson } from "https://deno.land/x/mongo@v0.22.0/mod.ts";

export const SERVER: string = "Deno";

let date = new Date().toUTCString();
setInterval(() => (date = new Date().toUTCString()), 850);
export const dyn_date = () => date;

export const MIME_JSON = "application/json";
export const MIME_HTML = "text/html; charset=utf-8";

export const HELLO_WORLD: Uint8Array = new TextEncoder().encode(
  "Hello, World!"
);

export const MIME_TEXT = "text/plain; charset=UTF-8";
