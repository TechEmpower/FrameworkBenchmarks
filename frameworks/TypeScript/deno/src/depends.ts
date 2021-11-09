export { Bson, MongoClient } from "https://deno.land/x/mongo@v0.28.0/mod.ts";

export const SERVER = "Deno";

let date = new Date().toUTCString();
setInterval(() => (date = new Date().toUTCString()), 850);
export const dynDate = () => date;

export const MIME_JSON = "application/json";
export const MIME_HTML = "text/html; charset=utf-8";

export const HELLO_WORLD: Uint8Array = new TextEncoder().encode(
  "Hello, World!",
);

export const MIME_TEXT = "text/plain; charset=UTF-8";
