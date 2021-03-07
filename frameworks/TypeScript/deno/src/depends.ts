export type {
  Response,
} from "https://deno.land/std@0.87.0/http/server.ts";

export { 
  ServerRequest,
  serve
} from "https://deno.land/std@0.87.0/http/server.ts";

export { 
  MongoClient, 
  Bson 
} from "https://deno.land/x/mongo@v0.21.0/mod.ts";

export const SERVER: string = "Deno";

export let dyn_date: string = new Date().toUTCString();
setInterval(() => { dyn_date = new Date().toUTCString(); }, 1000);

export const MIME_JSON = "application/json";

export const HELLO_WORLD: Uint8Array = new TextEncoder().encode("Hello, World!");

export const MIME_TEXT = "text/plain; charset=UTF-8"; 