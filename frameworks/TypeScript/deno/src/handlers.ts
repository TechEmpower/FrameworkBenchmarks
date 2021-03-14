import type { ServerRequest } from "./depends.ts";
import JSONHandler from "./_handlers/json.ts";
import PlaintextHandler from "./_handlers/plaintext.ts";

export interface Handler {
  (request: ServerRequest): Promise<void>;
}

export interface Handlers {
  [index: string]: Handler;
}

export default {
  "/json": JSONHandler,
  "/plaintext": PlaintextHandler,
} as Handlers;

