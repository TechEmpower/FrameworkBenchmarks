import JSONHandler from "./_handlers/json.ts";
import PlaintextHandler from "./_handlers/plaintext.ts";

export interface Handler {
  (request: Request): Promise<Response>;
}

export interface Handlers {
  [index: string]: Handler;
}

export default {
  "/json": JSONHandler,
  "/plaintext": PlaintextHandler,
} as Handlers;
