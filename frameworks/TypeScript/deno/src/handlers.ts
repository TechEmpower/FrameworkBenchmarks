import type { ServerRequest } from "./depends.ts";
import JSONHandler from "./_handlers/json.ts";
import PlaintextHandler from "./_handlers/plaintext.ts";

interface Handler {
  (request: ServerRequest): Promise<void>;
}

interface Handlers {
  [index: string]: Handler;
}

export const handlers: Handlers = {
  "/json": JSONHandler,
  "/plaintext": PlaintextHandler,
};
