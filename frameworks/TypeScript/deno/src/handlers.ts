import type {
  ServerRequest,
} from "./depends.ts";

interface Handler {
  (request: ServerRequest): Promise<void>;
}

interface Handlers {
  [index: string]: Handler
}

export const handlers: Handlers = {
  "/json": (await import("./_handlers/json.ts")).default as Handler,
  "/plaintext": (await import("./_handlers/plaintext.ts")).default as Handler
} ;
