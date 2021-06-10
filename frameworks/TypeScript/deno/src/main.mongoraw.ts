import { serve } from "https://deno.land/std@0.96.0/http/server.ts";
import DefaultHandlers from "./handlers.ts";
import { MongoRawHandlers } from "./_handlers/mongodb-raw/handlers.ts";
const handlers = {
  ...DefaultHandlers,
  ...MongoRawHandlers,
};

for await (const req of serve("0.0.0.0:8080")) {
  const url = new URL(req.url, "http://deno");
  if (handlers[url.pathname] != undefined) {
    handlers[url.pathname](req as any).catch((e) => {
      console.error(e);
      Deno.exit(9);
    });
  } else {
    req.respond({
      body: "404 Not Found",
    });
  }
  continue;
}
