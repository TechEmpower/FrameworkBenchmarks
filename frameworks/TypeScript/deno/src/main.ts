import { serve } from "https://deno.land/std@0.96.0/http/server.ts";
import Handlers from "./handlers.ts";
for await (const req of serve("0.0.0.0:8080")) {
  if (Handlers[req.url] != undefined) {
    Handlers[req.url](req as any).catch((e) => {
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
