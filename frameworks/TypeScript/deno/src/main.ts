import { serve } from "https://deno.land/std@0.87.0/http/server.ts";
import { handlers } from "./handlers.ts";
for await (const req of serve("0.0.0.0:8080")) {
  if (handlers[req.url] != undefined) {
    handlers[req.url](req).catch((e) => {
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
