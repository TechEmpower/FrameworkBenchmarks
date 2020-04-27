import { serve } from "https://deno.land/std@v0.41.0/http/server.ts";

import { handlers } from "./handlers.ts";

for await (const req of serve("0.0.0.0:8080")) {
  handlers[req.url](req);
}
