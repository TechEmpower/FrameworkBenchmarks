import handlers from "./handlers.ts";
import { runServer } from "./server.ts";

runServer(handlers, { port: 8080, hostname: "0.0.0.0" });
