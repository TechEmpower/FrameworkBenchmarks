import DefaultHandlers from "./handlers.ts";
import { runServer } from "./server.ts";
import { MongoRawHandlers } from "./_handlers/mongodb-raw/handlers.ts";
const handlers = {
  ...DefaultHandlers,
  ...MongoRawHandlers,
};

runServer(handlers, { port: 8080, hostname: "0.0.0.0" });
