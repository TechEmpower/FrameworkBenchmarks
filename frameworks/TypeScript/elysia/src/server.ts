import { Elysia } from "elysia";
import { dbHandlers } from "./db-handlers";

const app = new Elysia()
  .headers({
    server: "Elysia",
  })
  .get("/plaintext", "Hello, World!")
  // As state on xiv in https://github.com/TechEmpower/FrameworkBenchmarks/wiki/Project-Information-Framework-Tests-Overview#requirements
  // The serialization to JSON must not be cached;
  // the computational effort to serialize an object to JSON must occur within the scope of handling each request.
  .get("/json", () => ({ message: "Hello, World!" }))
  .use((app) => {
    if (Bun.env.DATABASE) app.use(dbHandlers);

    return app;
  })
  .listen(8080);

console.info(`🦊 Elysia is running at ${app.server!.url}`);
