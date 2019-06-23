import { Application, Router } from "https://deno.land/x/oak/mod.ts";

(async () => {
  const router = new Router();

  router
    .get("/json", context => {
      context.response.headers.set("Server", "Oak");
      context.response.headers.set("Date", new Date().toUTCString());
      context.response.body = { message: "Hello, World!" };
    })
    .get("/plaintext", context => {
      context.response.headers.set("Server", "Oak");
      context.response.headers.set("Date", new Date().toUTCString());
      context.response.body = "Hello, World!";
    });

  const app = new Application();
  app.use(router.routes());
  app.use(router.allowedMethods());

  await app.listen("0.0.0.0:8000");
})();
