import { Application } from "oak";
import { router } from "./routes.ts";

const app = new Application();

// headers
app.use(async (ctx, next) => {
  ctx.response.headers.set("Date", new Date().toUTCString());
  ctx.response.headers.set("Server", "Oak");
  await next();
});

app.use(router.routes());
app.use(router.allowedMethods());

export default { fetch: app.fetch };
