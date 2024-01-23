import { Application, DatabaseResult, Manager } from "./deps.ts";
import { router } from "./routes.ts";
import { getDbClient } from "./utils.ts";

const app = new Application<
  { manager: Manager; cached_worlds: DatabaseResult[] }
>();

// headers
app.use(async (ctx, next) => {
  ctx.response.headers.set("Date", new Date().toUTCString());
  ctx.response.headers.set("Server", "Oak");
  await next();
});

// database handling
app.use(async (ctx, next) => {
  const db = await getDbClient();
  ctx.state.manager = db.getManager();
  await next();
  await db.disconnect();
});

app.use(router.routes());
app.use(router.allowedMethods());

await app.listen({ port: 8080 });
