import { Fortune, World } from "./models.ts";
import { NotFound, Ok } from "./helpers.ts";
import {
  getDbClient,
  parseQuery,
  randomNumber,
  renderTemplate,
} from "./utils.ts";
import { Router } from "./deps.ts";

const cached_worlds = await (await getDbClient()).getManager().query(World)
  .limit(10000).all();

export const router = new Router()
  .get("/plaintext", (ctx) => Ok(ctx, "Hello, World!"))
  .get("/json", (ctx) => Ok(ctx, { message: "Hello, World!" }))
  .get("/db", async (ctx) => {
    const world = await ctx.state.manager.query(World).where(
      "id",
      randomNumber(),
    ).first();
    Ok(ctx, world);
  })
  .get("/queries", async (ctx) => {
    const worlds = [];
    const queries = parseQuery(ctx);
    for (let i = 0; i < queries; i++) {
      const world = await ctx.state.manager.query(World).where(
        "id",
        randomNumber(),
      ).first();
      worlds.push(world);
    }

    Ok(ctx, worlds);
  })
  .get("/updates", async (ctx) => {
    const worlds = [];
    const queries = parseQuery(ctx);

    for (let i = 0; i < queries; i++) {
      const world = await ctx.state.manager.query(World).where(
        "id",
        randomNumber(),
      ).first();
      world.randomnumber = randomNumber();
      worlds.push(world);

      await ctx.state.manager.save(world);
    }
    Ok(ctx, worlds);
  })
  .get("/fortunes", async (ctx) => {
    const fortunes: Fortune[] = await ctx.state.manager.query(Fortune).all();
    fortunes.push({
      id: 0,
      message: "Additional fortune added at request time.",
    });

    fortunes.sort((a: Fortune, b: Fortune) =>
      a.message.localeCompare(b.message)
    );

    ctx.response.headers.set("Content-Type", "text/html; charset=utf-8");
    Ok(ctx, renderTemplate(fortunes));
  })
  .get("/cached_queries", (ctx) => {
    const queries = parseQuery(ctx);
    const worlds = [];

    for (let i = 0; i < queries; i++) {
      worlds.push(cached_worlds[randomNumber()]);
    }
    Ok(ctx, worlds);
  })
  .get("/(.*)", (ctx) => NotFound(ctx));
