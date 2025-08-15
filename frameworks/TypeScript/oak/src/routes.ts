import { Router } from "oak";
import { LruCache } from "lru-cache";
import { NotFound, Ok } from "./helpers.ts";
import type { Fortune, World } from "./types.ts";
import {
  parseQuery,
  randomNumber,
  renderTemplate,
} from "./utils.ts";
import { 
  getWorld, 
  bulkUpdate, 
  allFortunes, 
  getAllWorlds 
} from "./postgres.ts";

export const router = new Router()
  .get("/plaintext", (ctx) => Ok(ctx, "Hello, World!"))
  .get("/json", (ctx) => Ok(ctx, { message: "Hello, World!" }));

if (Deno.env.has("DATABASE")) {
  const cache = new LruCache<number, World>(10_000);
  (await getAllWorlds()).forEach(world => cache.set(world.id, world));
  
  router.get("/db", async (ctx) => {
    const world = await getWorld(randomNumber());
    Ok(ctx, world);
  })
  .get("/queries", async (ctx) => {
    const worldsPromises = [];
    const queries = parseQuery(ctx);
    for (let i = 0; i < queries; i++) {
      worldsPromises.push(getWorld(randomNumber()));
    }

    const worlds = await Promise.all(worldsPromises);
    Ok(ctx, worlds);
  })
  .get("/updates", async (ctx) => {
    const worldsPromises: Promise<World>[] = [];
    const queries = parseQuery(ctx);

    for (let i = 0; i < queries; i++) {
      const world = getWorld(randomNumber());
      worldsPromises.push(world);
    }

    const worlds = await Promise.all(worldsPromises);

    const newWorlds = worlds.map((world) => {
      world.randomnumber = randomNumber();
      return world;
    });

    await bulkUpdate(newWorlds);

    Ok(ctx, newWorlds);
  })
  .get("/fortunes", async (ctx) => {
    const fortunes = await allFortunes();
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
      worlds.push(cache.get(randomNumber()));
    }
    Ok(ctx, worlds);
  })
}

router.get("/(.*)", (ctx) => NotFound(ctx));
