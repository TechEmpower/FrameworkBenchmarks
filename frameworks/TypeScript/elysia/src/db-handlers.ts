import { Elysia, t } from "elysia";
import * as db from "./postgres";
import { Fortune } from "./types";

function rand() {
  return Math.ceil(Math.random() * 10000);
}

function parseQueriesNumber(q?: string) {
  return Math.min(parseInt(q || "1") || 1, 500);
}

function renderTemplate(fortunes: Fortune[]) {
  const n = fortunes.length;

  let html = "";
  for (let i = 0; i < n; i++) {
    html += `<tr><td>${fortunes[i].id}</td><td>${Bun.escapeHTML(
      fortunes[i].message,
    )}</td></tr>`;
  }

  return `<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>${html}</table></body></html>`;
}

export const dbHandlers = new Elysia()
  .headers({
    server: "Elysia",
  })
  .get("/db", () => db.find(rand()))
  .get("/fortunes", async (c) => {
    const fortunes = await db.fortunes();

    fortunes.push({
      id: 0,
      message: "Additional fortune added at request time.",
    });

    fortunes.sort((a, b) => {
      if (a.message < b.message) return -1;

      return 1;
    });

    c.set.headers["content-type"] = "text/html; charset=utf-8";

    return renderTemplate(fortunes);
  })
  .get("/queries", (c) => {
    const num = parseQueriesNumber(c.query.queries);
    const worldPromises = new Array(num);

    for (let i = 0; i < num; i++) {
      worldPromises[i] = db.find(rand());
    }

    return Promise.all(worldPromises);
  })
  .get("/updates", async (c) => {
    const num = parseQueriesNumber(c.query.queries);
    const worldPromises = new Array(num);

    for (let i = 0; i < num; i++) {
      worldPromises[i] = db.find(rand());
    }

    const worlds = await Promise.all(worldPromises);

    for (let i = 0; i < num; i++) {
      worlds[i].randomNumber = rand();
    }

    await db.bulkUpdate(worlds);
    return worlds;
  });
