import { serve } from "@hono/node-server";
import { Hono } from "hono";
import {
  addBenchmarkHeaders,
  escape,
  generateRandomNumber,
  getQueriesCount,
  handleError,
  sortByMessage,
} from "./utils.js";

let db;
const { DATABASE } = process.env;
if (DATABASE) db = await import(`./database/${DATABASE}.js`);

const app = new Hono();

app
  .get("/plaintext", (c) => {
    addBenchmarkHeaders(c);
    return c.text("Hello, World!");
  })
  .get("/json", (c) => {
    addBenchmarkHeaders(c);
    return c.json({ message: "Hello, World!" });
  });

if (db) {
  const extra = { id: 0, message: "Additional fortune added at request time." };

  app
    .get("/db", async (c) => {
      const randomNumber = await db.find(generateRandomNumber());
      addBenchmarkHeaders(c);
      return c.json(randomNumber);
    })
    .get("/queries", async (c) => {
      const queriesCount = getQueriesCount(c);

      const databaseJobs = new Array(queriesCount);

      for (let i = 0; i < queriesCount; i++) {
        databaseJobs[i] = db.find(generateRandomNumber());
      }

      const worldObjects = await Promise.all(databaseJobs);

      addBenchmarkHeaders(c);
      return c.json(worldObjects);
    })
    .get("/fortunes", async (c) => {
      const rows = [extra, ...(await db.fortunes())];

      sortByMessage(rows);

      const n = rows.length;

      let html = "",
        i = 0;
      for (; i < n; i++) {
        html += `<tr><td>${rows[i].id}</td><td>${escape(
          rows[i].message
        )}</td></tr>`;
      }

      addBenchmarkHeaders(c);
      return c.html(
        `<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>${html}</table></body></html>`
      );
    })
    .get("/updates", async (c) => {
      const queriesCount = getQueriesCount(c);

      const databaseJobs = new Array(queriesCount);

      for (let i = 0; i < queriesCount; i++) {
        databaseJobs[i] = db.find(generateRandomNumber());
      }

      const worldObjects = await Promise.all(databaseJobs);

      for (let i = 0; i < queriesCount; i++) {
        worldObjects[i].randomNumber = generateRandomNumber();
      }

      await db.bulkUpdate(worldObjects);

      addBenchmarkHeaders(c);
      return c.json(worldObjects);
    });
}

app
  .all("/*", (c) => {
    addBenchmarkHeaders(c);
    return c.text("Not Found", 404);
  })
  .onError(handleError);

const port = parseInt(process.env.PORT || "8080");
const hostname = process.env.HOST || "0.0.0.0";
serve({ fetch: app.fetch, hostname, port }, (info) => {
  if (!info) {
    console.error(`Couldn't bind to http://${hostname}:${port}!`);
    process.exit(1);
  }
  console.log(`Successfully bound to http://${hostname}:${port}.`);
});
