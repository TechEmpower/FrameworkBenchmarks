import express from "express";
import {
  generateRandomNumber,
  getQueriesCount,
  escape,
  jsonSerializer,
  worldObjectSerializer,
  worldsObjectSerializer,
  GREETING,
} from "./utils.mjs";

let db;
const { DATABASE } = process.env;
if (DATABASE) db = await import(`./database/${DATABASE}.mjs`);

const extra = { id: 0, message: "Additional fortune added at request time." };

const app = express();

app.get("/plaintext", (req, res) => {
  res.writeHead(200, {
    "content-type": "text/plain",
    server: "Express",
  }).end(GREETING);
});

app.get("/json", (req, res) => {
  res.writeHead(200, {
    "content-type": "application/json",
    server: "Express",
  }).end(jsonSerializer({ message: GREETING }));
});

if (db) {
  app.get("/db", async (req, res) => {
    const row = await db.find(generateRandomNumber());
    res.writeHead(200, {
      "content-type": "application/json",
      server: "Express",
    }).end(worldObjectSerializer(row));
  });

  app.get("/queries", async (req, res) => {
    const queries = getQueriesCount(req);
    const worldPromises = new Array(queries);
    for (let i = 0; i < queries; i++) {
      worldPromises[i] = db.find(generateRandomNumber());
    }
    const worlds = await Promise.all(worldPromises);
    res.writeHead(200, {
      "content-type": "application/json",
      server: "Express",
    }).end(worldsObjectSerializer(worlds));
  });

  app.get("/fortunes", async (req, res) => {
    const rows = [extra, ...(await db.fortunes())];
    rows.sort((a, b) => (a.message < b.message) ? -1 : 1);
    const n = rows.length;
    let html = "",
      i = 0;
    for (; i < n; i++) {
      const row = rows[i];
      html += `<tr><td>${row.id}</td><td>${escape(row.message)}</td></tr>`;
    }
    res.writeHead(200, {
      "content-type": "text/html; charset=UTF-8",
      server: "Express",
    }).end(`<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>${html}</table></body></html>`);
  });

  app.get("/updates", async (req, res) => {
    const queriesCount = getQueriesCount(req);
    const databaseJobs = new Array(queriesCount);
    for (let i = 0; i < queriesCount; i++) {
      databaseJobs[i] = db.find(generateRandomNumber());
    }
    const worldObjects = await Promise.all(databaseJobs);

    for (let i = 0; i < queriesCount; i++) {
      worldObjects[i].randomNumber = generateRandomNumber();
    }
    await db.bulkUpdate(worldObjects);
    res.writeHead(200, {
      "content-type": "application/json",
      server: "Express",
    }).end(worldsSerializer(worldObjects));
  });

  let isCachePopulated = false
  app.get('/cached-worlds', async (req, res) => {
    if (!isCachePopulated) {
      const worlds = await db.getAllWorlds();
      for (let i = 0; i < worlds.length; i++) {
        cache.set(worlds[i].id, worlds[i]);
      }
      isCachePopulated = true;
    }
    const count = getQueriesCount(req);
    const worlds = new Array(count);

    for (let i = 0; i < count; i++) {
      worlds[i] = cache.get(generateRandomNumber());
    }

    res.writeHead(200, {
      "content-type": "application/json",
      server: "Express",
    }).end(worldsSerializer(worlds));
  });
}

const host = process.env.HOST || "0.0.0.0";
const port = parseInt(process.env.PORT || "8080");
app.listen(port, host, () => {
  console.log(`Server running at http://${host}:${port}/`);
});
