import express from "ultimate-express";
import {
  generateRandomNumber,
  getQueriesCount,
  handleError,
  escape,
  jsonSerializer,
  worldObjectSerializer,
  sortByMessage,
  GREETING,
} from "./utils.mjs";

let db;
const { DATABASE } = process.env;
if (DATABASE) db = await import(`./database/${DATABASE}.mjs`);

const extra = { id: 0, message: "Additional fortune added at request time." };

const app = express();

app.get("/plaintext", (req, res) => {
  res.setHeader("Content-Type", "text/plain");
  res.setHeader("Server", "UltimateExpress");
  res.send("Hello, World!");
});

app.get("/json", (req, res) => {
  res.send(jsonSerializer({ message: GREETING }));
});

if (db) {
  app.get("/db", async (req, res) => {
    try {
      const row = await db.find(generateRandomNumber());
      res.send(worldObjectSerializer(row));
    } catch (error) {
      handleError(error, res);
    }
  });

  app.get("/queries", async (req, res) => {
    try {
      const queriesCount = getQueriesCount(req);
      const databaseJobs = new Array(queriesCount)
        .fill()
        .map(() => db.find(generateRandomNumber()));
      const worldObjects = await Promise.all(databaseJobs);
      res.send(worldObjects);
    } catch (error) {
      handleError(error, res);
    }
  });

  app.get("/fortunes", async (req, res) => {
    res.setHeader("Content-Type", "text/html; charset=UTF-8");
    res.setHeader("Server", "UltimateExpress");
    try {
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

      res.send(
        `<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>${html}</table></body></html>`);
    } catch (error) {
      handleError(error, res);
    }
  });

  app.get("/updates", async (req, res) => {
    try {
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
      res.send(worldObjects);
    } catch (error) {
      handleError(error, res);
    }
  });
}

app.all("*", (req, res) => {
  res.status(404).send("Not Found");
});

const host = process.env.HOST || "0.0.0.0";
const port = parseInt(process.env.PORT || "8080");
app.listen(port, host, () => {
  console.log(`Server running at http://${host}:${port}/`);
});
