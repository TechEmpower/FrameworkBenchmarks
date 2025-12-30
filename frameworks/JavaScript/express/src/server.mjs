import express from "express";
import {
  generateRandomNumber,
  getQueriesCount,
  handleError,
  escape,
  jsonSerializer,
  worldObjectSerializer,
  sortByMessage,
  writeResponse,
  headerTypes,
  GREETING,
} from "./utils.mjs";

let db;
const { DATABASE } = process.env;
if (DATABASE) db = await import(`./database/${DATABASE}.mjs`);

const extra = { id: 0, message: "Additional fortune added at request time." };

const app = express();

app.get("/plaintext", (req, res) => {
  writeResponse(res, GREETING, headerTypes["plain"]);
});

app.get("/json", (req, res) => {
  writeResponse(res, jsonSerializer({ message: GREETING }));
});

if (db) {
  app.get("/db", async (req, res) => {
    try {
      const row = await db.find(generateRandomNumber());
      writeResponse(res, worldObjectSerializer(row));
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
      writeResponse(res, JSON.stringify(worldObjects));
    } catch (error) {
      handleError(error, res);
    }
  });

  app.get("/fortunes", async (req, res) => {
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

      writeResponse(
        res,
        `<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>${html}</table></body></html>`,
        headerTypes["html"]
      );
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
      writeResponse(res, JSON.stringify(worldObjects));
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
