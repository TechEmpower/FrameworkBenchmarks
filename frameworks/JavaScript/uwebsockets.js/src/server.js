import { escape } from "html-escaper";
import uWebSockets from "uWebSockets.js";
import {
  addBenchmarkHeaders,
  generateRandomNumber,
  getQueriesCount,
  handleError,
} from "./utils.js";

let db;
const { DATABASE } = process.env;
if (DATABASE) db = await import(`./database/${DATABASE}.js`);

const webserver = uWebSockets.App();

webserver.get("/plaintext", (response) => {
  addBenchmarkHeaders(response);
  response.writeHeader("Content-Type", "text/plain");
  response.end("Hello, World!");
});

webserver.get("/json", (response) => {
  addBenchmarkHeaders(response);
  response.writeHeader("Content-Type", "application/json");
  response.end(JSON.stringify({ message: "Hello, World!" }));
});

if (db) {
  webserver.get("/db", async (response) => {
    response.onAborted(() => {
      response.aborted = true;
    });

    try {
      const rows = await db.find(generateRandomNumber());

      if (response.aborted) {
        return;
      }

      if (rows.length < 1) {
        return handleError(new Error("Row not found"), response);
      }

      response.cork(() => {
        addBenchmarkHeaders(response);
        response.writeHeader("Content-Type", "application/json");
        response.end(JSON.stringify(rows));
      });
    } catch (error) {
      if (response.aborted) {
        return;
      }

      handleError(error, response);
    }
  });

  webserver.get("/queries", async (response, request) => {
    response.onAborted(() => {
      response.aborted = true;
    });

    try {
      const queriesCount = getQueriesCount(request);

      const databaseJobs = [];

      for (let i = 0; i < queriesCount; i++) {
        databaseJobs.push(db.find(generateRandomNumber()));
      }

      const worldObjects = await Promise.all(databaseJobs);

      if (response.aborted) {
        return;
      }

      response.cork(() => {
        addBenchmarkHeaders(response);
        response.writeHeader("Content-Type", "application/json");
        response.end(JSON.stringify(worldObjects));
      });
    } catch (error) {
      if (response.aborted) {
        return;
      }

      handleError(error, response);
    }
  });

  webserver.get("/fortunes", async (response) => {
    response.onAborted(() => {
      response.aborted = true;
    });

    try {
      const rows = await db.fortunes();

      if (response.aborted) {
        return;
      }

      if (rows.length < 1) {
        return handleError(new Error("Row not found"), response);
      }

      rows.push({
        id: 0,
        message: "Additional fortune added at request time.",
      });

      rows.sort((a, b) => a.message.localeCompare(b.message));

      let html =
        "<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>";

      for (let i = 0; i < rows.length; i++) {
        html += `<tr><td>${rows[i].id}</td><td>${escape(
          rows[i].message
        )}</td></tr>`;
      }

      html += "</table></body></html>";

      response.cork(() => {
        addBenchmarkHeaders(response);
        response.writeHeader("Content-Type", "text/html; charset=utf-8");
        response.end(html);
      });
    } catch (error) {
      if (response.aborted) {
        return;
      }

      handleError(error, response);
    }
  });

  webserver.get("/updates", async (response, request) => {
    response.onAborted(() => {
      response.aborted = true;
    });

    try {
      const queriesCount = getQueriesCount(request);

      const databaseReadJobs = [];

      for (let i = 0; i < queriesCount; i++) {
        databaseReadJobs.push(db.find(generateRandomNumber()));
      }

      const worldObjects = await Promise.all(databaseReadJobs);

      if (response.aborted) {
        return;
      }

      const databaseWriteJobs = [];

      for (let i = 0; i < worldObjects.length; i++) {
        worldObjects[i].randomNumber = generateRandomNumber();
        databaseWriteJobs.push(db.update(worldObjects[i]));
      }

      if (response.aborted) {
        return;
      }

      await Promise.all(databaseWriteJobs);

      response.cork(() => {
        addBenchmarkHeaders(response);
        response.writeHeader("Content-Type", "application/json");
        response.end(JSON.stringify(worldObjects));
      });
    } catch (error) {
      if (response.aborted) {
        return;
      }

      handleError(error, response);
    }
  });
}

webserver.any("/*", (response) => {
  response.writeStatus("404 Not Found");
  addBenchmarkHeaders(response);
  response.writeHeader("Content-Type", "text/plain");
  response.end("Not Found");
});

const host = process.env.HOST || "0.0.0.0";
const port = parseInt(process.env.PORT || "8080");
webserver.listen(host, port, (socket) => {
  if (!socket) {
    console.error(`Couldn't bind to http://${host}:${port}!`);
    process.exit(1);
  }

  console.log(`Successfully bound to http://${host}:${port}.`);
});
