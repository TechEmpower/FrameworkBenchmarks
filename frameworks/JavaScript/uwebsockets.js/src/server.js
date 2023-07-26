import uWebSockets from "uWebSockets.js";
import {
  addBenchmarkHeaders,
  generateRandomNumber,
  getQueriesCount,
  handleError,
  escape,
} from "./utils.js";

let db;
const { DATABASE } = process.env;
if (DATABASE) db = await import(`./database/${DATABASE}.js`);

const webserver = uWebSockets.App();

webserver.get("/plaintext", (response) => {
  response.cork(() => {
    addBenchmarkHeaders(response);
    response.writeHeader("Content-Type", "text/plain");
    response.end("Hello, World!");
  });
});

webserver.get("/json", (response) => {
  response.cork(() => {
    addBenchmarkHeaders(response);
    response.writeHeader("Content-Type", "application/json");
    response.end(JSON.stringify({ message: "Hello, World!" }));
  });
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

      const databaseJobs = new Array(queriesCount);

      for (let i = 0; i < queriesCount; i++) {
        databaseJobs[i] = db.find(generateRandomNumber());
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

      rows.push({
        id: 0,
        message: "Additional fortune added at request time.",
      });

      rows.sort((a, b) => (a.message < b.message) ? -1 : 1);

      const n = rows.length

      let html = "", i = 0;
      for (; i < n; i++) {
        html += `<tr><td>${rows[i].id}</td><td>${escape(rows[i].message)}</td></tr>`;
      }

      response.cork(() => {
        addBenchmarkHeaders(response);
        response.writeHeader("Content-Type", "text/html; charset=utf-8");
        response.end(`<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>${html}</table></body></html>`);
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

      const databaseJobs = new Array(queriesCount);

      for (let i = 0; i < queriesCount; i++) {
        databaseJobs[i] = db.find(generateRandomNumber());
      }

      const worldObjects = await Promise.all(databaseJobs);

      if (response.aborted) {
        return;
      }

      for (let i = 0; i < queriesCount; i++) {
        worldObjects[i].randomNumber = generateRandomNumber();
        databaseJobs[i] = db.update(worldObjects[i]);
      }

      if (response.aborted) {
        return;
      }

      await Promise.all(databaseJobs);

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
