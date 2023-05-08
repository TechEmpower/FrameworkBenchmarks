import mysql from "mysql2/promise";
import uWebSockets from "uWebSockets.js";
import { addBenchmarkHeaders, handleError } from "./utils.js";

const webserver = uWebSockets.App();
const mysqlPool = mysql.createPool({
  host: "tfb-database",
  user: "benchmarkdbuser",
  password: "benchmarkdbpass",
  database: "hello_world",
  waitForConnections: true,
  connectionLimit: 512,
  queueLimit: 0,
});

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

webserver.get("/db", async (response) => {
  response.onAborted(() => {
    response.aborted = true;
  });

  try {
    const random = Math.floor(Math.random() * 9999) + 1;
    const [rows] = await mysqlPool.execute(
      "SELECT * FROM world WHERE id = ? LIMIT 1",
      [random]
    );

    if (response.aborted) {
      return;
    }

    if (rows.length < 1) {
      return handleError(new Error("Row not found"), response);
    }

    response.cork(() => {
      addBenchmarkHeaders(response);
      response.writeHeader("Content-Type", "application/json");
      response.end(JSON.stringify(rows[0]));
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
    let queriesCount = 1;
    if (request.getQuery("queries")) {
      try {
        const queries = parseInt(request.getQuery("queries"));
        if (queries <= 500 && queries >= 1) {
          queriesCount = queries;
        } else if (queries > 500) {
          queriesCount = 500;
        }
      } catch {}
    }

    const databaseJobs = [];

    for (let i = 0; i < queriesCount; i++) {
      const random = Math.floor(Math.random() * 9999) + 1;
      databaseJobs.push(
        mysqlPool.execute("SELECT * FROM world WHERE id = ? LIMIT 1", [random])
      );
    }

    const databaseJobsResults = await Promise.all(databaseJobs);
    const worldObjects = [];
    for (let i = 0; i < databaseJobsResults.length; i++) {
      const [rows] = databaseJobsResults[i];
      worldObjects.push(rows[0]);
    }

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
