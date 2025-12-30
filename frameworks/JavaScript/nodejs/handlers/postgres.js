const postgres = require("postgres");
const NodeCache = require("node-cache");
const h = require("../helper");

const sql = postgres({
  host: "tfb-database",
  user: "benchmarkdbuser",
  password: "benchmarkdbpass",
  database: "hello_world",
  max: 1,
});

const dbfortunes = async () => await sql`SELECT id, message FROM fortune`;

const dbfind = async (id) =>
  await sql`SELECT id, randomNumber FROM world WHERE id = ${id}`.then(
    (arr) => arr[0]
  );

const dbbulkUpdate = async (worlds) => {
  const sorted = sql(worlds
    .map((world) => [world.id, world.randomNumber])
    .sort((a, b) => (a[0] < b[0] ? -1 : 1)));
  await sql`UPDATE world SET randomNumber = (update_data.randomNumber)::int
  FROM (VALUES ${sorted}) AS update_data (id, randomNumber)
  WHERE world.id = (update_data.id)::int`;
};

const dbgetAllWorlds = async () => sql`SELECT id, randomNumber FROM world`;

const extra = h.additionalFortune();

const myCache = new NodeCache({ stdTTL: 0, checkperiod: 0 });

let isCachePopulated = false;

const populateCache = (callback) => {
  if (isCachePopulated) return callback();

  dbgetAllWorlds().then((worlds) => {
    for (let i = 0; i < worlds.length; i++) {
      myCache.set(worlds[i].id, worlds[i]);
    }
    isCachePopulated = true;
    callback();
  });
};

module.exports = {
  SingleQuery: async (req, res) => {
    const row = await dbfind(h.generateRandomNumber());
    h.writeResponse(res, h.worldObjectSerializer(row));
  },

  MultipleQueries: async (queries, req, res) => {
    const databaseJobs = new Array(queries);
    for (let i = 0; i < queries; i++) {
      databaseJobs[i] = dbfind(h.generateRandomNumber());
    }
    const worldObjects = await Promise.all(databaseJobs);

    h.writeResponse(res, JSON.stringify(worldObjects));
  },

  Fortunes: async (req, res) => {
    const rows = [extra, ...(await dbfortunes())];
    h.sortByMessage(rows);
    const n = rows.length;
    let html = "", i = 0;
    for (; i < n; i++) {
      html += `<tr><td>${rows[i].id}</td><td>${h.escapeHtmlFromText(rows[i].message)}</td></tr>`;
    }

    h.writeResponse(
      res,
      `<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>${html}</table></body></html>`,
      h.headerTypes["html"]
    );
  },

  Updates: async (queries, req, res) => {
    const databaseJobs = new Array(queries);

    for (let i = 0; i < queries; i++) {
      databaseJobs[i] = dbfind(h.generateRandomNumber());
    }

    const worldObjects = await Promise.all(databaseJobs);

    for (let i = 0; i < queries; i++) {
      worldObjects[i].randomNumber = h.generateRandomNumber();
    }

    await dbbulkUpdate(worldObjects);

    h.writeResponse(res, JSON.stringify(worldObjects));
  },
  CachedQueries: (queries, req, res) => {
    populateCache(() => {
      const worlds = new Array(queries);

      for (let i = 0; i < queries; i++) {
        worlds[i] = myCache.get(h.generateRandomNumber());
      }

      h.writeResponse(res, JSON.stringify(worlds));
    });
  },
};
