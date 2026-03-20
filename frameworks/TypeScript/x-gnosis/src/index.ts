import { createServer } from "node:http";
import { Client } from "pg";

// Date header — re-render once per second (permitted optimization per spec)
let dateHeader = new Date().toUTCString();
setInterval(() => { dateHeader = new Date().toUTCString(); }, 1000);

// Lazy database connection -- only initialized when a DB endpoint is first hit
let db: Client | null = null;

async function getDB(): Promise<Client> {
  if (!db) {
    db = new Client({
      host: process.env.DBHOST ?? "tfb-database",
      port: 5432,
      user: "benchmarkdbuser",
      password: "benchmarkdbpass",
      database: "hello_world",
    });
    await db.connect();
  }
  return db;
}

// In-memory cache for cached-queries test
const worldCache = new Map<number, World>();
let cacheReady = false;

interface World {
  id: number;
  randomNumber: number;
}

interface Fortune {
  id: number;
  message: string;
}

// --- Helpers ---

function rand(): number {
  return 1 + ((Math.random() * 10000) | 0);
}

function clampQueries(q: string | null | undefined): number {
  const n = +(q ?? "");
  return Math.min(Math.max(n || 1, 1), 500);
}

function getQueryParam(url: string, key: string): string | null {
  const qIdx = url.indexOf("?");
  if (qIdx === -1) return null;
  const qs = url.substring(qIdx + 1);
  const params = qs.split("&");
  for (let i = 0; i < params.length; i++) {
    const eqIdx = params[i].indexOf("=");
    if (eqIdx !== -1 && params[i].substring(0, eqIdx) === key) {
      return params[i].substring(eqIdx + 1);
    }
  }
  return null;
}

// --- HTML escaping (replaces Bun.escapeHTML) ---

function escapeHTML(s: string): string {
  let out = "";
  for (let i = 0; i < s.length; i++) {
    const ch = s[i];
    switch (ch) {
      case "&": out += "&amp;"; break;
      case "<": out += "&lt;"; break;
      case ">": out += "&gt;"; break;
      case '"': out += "&quot;"; break;
      case "'": out += "&#x27;"; break;
      default: out += ch;
    }
  }
  return out;
}

// --- DB operations ---

async function findWorld(id: number): Promise<World> {
  const client = await getDB();
  const res = await client.query("SELECT id, randomnumber AS \"randomNumber\" FROM world WHERE id = $1", [id]);
  return res.rows[0];
}

async function findWorldThenRand(id: number): Promise<World> {
  const w = await findWorld(id);
  w.randomNumber = rand();
  return w;
}

async function fetchFortunes(): Promise<Fortune[]> {
  const client = await getDB();
  const res = await client.query("SELECT id, message FROM fortune");
  return res.rows;
}

async function bulkUpdate(worlds: World[]): Promise<void> {
  const client = await getDB();
  worlds = worlds.toSorted((a, b) => a.id - b.id);
  const values: string[] = [];
  for (let i = 0; i < worlds.length; i++) {
    values.push(`(${worlds[i].id}, ${worlds[i].randomNumber})`);
  }
  await client.query(
    `UPDATE world SET randomnumber = (update_data.randomnumber)::int
     FROM (VALUES ${values.join(",")}) AS update_data (id, randomnumber)
     WHERE world.id = (update_data.id)::int`
  );
}

// --- Cache initialization ---

async function initCache(): Promise<void> {
  const client = await getDB();
  const res = await client.query('SELECT id, randomnumber AS "randomNumber" FROM world');
  for (const row of res.rows) {
    worldCache.set(row.id, row);
  }
  cacheReady = true;
}

// Fortune HTML template
const FORTUNE_PREFIX = "<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>";
const FORTUNE_SUFFIX = "</table></body></html>";

// --- Server ---

const server = createServer(async (req, res) => {
  const url = req.url ?? "/";
  const qIdx = url.indexOf("?");
  const pathname = qIdx === -1 ? url : url.substring(0, qIdx);

  switch (pathname) {
    case "/plaintext": {
      const body = "Hello, World!";
      res.writeHead(200, {
        "Content-Type": "text/plain",
        "Content-Length": Buffer.byteLength(body),
        "Server": "x-gnosis",
        "Date": dateHeader,
      });
      res.end(body);
      return;
    }

    case "/json": {
      const body = JSON.stringify({ message: "Hello, World!" });
      res.writeHead(200, {
        "Content-Type": "application/json",
        "Content-Length": Buffer.byteLength(body),
        "Server": "x-gnosis",
        "Date": dateHeader,
      });
      res.end(body);
      return;
    }

    case "/db": {
      const world = await findWorld(rand());
      const body = JSON.stringify(world);
      res.writeHead(200, {
        "Content-Type": "application/json",
        "Content-Length": Buffer.byteLength(body),
        "Server": "x-gnosis",
        "Date": dateHeader,
      });
      res.end(body);
      return;
    }

    case "/queries": {
      const num = clampQueries(getQueryParam(url, "queries"));
      const worldPromises = new Array(num);
      for (let i = 0; i < num; i++) {
        worldPromises[i] = findWorld(rand());
      }
      const worlds = await Promise.all(worldPromises);
      const body = JSON.stringify(worlds);
      res.writeHead(200, {
        "Content-Type": "application/json",
        "Content-Length": Buffer.byteLength(body),
        "Server": "x-gnosis",
        "Date": dateHeader,
      });
      res.end(body);
      return;
    }

    case "/updates": {
      const num = clampQueries(getQueryParam(url, "queries"));
      const worldPromises = new Array(num);
      for (let i = 0; i < num; i++) {
        worldPromises[i] = findWorldThenRand(rand());
      }
      const worlds = await Promise.all(worldPromises);
      await bulkUpdate(worlds);
      const body = JSON.stringify(worlds);
      res.writeHead(200, {
        "Content-Type": "application/json",
        "Content-Length": Buffer.byteLength(body),
        "Server": "x-gnosis",
        "Date": dateHeader,
      });
      res.end(body);
      return;
    }

    case "/fortunes": {
      const fortunes = await fetchFortunes();
      fortunes.push({ id: 0, message: "Additional fortune added at request time." });
      fortunes.sort((a, b) => (a.message < b.message ? -1 : 1));

      let html = FORTUNE_PREFIX;
      for (let i = 0; i < fortunes.length; i++) {
        html += `<tr><td>${fortunes[i].id}</td><td>${escapeHTML(fortunes[i].message)}</td></tr>`;
      }
      html += FORTUNE_SUFFIX;

      const body = Buffer.from(html);
      res.writeHead(200, {
        "Content-Type": "text/html; charset=utf-8",
        "Content-Length": body.length,
        "Server": "x-gnosis",
        "Date": dateHeader,
      });
      res.end(body);
      return;
    }

    case "/cached-queries": {
      if (!cacheReady) {
        await initCache();
      }
      const count = clampQueries(getQueryParam(url, "count"));
      const worlds = new Array(count);
      for (let i = 0; i < count; i++) {
        worlds[i] = worldCache.get(rand());
      }
      const body = JSON.stringify(worlds);
      res.writeHead(200, {
        "Content-Type": "application/json",
        "Content-Length": Buffer.byteLength(body),
        "Server": "x-gnosis",
        "Date": dateHeader,
      });
      res.end(body);
      return;
    }

    default: {
      res.writeHead(404);
      res.end();
    }
  }
});

server.listen(8080, () => {
  console.log("x-gnosis listening on http://localhost:8080/");
});
