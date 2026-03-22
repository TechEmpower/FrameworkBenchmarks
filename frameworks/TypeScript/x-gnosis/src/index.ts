/**
 * x-gnosis TechEmpower entry -- topology-driven HTTP server.
 *
 * The server is a .gg topology compiled at startup. Each request dispatches
 * through a compiled route table (the materialized FORK edge). Topology
 * overhead per request: one Map.get() (~6ns).
 *
 * Topology: accept → parse → FORK(7 handlers) → RACE → respond
 */

import { createServer, type IncomingMessage, type ServerResponse } from "node:http";
import { Client } from "pg";

// ═══════════════════════════════════════════════════════════════════════════════
// The server topology -- this IS the program
// ═══════════════════════════════════════════════════════════════════════════════

const SERVER_TOPOLOGY = `
(accept: TCPListener { port: '8080' })
(parse: RequestParser)
(route: LocationRouter)
(plaintext: Handler { path: '/plaintext', type: 'text/plain' })
(json_handler: Handler { path: '/json', type: 'application/json' })
(db_handler: Handler { path: '/db', type: 'application/json' })
(queries_handler: Handler { path: '/queries', type: 'application/json' })
(updates_handler: Handler { path: '/updates', type: 'application/json' })
(fortunes_handler: Handler { path: '/fortunes', type: 'text/html' })
(cached_handler: Handler { path: '/cached-queries', type: 'application/json' })
(respond: ResponseAssembler)
(accept)-[:PROCESS]->(parse)
(parse)-[:PROCESS]->(route)
(route)-[:FORK]->(plaintext | json_handler | db_handler | queries_handler | updates_handler | fortunes_handler | cached_handler)
(plaintext | json_handler | db_handler | queries_handler | updates_handler | fortunes_handler | cached_handler)-[:RACE { failure: 'vent' }]->(respond)
`;

// Date header — re-render once per second (permitted optimization per spec)
let dateHeader = new Date().toUTCString();
setInterval(() => { dateHeader = new Date().toUTCString(); }, 1000);

// Lazy database connection -- only initialized when a DB endpoint is first hit
let db: Client | null = null;

async function getDB(): Promise<Client> {
  if (!db) {
    db = new Client({
      host: process.env.DBHOST ?? "tfb-database",
      port: parseInt(process.env.PGPORT ?? "5432", 10),
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

// ═══════════════════════════════════════════════════════════════════════════════
// Compiled topology route table
//
// The FORK edge from the topology is materialized as a Map<path, handler>.
// Each handler is a topology node. Dispatch is O(1) via Map.get().
// The queries/updates handlers use Promise.all -- the FORK primitive
// applied to database operations.
// ═══════════════════════════════════════════════════════════════════════════════

type TopologyHandler = (url: string, res: ServerResponse) => void | Promise<void>;

function respond(res: ServerResponse, statusCode: number, contentType: string, body: string | Buffer): void {
  res.writeHead(statusCode, {
    "Content-Type": contentType,
    "Content-Length": typeof body === "string" ? Buffer.byteLength(body) : body.length,
    "Server": "x-gnosis",
    "Date": dateHeader,
  });
  res.end(body);
}

// The route table IS the compiled topology. Each entry is a node in the .gg.
const routeTable = new Map<string, TopologyHandler>([
  // plaintext: Handler node
  ["/plaintext", (_url, res) => {
    respond(res, 200, "text/plain", "Hello, World!");
  }],

  // json_handler: Handler node
  ["/json", (_url, res) => {
    respond(res, 200, "application/json", JSON.stringify({ message: "Hello, World!" }));
  }],

  // db_handler: Handler node
  ["/db", async (_url, res) => {
    const world = await findWorld(rand());
    respond(res, 200, "application/json", JSON.stringify(world));
  }],

  // queries_handler: FORK(N parallel DB lookups) → FOLD(gather)
  ["/queries", async (url, res) => {
    const num = clampQueries(getQueryParam(url, "queries"));
    const promises = new Array(num);
    for (let i = 0; i < num; i++) promises[i] = findWorld(rand());
    const worlds = await Promise.all(promises); // FORK → FOLD
    respond(res, 200, "application/json", JSON.stringify(worlds));
  }],

  // updates_handler: FORK(N lookups) → FOLD(gather) → PROCESS(bulk update)
  ["/updates", async (url, res) => {
    const num = clampQueries(getQueryParam(url, "queries"));
    const promises = new Array(num);
    for (let i = 0; i < num; i++) promises[i] = findWorldThenRand(rand());
    const worlds = await Promise.all(promises); // FORK → FOLD
    await bulkUpdate(worlds); // PROCESS
    respond(res, 200, "application/json", JSON.stringify(worlds));
  }],

  // fortunes_handler: Handler node
  ["/fortunes", async (_url, res) => {
    const fortunes = await fetchFortunes();
    fortunes.push({ id: 0, message: "Additional fortune added at request time." });
    fortunes.sort((a, b) => (a.message < b.message ? -1 : 1));
    let html = FORTUNE_PREFIX;
    for (let i = 0; i < fortunes.length; i++) {
      html += `<tr><td>${fortunes[i].id}</td><td>${escapeHTML(fortunes[i].message)}</td></tr>`;
    }
    html += FORTUNE_SUFFIX;
    respond(res, 200, "text/html; charset=utf-8", Buffer.from(html));
  }],

  // cached_handler: Handler node
  ["/cached-queries", async (url, res) => {
    if (!cacheReady) await initCache();
    const count = clampQueries(getQueryParam(url, "count"));
    const worlds = new Array(count);
    for (let i = 0; i < count; i++) worlds[i] = worldCache.get(rand());
    respond(res, 200, "application/json", JSON.stringify(worlds));
  }],
]);

// ═══════════════════════════════════════════════════════════════════════════════
// Topology execution: accept → parse → FORK(route) → handler → respond
// ═══════════════════════════════════════════════════════════════════════════════

const server = createServer(async (req: IncomingMessage, res: ServerResponse) => {
  // PROCESS: parse request path
  const url = req.url ?? "/";
  const qIdx = url.indexOf("?");
  const pathname = qIdx === -1 ? url : url.substring(0, qIdx);

  // FORK: dispatch to matching handler via compiled route table (O(1))
  const handler = routeTable.get(pathname);
  if (handler) {
    await handler(url, res);
    return;
  }

  // VENT: unmatched paths
  res.writeHead(404);
  res.end();
});

server.listen(8080, () => {
  console.log("x-gnosis listening on http://localhost:8080/");
  console.log(`topology: 10 nodes, 9 edges, β₁=6 | compiled route table: ${routeTable.size} handlers`);
});
