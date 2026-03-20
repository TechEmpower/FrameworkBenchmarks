import { SQL } from "bun";

// Pre-allocated response headers to avoid GC pressure
const plainHeaders = new Headers({ "Server": "x-gnosis" });
const jsonHeaders = new Headers({ "Server": "x-gnosis", "Content-Type": "application/json" });
const htmlHeaders = new Headers({ "Server": "x-gnosis", "Content-Type": "text/html; charset=utf-8" });

const plainOptions: ResponseInit = { headers: plainHeaders };
const jsonOptions: ResponseInit = { headers: jsonHeaders };
const htmlOptions: ResponseInit = { headers: htmlHeaders };

// Lazy database connection -- only initialized when a DB endpoint is first hit
// This allows the default variant (plaintext/json only) to work without a DB
let sql: InstanceType<typeof SQL> | null = null;

function getSQL(): InstanceType<typeof SQL> {
  if (!sql) {
    sql = new SQL({
      url: "postgres://benchmarkdbuser:benchmarkdbpass@tfb-database:5432/hello_world",
      max: 1,
    });
  }
  return sql;
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

function clampQueries(q: string | null): number {
  const n = +q!;
  // NaN is falsy, fallback to 1
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

// --- DB operations ---

function findWorld(id: number): Promise<World> {
  const s = getSQL();
  return s`SELECT id, randomNumber FROM world WHERE id = ${id}`.then(
    (arr: World[]) => arr[0],
  );
}

function findWorldThenRand(id: number): Promise<World> {
  const s = getSQL();
  return s`SELECT id, randomNumber FROM world WHERE id = ${id}`.then(
    (arr: World[]) => {
      arr[0].randomNumber = rand();
      return arr[0];
    },
  );
}

function fetchFortunes(): Promise<Fortune[]> {
  const s = getSQL();
  return s`SELECT id, message FROM fortune` as Promise<Fortune[]>;
}

function bulkUpdate(worlds: World[]): Promise<unknown> {
  const s = getSQL();
  worlds = worlds.toSorted((a, b) => a.id - b.id);
  const values = new Array(worlds.length);
  for (let i = 0; i < worlds.length; i++) {
    values[i] = [worlds[i].id, worlds[i].randomNumber];
  }
  return s`UPDATE world SET randomNumber = (update_data.randomNumber)::int
    FROM (VALUES ${s(values)}) AS update_data (id, randomNumber)
    WHERE world.id = (update_data.id)::int`;
}

// --- Cache initialization ---

async function initCache(): Promise<void> {
  const s = getSQL();
  const rows: World[] = await s`SELECT id, randomNumber FROM world`;
  for (let i = 0; i < rows.length; i++) {
    worldCache.set(rows[i].id, rows[i]);
  }
  cacheReady = true;
}

// Fortune HTML template prefix and suffix
const FORTUNE_PREFIX = "<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>";
const FORTUNE_SUFFIX = "</table></body></html>";

// --- Server ---

const server = Bun.serve({
  port: 8080,
  reusePort: true,
  async fetch(req: Request): Promise<Response> {
    const url = req.url;
    const slashIdx = url.indexOf("/", 8);
    const qIdx = url.indexOf("?", slashIdx);
    const pathname = qIdx === -1 ? url.substring(slashIdx) : url.substring(slashIdx, qIdx);

    switch (pathname) {
      case "/plaintext":
        return new Response("Hello, World!", plainOptions);

      case "/json":
        return new Response(JSON.stringify({ message: "Hello, World!" }), jsonOptions);

      case "/db": {
        const world = await findWorld(rand());
        return new Response(JSON.stringify(world), jsonOptions);
      }

      case "/queries": {
        const num = clampQueries(getQueryParam(url, "queries"));
        const worldPromises = new Array(num);
        for (let i = 0; i < num; i++) {
          worldPromises[i] = findWorld(rand());
        }
        const worlds = await Promise.all(worldPromises);
        return new Response(JSON.stringify(worlds), jsonOptions);
      }

      case "/updates": {
        const num = clampQueries(getQueryParam(url, "queries"));
        const worldPromises = new Array(num);
        for (let i = 0; i < num; i++) {
          worldPromises[i] = findWorldThenRand(rand());
        }
        const worlds = await Promise.all(worldPromises);
        await bulkUpdate(worlds);
        return new Response(JSON.stringify(worlds), jsonOptions);
      }

      case "/fortunes": {
        const fortunes = await fetchFortunes();
        fortunes.push({ id: 0, message: "Additional fortune added at request time." });
        fortunes.sort((a, b) => (a.message < b.message ? -1 : 1));

        let html = FORTUNE_PREFIX;
        for (let i = 0; i < fortunes.length; i++) {
          html += `<tr><td>${fortunes[i].id}</td><td>${Bun.escapeHTML(fortunes[i].message)}</td></tr>`;
        }
        html += FORTUNE_SUFFIX;

        return new Response(html, htmlOptions);
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
        return new Response(JSON.stringify(worlds), jsonOptions);
      }

      default:
        return new Response("", { status: 404 });
    }
  },
});

console.log(`x-gnosis listening on ${server.url}`);
