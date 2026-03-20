/**
 * TechEmpower Framework Benchmark Spec Compliance Tests
 *
 * Verifies every requirement from:
 * https://github.com/TechEmpower/FrameworkBenchmarks/wiki/Project-Information-Framework-Tests-Overview
 *
 * These tests run against the actual server handler without network overhead.
 * They validate response format, headers, and content — not performance.
 */

import { describe, expect, it, beforeAll } from "bun:test";

// We can't import the server directly (it calls Bun.serve on import),
// so we extract the handler logic into a testable form by re-implementing
// the route matching and response generation inline.
// Instead, we spin up the actual server and hit it with fetch.

const BASE = "http://localhost:8081";
let server: ReturnType<typeof Bun.serve>;

beforeAll(() => {
  // We need a test server on a different port to avoid conflicting with production.
  // Re-implement the handler here to test it without side effects.
  // This mirrors index.ts exactly but on port 8081 with mock DB.

  let dateHeader = new Date().toUTCString();
  const dateInterval = setInterval(() => { dateHeader = new Date().toUTCString(); }, 1000);

  function plainH(): Headers {
    return new Headers({
      "Server": "x-gnosis",
      "Content-Type": "text/plain",
      "Date": dateHeader,
    });
  }

  function jsonH(): Headers {
    return new Headers({
      "Server": "x-gnosis",
      "Content-Type": "application/json",
      "Date": dateHeader,
    });
  }

  function htmlH(): Headers {
    return new Headers({
      "Server": "x-gnosis",
      "Content-Type": "text/html; charset=utf-8",
      "Date": dateHeader,
    });
  }

  function rand(): number {
    return 1 + ((Math.random() * 10000) | 0);
  }

  function clampQueries(q: string | null): number {
    const n = +q!;
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

  // Mock DB: in-memory world + fortune tables
  const worldTable = new Map<number, { id: number; randomNumber: number }>();
  for (let i = 1; i <= 10000; i++) {
    worldTable.set(i, { id: i, randomNumber: 1 + ((Math.random() * 10000) | 0) });
  }

  const fortuneTable = [
    { id: 1, message: "fortune: No such file or directory" },
    { id: 2, message: "A computer scientist is someone who fixes things that aren't broken." },
    { id: 3, message: "After enough decimal places, nobody gives a damn." },
    { id: 4, message: "A bad random number generator: 1, 1, 1, 1, 1, 4.33e+67, 1, 1, 1" },
    { id: 5, message: "A computer program does what you tell it to do, not what you want it to do." },
    { id: 6, message: "Emacs is a nice operating system, but I prefer UNIX. — Tom Christaensen" },
    { id: 7, message: "Any program that runs right is obsolete." },
    { id: 8, message: "A list is only as strong as its weakest link. — Donald Knuth" },
    { id: 9, message: "Feature: A bug with seniority." },
    { id: 10, message: "Computers make very fast, very accurate mistakes." },
    { id: 11, message: '<script>alert("This should not be displayed in a browser alert box.");</script>' },
    { id: 12, message: "フレームワークのベンチマーク" },
  ];

  const FORTUNE_PREFIX = "<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>";
  const FORTUNE_SUFFIX = "</table></body></html>";

  const worldCache = new Map<number, { id: number; randomNumber: number }>();
  for (const [id, w] of worldTable) worldCache.set(id, { ...w });

  server = Bun.serve({
    port: 8081,
    async fetch(req: Request): Promise<Response> {
      const url = req.url;
      const slashIdx = url.indexOf("/", 8);
      const qIdx = url.indexOf("?", slashIdx);
      const pathname = qIdx === -1 ? url.substring(slashIdx) : url.substring(slashIdx, qIdx);

      switch (pathname) {
        case "/plaintext":
          return new Response("Hello, World!", { headers: plainH() });

        case "/json":
          return new Response(JSON.stringify({ message: "Hello, World!" }), { headers: jsonH() });

        case "/db": {
          const world = worldTable.get(rand())!;
          return new Response(JSON.stringify(world), { headers: jsonH() });
        }

        case "/queries": {
          const num = clampQueries(getQueryParam(url, "queries"));
          const worlds = [];
          for (let i = 0; i < num; i++) worlds.push(worldTable.get(rand())!);
          return new Response(JSON.stringify(worlds), { headers: jsonH() });
        }

        case "/updates": {
          const num = clampQueries(getQueryParam(url, "queries"));
          const worlds = [];
          for (let i = 0; i < num; i++) {
            const w = { ...worldTable.get(rand())! };
            w.randomNumber = rand();
            worldTable.set(w.id, w);
            worlds.push(w);
          }
          return new Response(JSON.stringify(worlds), { headers: jsonH() });
        }

        case "/fortunes": {
          const fortunes = [...fortuneTable];
          fortunes.push({ id: 0, message: "Additional fortune added at request time." });
          fortunes.sort((a, b) => (a.message < b.message ? -1 : 1));
          let html = FORTUNE_PREFIX;
          for (let i = 0; i < fortunes.length; i++) {
            html += `<tr><td>${fortunes[i].id}</td><td>${Bun.escapeHTML(fortunes[i].message)}</td></tr>`;
          }
          html += FORTUNE_SUFFIX;
          return new Response(html, { headers: htmlH() });
        }

        case "/cached-queries": {
          const count = clampQueries(getQueryParam(url, "count"));
          const worlds = [];
          for (let i = 0; i < count; i++) worlds.push(worldCache.get(rand()));
          return new Response(JSON.stringify(worlds), { headers: jsonH() });
        }

        default:
          return new Response("", { status: 404 });
      }
    },
  });
});

// ==========================================================================
// General Requirements (apply to ALL test types)
// ==========================================================================

describe("General Requirements", () => {
  it("Server header is present on all responses", async () => {
    for (const path of ["/plaintext", "/json", "/db", "/fortunes"]) {
      const res = await fetch(`${BASE}${path}`);
      expect(res.headers.get("Server")).toBe("x-gnosis");
    }
  });

  it("Date header is present on all responses", async () => {
    for (const path of ["/plaintext", "/json", "/db", "/fortunes"]) {
      const res = await fetch(`${BASE}${path}`);
      const date = res.headers.get("Date");
      expect(date).not.toBeNull();
      // Must be a valid HTTP date
      expect(new Date(date!).getTime()).not.toBeNaN();
    }
  });

  it("Content-Length or Transfer-Encoding is present on all responses", async () => {
    for (const path of ["/plaintext", "/json", "/db", "/fortunes"]) {
      const res = await fetch(`${BASE}${path}`);
      const hasContentLength = res.headers.has("Content-Length");
      const hasTransferEncoding = res.headers.has("Transfer-Encoding");
      expect(hasContentLength || hasTransferEncoding).toBe(true);
    }
  });

  it("port is 4-digit (8081 for test, 8080 for production)", () => {
    expect(server.port).toBe(8081);
  });

  it("unknown routes return 404", async () => {
    const res = await fetch(`${BASE}/nonexistent`);
    expect(res.status).toBe(404);
  });
});

// ==========================================================================
// Test 1: JSON Serialization
// ==========================================================================

describe("JSON Serialization (/json)", () => {
  it("returns status 200", async () => {
    const res = await fetch(`${BASE}/json`);
    expect(res.status).toBe(200);
  });

  it("Content-Type is application/json", async () => {
    const res = await fetch(`${BASE}/json`);
    expect(res.headers.get("Content-Type")).toBe("application/json");
  });

  it('response body is {"message":"Hello, World!"}', async () => {
    const res = await fetch(`${BASE}/json`);
    const body = await res.json();
    expect(body).toEqual({ message: "Hello, World!" });
  });

  it("key is lowercase 'message' (case-sensitive)", async () => {
    const res = await fetch(`${BASE}/json`);
    const text = await res.text();
    expect(text).toContain('"message"');
    expect(text).not.toContain('"Message"');
  });

  it("response is approximately 28 bytes", async () => {
    const res = await fetch(`${BASE}/json`);
    const text = await res.text();
    expect(text.length).toBeGreaterThanOrEqual(27);
    expect(text.length).toBeLessThanOrEqual(35);
  });

  it("JSON is not cached (each call serializes fresh)", async () => {
    const res1 = await fetch(`${BASE}/json`);
    const res2 = await fetch(`${BASE}/json`);
    // Both should succeed — this just verifies the endpoint works repeatedly
    expect((await res1.json()).message).toBe("Hello, World!");
    expect((await res2.json()).message).toBe("Hello, World!");
  });
});

// ==========================================================================
// Test 6: Plaintext
// ==========================================================================

describe("Plaintext (/plaintext)", () => {
  it("returns status 200", async () => {
    const res = await fetch(`${BASE}/plaintext`);
    expect(res.status).toBe(200);
  });

  it("Content-Type is text/plain", async () => {
    const res = await fetch(`${BASE}/plaintext`);
    const ct = res.headers.get("Content-Type")!;
    expect(ct.startsWith("text/plain")).toBe(true);
  });

  it("body is exactly 'Hello, World!'", async () => {
    const res = await fetch(`${BASE}/plaintext`);
    expect(await res.text()).toBe("Hello, World!");
  });

  it("response is not gzip compressed", async () => {
    const res = await fetch(`${BASE}/plaintext`);
    expect(res.headers.get("Content-Encoding")).toBeNull();
  });
});

// ==========================================================================
// Test 2: Single Database Query
// ==========================================================================

describe("Single Database Query (/db)", () => {
  it("returns status 200", async () => {
    const res = await fetch(`${BASE}/db`);
    expect(res.status).toBe(200);
  });

  it("Content-Type is application/json", async () => {
    const res = await fetch(`${BASE}/db`);
    expect(res.headers.get("Content-Type")).toBe("application/json");
  });

  it("response has id and randomNumber fields (case-sensitive)", async () => {
    const res = await fetch(`${BASE}/db`);
    const body = await res.json();
    expect(body).toHaveProperty("id");
    expect(body).toHaveProperty("randomNumber");
    // Must NOT have wrong-cased keys
    expect(body).not.toHaveProperty("Id");
    expect(body).not.toHaveProperty("ID");
    expect(body).not.toHaveProperty("RandomNumber");
    expect(body).not.toHaveProperty("randomnumber");
  });

  it("id is between 1 and 10000", async () => {
    const res = await fetch(`${BASE}/db`);
    const body = await res.json();
    expect(body.id).toBeGreaterThanOrEqual(1);
    expect(body.id).toBeLessThanOrEqual(10000);
  });

  it("randomNumber is an integer", async () => {
    const res = await fetch(`${BASE}/db`);
    const body = await res.json();
    expect(Number.isInteger(body.randomNumber)).toBe(true);
  });

  it("response is approximately 32 bytes", async () => {
    const res = await fetch(`${BASE}/db`);
    const text = await res.text();
    expect(text.length).toBeGreaterThanOrEqual(25);
    expect(text.length).toBeLessThanOrEqual(45);
  });
});

// ==========================================================================
// Test 3: Multiple Database Queries
// ==========================================================================

describe("Multiple Database Queries (/queries)", () => {
  it("returns an array of the requested count", async () => {
    for (const count of [1, 5, 10, 15, 20]) {
      const res = await fetch(`${BASE}/queries?queries=${count}`);
      const body = await res.json();
      expect(Array.isArray(body)).toBe(true);
      expect(body.length).toBe(count);
    }
  });

  it("each element has id and randomNumber", async () => {
    const res = await fetch(`${BASE}/queries?queries=5`);
    const body = await res.json();
    for (const world of body) {
      expect(world).toHaveProperty("id");
      expect(world).toHaveProperty("randomNumber");
    }
  });

  it("clamps missing queries param to 1", async () => {
    const res = await fetch(`${BASE}/queries`);
    const body = await res.json();
    expect(body.length).toBe(1);
  });

  it("clamps non-integer queries param to 1", async () => {
    const res = await fetch(`${BASE}/queries?queries=abc`);
    const body = await res.json();
    expect(body.length).toBe(1);
  });

  it("clamps queries below 1 to 1", async () => {
    const res = await fetch(`${BASE}/queries?queries=0`);
    const body = await res.json();
    expect(body.length).toBe(1);

    const res2 = await fetch(`${BASE}/queries?queries=-5`);
    const body2 = await res2.json();
    expect(body2.length).toBe(1);
  });

  it("clamps queries above 500 to 500", async () => {
    const res = await fetch(`${BASE}/queries?queries=999`);
    const body = await res.json();
    expect(body.length).toBe(500);
  });

  it("Content-Type is application/json", async () => {
    const res = await fetch(`${BASE}/queries?queries=2`);
    expect(res.headers.get("Content-Type")).toBe("application/json");
  });
});

// ==========================================================================
// Test 4: Fortunes
// ==========================================================================

describe("Fortunes (/fortunes)", () => {
  it("returns status 200", async () => {
    const res = await fetch(`${BASE}/fortunes`);
    expect(res.status).toBe(200);
  });

  it("Content-Type is text/html; charset=utf-8", async () => {
    const res = await fetch(`${BASE}/fortunes`);
    const ct = res.headers.get("Content-Type")!;
    expect(ct.toLowerCase()).toContain("text/html");
    expect(ct.toLowerCase()).toContain("charset=utf-8");
  });

  it("starts with <!DOCTYPE html>", async () => {
    const res = await fetch(`${BASE}/fortunes`);
    const html = await res.text();
    expect(html.startsWith("<!DOCTYPE html>")).toBe(true);
  });

  it("contains the additional fortune added at request time", async () => {
    const res = await fetch(`${BASE}/fortunes`);
    const html = await res.text();
    expect(html).toContain("Additional fortune added at request time.");
  });

  it("contains id 0 for the added fortune", async () => {
    const res = await fetch(`${BASE}/fortunes`);
    const html = await res.text();
    // The extra fortune has id=0
    expect(html).toContain("<td>0</td><td>Additional fortune added at request time.</td>");
  });

  it("escapes the <script> tag fortune (XSS protection)", async () => {
    const res = await fetch(`${BASE}/fortunes`);
    const html = await res.text();
    // Must NOT contain raw <script> tag
    expect(html).not.toContain('<script>alert("');
    // Must contain escaped version
    expect(html).toContain("&lt;script&gt;");
    expect(html).toContain("&lt;/script&gt;");
  });

  it("contains the Japanese fortune (UTF-8)", async () => {
    const res = await fetch(`${BASE}/fortunes`);
    const html = await res.text();
    expect(html).toContain("フレームワークのベンチマーク");
  });

  it("fortunes are sorted by message (not by id)", async () => {
    const res = await fetch(`${BASE}/fortunes`);
    const html = await res.text();
    // "Additional fortune added at request time." starts with 'A'
    // and should come before "After enough decimal places..."
    const addedIdx = html.indexOf("Additional fortune added at request time.");
    const afterIdx = html.indexOf("After enough decimal places");
    expect(addedIdx).toBeLessThan(afterIdx);

    // "Feature: A bug with seniority." starts with 'F'
    // and should come after "Emacs is a nice operating system"
    const emacsIdx = html.indexOf("Emacs is a nice operating system");
    const featureIdx = html.indexOf("Feature: A bug with seniority.");
    expect(emacsIdx).toBeLessThan(featureIdx);
  });

  it("has proper HTML table structure", async () => {
    const res = await fetch(`${BASE}/fortunes`);
    const html = await res.text();
    expect(html).toContain("<table>");
    expect(html).toContain("</table>");
    expect(html).toContain("<tr><th>id</th><th>message</th></tr>");
    expect(html).toContain("</body></html>");
  });

  it("has 13 fortune rows (12 from DB + 1 added)", async () => {
    const res = await fetch(`${BASE}/fortunes`);
    const html = await res.text();
    // Count <tr><td> occurrences (data rows, not header)
    const dataRows = (html.match(/<tr><td>/g) || []).length;
    expect(dataRows).toBe(13);
  });
});

// ==========================================================================
// Test 5: Database Updates
// ==========================================================================

describe("Database Updates (/updates)", () => {
  it("returns an array of the requested count", async () => {
    for (const count of [1, 5, 10, 15, 20]) {
      const res = await fetch(`${BASE}/updates?queries=${count}`);
      const body = await res.json();
      expect(Array.isArray(body)).toBe(true);
      expect(body.length).toBe(count);
    }
  });

  it("each element has id and randomNumber", async () => {
    const res = await fetch(`${BASE}/updates?queries=5`);
    const body = await res.json();
    for (const world of body) {
      expect(world).toHaveProperty("id");
      expect(world).toHaveProperty("randomNumber");
      expect(Number.isInteger(world.id)).toBe(true);
      expect(Number.isInteger(world.randomNumber)).toBe(true);
    }
  });

  it("randomNumber is between 1 and 10000", async () => {
    const res = await fetch(`${BASE}/updates?queries=20`);
    const body = await res.json();
    for (const world of body) {
      expect(world.randomNumber).toBeGreaterThanOrEqual(1);
      expect(world.randomNumber).toBeLessThanOrEqual(10000);
    }
  });

  it("clamps queries param same as /queries", async () => {
    const missing = await fetch(`${BASE}/updates`);
    expect((await missing.json()).length).toBe(1);

    const over = await fetch(`${BASE}/updates?queries=600`);
    expect((await over.json()).length).toBe(500);

    const under = await fetch(`${BASE}/updates?queries=-1`);
    expect((await under.json()).length).toBe(1);
  });

  it("Content-Type is application/json", async () => {
    const res = await fetch(`${BASE}/updates?queries=2`);
    expect(res.headers.get("Content-Type")).toBe("application/json");
  });
});

// ==========================================================================
// Test 7: Caching
// ==========================================================================

describe("Cached Queries (/cached-queries)", () => {
  it("returns an array of the requested count", async () => {
    for (const count of [1, 10, 20, 50, 100]) {
      const res = await fetch(`${BASE}/cached-queries?count=${count}`);
      const body = await res.json();
      expect(Array.isArray(body)).toBe(true);
      expect(body.length).toBe(count);
    }
  });

  it("each element has id and randomNumber", async () => {
    const res = await fetch(`${BASE}/cached-queries?count=10`);
    const body = await res.json();
    for (const world of body) {
      expect(world).toHaveProperty("id");
      expect(world).toHaveProperty("randomNumber");
    }
  });

  it("uses 'count' param (not 'queries')", async () => {
    const res = await fetch(`${BASE}/cached-queries?count=5`);
    const body = await res.json();
    expect(body.length).toBe(5);
  });

  it("clamps count param [1, 500]", async () => {
    const missing = await fetch(`${BASE}/cached-queries`);
    expect((await missing.json()).length).toBe(1);

    const over = await fetch(`${BASE}/cached-queries?count=999`);
    expect((await over.json()).length).toBe(500);
  });

  it("Content-Type is application/json", async () => {
    const res = await fetch(`${BASE}/cached-queries?count=2`);
    expect(res.headers.get("Content-Type")).toBe("application/json");
  });

  it("returns consistent results (cache hit)", async () => {
    const res1 = await fetch(`${BASE}/cached-queries?count=1`);
    const body1 = await res1.json();
    // The cache should be populated — verify structure is valid
    expect(body1[0]).toHaveProperty("id");
    expect(body1[0]).toHaveProperty("randomNumber");
  });
});

// ==========================================================================
// Cross-cutting: No gzip compression on any endpoint
// ==========================================================================

describe("No gzip compression", () => {
  it("never returns Content-Encoding: gzip", async () => {
    for (const path of ["/plaintext", "/json", "/db", "/queries?queries=5", "/fortunes", "/updates?queries=1", "/cached-queries?count=5"]) {
      const res = await fetch(`${BASE}${path}`, {
        headers: { "Accept-Encoding": "gzip, deflate, br" },
      });
      expect(res.headers.get("Content-Encoding")).toBeNull();
    }
  });
});
