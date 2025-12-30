import express from 'ultimate-express';
import { LRUCache } from 'lru-cache';
import cluster, { isWorker } from 'node:cluster';
import { maxQuery, maxRows } from './config.js';
import fjs from 'fast-json-stringify';

const { DATABASE } = process.env;
const db = DATABASE ? await import(`./database/${DATABASE}.js`) : null;

const jsonSerializer = fjs({
  type: 'object',
  properties: {
    message: {
      type: 'string',
      format: 'unsafe',
    }
  }
});

const generateRandomNumber = () => ((Math.random() * maxRows) | 0) + 1;

const parseQueries = (i) => i > maxQuery ? maxQuery : (i | 0) || 1;

const escapeHTMLRules = { '&': '&#38;', '<': '&#60;', '>': '&#62;', '"': '&#34;', "'": '&#39;', '/': '&#47;' };

const unsafeHTMLMatcher = /[&<>"'\/]/g;

const escapeHTMLCode = (text) => unsafeHTMLMatcher.test(text) ? text.replace(unsafeHTMLMatcher,  m => escapeHTMLRules[m] || m) : text;

const cache = new LRUCache({
  max: maxRows
});

const app = express();
app.set("etag", false);
app.set("x-powered-by", false);

app.get('/plaintext', (req, res) => {
  res.setHeader('Server', 'UltimateExpress');
  res.setHeader('Content-Type', 'text/plain');
  res.end('Hello, World!');
});

app.get('/json', (req, res) => {
  res.setHeader('Server', 'UltimateExpress');
  res.setHeader('Content-Type', 'application/json');
  res.end(jsonSerializer({ message: "Hello, World!" }));
});

if (db) {
  app.get('/db', async (req, res) => {
    res.setHeader('Server', 'UltimateExpress');
    const world = await db.find(generateRandomNumber());
    res.json(world);
  });

  app.get('/queries', async (req, res) => {
    res.setHeader('Server', 'UltimateExpress');

    const queries = parseQueries(req.query.queries);
    const worldPromises = new Array(queries);

    for (let i = 0; i < queries; i++) {
      worldPromises[i] = db.find(generateRandomNumber());
    }

    const worlds = await Promise.all(worldPromises);

    res.json(worlds);
  })

  app.get('/updates', async (req, res) => {
    res.setHeader('Server', 'UltimateExpress');

    const queries = parseQueries(req.query.queries);
    const worldPromises = new Array(queries);

    for (let i = 0; i < queries; i++) {
      worldPromises[i] = db.find(generateRandomNumber());
    }

    const worlds = await Promise.all(worldPromises);

    for (let i = 0; i < queries; i++) {
      worlds[i].randomNumber = generateRandomNumber();
    }

    await db.bulkUpdate(worlds);

    res.json(worlds);
  })

  app.get('/fortunes', async (req, res) => {
    res.setHeader('Server', 'UltimateExpress');

    const fortunes = await db.fortunes()

    fortunes.push({ id: 0, message: 'Additional fortune added at request time.' });

    fortunes.sort((a, b) => (a.message < b.message) ? -1 : 1);

    const n = fortunes.length

    let i = 0, html = ''
    for (; i < n; i++) html += `<tr><td>${fortunes[i].id}</td><td>${escapeHTMLCode(fortunes[i].message)}</td></tr>`

    res
      .header('Content-Type', 'text/html; charset=utf-8')
      .end(`<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>${html}</table></body></html>`);
  })

  let isCachePopulated = false
  app.get('/cached-worlds', async (req, res) => {
    res.setHeader('Server', 'UltimateExpress');

    if (!isCachePopulated) {
      const worlds = await db.getAllWorlds();
      for (let i = 0; i < worlds.length; i++) {
        cache.set(worlds[i].id, worlds[i]);
      }
      isCachePopulated = true;
    }
    const count = parseQueries(req.query.count);
    const worlds = new Array(count);

    for (let i = 0; i < count; i++) {
      worlds[i] = cache.get(generateRandomNumber());
    }

    res.json(worlds);
  });
}

app.listen(8080, () => {
  console.log(`${isWorker ? `${cluster.worker.id}: ` : ''}Successfully bound to http://0.0.0.0:8080`);
});