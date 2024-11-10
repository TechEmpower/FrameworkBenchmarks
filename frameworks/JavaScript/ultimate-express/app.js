import express from 'ultimate-express';
import { LRUCache } from 'lru-cache';
import cluster, { isWorker } from 'node:cluster';
import { maxQuery, maxRows } from './config.js';
import { sjs, attr } from 'slow-json-stringify'

const { DATABASE } = process.env;
const db = DATABASE ? await import(`./database/${DATABASE}.js`) : null;

const jsonSerializer = sjs({ message: attr("string")}); 

const generateRandomNumber = () => Math.floor(Math.random() * maxRows) + 1;

const parseQueries = (i) => Math.min(parseInt(i) || 1, maxQuery);

const escapeHTMLRules = { '&': '&#38;', '<': '&#60;', '>': '&#62;', '"': '&#34;', "'": '&#39;', '/': '&#47;' };

const unsafeHTMLMatcher = /[&<>"'\/]/g;

const escapeHTMLCode = (text) => unsafeHTMLMatcher.test(text) ? text.replace(unsafeHTMLMatcher, function (m) { return escapeHTMLRules[m] || m; }) : text;

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

    try {
      const world = await db.find(generateRandomNumber());
      res.json(world);
    } catch (error) {
      throw error;
    }
  });

  app.get('/queries', async (req, res) => {
    res.setHeader('Server', 'UltimateExpress');

    try {
      const queries = parseQueries(req.query.queries);
      const worldPromises = new Array(queries);

      for (let i = 0; i < queries; i++) {
        worldPromises[i] = db.find(generateRandomNumber());
      }

      const worlds = await Promise.all(worldPromises);

      res.json(worlds);
    } catch (error) {
      throw error;
    }
  })

  app.get('/updates', async (req, res) => {
    res.setHeader('Server', 'UltimateExpress');

    try {
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
    } catch (error) {
      throw error;
    }
  })

  app.get('/fortunes', async (req, res) => {
    res.setHeader('Server', 'UltimateExpress');

    try {
      const fortunes = await db.fortunes()

      fortunes.push({ id: 0, message: 'Additional fortune added at request time.' });

      fortunes.sort((a, b) => (a.message < b.message) ? -1 : 1);

      const n = fortunes.length

      let i = 0, html = ''
      for (; i < n; i++) html += `<tr><td>${fortunes[i].id}</td><td>${escapeHTMLCode(fortunes[i].message)}</td></tr>`

      res
        .header('Content-Type', 'text/html; charset=utf-8')
        .end(`<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>${html}</table></body></html>`);
    } catch (error) {
      throw error;
    }
  })

  let isCachePopulated = false
  app.get('/cached-worlds', async (req, res) => {
    res.setHeader('Server', 'UltimateExpress');

    try {
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
    } catch (error) {
      throw error;
    }
  });
}

app.listen(8080, () => {
  console.log(`${isWorker ? `${cluster.worker.id}: ` : ''}Successfully bound to http://0.0.0.0:8080`);
});