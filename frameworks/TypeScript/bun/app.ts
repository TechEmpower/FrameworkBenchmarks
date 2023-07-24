import * as db from './database/postgres.ts';
import { headerPlainText, headerJSON, headerHTML } from './headers.ts';
import { parseQueries, generateRandomNumber } from './utils.ts';

const urlParser = require('fast-url-parser');

const servePlainText = () => new Response('Hello, World!', headerPlainText);

const serveJSON = () =>
  new Response(JSON.stringify({ message: 'Hello, World!' }), headerJSON);

const serveDB = async () => {
  const world = await db.find(generateRandomNumber());
  return new Response(JSON.stringify(world), headerJSON);
};

const serveFortune = async () => {
  const fortunes = await db.fortunes();

  fortunes.push({
    id: 0,
    message: 'Additional fortune added at request time.',
  });

  fortunes.sort((a, b) => a.message.localeCompare(b.message));

  let i = 0,
    html = '';
  for (; i < fortunes.length; i++)
    html += `<tr><td>${fortunes[i].id}</td><td>${Bun.escapeHTML(
      fortunes[i].message
    )}</td></tr>`;

  return new Response(
    `<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>${html}</table></body></html>`,
    headerHTML
  );
};

const serveQuery = async (n: number) => {
  const worldPromises = [];

  for (let i = 0; i < n; i++) {
    worldPromises.push(db.find(generateRandomNumber()));
  }

  const worlds = await Promise.all(worldPromises);
  return new Response(JSON.stringify(worlds), headerJSON);
};

const serveUpdate = async (n: number) => {
  const worldPromises = [];

  for (let i = 0; i < n; i++) {
    worldPromises.push(db.find(generateRandomNumber()));
  }

  const worlds = await Promise.all(worldPromises);

  await Promise.all(
    worlds.map(async (world) => {
      world.randomNumber = generateRandomNumber();
      await db.update(world);
    })
  );
  return new Response(JSON.stringify(worlds), headerJSON);
};

Bun.serve({
  async fetch(req) {
    const url = urlParser.parse(req.url);
    switch (url.pathname) {
      case '/plaintext':
        return servePlainText();
        break;
      case '/json':
        return serveJSON();
        break;
      case '/db':
        return serveDB();
        break;
      case '/fortunes':
        return serveFortune();
        break;
      case '/queries':
        return serveQuery(parseQueries(url.search));
        break;
      case '/updates':
        return serveUpdate(parseQueries(url.search));
        break;
      default:
        return new Response('Not Found', { status: 404 });
    }
  },
  port: 8080,
});
