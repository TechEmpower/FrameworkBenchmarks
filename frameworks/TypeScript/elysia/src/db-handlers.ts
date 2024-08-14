import Elysia from 'elysia';
import * as db from './postgres';
import { Fortune } from './types';

function rand () {
  return Math.ceil(Math.random() * 10000)
}

function parseQueriesNumber (q?: string) {
  return Math.min(parseInt(q || '1') || 1, 500)
}

function renderTemplate (fortunes: Fortune[]) {
  const n = fortunes.length;

  let html = '';
  for (let i = 0; i < n; i++) {
    html += `<tr><td>${fortunes[i].id}</td><td>${Bun.escapeHTML(
      fortunes[i].message
    )}</td></tr>`;
  }

  return `<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>${html}</table></body></html>`;
}

const dbHandlers = new Elysia({
  name: 'db-handlers',
})
  .onAfterHandle(({ set }) => {
    set.headers['server'] = 'Elysia';
  })

  .get('/db', () => db.find(rand()))

  .get('/fortunes', async ({ set }) => {
    const fortunes = await db.fortunes();

    fortunes.push({
      id: 0,
      message: 'Additional fortune added at request time.',
    });

    fortunes.sort((a, b) => (a.message < b.message ? -1 : 1));

    set.headers['content-type'] = 'text/html; charset=utf-8';
    return renderTemplate(fortunes);
  })

  .get('/queries', async ({ query }) => {
    const num = parseQueriesNumber(query.queries)
    const worldPromises = new Array(num);

    for (let i = 0; i < num; i++) {
      worldPromises[i] = db.find(rand());
    }

    return await Promise.all(worldPromises);
  })

  .get('/updates', async ({ query }) => {
    const num = parseQueriesNumber(query.queries)
    const worldPromises = new Array(num);

    for (let i = 0; i < num; i++) {
      worldPromises[i] = db.find(rand());
    }

    const worlds = await Promise.all(worldPromises);

    for (let i = 0; i < num; i++) {
      worlds[i].randomNumber = rand();
    }

    await db.bulkUpdate(worlds);
    return worlds;
  });

export default dbHandlers;
