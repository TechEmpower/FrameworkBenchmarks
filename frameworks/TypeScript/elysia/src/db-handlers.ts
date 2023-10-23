import Elysia from 'elysia';
import * as postgres from './postgres';
import { Fortune, World } from './types';

const deps = new Elysia({
  name: 'deps',
})
  .decorate('db', postgres)
  .decorate('generateRandomNumber', () => Math.ceil(Math.random() * 10000))
  .decorate('html', (fortunes: Fortune[]) => {
    const n = fortunes.length;

    let html = '';
    for (let i = 0; i < n; i++) {
      html += `<tr><td>${fortunes[i].id}</td><td>${Bun.escapeHTML(
        fortunes[i].message
      )}</td></tr>`;
    }

    return `<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>${html}</table></body></html>`;
  });

const dbHandlers = new Elysia({
  name: 'db-handlers',
})
  .use(deps)
  .get(
    '/db',
    async ({ db, generateRandomNumber }) =>
      await db.find(generateRandomNumber())
  )
  .get(
    '/fortunes',
    async ({ db, html }) => {
      const fortunes = await db.fortunes();

      fortunes.push({
        id: 0,
        message: 'Additional fortune added at request time.',
      });

      fortunes.sort((a, b) => (a.message < b.message ? -1 : 1));

      return html(fortunes);
    },
    {
      afterHandle({ set }) {
        set.headers['content-type'] = 'text/html; charset=utf-8';
      },
    }
  )
  .derive(({ query }) => ({
    numberOfObjects: Math.min(parseInt(query.queries || '1') || 1, 500),
  }))
  .get('/queries', async ({ db, generateRandomNumber, numberOfObjects }) => {
    const worldPromises = new Array<Promise<World>>(numberOfObjects);

    for (let i = 0; i < numberOfObjects; i++) {
      worldPromises[i] = db.find(generateRandomNumber());
    }

    const worlds = await Promise.all(worldPromises);

    return worlds;
  })
  .get('/updates', async ({ db, generateRandomNumber, numberOfObjects }) => {
    const worldPromises = new Array<Promise<World>>(numberOfObjects);

    for (let i = 0; i < numberOfObjects; i++) {
      worldPromises[i] = db.find(generateRandomNumber());
    }

    const worlds = await Promise.all(worldPromises);

    for (let i = 0; i < numberOfObjects; i++) {
      worlds[i].randomNumber = generateRandomNumber();
    }

    await db.bulkUpdate(worlds);

    return worlds;
  });

export default dbHandlers;
