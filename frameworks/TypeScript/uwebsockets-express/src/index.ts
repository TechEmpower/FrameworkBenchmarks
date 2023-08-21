import uWS from 'uWebSockets.js';
import { Application } from 'uwebsockets-express';
import db from './db/postgresql';
import { generateRandomNumber, parseQuery } from './utils';
import { fileURLToPath } from 'url';
import path, { dirname } from 'path';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

const uwsApp = uWS.App();
const app = new Application(uwsApp);

app.set('views', path.join(__dirname, 'views'));
app.set('view engine', 'ejs');

app.use((_request, response, next) => {
  response.header('Server', 'uwsexpress');
  return next();
});

app.get('/plaintext', (_request, response) => {
  response.type('text').send('Hello, World!');
});

app.get('/json', (_request, response) => {
  response.json({ message: 'Hello, World!' });
});

app.get('/db', async (_request, response) => {
  try {
    const world = await db.find(generateRandomNumber());
    response.json(world);
  } catch (error) {
    throw error;
  }
});

app.get('/queries', async (request, response) => {
  try {
    const n = parseQuery(request.query.n);
    const worlds = await Promise.all(
      Array.from({ length: n }, () => db.find(generateRandomNumber()))
    );
    response.json(worlds);
  } catch (error) {
    throw error;
  }
});

app.get('/updates', async (request, response) => {
  try {
    const n = parseQuery(request.query.n);
    const worldPromises = Array.from({ length: n }, () =>
      db.find(generateRandomNumber())
    );

    const worlds = await Promise.all(worldPromises);

    for (let i = 0; i < n; i++) {
      worlds[i].randomnumber = generateRandomNumber();
      worldPromises[i] = db.update(worlds[i]);
    }

    await Promise.all(worldPromises);
    return response.json(worlds);
  } catch (error) {
    throw error;
  }
});

app.get('/fortunes', async (_request, response) => {
  try {
    const fortunes = await db.allFortune();
    fortunes.push({
      id: 0,
      message: 'Additional fortune added at request time.',
    });
    fortunes.sort((a, b) => {
      if (a.message === null) {
        return -1;
      }
      if (b.message === null) {
        return 1;
      }
      return a.message < b.message ? -1 : 1;
    });
    response.setHeader('Content-Type', 'text/html; charset=UTF-8');
    response.render('fortunes.ejs', { fortunes });
  } catch (error) {
    throw error;
  }
});

app.listen(8080, () => {
  console.log('Server listening to port 8080.');
});
