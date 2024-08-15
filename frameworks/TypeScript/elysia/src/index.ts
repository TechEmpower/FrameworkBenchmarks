import { Elysia } from 'elysia';
import dbHandlers from './db-handlers';

const app = new Elysia({
  serve: {
    reusePort: true,
  },
})
  .get('/plaintext', ({ set }) => {
    set.headers['server'] = 'Elysia';
    return 'Hello, World!';
  })

  .get('/json', ({ set }) => {
    set.headers = {
      'content-type': 'application/json',
      'server': 'Elysia',
    };

    return JSON.stringify({ message: 'Hello, World!' });
  });

if (Bun.env.DATABASE) {
  app.use(dbHandlers);
}

app.listen(8080);

console.info(`🦊 Elysia is running at ${app.server!.url}`);
