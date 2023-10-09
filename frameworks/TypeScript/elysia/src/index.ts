import { Elysia } from 'elysia';
import dbHandlers from './db-handlers';

const app = new Elysia({
  serve: {
    // @ts-ignore
    reusePort: true,
  },
})
  .onAfterHandle(({ set }) => {
    set.headers['server'] = 'Elysia';
  })
  .decorate('HELLO_WORLD_STR', 'Hello, World!')
  .get('/plaintext', ({ HELLO_WORLD_STR }) => HELLO_WORLD_STR)
  .get('/json', ({ HELLO_WORLD_STR }) => ({ message: HELLO_WORLD_STR }));

if (process.env.DATABASE) {
  app.use(dbHandlers);
}

app.listen(8080);

console.info(
  `🦊 Elysia is running at ${app.server?.hostname}:${app.server?.port}`
);
