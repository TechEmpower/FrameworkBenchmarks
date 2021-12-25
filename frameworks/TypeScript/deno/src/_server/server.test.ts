import { HttpServer } from './server.ts';

const server = new HttpServer({
  port: 3008
})

server.add("/", async (ev, _) => {
  await ev.respondWith(new Response("Hello, world"))
})

await server.listen()