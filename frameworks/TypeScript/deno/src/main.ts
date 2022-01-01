// deno-lint-ignore-file require-await
import { HttpServer } from './_server/mod.ts'

const headers = new Headers([
  ["server", 'Deno'],
  ["content-type", 'application/json'],
]);

setInterval(() => (headers.set('date', new Date().toUTCString())), 850);

export const greeting: Uint8Array = new TextEncoder().encode(
  "Hello, World!"
);

await new HttpServer()

  .add('/json', async () => new Response(JSON.stringify({ message: "Hello, World!" }), {headers}))
  .serve(Deno.listen({
    port: 8088
  }))