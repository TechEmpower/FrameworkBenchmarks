import { dynDate, HELLO_WORLD, MIME_TEXT, SERVER } from "../depends.ts";

const headers = new Headers([
  ["server", SERVER],
  ["content-type", MIME_TEXT],
]);

export default (_: Request): Promise<Response> => {
  headers.set("date", dynDate());
  return Promise.resolve(new Response(HELLO_WORLD, { headers }));
};
