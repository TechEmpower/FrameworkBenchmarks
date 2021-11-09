import { dynDate, MIME_JSON, SERVER } from "../depends.ts";

export const headers = new Headers([
  ["server", SERVER],
  ["content-type", MIME_JSON],
]);

export default (_: Request): Promise<Response> => {
  const HELLO_OBJ = { message: "Hello, World!" };
  headers.set("date", dynDate());
  return Promise.resolve(new Response(JSON.stringify(HELLO_OBJ), { headers }));
};
