import { dynDate, MIME_JSON, SERVER } from "../../depends.ts";
import { randomWorld } from "./database.ts";

export const headers = new Headers([
  ["server", SERVER],
  ["content-type", MIME_JSON],
]);

export default async (_req: Request): Promise<Response> => {
  const rnd = await randomWorld();
  headers.set("date", dynDate());
  return new Response(JSON.stringify(rnd), { headers });
};
