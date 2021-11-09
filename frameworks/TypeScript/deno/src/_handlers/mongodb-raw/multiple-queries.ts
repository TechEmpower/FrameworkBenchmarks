import { dynDate, MIME_JSON, SERVER } from "../../depends.ts";
import {
  fillArrayWithFn,
  randomWorld,
  resolveQueryNumber,
} from "./database.ts";

export const headers = new Headers([
  ["server", SERVER],
  ["content-type", MIME_JSON],
]);

export default async (req: Request): Promise<Response> => {
  const u = new URL(req.url, "http://deno");
  const l = resolveQueryNumber(u.searchParams.get("queries") ?? "1");
  const rnd = await Promise.all(await fillArrayWithFn(() => randomWorld(), l));
  headers.set("date", dynDate());
  return new Response(JSON.stringify(rnd), { headers });
};
