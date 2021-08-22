import { ServerRequest, SERVER, dyn_date, MIME_JSON } from "../../depends.ts";
import { updateQuery, fillArrayWithFn, resolveQueryNumber } from "./database.ts";

export const headers = new Headers([
  ["server", SERVER],
  ["content-type", MIME_JSON],
]);

export default async (req: ServerRequest): Promise<void> => {
  const u = new URL(req.url, "http://deno");
  const l = resolveQueryNumber(u.searchParams.get("queries") ?? "1");
  const rnd = await Promise.all(await fillArrayWithFn(() => updateQuery(), l));
  headers.set("date", dyn_date());
  req.respond({
    headers,
    body: JSON.stringify(rnd),
  });
};
