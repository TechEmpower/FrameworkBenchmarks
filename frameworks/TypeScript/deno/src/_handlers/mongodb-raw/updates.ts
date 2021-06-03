import { ServerRequest, SERVER, dyn_date, MIME_JSON } from "../../depends.ts";
import { updateQuery, fillArray } from "./_db_helpers.ts";

export const headers = new Headers([
  ["server", SERVER],
  ["content-type", MIME_JSON],
]);

export default async (req: ServerRequest): Promise<void> => {
  const u = new URL(req.url, "http://deno");
  const l = Number(u.searchParams.get("queries"));
  const rnd = await Promise.all(await fillArray(updateQuery(), l));
  headers.set("date", dyn_date());
  req.respond({
    headers,
    body: JSON.stringify(rnd),
  });
};
