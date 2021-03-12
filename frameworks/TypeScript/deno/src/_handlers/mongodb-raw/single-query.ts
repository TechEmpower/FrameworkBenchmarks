import { ServerRequest, SERVER, dyn_date, MIME_JSON } from "../../depends.ts";
import { randomWorld } from "./_db_helpers.ts";

export const headers = new Headers([
  ["server", SERVER],
  ["content-type", MIME_JSON],
  ["date", dyn_date],
]);

export default async (req: ServerRequest): Promise<void> => {
  const rnd = await randomWorld();
  req.respond({
    headers,
    body: JSON.stringify(rnd),
  });
};
