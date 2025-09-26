import { ServerRequest, SERVER, dyn_date, MIME_JSON } from "../../depends.ts";
import { randomWorld } from "./database.ts";

export const headers = new Headers([
  ["server", SERVER],
  ["content-type", MIME_JSON],
]);

export default async (req: ServerRequest): Promise<void> => {
  const rnd = await randomWorld();
  headers.set("date", dyn_date());
  req.respond({
    headers,
    body: JSON.stringify(rnd),
  });
};
