import { ServerRequest, SERVER, dyn_date, MIME_HTML } from "../../depends.ts";
import {
  getAllFortunes,
  additionalFortune,
  generateFortunes,
  FortuneData,
} from "./_db_helpers.ts";

export const headers = new Headers([
  ["server", SERVER],
  ["content-type", MIME_HTML],
  ["date", dyn_date],
]);

export default async (req: ServerRequest): Promise<void> => {
  let fortunes = await getAllFortunes();
  fortunes.push(additionalFortune);
  fortunes.sort((a: any, b: any) => a.message.localeCompare(b.message));
  req.respond({
    headers,
    body: generateFortunes(fortunes as FortuneData[]),
  });
};
