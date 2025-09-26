import { ServerRequest, SERVER, dyn_date, MIME_HTML } from "../../depends.ts";
import {
  getAllFortunes,
  additionalFortune,
  generateFortunes,
  FortuneData,
} from "./database.ts";

export const headers = new Headers([
  ["server", SERVER],
  ["content-type", MIME_HTML],
]);

export default async (req: ServerRequest): Promise<void> => {
  let fortunes = await getAllFortunes();
  fortunes.push(additionalFortune);
  fortunes.sort((a: any, b: any) => a.message.localeCompare(b.message));
  headers.set("date", dyn_date());
  req.respond({
    headers,
    body: generateFortunes(fortunes as FortuneData[]),
  });
};
