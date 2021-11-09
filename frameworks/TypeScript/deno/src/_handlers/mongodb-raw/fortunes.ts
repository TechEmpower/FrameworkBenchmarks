import { dynDate, MIME_HTML, SERVER } from "../../depends.ts";
import {
  additionalFortune,
  FortuneData,
  generateFortunes,
  getAllFortunes,
} from "./database.ts";

export const headers = new Headers([
  ["server", SERVER],
  ["content-type", MIME_HTML],
]);

export default async (_: Request): Promise<Response> => {
  const fortunes = await getAllFortunes();
  fortunes.push(additionalFortune);
  fortunes.sort((a: any, b: any) => a.message.localeCompare(b.message));
  headers.set("date", dynDate());
  return new Response(generateFortunes(fortunes as FortuneData[]), { headers });
};
