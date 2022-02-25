import {
  ServerRequest,
  SERVER,
  dyn_date,
  MIME_TEXT,
  HELLO_WORLD,
} from "../depends.ts";

const headers = new Headers([
  ["server", SERVER],
  ["content-type", MIME_TEXT],
]);

export default async (req: ServerRequest): Promise<void> => {
  headers.set("date", dyn_date());
  req.respond({ headers, body: HELLO_WORLD });
};
