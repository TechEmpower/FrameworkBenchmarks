import { ServerRequest, SERVER, dyn_date, MIME_TEXT, HELLO_WORLD } from "../depends.ts";

export default async (req: ServerRequest): Promise<void> => {
  const headers = new Headers([
    ["server", SERVER],
    ["content-type", MIME_TEXT],
    ["date", dyn_date]
  ]);

  req.respond({ headers, body: HELLO_WORLD });
};