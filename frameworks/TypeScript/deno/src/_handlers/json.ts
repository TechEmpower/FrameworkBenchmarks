import { ServerRequest, SERVER, dyn_date, MIME_JSON } from "../depends.ts";

export default async (req: ServerRequest): Promise<void> => {
  const headers = new Headers([
    ["server", SERVER],
    ["content-type", MIME_JSON],
    ["date", dyn_date]
  ]);

  req.respond({
    headers,
    body: JSON.stringify({ message: "Hello, World!" })
  });
  return;
};