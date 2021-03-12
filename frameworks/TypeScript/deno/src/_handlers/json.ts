import { ServerRequest, SERVER, dyn_date, MIME_JSON } from "../depends.ts";

export const headers = new Headers([
  ["server", SERVER],
  ["content-type", MIME_JSON],
]);

export default async (req: ServerRequest): Promise<void> => {
  const HELLO_OBJ = { message: "Hello, World!" };
  headers.set("date", dyn_date());
  req.respond({
    headers,
    body: JSON.stringify(HELLO_OBJ),
  });
  return;
};
