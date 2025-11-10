import { Context, Status } from "oak";
import { ResponseBody } from "oak/response";

export function Ok(ctx: Context, body: ResponseBody) {
  ctx.response.status = Status.OK;
  ctx.response.body = body;
  return;
}

export function NotFound(ctx: Context) {
  ctx.response.status = Status.NotFound;
  ctx.response.body = "Not found";
  return;
}
