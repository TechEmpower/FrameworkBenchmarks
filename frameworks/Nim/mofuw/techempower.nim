import mofuw, packedjson

proc h(ctx: MofuwCtx) {.async.} =
  case ctx.getPath
  of "/plaintext":
    mofuwResp(HTTP200, "text/plain", "Hello, World!")
  of "/json":
    mofuwResp(HTTP200, "application/json", $(%{"message": %"Hello, World!"}))
  else:
    mofuwResp(HTTP404, "text/plain", "NOT FOUND")

newServeCtx(
  port = 8080,
  handler = h
).serve()
