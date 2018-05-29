import mofuw, json

proc h(req: mofuwReq, res: mofuwRes) {.async.} =
  case req.getPath
  of "/plaintext":
    mofuwResp(HTTP200, "text/plain", "Hello, World!")
  of "/json":
    mofuwResp(HTTP200, "application/json", $(%{"message": %"Hello, World!"}))
  else:
    mofuwResp(HTTP404, "text/plain", "NOT FOUND")

h.mofuwRun(port = 8080)