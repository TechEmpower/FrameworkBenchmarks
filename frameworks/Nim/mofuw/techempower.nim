import mofuw

proc h(req: mofuwReq, res: mofuwRes) {.async.} =
  if req.getPath == "/plaintext":
    mofuwResp(HTTP200, "text/plain", "Hello, World!")
  else:
    mofuwResp(HTTP200, "text/plain", "404 NOT FOUND")

h.mofuwRun(port = 8080, bufSize = 512)