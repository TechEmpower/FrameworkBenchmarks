import options, asyncdispatch, json

import httpbeast

proc onRequest(req: Request): Future[void] =
  if req.httpMethod == some(HttpGet):
    case req.path.get()
    of "/json":
      var data = $(%*{"message": "Hello, World!"})
      const headers = "Content-Type: application/json"
      req.send(Http200, data, headers)
    of "/plaintext":
      const data = "Hello, World!"
      const headers = "Content-Type: text/plain"
      req.send(Http200, data, headers)
    else:
      req.send(Http404)

run(onRequest)
