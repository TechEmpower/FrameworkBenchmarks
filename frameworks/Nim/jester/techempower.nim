import json

import jester

settings:
  port = Port(8080)

routes:
  get "/json":
    var data = $(%*{"message": "Hello, World!"})
    resp data, "application/json"

  get "/plaintext":
    const data = "Hello, World!"
    resp data, "text/plain"
