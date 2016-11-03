local json = require "json"

ngx.header.content_type = 'application/json'
ngx.print(json.encode({message = "Hello, World!"}))