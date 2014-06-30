require 'leda.http_server'

with HTTPServer!
    .port = 8080
    .host = ''
    .on_request = (server, request, response)  ->
        if request.url\find("/json")
            response.body = {message: 'Hello, World!'}

        if request.url\find("/plaintext")
            response.body = 'Hello, World! '

    \start!
