#!/usr/bin/env python
import json

import falcon


# resource endpoints

class JSONResource(object):
    def on_get(self, request, response):
        json_data = {'message': "Hello, world!"}
        response.body = json.dumps(json_data)


class PlaintextResource(object):
    def on_get(self, request, response):
        response.set_header('Content-Type', 'text/plain')
        response.body = b'Hello, world!'

# setup

app = falcon.API()
app.add_route("/json", JSONResource())
app.add_route("/plaintext", PlaintextResource())

# entry point for debugging
if __name__ == "__main__":
    from wsgiref import simple_server

    httpd = simple_server.make_server('localhost', 8080, app)
    httpd.serve_forever()
