
from wheezy.http import HTTPResponse
from wheezy.http import WSGIApplication
from wheezy.routing import url
from wheezy.web.handlers import BaseHandler
from wheezy.web.middleware import bootstrap_defaults
from wheezy.web.middleware import path_routing_middleware_factory

class JsonHandler(BaseHandler):

  def get(self):
    return self.json_response({"message": "Hello, world!"})

def plaintext(request):
  response = HTTPResponse()
  response.write("Hello, world!")
  return response

all_urls = [
  url("plaintext", plaintext, name="plaintext"),
  url("json", JsonHandler, name="json")
]

options = {}

main = WSGIApplication(
  middleware = [
    bootstrap_defaults(url_mapping=all_urls),
    path_routing_middleware_factory
  ],
  options = options
)

if __name__ == "__main__":
  from wsgiref.simple_server import make_server
  try:
    make_server("", 8080, main).serve_forever()
  except KeyboardInterrupt:
    pass
