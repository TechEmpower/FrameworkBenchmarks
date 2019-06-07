import ujson as json
import tornado.web


class BaseHandler(tornado.web.RequestHandler):
    RANDOM_NUMBER = "randomNumber"
    ID = "id"
    QUERIES = "queries"

    def compute_etag(self):
        return None


class PlainHandler(BaseHandler):
    def set_default_headers(self):
        self.set_header("Content-Type", "text/plain")

class HtmlHandler(BaseHandler):

    def set_default_headers(self):
        self.set_header("Content-Type", "text/html; charset=UTF-8")

class JsonHandler(BaseHandler):
    def set_default_headers(self):
        self.set_header("Content-Type", "application/json")


class PlaintextHelloWorldHandler(PlainHandler):
    HELLO_WORLD = b"Hello, World!"

    def get(self):
        self.finish(self.HELLO_WORLD)


class JsonHelloWorldHandler(JsonHandler):
    def get(self):
        obj = json.dumps({"message": "Hello, World!"})
        self.finish(obj)
