import json
import tornado.web


def fortunes_sort(o1, o2):
    return o1['message'] < o2['message']


class BaseHandler(tornado.web.RequestHandler):
    RANDOM_NUMBER = "randomNumber"
    ID = "id"
    QUERIES = "queries"

    def compute_etag(self):
        return None


class PlainHandler(BaseHandler):

    def set_default_headers(self):
        self.set_header("Content-Type", 'text/plain')


class JsonHandler(BaseHandler):

    def set_default_headers(self):
        self.set_header("Content-Type", "application/json; charset=UTF-8")


class PlaintextHelloWorldHandler(PlainHandler):
    HELLO_WORLD = b"Hello, World!"

    def get(self):
        self.finish(self.HELLO_WORLD)


class JsonHelloWorldHandler(JsonHandler):
    HELLO_WORLD = {"message": "Hello, World!"}

    def get(self):
        obj = json.dumps(self.HELLO_WORLD)
        self.finish(obj)
