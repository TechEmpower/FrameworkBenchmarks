import random
import sys

import json
import motor
import tornado.ioloop
import tornado.web
from tornado import gen, escape
import tornado.options
from tornado.options import options
import tornado.httpserver

PY3 = False
if sys.version_info[0] == 3:
    PY3 = True
    xrange = range

tornado.options.define('port', default=8888, type=int, help="Server port")
tornado.options.define('mongo', default='localhost', type=str, help="MongoDB host")


class BaseHandler(tornado.web.RequestHandler):
    def compute_etag(self):
        return None

class JsonSerializeTestHandler(BaseHandler):
    def get(self):
        obj = dict(message="Hello, World!")
        self.write(obj)

class PlaintextHandler(BaseHandler):
    def get(self):
        self.set_header('Content-Type', 'text/plain')
        self.write(b"Hello, World!")

class QueryTestHandler(BaseHandler):
    @tornado.web.asynchronous
    @gen.coroutine
    def get(self):
        queries = int(self.get_argument("queries", 0))

        if queries == 0:
            random_id = random.randint(1, 10000)
            world = yield motor.Op(db.World.find_one,{"_id": random_id}, fields={"_id": 1, "randomNumber": 1})
            # Get first postion on arguments, and so first postion in mongo return
            response = json.dumps(world)
        else:
            worlds = []
            for i in xrange(int(queries)):
                random_id = random.randint(1, 10000)
                world = yield motor.Op(db.World.find_one,{"_id": random_id}, fields={"_id": 1, "randomNumber": 1})
                # Get first postion on arguments, and so first postion in mongo return
                worlds.append(world)
            response = json.dumps(worlds)
        self.set_header("Content-Type", "application/json; charset=UTF-8")
        self.finish(response)

application = tornado.web.Application([
    (r"/json", JsonSerializeTestHandler),
    (r"/plaintext", PlaintextHandler),
    (r"/db", QueryTestHandler),
])

if __name__ == "__main__":
    tornado.options.parse_command_line()
    server = tornado.httpserver.HTTPServer(application)
    server.bind(options.port)
    server.start(0)
    db = motor.MotorClient(options.mongo).open_sync().hello_world
    tornado.ioloop.IOLoop.instance().start()
