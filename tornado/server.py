#!/usr/bin/env python

import sys
import json
from random import randint

import motor
import tornado.ioloop
import tornado.web
from tornado import gen
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
        obj = {"message": "Hello, World!", }
        self.write(obj)


class PlaintextHandler(BaseHandler):
    def get(self):
        self.set_header('Content-Type', 'text/plain')
        self.write(b"Hello, World!")


class QueryTestHandler(BaseHandler):
    @gen.coroutine
    def get(self):
        try:
            queries = int(self.get_argument("queries"))
        except Exception:
            queries = 1
        else:
            if queries < 1:
                queries = 1
            elif queries > 500:
                queries = 500

        worlds = yield [motor.Op(db.World.find_one, randint(1, 10000))
                        for _ in xrange(queries)]
        for world in worlds:
            # Get first postion on arguments, and so first postion in mongo return
            world['id'] = str(world.pop('_id'))
        response = json.dumps(worlds)
        self.set_header("Content-Type", "application/json; charset=UTF-8")
        self.write(response)


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
