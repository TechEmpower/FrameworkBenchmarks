#!/usr/bin/env python

import sys
import json
from random import randint

import momoko
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
tornado.options.define('postgres', default=None, type=str, help="PostgreSQL host")


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


class DBTestHandler(BaseHandler):
    @gen.coroutine
    def get(self):
        world = yield db.World.find_one(randint(1, 10000))
        # Get first postion on arguments, and so first postion in mongo return
        world['id'] = str(world.pop('_id'))
        response = json.dumps(world)
        self.set_header("Content-Type", "application/json; charset=UTF-8")
        self.write(response)


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

        worlds = yield [db.World.find_one(randint(1, 10000))
                        for _ in xrange(queries)]
        for world in worlds:
            # Get first postion on arguments, and so first postion in mongo return
            world['id'] = str(world.pop('_id'))
        response = json.dumps(worlds)
        self.set_header("Content-Type", "application/json; charset=UTF-8")
        self.write(response)


class QueryPostgresRawTestHandler(BaseHandler):
    @gen.coroutine
    def get(self):
        sql = "SELECT id, randomNumber FROM World WHERE id=%s"

        random_id = randint(1, 10000)
        cursor = yield momoko.Op(
            self.application.db.execute, sql, (random_id,)
        )
        row = cursor.fetchone()
        response = json.dumps({"id": row[0], "randomNumber": row[1]})

        self.set_header("Content-Type", "application/json; charset=UTF-8")
        self.write(response)


class MultipleQueriesPostgresRawTestHandler(BaseHandler):
    @gen.coroutine
    def get(self):
        queries = self.get_argument("queries", "1")
        try:
            queries = int(queries.strip())
        except ValueError:
            queries = 1

        queries = min(max(1, queries), 500)

        sql = "SELECT id, randomNumber FROM World WHERE id=%s"

        worlds = []
        for i in xrange(int(queries)):
            random_id = randint(1, 10000)
            cursor = yield momoko.Op(
                self.application.db.execute, sql, (random_id,)
            )
            row = cursor.fetchone()
            worlds.append({"id": row[0], "randomNumber": row[1]})
        response = json.dumps(worlds)
        self.set_header("Content-Type", "application/json; charset=UTF-8")
        self.write(response)

application = tornado.web.Application([
    (r"/json", JsonSerializeTestHandler),
    (r"/plaintext", PlaintextHandler),
    (r"/db", DBTestHandler),
    (r"/queries", QueryTestHandler),
    (r"/dbraw", QueryPostgresRawTestHandler),
    (r"/queriesraw", MultipleQueriesPostgresRawTestHandler)
])


if __name__ == "__main__":
    tornado.options.parse_command_line()
    server = tornado.httpserver.HTTPServer(application)
    server.bind(options.port)
    server.start(0)
    if options.postgres:
        dsn = "user=benchmarkdbuser password=benchmarkdbpass dbname=hello_world host=%s" % options.postgres
        application.db = momoko.Pool(dsn, size=1)
    else:
        db = motor.MotorClient(options.mongo).hello_world
    tornado.ioloop.IOLoop.instance().start()
