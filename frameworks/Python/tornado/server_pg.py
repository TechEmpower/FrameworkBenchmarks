#!/usr/bin/env python
import json
from random import randint
import momoko
import tornado.ioloop
import tornado.web
from tornado import gen
import tornado.options
from tornado.options import options
import tornado.httpserver
from commons import JsonHandler, JsonHelloWorldHandler, PlaintextHelloWorldHandler, BaseHandler


tornado.options.define('port', default=8888, type=int, help="Server port")
tornado.options.define('postgres', default="localhost",
                       type=str, help="PostgreSQL host")
tornado.options.define('backlog', default=8192, type=int,
                       help="Server backlog")


class SingleQueryHandler(JsonHandler):
    SQL = "SELECT id, randomNumber FROM World WHERE id=%s"

    @gen.coroutine
    def get(self):

        random_id = randint(1, 10000)
        cursor = yield db.execute(self.SQL, (random_id,))
        row = cursor.fetchone()
        response = json.dumps({self.ID: row[0], self.RANDOM_NUMBER: row[1]})
        self.finish(response)


class MultipleQueriesHandler(JsonHandler):
    SQL = "SELECT id, randomNumber FROM World WHERE id=%s"

    @gen.coroutine
    def get(self):
        queries = self.get_argument(self.QUERIES, "1")
        try:
            queries = int(queries.strip())
        except ValueError:
            queries = 1

        queries = min(max(1, queries), 500)
        worlds = []

        cursors = yield [db.execute(self.SQL, (randint(1, 10000),)) for _ in xrange(queries)]
        for cursor in cursors:
            row = cursor.fetchone()
            worlds.append({self.ID: row[0], self.RANDOM_NUMBER: row[1]})

        response = json.dumps(worlds)
        self.finish(response)


application = tornado.web.Application([
    (r"/json", JsonHelloWorldHandler),
    (r"/plaintext", PlaintextHelloWorldHandler),
    (r"/db", SingleQueryHandler),
    (r"/queries", MultipleQueriesHandler)
],
    template_path="templates"
)
application.ui_modules = {}

if __name__ == "__main__":
    tornado.options.parse_command_line()
    server = tornado.httpserver.HTTPServer(application)
    server.bind(options.port, backlog=options.backlog)
    server.start(0)

    ioloop = tornado.ioloop.IOLoop.instance()
    dsn = "user=benchmarkdbuser password=benchmarkdbpass dbname=hello_world host=%s" % options.postgres
    db = momoko.Pool(dsn, size=100, max_size=200)
    ioloop.run_sync(db.connect)
    ioloop.start()
