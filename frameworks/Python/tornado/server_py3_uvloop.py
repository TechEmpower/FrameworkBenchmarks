#!/usr/bin/env python

from os import getenv
from operator import itemgetter
from functools import partial
from random import randint

import ujson as json
import uvloop
import asyncpg
import tornado.ioloop
import tornado.web
import tornado.httpserver
from tornado.options import options

options.define('port', default=8080, type=int, help="Server port")

READ_ROW_SQL = 'SELECT "randomnumber", "id" FROM "world" WHERE id = $1'
WRITE_ROW_SQL = 'UPDATE "world" SET "randomnumber"=$1 WHERE id=$2'
ADDITIONAL_ROW = [0, 'Additional fortune added at request time.']

sort_fortunes_key = itemgetter(1)


async def init_db_pool():
    return await asyncpg.create_pool(
        host="tfb-database",
        user="benchmarkdbuser",
        password="benchmarkdbpass",
        database="hello_world")


def get_num_queries(request):
    try:
        query_count = int(request.get_query_argument('queries'))
    except (KeyError, IndexError, ValueError):
        return 1

    if query_count < 1:
        return 1
    if query_count > 500:
        return 500
    return query_count


class JsonHandler(tornado.web.RequestHandler):
    def set_default_headers(self):
        self.set_header("Content-Type", "application/json")

    def write_response(self, data):
        self.write(json.dumps(data))


class JsonHelloWorldHandler(JsonHandler):
    def get(self):
        self.write_response({"message": "Hello, World!"})


class PlaintextHelloWorldHandler(tornado.web.RequestHandler):
    def get(self):
        self.set_header("Content-Type", "text/plain")
        self.write(b'Hello, world!')


class SingleQueryHandler(JsonHandler):
    async def get(self):
        row_id = randint(1, 10000)
        async with self.application.pool.acquire() as connection:
            number = await connection.fetchval(READ_ROW_SQL, row_id)

        self.write_response({'id': row_id, 'randomNumber': number})


class MultipleQueriesHandler(JsonHandler):
    async def get(self):
        num_queries = get_num_queries(self)
        row_ids = [randint(1, 10000) for _ in range(num_queries)]
        worlds = []

        async with self.application.pool.acquire() as connection:
            statement = await connection.prepare(READ_ROW_SQL)
            for row_id in row_ids:
                number = await statement.fetchval(row_id)
                worlds.append({'id': row_id, 'randomNumber': number})
        self.write_response(worlds)


class UpdateHandler(JsonHandler):
    async def get(self):
        num_queries = get_num_queries(self)
        updates = [(randint(1, 10000), randint(1, 10000)) for _ in range(num_queries)]
        worlds = [{'id': row_id, 'randomNumber': number} for row_id, number in updates]

        async with self.application.pool.acquire() as connection:
            statement = await connection.prepare(READ_ROW_SQL)
            for row_id, number in updates:
                await statement.fetchval(row_id)
            await connection.executemany(WRITE_ROW_SQL, updates)

        self.write_response(worlds)


class FortuneHandler(tornado.web.RequestHandler):
    async def get(self):
        async with self.application.pool.acquire() as connection:
            fortunes = await connection.fetch('SELECT * FROM Fortune')

        fortunes.append(ADDITIONAL_ROW)
        fortunes.sort(key=sort_fortunes_key)

        self.set_header("Content-Type", "text/html; charset=UTF-8")
        self.render('fortunes_asyncpg.html', fortunes=fortunes)


def make_app():
    return tornado.web.Application([
        (r"/json", JsonHelloWorldHandler),
        (r"/plaintext", PlaintextHelloWorldHandler),
        (r"/db", SingleQueryHandler),
        (r"/queries", MultipleQueriesHandler),
        (r"/updates", UpdateHandler),
        (r"/fortunes", FortuneHandler),
    ],
        template_path="templates"
    )


async def setup_database():
    return await asyncpg.create_pool(
        user=getenv('PGUSER', 'benchmarkdbuser'),
        password=getenv('PGPASS', 'benchmarkdbpass'),
        database='hello_world',
        host='tfb-database',
        port=5432
    )

if __name__ == "__main__":
    options.parse_command_line()
    uvloop.install()

    app = make_app()
    server = tornado.httpserver.HTTPServer(app)
    server.bind(options.port)
    server.start(0)

    ioloop = tornado.ioloop.IOLoop.current()
    app.pool = ioloop.run_sync(partial(setup_database))

    ioloop.start()
