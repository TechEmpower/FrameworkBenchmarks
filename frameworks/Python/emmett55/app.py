import os
from operator import itemgetter
from random import randint, sample

import asyncpg
from emmett55 import App, Pipe, current, request, response
from emmett55.extensions import Extension, Signals, listen_signal
from emmett55.tools import service
from renoir import Renoir


class AsyncPG(Extension):
    __slots__ = ["pool"]

    def on_load(self):
        self.pool = None
        self.pipe = AsyncPGPipe(self)

    async def build_pool(self):
        self.pool = await asyncpg.create_pool(
            user=os.getenv('PGUSER', 'benchmarkdbuser'),
            password=os.getenv('PGPASS', 'benchmarkdbpass'),
            database='hello_world',
            host='tfb-database',
            port=5432,
            min_size=16,
            max_size=16,
            max_queries=64_000_000_000,
            max_inactive_connection_lifetime=0
        )

    @listen_signal(Signals.after_loop)
    def _init_pool(self, loop):
        loop.run_until_complete(self.build_pool())


class AsyncPGPipe(Pipe):
    __slots__ = ["ext"]

    def __init__(self, ext):
        self.ext = ext

    async def open(self):
        conn = current._db_conn = self.ext.pool.acquire()
        current.db = await conn.__aenter__()

    async def close(self):
        await current._db_conn.__aexit__()


app = App(__name__)
app.config.handle_static = False
templates = Renoir()

db_ext = app.use_extension(AsyncPG)

SQL_SELECT = 'SELECT "randomnumber", "id" FROM "world" WHERE id = $1'
SQL_UPDATE = 'UPDATE "world" SET "randomnumber"=$1 WHERE id=$2'
ROW_ADD = [0, 'Additional fortune added at request time.']
sort_key = itemgetter(1)


@app.route()
@service.json
async def json():
    return {'message': 'Hello, World!'}


@app.route("/db", pipeline=[db_ext.pipe])
@service.json
async def get_random_world():
    row_id = randint(1, 10000)
    number = await current.db.fetchval(SQL_SELECT, row_id)
    return {'id': row_id, 'randomNumber': number}


def get_qparam():
    try:
        rv = int(request.query_params.queries or 1)
    except ValueError:
        return 1
    if rv < 1:
        return 1
    if rv > 500:
        return 500
    return rv


@app.route("/queries", pipeline=[db_ext.pipe])
@service.json
async def get_random_worlds():
    num_queries = get_qparam()
    row_ids = sample(range(1, 10000), num_queries)
    worlds = []
    statement = await current.db.prepare(SQL_SELECT)
    for row_id in row_ids:
        number = await statement.fetchval(row_id)
        worlds.append({'id': row_id, 'randomNumber': number})
    return worlds


@app.route(pipeline=[db_ext.pipe], output='str')
async def fortunes():
    response.content_type = "text/html; charset=utf-8"
    fortunes = await current.db.fetch('SELECT * FROM Fortune')
    fortunes.append(ROW_ADD)
    fortunes.sort(key=sort_key)
    return templates.render("templates/fortunes.html", {"fortunes": fortunes})


@app.route(pipeline=[db_ext.pipe])
@service.json
async def updates():
    num_queries = get_qparam()
    updates = list(zip(
        sample(range(1, 10000), num_queries),
        sorted(sample(range(1, 10000), num_queries))
    ))
    worlds = [{'id': row_id, 'randomNumber': number} for row_id, number in updates]
    statement = await current.db.prepare(SQL_SELECT)
    for row_id, _ in updates:
        await statement.fetchval(row_id)
    await current.db.executemany(SQL_UPDATE, updates)
    return worlds


@app.route(output='bytes')
async def plaintext():
    return b'Hello, World!'
