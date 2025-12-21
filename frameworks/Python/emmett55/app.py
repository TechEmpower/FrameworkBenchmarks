import os
from operator import itemgetter
from random import randint, sample

import asyncpg
from emmett55 import App, Pipe, request, response
from emmett55.extensions import Extension, Signals, listen_signal
from emmett55.tools import ServicePipe
from renoir import Renoir


class NoResetConnection(asyncpg.Connection):
    __slots__ = ()

    def get_reset_query(self):
        return ""


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
            min_size=4,
            max_size=4,
            connection_class=NoResetConnection,
        )

    @listen_signal(Signals.after_loop)
    def _init_pool(self, loop):
        loop.run_until_complete(self.build_pool())


class AsyncPGPipe(Pipe):
    __slots__ = ["ext"]

    def __init__(self, ext):
        self.ext = ext

    async def pipe(self, next_pipe, **kwargs):
        async with self.ext.pool.acquire() as conn:
            kwargs['db'] = conn
            return await next_pipe(**kwargs)


class TemplatePipe(Pipe):
    __slots__ = ["template"]
    output = "str"

    def __init__(self, template):
        self.template = f"templates/{template}"

    async def pipe(self, next_pipe, **kwargs):
        response.content_type = "text/html; charset=utf-8"
        ctx = await next_pipe(**kwargs)
        return templates.render(self.template, ctx)


app = App(__name__)
app.config.handle_static = False
templates = Renoir()

json_routes = app.module(__name__, 'json')
json_routes.pipeline = [ServicePipe('json')]

db_ext = app.use_extension(AsyncPG)

SQL_SELECT = 'SELECT "randomnumber", "id" FROM "world" WHERE id = $1'
SQL_UPDATE = 'UPDATE "world" SET "randomnumber"=$1 WHERE id=$2'
ROW_ADD = [0, 'Additional fortune added at request time.']
sort_key = itemgetter(1)


@json_routes.route()
async def json():
    return {'message': 'Hello, World!'}


@json_routes.route("/db", pipeline=[db_ext.pipe])
async def get_random_world(db):
    row_id = randint(1, 10000)
    number = await db.fetchval(SQL_SELECT, row_id)
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


@json_routes.route("/queries", pipeline=[db_ext.pipe])
async def get_random_worlds(db):
    num_queries = get_qparam()
    row_ids = sample(range(1, 10000), num_queries)
    rows = await db.fetchmany(SQL_SELECT, [(v,) for v in row_ids])
    return [{'id': row_id, 'randomNumber': number[0]} for row_id, number in zip(row_ids, rows)]


@app.route(pipeline=[TemplatePipe("fortunes.html"), db_ext.pipe])
async def fortunes(db):
    fortunes = await db.fetch('SELECT * FROM Fortune')
    fortunes.append(ROW_ADD)
    fortunes.sort(key=sort_key)
    return {"fortunes": fortunes}


@json_routes.route(pipeline=[db_ext.pipe])
async def updates(db):
    num_queries = get_qparam()
    updates = list(zip(
        sample(range(1, 10000), num_queries),
        sorted(sample(range(1, 10000), num_queries))
    ))
    worlds = [{'id': row_id, 'randomNumber': number} for row_id, number in updates]
    await db.executemany(SQL_SELECT, [(i[0],) for i in updates])
    await db.executemany(SQL_UPDATE, updates)
    return worlds


@app.route(output='bytes')
async def plaintext():
    return b'Hello, World!'
