import asyncpg
import os
import jinja2
from logging import getLogger
from random import randint, sample
from operator import itemgetter

import multiprocessing
from wsgiref.handlers import format_date_time

import sanic
from sanic import response


logger = getLogger(__name__)


READ_ROW_SQL = 'SELECT "randomnumber", "id" FROM "world" WHERE id = $1'
READ_ROW_SQL_TO_UPDATE = 'SELECT "id", "randomnumber" FROM "world" WHERE id = $1'
WRITE_ROW_SQL = 'UPDATE "world" SET "randomnumber"=$1 WHERE id=$2'
ADDITIONAL_ROW = [0, 'Additional fortune added at request time.']


def load_fortunes_template():
    path = os.path.join('templates', 'fortune.html')
    with open(path, 'r') as template_file:
        template_text = template_file.read()
        return jinja2.Template(template_text)


def get_num_queries(queries):
    try:
        query_count = int(queries)
    except (ValueError, TypeError):
        return 1

    if query_count < 1:
        return 1
    if query_count > 500:
        return 500
    return query_count


connection_pool = None
sort_fortunes_key = itemgetter(1)
template = load_fortunes_template()

app = sanic.Sanic(name=__name__)


@app.listener('before_server_start')
async def setup_database(app, loop):
        global connection_pool
        connection_pool = await asyncpg.create_pool(
            user=os.getenv('PGUSER', 'benchmarkdbuser'),
            password=os.getenv('PGPASS', 'benchmarkdbpass'),
            database='hello_world',
            host='tfb-database',
            port=5432
        )


@app.get('/json')
def json_view(request):
    return response.json({'message': 'Hello, world!'}, headers=get_headers())


@app.get('/db')
async def single_database_query_view(request):
    row_id = randint(1, 10000)

    async with connection_pool.acquire() as connection:
        number = await connection.fetchval(READ_ROW_SQL, row_id)

    return response.json(
        {'id': row_id, 'randomNumber': number},
        headers=get_headers()
    )


@app.get('/queries')
async def multiple_database_queries_view(request):
    num_queries = get_num_queries(request.args.get('queries', 1))
    row_ids = sample(range(1, 10000), num_queries)
    worlds = []

    async with connection_pool.acquire() as connection:
        statement = await connection.prepare(READ_ROW_SQL)
        for row_id in row_ids:
            number = await statement.fetchval(row_id)
            worlds.append(
                dict(
                    id=row_id,
                    randomNumber=number
                )
            )

    return response.json(worlds, headers=get_headers())


@app.get('/fortunes')
async def fortunes_view(request):
    async with connection_pool.acquire() as connection:
        fortunes = await connection.fetch('SELECT * FROM Fortune')

    fortunes.append(ADDITIONAL_ROW)
    fortunes.sort(key=sort_fortunes_key)
    content = template.render(fortunes=fortunes)
    return response.html(content, headers=get_headers())



@app.get('/updates')
async def database_updates_view(request):
    worlds = []
    updates = set()
    queries = request.args.get('queries', 1)

    async with connection_pool.acquire() as connection:
        statement = await connection.prepare(READ_ROW_SQL_TO_UPDATE)

        for row_id in sample(range(1, 10000), get_num_queries(queries)):
            record = await statement.fetchrow(row_id)
            world = dict(
                id=record['id'], randomNumber=record['randomnumber']
            )
            world['randomNumber'] = randint(1, 10000)
            worlds.append(world)
            updates.add((world['id'], world['randomNumber']))

        await connection.executemany(WRITE_ROW_SQL, updates)

    return response.json(worlds, headers=get_headers())


@app.get('/plaintext')
def plaintext_view(request):
    return response.text('Hello, world!', headers=get_headers())


def get_headers(server='Sanic/{}'.format(sanic.__version__)):
    return {
        'Server': server,
        'Date': format_date_time(None),
    }

if __name__ == '__main__':
    app.run('0.0.0.0', 8080, access_log=False,
            workers=multiprocessing.cpu_count())
