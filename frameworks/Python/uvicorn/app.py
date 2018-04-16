import asyncio
import asyncpg
import jinja2
import os
import ujson as json
from functools import partial
from random import randint
from operator import itemgetter
from urllib import parse


async def setup():
    global pool
    pool = await asyncpg.create_pool(
        user=os.getenv('PGUSER', 'benchmarkdbuser'),
        password=os.getenv('PGPASS', 'benchmarkdbpass'),
        database='hello_world',
        host='tfb-database',
        port=5432
    )


pool = None
additional = [0, 'Additional fortune added at request time.']
key = itemgetter(1)
template = None
path = os.path.join('templates', 'fortune.html')
with open(path, 'r') as template_file:
    template_text = template_file.read()
    template = jinja2.Template(template_text)

loop = asyncio.get_event_loop()
loop.run_until_complete(setup())


def get_query_count(query_string):
    # helper to deal with the querystring passed in
    queries = parse.parse_qs(query_string).get(b'queries', [None])[0]
    if queries:
        try:
            query_count = int(queries)
            if query_count < 1:
                return 1
            if query_count > 500:
                return 500
            return query_count
        except ValueError:
            pass
    return 1


random_int = partial(randint, 1, 10000)


async def json_endpoint(message, channels):
    content = json.dumps({'message': 'Hello, world!'}).encode('utf-8')
    await channels['reply'].send({
        'status': 200,
        'headers': [
            [b'content-type', b'application/json'],
        ],
        'content': content
    })


async def fortunes_endpoint(message, channels):
    connection = await pool.acquire()
    try:
        fortunes = await connection.fetch('SELECT * FROM Fortune')
        fortunes.append(additional)
        fortunes.sort(key=key)
        content = template.render(fortunes=fortunes).encode('utf-8')
        await channels['reply'].send({
            'status': 200,
            'headers': [
                [b'content-type', b'text/html; charset=utf-8'],
            ],
            'content': content
        })
    finally:
        await pool.release(connection)


async def plaintext_endpoint(message, channels):
    await channels['reply'].send({
        'status': 200,
        'headers': [
            [b'content-type', b'text/plain'],
        ],
        'content': b'Hello, world!'
    })


async def handle_404(message, channels):
    await channels['reply'].send({
        'status': 404,
        'headers': [
            [b'content-type', b'text/plain'],
        ],
        'content': b'Not found'
    })


async def db_endpoint(message, channels):
    """Test Type 2: Single database object"""
    async with pool.acquire() as connection:
        row = await connection.fetchrow('SELECT id, "randomnumber" FROM "world" WHERE id = ' + str(random_int()))
        world = {'id': row[0], 'randomNumber': row[1]}
        await channels['reply'].send({
            'status': 200,
            'headers': [
                [b'content-type', b'application/json'],
            ],
            'content': json.dumps(world).encode('utf-8')
        })


async def queries_endpoint(message, channels):
    """Test Type 3: Multiple database queries"""
    queries = get_query_count(message.get('query_string', {}))
    async with pool.acquire() as connection:
        worlds = []
        for i in range(queries):
            sql = 'SELECT id, "randomnumber" FROM "world" WHERE id = ' + str(random_int())
            row = await connection.fetchrow(sql)
            worlds.append({'id': row[0], 'randomNumber': row[1]})
        await channels['reply'].send({
            'status': 200,
            'headers': [
                [b'content-type', b'application/json'],
            ],
            'content': json.dumps(worlds).encode('utf-8')
        })


async def updates_endpoint(message, channels):
    """Test 5: Database Updates"""
    queries = get_query_count(message.get('query_string', {}))
    async with pool.acquire() as connection:
        worlds = []
        for i in range(queries):
            row = await connection.fetchrow('SELECT id FROM "world" WHERE id=' + str(random_int()))
            worlds.append({'id': row[0], 'randomNumber': random_int()})
            await connection.execute('UPDATE "world" SET "randomnumber"=%s WHERE id=%s' % (random_int(), row[0]))
        await channels['reply'].send({
            'status': 200,
            'headers': [
                [b'content-type', b'application/json'],
            ],
            'content': json.dumps(worlds).encode('utf-8')
        })


routes = {
    '/json': json_endpoint,
    '/fortunes': fortunes_endpoint,
    '/plaintext': plaintext_endpoint,
    '/db': db_endpoint,
    '/queries': queries_endpoint,
    '/updates': updates_endpoint,
}


async def main(message, channels):
    path = message['path']
    await routes.get(path, handle_404)(message, channels)
