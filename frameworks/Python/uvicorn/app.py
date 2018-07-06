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


class JSONSerialization:
    """
    Test type 1: JSON Serialization
    """
    def __init__(self, scope):
        pass

    async def __call__(self, receive, send):
        content = json.dumps({'message': 'Hello, world!'}).encode('utf-8')
        await send({
            'type': 'http.response.start',
            'status': 200,
            'headers': [
                [b'content-type', b'application/json'],
                [b'content-length', str(len(content)).encode()]
            ]
        })
        await send({
            'type': 'http.response.body',
            'body': content,
            'more_body': False
        })


class SingleDatabaseQuery:
    """
    Test type 2: Single database object
    """
    def __init__(self, scope):
        pass

    async def __call__(self, receive, send):
        connection = await pool.acquire()
        try:
            row = await connection.fetchrow('SELECT id, "randomnumber" FROM "world" WHERE id = ' + str(random_int()))
            world = {'id': row[0], 'randomNumber': row[1]}

            content = json.dumps(world).encode('utf-8')
            await send({
                'type': 'http.response.start',
                'status': 200,
                'headers': [
                    [b'content-type', b'application/json'],
                    [b'content-length', str(len(content)).encode()]
                ]
            })
            await send({
                'type': 'http.response.body',
                'body': content,
                'more_body': False
            })
        finally:
            await pool.release(connection)


class MultipleDatabaseQueries:
    """
    Test type 3: Multiple database queries
    """
    def __init__(self, scope):
        self.queries = get_query_count(scope.get('query_string', {}))

    async def __call__(self, receive, send):
        connection = await pool.acquire()
        try:
            worlds = []
            for i in range(self.queries):
                sql = 'SELECT id, "randomnumber" FROM "world" WHERE id = ' + str(random_int())
                row = await connection.fetchrow(sql)
                worlds.append({'id': row[0], 'randomNumber': row[1]})

            content = json.dumps(worlds).encode('utf-8')
            await send({
                'type': 'http.response.start',
                'status': 200,
                'headers': [
                    [b'content-type', b'application/json'],
                    [b'content-length', str(len(content)).encode()]
                ]
            })
            await send({
                'type': 'http.response.body',
                'body': content,
                'more_body': False
            })
        finally:
            await pool.release(connection)


class Fortunes:
    """
    Test type 4: Fortunes
    """
    def __init__(self, scope):
        pass

    async def __call__(self, receive, send):
        connection = await pool.acquire()
        try:
            fortunes = await connection.fetch('SELECT * FROM Fortune')
            fortunes.append(additional)
            fortunes.sort(key=key)
            content = template.render(fortunes=fortunes).encode('utf-8')
            await send({
                'type': 'http.response.start',
                'status': 200,
                'headers': [
                    [b'content-type', b'text/html; charset=utf-8'],
                    [b'content-length', str(len(content)).encode()]
                ]
            })
            await send({
                'type': 'http.response.body',
                'body': content,
                'more_body': False
            })
        finally:
            await pool.release(connection)


class DatabaseUpdates:
    """
    Test type 5: Database updates
    """
    def __init__(self, scope):
        self.queries = get_query_count(scope.get('query_string', {}))

    async def __call__(self, receive, send):
        connection = await pool.acquire()
        try:
            worlds = []
            for i in range(self.queries):
                row = await connection.fetchrow('SELECT id FROM "world" WHERE id=' + str(random_int()))
                worlds.append({'id': row[0], 'randomNumber': random_int()})
                await connection.execute('UPDATE "world" SET "randomnumber"=%s WHERE id=%s' % (random_int(), row[0]))

            content = json.dumps(worlds).encode('utf-8')
            await send({
                'type': 'http.response.start',
                'status': 200,
                'headers': [
                    [b'content-type', b'application/json'],
                    [b'content-length', str(len(content)).encode()]
                ]
            })
            await send({
                'type': 'http.response.body',
                'body': content,
                'more_body': False
            })
        finally:
            await pool.release(connection)


class Plaintext:
    """
    Test type 6: Plaintext
    """
    def __init__(self, scope):
        pass

    async def __call__(self, receive, send):
        content = b'Hello, world!'
        await send({
            'type': 'http.response.start',
            'status': 200,
            'headers': [
                [b'content-type', b'text/plain'],
                [b'content-length', str(len(content)).encode()]
            ]
        })
        await send({
            'type': 'http.response.body',
            'body': content,
            'more_body': False
        })


class Handle404:
    def __init__(self, scope):
        pass

    async def __call__(self, receive, send):
        content = b'Not found'
        await send({
            'type': 'http.response.start',
            'status': 200,
            'headers': [
                [b'content-type', b'text/plain'],
                [b'content-length', str(len(content)).encode()]
            ]
        })
        await send({
            'type': 'http.response.body',
            'body': content,
            'more_body': False
        })


routes = {
    '/json': JSONSerialization,
    '/db': SingleDatabaseQuery,
    '/queries': MultipleDatabaseQueries,
    '/fortunes': Fortunes,
    '/updates': DatabaseUpdates,
    '/plaintext': Plaintext,
}


def main(scope):
    path = scope['path']
    return routes.get(path, Handle404)(scope)
