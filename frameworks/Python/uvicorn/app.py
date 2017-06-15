import asyncio
import asyncpg
import jinja2
import os
import ujson as json
from operator import itemgetter


async def setup():
    global pool
    pool = await asyncpg.create_pool(
        user=os.getenv('PGUSER', 'benchmarkdbuser'),
        password=os.getenv('PGPASS', 'benchmarkdbpass'),
        database='hello_world',
        host=os.getenv('DBHOST', 'localhost'),
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


routes = {
    '/json': json_endpoint,
    '/fortunes': fortunes_endpoint,
    '/plaintext': plaintext_endpoint
}


async def main(message, channels):
    path = message['path']
    await routes.get(path, handle_404)(message, channels)
