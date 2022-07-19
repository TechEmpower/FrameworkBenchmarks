import multiprocessing
from wsgiref.handlers import format_date_time
import random

import japronto
import ujson as json

from db import init_db, close_db


def get_headers():
    return {
        'Server': 'Japronto/0.1.1',
        'Date': format_date_time(None),
    }


def json_view(request):
    return request.Response(
        text=json.dumps({'message': 'Hello, world!'}),
        mime_type='application/json',
        headers=get_headers(),
    )


def plaintext_view(request):
    return request.Response(
        body=b'Hello, world!',
        mime_type='text/plain',
        headers=get_headers(),
    )


async def db_view(request):
    async with app.db_pool.acquire() as conn:
        world = await conn.fetchrow("select id,randomnumber from world where id=%s" % random.randint(1, 10000))
    return request.Response(
        text=json.dumps(dict(world)),
        mime_type='application/json', headers=get_headers())


app = japronto.Application()
app.on_startup.append(init_db)
app.on_cleanup.append(close_db)
app.router.add_route('/json', json_view, 'GET')
app.router.add_route('/plaintext', plaintext_view, 'GET')
app.router.add_route('/db', db_view, 'GET')


if __name__ == '__main__':
    app.run('0.0.0.0', 8080, worker_num=multiprocessing.cpu_count())
