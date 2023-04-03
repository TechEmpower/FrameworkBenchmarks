import os
import sys
import multiprocessing
from wsgiref.handlers import format_date_time
import japronto
import ujson as json
import random
import asyncio
import asyncpg

db_pool = None

async def db_setup():
    global db_pool
    db_pool = await asyncpg.create_pool(
        user = os.getenv('PGUSER', 'benchmarkdbuser'),
        password = os.getenv('PGPASS', 'benchmarkdbpass'),
        database = 'hello_world',
        host = 'tfb-database',
        port = 5432        
    )

READ_ROW_SQL = 'SELECT "randomnumber", "id" FROM "world" WHERE id = $1'


def get_headers():
    return {
        'Server': 'Japronto/0.1.2',
        'Date': format_date_time(None),
    }

# -----------------------------------------------------------------------------------

def json_view(request):
    return request.Response(
        text = json.dumps( {'message': 'Hello, world!'} ),
        mime_type = 'application/json',
        headers = get_headers(),
    )


def plaintext_view(request):
    return request.Response(
        body = b'Hello, world!',
        mime_type = 'text/plain',
        headers = get_headers(),
    )


async def db_view(request):
    global db_pool
    row_id = random.randint(1, 10000)
    async with db_pool.acquire() as connection:
        number = await connection.fetchval(READ_ROW_SQL, row_id)

    text = json.dumps( {'id': row_id, 'randomNumber': number} )
    return request.Response(text = text, mime_type = 'application/json', headers = get_headers())

# -----------------------------------------------------------------------------------

app = japronto.Application()
app.router.add_route('/json', json_view, 'GET')
app.router.add_route('/plaintext', plaintext_view, 'GET')
#app.router.add_route('/db', db_view, 'GET')

#asyncio.set_event_loop(app.loop)
#app.loop.run_until_complete(db_setup())

# -----------------------------------------------------------------------------------

if __name__ == '__main__':    
    _is_travis = os.environ.get('TRAVIS') == 'true'
    
    workers = int( multiprocessing.cpu_count() )
    if _is_travis:
        workers = 2

    app.run('0.0.0.0', 8080, worker_num = workers)
