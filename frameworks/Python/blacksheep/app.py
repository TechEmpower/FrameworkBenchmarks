import os
import ujson
import asyncpg
import multiprocessing
import random
import blacksheep as bs
import jinja2
from email.utils import formatdate

try:
    from ujson import dumps as jsonify
except:
    from json import dumps as jsonify


_is_travis = os.environ.get('TRAVIS') == 'true'

_is_gunicorn = "gunicorn" in os.environ.get("SERVER_SOFTWARE", "")

_cpu_count = multiprocessing.cpu_count()
if _is_travis:
    _cpu_count = 2


#from blacksheep.settings.json import json_settings
#json_settings.use(dumps=jsonify)

DBDRV  = "postgres"
DBHOST = "tfb-database"
DBUSER = "benchmarkdbuser"
DBPSWD = "benchmarkdbpass"

READ_ROW_SQL = 'SELECT "id", "randomnumber" FROM "world" WHERE id = $1'
WRITE_ROW_SQL = 'UPDATE "world" SET "randomnumber"=$1 WHERE id=$2'
ADDITIONAL_ROW = [0, "Additional fortune added at request time."]
MAX_POOL_SIZE = 1000 // multiprocessing.cpu_count()
MIN_POOL_SIZE = max(int(MAX_POOL_SIZE / 2), 1)

db_pool = None

g_response_server = None
g_response_add_date = False


async def setup_db(app):
    global db_pool
    db_pool = await asyncpg.create_pool(
        user=os.getenv('PGUSER', DBUSER),
        password=os.getenv('PGPASS', DBPSWD),
        database='hello_world',
        host=DBHOST,
        port=5432,
        min_size=MIN_POOL_SIZE,
        max_size=MAX_POOL_SIZE,        
    )


def load_fortunes_template():
    path = os.path.join('templates', 'fortune.html')
    with open(path, 'r') as template_file:
        template_text = template_file.read()
        return jinja2.Template(template_text)


fortune_template = load_fortunes_template()

app = bs.Application()
app.on_start += setup_db


def get_num_queries(request):
    try:
        value = request.query.get('queries')
        if value is None:
            return 1
        query_count = int(value[0])
    except (KeyError, IndexError, ValueError):
        return 1
    if query_count < 1:
        return 1
    if query_count > 500:
        return 500
    return query_count


# ------------------------------------------------------------------------------------------

async def bs_middleware(request, handler):
    global g_response_server, g_response_add_date
    response = await handler(request)
    if g_response_server:
        response.headers[b'Server'] = g_response_server
    if g_response_add_date:
        response.headers[b'Date'] = formatdate(timeval=None, localtime=False, usegmt=True)            
    return response


@app.route('/json')
async def json_test(request):
    return bs.json( {'message': 'Hello, world!'} )


@app.route('/db')
async def single_db_query_test(request):
    row_id = random.randint(1, 10000)
    
    async with db_pool.acquire() as db_conn:
        number = await db_conn.fetchval(READ_ROW_SQL, row_id)
    
    world = {'id': row_id, 'randomNumber': number}
    return bs.json(world)


@app.route('/queries')
async def multiple_db_queries_test(request):
    num_queries = get_num_queries(request)
    row_ids = random.sample(range(1, 10000), num_queries)
    worlds = [ ]

    async with db_pool.acquire() as db_conn:
        statement = await db_conn.prepare(READ_ROW_SQL)
        for row_id in row_ids:
            number = await statement.fetchval(row_id)
            worlds.append( {"id": row_id, "randomNumber": number} )

    return bs.json(worlds)


@app.route('/fortunes')
async def fortunes_test(request):
    async with db_pool.acquire() as db_conn:
        fortunes = await db_conn.fetch("SELECT * FROM Fortune")

    fortunes.append(ADDITIONAL_ROW)
    fortunes.sort(key = lambda row: row[1])
    data = fortune_template.render(fortunes=fortunes)
    return bs.html(data)


@app.route('/updates')
async def db_updates_test(request):
    num_queries = get_num_queries(request)
    ids = sorted(random.sample(range(1, 10000 + 1), num_queries))
    numbers = sorted(random.sample(range(1, 10000), num_queries))
    updates = list(zip(ids, numbers))

    worlds = [ {"id": row_id, "randomNumber": number} for row_id, number in updates ]

    async with db_pool.acquire() as db_conn:
        statement = await db_conn.prepare(READ_ROW_SQL)
        for row_id, _ in updates:
            await statement.fetchval(row_id)
        await db_conn.executemany(WRITE_ROW_SQL, updates)
 
    return bs.json(worlds)


@app.route('/plaintext')
async def plaintext_test(request):
    return bs.Response(200, content=bs.Content(b"text/plain", b'Hello, World!'))
    #return bs.text('Hello, World!')


# -----------------------------------------------------------------------------------

if __name__ == "__main__":
    import optparse
    import logging
    import re

    parser = optparse.OptionParser("usage: %prog [options]", add_help_option=False)
    parser.add_option("-h", "--host", dest="host", default='0.0.0.0', type="string")
    parser.add_option("-p", "--port", dest="port", default=8080, type="int")
    parser.add_option("-s", "--server", dest="server", default="uvicorn", type="string")
    parser.add_option("-w", "--workers", dest="workers", default=0, type="int")
    parser.add_option("-k", "--keepalive", dest="keepalive", default=60, type="int")
    parser.add_option("-v", "--verbose", dest="verbose", default=0, type="int")
    (opt, args) = parser.parse_args() 

    workers = _cpu_count
    if workers > 0:
        workers = opt.workers

    if _is_travis:
        workers = 2

    def run_app():
        global g_response_server, g_response_add_date

        if opt.gateway == "uvicorn":
            import uvicorn
            log_level = logging.ERROR
            uvicorn.run(app, host=opt.host, port=opt.port, workers=1, loop="uvloop", log_level=log_level, access_log=False)
        
        if opt.server == 'fastwsgi':
            import fastwsgi
            from blacksheep.utils.aio import get_running_loop
            g_response_server = b'FastWSGI'
            app.middlewares.append(bs_middleware)
            loop = get_running_loop()
            loop.run_until_complete(app.start())
            fastwsgi.run(app, host=opt.host, port=opt.port, loglevel=opt.verbose)

        if opt.server == 'socketify':
            import socketify
            msg = "Listening on http://0.0.0.0:{port} now\n".format(port=opt.port)
            socketify.WSGI(app).listen(opt.port, lambda config: logging.info(msg)).run()

    def create_fork():
        n = os.fork()
        # n greater than 0 means parent process
        if not n > 0:
            run_app()

    # fork limiting the cpu count - 1
    for i in range(1, workers):
        create_fork()

    run_app()  # run app on the main process too :)

