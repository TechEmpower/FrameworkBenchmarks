import multiprocessing
import os
from random import randint
from operator import itemgetter

from vibora import Vibora, Request, JsonResponse, Response
from vibora.hooks import Events

from db import Pool
DEFAULT_POOL_SIZE = 1000//multiprocessing.cpu_count()

READ_ROW_SQL = 'SELECT * FROM "world" WHERE id={0}'
WRITE_ROW_SQL = 'UPDATE "world" SET "randomnumber"={0} WHERE id={1} RETURNING id, randomNumber'
READ_ALL_FORTUNES = 'SELECT * FROM "fortune"'
ADDITIONAL_ROW = [0, 'Additional fortune added at request time.']
sort_fortunes_key = itemgetter(1)

app = Vibora(template_dirs=['templates'])


@app.handle(Events.BEFORE_SERVER_START)
async def init_db(app: Vibora):
    app.components.add(await Pool("postgresql://%s:%s@%s:5432/%s" % (os.getenv("PGUSER", "benchmarkdbuser"), os.getenv("PSPASS", "benchmarkdbpass"), os.getenv("PGADDR", "tfb-database"), os.getenv("PGDB", "hello_world")), max_size=int(os.getenv("PGPOOLSIZE", DEFAULT_POOL_SIZE))))


@app.handle(Events.BEFORE_SERVER_STOP)
async def close_db(app: Vibora):
    await asyncio.wait_for(app.components.get(Pool).close(), timeout=10)


def getQueriesTotal(params):
    try:
        queries = params['queries'][0]
        query_count = int(queries)
    except:
        return 1

    if query_count < 1:
        return 1
    if query_count > 500:
        return 500
    return query_count


async def fetchWorld(pool):
    async with pool.acquire() as conn:
        return await conn.fetchrow(READ_ROW_SQL.format(randint(1, 10000)))


async def updateWorld(world_id, pool):
    async with pool.acquire() as conn:
        return await conn.fetchrow(WRITE_ROW_SQL.format(randint(1, 10000), world_id))


async def fetchMultipleWorlds(total, pool):
    worlds = []
    for x in range(total):
        res = await fetchWorld(pool)
        worlds.append({'id': res[0], 'randomNumber': res[1]})
    return worlds


async def updateMultipleWorlds(total, pool):
    worlds = []
    for x in range(total):
        res = await fetchWorld(pool)
        updated = await updateWorld(res[0], pool)
        worlds.append({'id': updated[0], 'randomNumber': updated[1]})
    return worlds


async def fetchFortunes(pool):
    async with pool.acquire() as conn:
        return await conn.fetch(READ_ALL_FORTUNES)


@app.route('/fortunes')
async def fortunes(pool: Pool):
    fortunes = await fetchFortunes(pool)
    fortunes.append(ADDITIONAL_ROW)
    fortunes.sort(key=sort_fortunes_key)
    return await app.render('index.html', fortunes=fortunes)


@app.route('/db')
async def single_query(request: Request, pool: Pool):
    res = await fetchWorld(pool)
    return JsonResponse({'id': res[0], 'randomNumber': res[1]}, headers={'Server': 'Vibora'})


@app.route('/plaintext')
async def plaintext():
    return Response(b'Hello, World!', headers={'Server': 'Vibora', 'Content-Type': 'text/plain'})


@app.route('/json')
async def json():
    return JsonResponse({'message': 'Hello, World!'}, headers={'Server': 'Vibora'})


@app.route('/queries')
async def multiple_queries(request: Request, pool: Pool):
    total_queries = getQueriesTotal(request.args)
    worlds = await fetchMultipleWorlds(total_queries, pool)
    return JsonResponse(worlds, headers={'Server': 'Vibora', 'Content-Type': 'application/json', 'Content-Length': str(total_queries)})


@app.route('/updates')
async def update_queries(request: Request, pool: Pool):
    total_queries = getQueriesTotal(request.args)
    worlds = await updateMultipleWorlds(total_queries, pool)
    return JsonResponse(worlds, headers={'Server': 'Vibora', 'Content-Type': 'application/json', 'Content-Length': str(total_queries)})

if __name__ == '__main__':
    app.run(host="0.0.0.0", port=8000, workers=multiprocessing.cpu_count())
