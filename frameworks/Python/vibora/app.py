from vibora import Vibora
from vibora.request import Request
from vibora.responses import JsonResponse, Response
from vibora.hooks import Events
from random import randint
from operator import itemgetter
import psycopg2

READ_ROW_SQL = 'SELECT * FROM "world" WHERE id={0}'
WRITE_ROW_SQL = 'UPDATE "world" SET "randomnumber"={0} WHERE id={1} RETURNING id, randomNumber'
READ_ALL_FORTUNES = 'SELECT * FROM "fortune"'
ADDITIONAL_ROW = [0, 'Additional fortune added at request time.']
sort_fortunes_key = itemgetter(1)

app = Vibora()
con = psycopg2.connect("dbname=hello_world user=benchmarkdbuser host=tfb-database password=benchmarkdbpass")
cur = con.cursor()

def getQueriesTotal(params):
    try:
        queries = params['queries']
        query_count = int(queries)
    except:
        return 1
    
    if query_count < 1:
        return 1
    if query_count > 500:
        return 500
    return query_count


async def fetchWorld():
    rand = randint(1, 10000)
    cur.execute(READ_ROW_SQL.format(rand))
    res = cur.fetchone()
    return res

async def updateWorld(world_id):
    new_num_rand = randint(1, 10000)
    cur.execute(WRITE_ROW_SQL.format(new_num_rand, world_id))
    res = cur.fetchone()
    return res

async def fetchMultipleWorlds(total):
    worlds = []
    for x in range(total):
        res = await fetchWorld()
        worlds.append({'id': res[0], 'randomNumber': res[1]})
    return worlds

async def updateMultipleWorlds(total):
    worlds = []
    for x in range(total):
        res = await fetchWorld()
        updated = await updateWorld(res[0])
        worlds.append({'id': updated[0], 'randomNumber': updated[1]})
    return worlds

async def fetchFortunes():
    cur.execute(READ_ALL_FORTUNES)
    res = cur.fetchall()
    return res

@app.route('/fortunes')
async def fortunes():
    fortunes = await fetchFortunes()
    fortunes.append(ADDITIONAL_ROW)
    fortunes = fortunes.sort(key=sort_fortunes_key)
    return await app.render('index.html', fortunes=fortunes)

@app.route('/db')
async def single_query(request: Request):
    res = await fetchWorld()
    return JsonResponse({'id': res[0], 'randomNumber': res[1]}, headers={'Server': 'Vibora'})

@app.route('/plaintext')
async def plaintext():
    return Response(b'Hello, World!', headers={'Server': 'Vibora', 'Content-Type': 'text/plain'})

@app.route('/json')
async def json():
    return JsonResponse({'message': 'Hello, World!'}, headers={'Server': 'Vibora'})

@app.route('/queries')
async def multiple_queries(request: Request):
    total_queries = getQueriesTotal(request.args)
    worlds = await fetchMultipleWorlds(total_queries)
    return JsonResponse(worlds, headers={'Server': 'Vibora', 'Content-Type': 'application/json', 'Content-Length': str(total_queries)})

@app.route('/updates')
async def update_queries(request: Request):
    total_queries = getQueriesTotal(request.args)
    worlds = updateMultipleWorlds(total_queries)
    return JsonResponse(worlds, headers={'Server': 'Vibora', 'Content-Type': 'application/json', 'Content-Length': str(total_queries)})

if __name__ == '__main__':
    app.run(host="0.0.0.0", port=8000)