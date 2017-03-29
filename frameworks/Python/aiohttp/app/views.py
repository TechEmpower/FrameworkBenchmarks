from operator import attrgetter, itemgetter
from random import randint

from aiohttp_jinja2 import template
from aiohttp.web import Response
import ujson

from sqlalchemy import select

from .models import sa_fortunes, sa_worlds, Fortune


def json_response(data):
    body = ujson.dumps(data)
    return Response(body=body.encode(), content_type='application/json')


def get_num_queries(request):
    try:
        num_queries = int(request.match_info.get('queries', 1))
    except ValueError:
        return 1
    if num_queries < 1:
        return 1
    if num_queries > 500:
        return 500
    return num_queries


async def json(request):
    """
    Test 1
    """
    return json_response({'message': 'Hello, World!'})


async def single_database_query_orm(request):
    """
    Test 2 ORM
    """
    id_ = randint(1, 10000)
    async with request.app['pg'].acquire() as conn:
        cur = await conn.execute(select([sa_worlds.c.randomnumber]).where(sa_worlds.c.id == id_))
        r = await cur.first()
    return json_response({'id': id_, 'randomNumber': r[0]})


async def single_database_query_raw(request):
    """
    Test 2 RAW
    """
    id_ = randint(1, 10000)

    async with request.app['pg'].acquire() as conn:
        r = await conn.fetchval('SELECT randomnumber FROM world WHERE id = $1', id_)
    return json_response({'id': id_, 'randomNumber': r})


async def multiple_database_queries_orm(request):
    """
    Test 3 ORM
    """
    num_queries = get_num_queries(request)

    ids = [randint(1, 10000) for _ in range(num_queries)]
    ids.sort()

    result = []
    async with request.app['pg'].acquire() as conn:
        for id_ in ids:
            cur = await conn.execute(select([sa_worlds.c.randomnumber]).where(sa_worlds.c.id == id_))
            r = await cur.first()
            result.append({'id': id_, 'randomNumber': r[0]})
    return json_response(result)


async def multiple_database_queries_raw(request):
    """
    Test 3 RAW
    """
    num_queries = get_num_queries(request)

    ids = [randint(1, 10000) for _ in range(num_queries)]
    ids.sort()

    result = []
    async with request.app['pg'].acquire() as conn:
        stmt = await conn.prepare('SELECT randomnumber FROM world WHERE id = $1')
        for id_ in ids:
            result.append({
                'id': id_,
                'randomNumber': await stmt.fetchval(id_),
            })
    return json_response(result)


@template('fortune.jinja')
async def fortunes(request):
    """
    Test 4 ORM
    """
    async with request.app['pg'].acquire() as conn:
        cur = await conn.execute(select([sa_fortunes.c.id, sa_fortunes.c.message]))
        fortunes = list(await cur.fetchall())
    fortunes.append(Fortune(id=0, message='Additional fortune added at request time.'))
    fortunes.sort(key=attrgetter('message'))
    return {'fortunes': fortunes}


@template('fortune.jinja')
async def fortunes_raw(request):
    """
    Test 4 RAW
    """
    async with request.app['pg'].acquire() as conn:
        fortunes = await conn.fetch('SELECT * FROM Fortune')
    fortunes.append(dict(id=0, message='Additional fortune added at request time.'))
    fortunes.sort(key=itemgetter('message'))
    return {'fortunes': fortunes}


async def updates(request):
    """
    Test 5 ORM
    """
    num_queries = get_num_queries(request)
    result = []

    ids = [randint(1, 10000) for _ in range(num_queries)]
    ids.sort()

    async with request.app['pg'].acquire() as conn:
        for id_ in ids:
            cur = await conn.execute(
                select([sa_worlds.c.randomnumber])
                .where(sa_worlds.c.id == id_)
            )
            # the result of this is a dict with the previous random number `randomnumber` which we don't actually use
            await cur.first()
            rand_new = randint(1, 10000)
            await conn.execute(
                sa_worlds.update()
                .where(sa_worlds.c.id == id_)
                .values(randomnumber=rand_new)
            )
            result.append({'id': id_, 'randomNumber': rand_new})
    return json_response(result)

async def updates_raw(request):
    """
    Test 5 RAW
    """
    num_queries = get_num_queries(request)

    ids = [randint(1, 10000) for _ in range(num_queries)]
    ids.sort()

    result = []
    updates = []
    async with request.app['pg'].acquire() as conn:
        stmt = await conn.prepare('SELECT randomnumber FROM world WHERE id = $1')
        for id_ in ids:
            # the result of this is the int previous random number which we don't actually use
            await stmt.fetchval(id_)
            rand_new = randint(1, 10000)
            result.append({'id': id_, 'randomNumber': rand_new})
            updates.append((rand_new, id_))
        await conn.executemany('UPDATE world SET randomnumber=$1 WHERE id=$2', updates)

    return json_response(result)


async def plaintext(request):
    """
    Test 6
    """
    return Response(body=b'Hello, World!', content_type='text/plain')
