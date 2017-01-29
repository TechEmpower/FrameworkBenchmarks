from operator import attrgetter
from random import randint

from aiohttp_jinja2 import template
from aiohttp.web import Response
import ujson

from sqlalchemy import select

from .models import sa_fortunes, sa_worlds, Fortune


def _json_response(data):
    body = ujson.dumps(data)
    return Response(body=body.encode(), content_type='application/json')


async def json(request):
    """
    Test 1
    """
    return _json_response({'message': 'Hello, World!'})


async def single_database_query_orm(request):
    """
    Test 2 with ORM
    """
    id_ = randint(1, 10000)
    async with request.app['aiopg_engine'].acquire() as conn:
        cur = await conn.execute(select([sa_worlds.c.randomnumber]).where(sa_worlds.c.id == id_))
        r = await cur.first()
        return _json_response({'id': id_, 'randomNumber': r[0]})


async def multiple_database_queries_orm(request):
    """
    Test 3 with ORM
    """
    num_queries = int(request.GET.get('queries', 1))
    if num_queries < 1:
        num_queries = 1
    elif num_queries > 500:
        num_queries = 500

    ids = [randint(1, 10000) for _ in range(num_queries)]
    ids.sort()

    result = []
    async with request.app['aiopg_engine'].acquire() as conn:
        for id_ in ids:
            cur = await conn.execute(select([sa_worlds.c.randomnumber]).where(sa_worlds.c.id == id_))
            r = await cur.first()
            result.append({'id': id_, 'randomNumber': r[0]})
    return _json_response(result)


@template('fortune.jinja')
async def fortunes(request):
    """
    Test 4 with ORM
    """
    async with request.app['aiopg_engine'].acquire() as conn:
        cur = await conn.execute(select([sa_fortunes.c.id, sa_fortunes.c.message]))
        fortunes = list(await cur.fetchall())
    fortunes.append(Fortune(id=0, message='Additional fortune added at request time.'))
    fortunes.sort(key=attrgetter('message'))
    return {'fortunes': fortunes}


async def updates(request):
    """
    Test 5 with ORM
    """
    num_queries = int(request.GET.get('queries', 1))
    if num_queries < 1:
        num_queries = 1
    elif num_queries > 500:
        num_queries = 500
    result = []

    ids = [randint(1, 10000) for _ in range(num_queries)]
    ids.sort()

    async with request.app['aiopg_engine'].acquire() as conn:
        for id_ in ids:
            cur = await conn.execute(
                select([sa_worlds.c.id, sa_worlds.c.randomnumber])
                .where(sa_worlds.c.id == id_)
            )
            r = await cur.first()
            await conn.execute(
                sa_worlds.update()
                .where(sa_worlds.c.id == id_)
                .values(randomnumber=randint(1, 10000))
            )
            result.append({'id': r.id, 'randomNumber': r.randomnumber})

    return _json_response(result)


async def plaintext(request):
    """
    Test 6
    """
    return Response(body=b'Hello, World!', content_type='text/plain')
