from random import randint

from aiohttp.web import Response
import ujson

from sqlalchemy import select

from .models import sa_worlds


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
    wid = randint(1, 10000)
    async with request.app['aiopg_engine'].acquire() as conn:
        v = await conn.execute(
            select([sa_worlds.c.id, sa_worlds.c.randomnumber])
            .where(sa_worlds.c.id == wid)
        )
        r = await v.first()
        return _json_response({'id': r.id, 'randomNumber': r.randomnumber})


async def multiple_database_queries_orm(request):
    """
    Test 3 with ORM
    """
    num_queries = int(request.GET.get('queries', 1))
    if num_queries < 1:
        num_queries = 1
    elif num_queries > 500:
        num_queries = 500
    result = []
    async with request.app['aiopg_engine'].acquire() as conn:
        for _ in range(num_queries):
            v = await conn.execute(
                select([sa_worlds.c.id, sa_worlds.c.randomnumber])
                .where(sa_worlds.c.id == randint(1, 10000))
            )
            r = await v.first()
            result.append({'id': r.id, 'randomNumber': r.randomnumber})
    return _json_response(result)


async def plaintext(request):
    """
    Test 6
    """
    return Response(body=b'Hello, World!', content_type='text/plain')
