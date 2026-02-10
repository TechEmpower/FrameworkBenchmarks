import platform
from operator import attrgetter, itemgetter
from pathlib import Path
from random import randint, sample

import jinja2
from aiohttp.web import Response
from sqlalchemy import bindparam, select
from sqlalchemy.orm.attributes import flag_modified

from .models import Fortune, World

if platform.python_implementation() == "PyPy":
    from aiohttp.web import json_response
else:
    from orjson import dumps

    def json_response(payload):
        return Response(
            body=dumps(payload),
            content_type="application/json",
        )

READ_ROW_SQL = 'SELECT "randomnumber", "id" FROM "world" WHERE id = $1'
READ_SELECT_ORM = select(World.randomnumber).where(World.id == bindparam("id"))
READ_FORTUNES_ORM = select(Fortune.id, Fortune.message)
WRITE_ROW_SQL = 'UPDATE "world" SET "randomnumber"=$2 WHERE id=$1'
WRITE_ROW_BATCH_SQL = 'UPDATE "world" w SET "randomnumber"=u.new_val FROM (SELECT unnest($1::int[]) as id, unnest($2::int[]) as new_val) u WHERE w.id = u.id'

template_path = Path(__file__).parent / 'templates' / 'fortune.jinja'
template = jinja2.Template(template_path.read_text())
sort_fortunes_orm = attrgetter('message')
sort_fortunes_raw = itemgetter('message')


def get_num_queries(request):
    try:
        num_queries = int(request.match_info['queries'])
    except (KeyError, ValueError):
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
    async with request.app['db_session']() as sess:
        num = await sess.scalar(READ_SELECT_ORM, {"id": id_})
    return json_response({'id': id_, 'randomNumber': num})


async def single_database_query_raw(request):
    """
    Test 2 RAW
    """
    id_ = randint(1, 10000)

    async with request.app['pg'].acquire() as conn:
        r = await conn.fetchval(READ_ROW_SQL, id_)
    return json_response({'id': id_, 'randomNumber': r})


async def multiple_database_queries_orm(request):
    """
    Test 3 ORM
    """
    num_queries = get_num_queries(request)

    ids = [randint(1, 10000) for _ in range(num_queries)]

    result = []
    async with request.app['db_session']() as sess:
        for id_ in ids:
            num = await sess.scalar(READ_SELECT_ORM, {"id": id_})
            result.append({'id': id_, 'randomNumber': num})
    return json_response(result)


async def multiple_database_queries_raw(request):
    """
    Test 3 RAW
    """
    num_queries = get_num_queries(request)

    ids = [(randint(1, 10000), ) for _ in range(num_queries)]

    async with request.app['pg'].acquire() as conn:
        rows = await conn.fetchmany(READ_ROW_SQL, ids)
        result = [{'id': id_[0], 'randomNumber': row[0]} for id_, row in zip(ids, rows)]
    return json_response(result)


async def fortunes(request):
    """
    Test 4 ORM
    """
    async with request.app['db_session']() as sess:
        ret = await sess.execute(READ_FORTUNES_ORM)
        fortunes = ret.all()
    fortunes.append(Fortune(id=0, message='Additional fortune added at request time.'))
    fortunes.sort(key=sort_fortunes_orm)
    content = template.render(fortunes=fortunes)
    return Response(text=content, content_type='text/html')


async def fortunes_raw(request):
    """
    Test 4 RAW
    """
    async with request.app['pg'].acquire() as conn:
        fortunes = await conn.fetch('SELECT * FROM Fortune')
    fortunes.append({'id': 0, 'message': 'Additional fortune added at request time.'})
    fortunes.sort(key=sort_fortunes_raw)
    content = template.render(fortunes=fortunes)
    return Response(text=content, content_type='text/html')


async def updates(request):
    """
    Test 5 ORM
    """
    num_queries = get_num_queries(request)
    update_ids = sample(range(1, 10001), num_queries)
    update_ids.sort()
    updates = tuple(zip(update_ids, sample(range(1, 10001), num_queries)))
    worlds = [{'id': row_id, 'randomNumber': number} for row_id, number in updates]

    async with request.app['db_session'].begin() as sess:
        for id_, number in updates:
            world = await sess.get(World, id_, populate_existing=True)
            world.randomnumber = number
            # Force sqlalchemy to UPDATE entry even if the value has not changed
            # doesn't make sense in a real application, added only to pass tests.
            flag_modified(world, "randomnumber")
    return json_response(worlds)

async def updates_raw(request):
    """
    Test 5 RAW
    """
    num_queries = get_num_queries(request)
    update_ids = sample(range(1, 10001), num_queries)
    update_ids.sort()

    numbers = sample(range(1, 10001), num_queries)
    fetch_params = [(i,) for i in update_ids]
    row_updates = [*zip(update_ids, numbers)]

    async with request.app['pg'].acquire() as conn:
        # the result of this is the int previous random number which we don't actually use
        await conn.executemany(READ_ROW_SQL, fetch_params)
        if num_queries <= 5:
            await conn.executemany(WRITE_ROW_SQL, row_updates)
        else:
            await conn.execute(WRITE_ROW_BATCH_SQL, update_ids, numbers)

    worlds = [{'id': row_id, 'randomNumber': number} for row_id, number in row_updates]
    return json_response(worlds)


async def plaintext(request):
    """
    Test 6
    """
    return Response(body=b'Hello, World!', content_type='text/plain')
