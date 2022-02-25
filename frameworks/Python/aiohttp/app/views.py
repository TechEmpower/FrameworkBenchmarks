from functools import partial
from operator import attrgetter, itemgetter
from pathlib import Path
from random import randint

import jinja2
import ujson
from aiohttp.web import Response, json_response
from sqlalchemy import select

from .models import sa_fortunes, sa_worlds, Fortune, World

ADDITIONAL_FORTUNE_ORM = Fortune(id=0, message='Additional fortune added at request time.')
ADDITIONAL_FORTUNE_ROW = {'id': 0, 'message': 'Additional fortune added at request time.'}
READ_ROW_SQL = 'SELECT "randomnumber", "id" FROM "world" WHERE id = $1'
READ_SELECT_ORM = select(World.randomnumber)
WRITE_ROW_SQL = 'UPDATE "world" SET "randomnumber"=$2 WHERE id=$1'

json_response = partial(json_response, dumps=ujson.dumps)
template_path = Path(__file__).parent / 'templates' / 'fortune.jinja'
template = jinja2.Template(template_path.read_text())
sort_fortunes_orm = attrgetter('message')
sort_fortunes_raw = itemgetter('message')


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
    async with request.app['db_session']() as sess:
        num = await sess.scalar(select(World.randomnumber).filter_by(id=id_))
    return json_response({'id': id_, 'randomNumber': num})


async def single_database_query_raw(request):
    """
    Test 2 RAW
    """
    id_ = randint(1, 10000)

    async with request.app['pg'].acquire() as conn:
        r = await conn.fetchval('SELECT id,randomnumber FROM world WHERE id = $1', id_)
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
            num = await sess.scalar(READ_SELECT_ORM.filter_by(id=id_))
            result.append({'id': id_, 'randomNumber': num})
    return json_response(result)


async def multiple_database_queries_raw(request):
    """
    Test 3 RAW
    """
    num_queries = get_num_queries(request)

    ids = [randint(1, 10000) for _ in range(num_queries)]

    result = []
    async with request.app['pg'].acquire() as conn:
        stmt = await conn.prepare(READ_ROW_SQL)
        for id_ in ids:
            result.append({
                'id': id_,
                'randomNumber': await stmt.fetchval(id_),
            })
    return json_response(result)


async def fortunes(request):
    """
    Test 4 ORM
    """
    async with request.app['db_session']() as sess:
        ret = await sess.execute(select(Fortune.id, Fortune.message))
        fortunes = ret.all()
    fortunes.append(ADDITIONAL_FORTUNE_ORM)
    fortunes.sort(key=sort_fortunes_orm)
    content = template.render(fortunes=fortunes)
    return Response(text=content, content_type='text/html')


async def fortunes_raw(request):
    """
    Test 4 RAW
    """
    async with request.app['pg'].acquire() as conn:
        fortunes = await conn.fetch('SELECT * FROM Fortune')
    fortunes.append(ADDITIONAL_FORTUNE_ROW)
    fortunes.sort(key=sort_fortunes_raw)
    content = template.render(fortunes=fortunes)
    return Response(text=content, content_type='text/html')


async def updates(request):
    """
    Test 5 ORM
    """
    num_queries = get_num_queries(request)
    updates = [(randint(1, 10000), randint(1, 10000)) for _ in range(num_queries)]
    updates.sort()
    worlds = [{'id': row_id, 'randomNumber': number} for row_id, number in updates]

    async with request.app['db_session'].begin() as sess:
        for id_, number in updates:
            world = await sess.get(World, id_, populate_existing=True)
            world.randomnumber = number
    return json_response(worlds)

async def updates_raw(request):
    """
    Test 5 RAW
    """
    num_queries = get_num_queries(request)
    updates = [(randint(1, 10000), randint(1, 10000)) for _ in range(num_queries)]
    updates.sort()
    worlds = [{'id': row_id, 'randomNumber': number} for row_id, number in updates]

    async with request.app['pg'].acquire() as conn:
        stmt = await conn.prepare(READ_ROW_SQL)
        for id_, _ in updates:
            # the result of this is the int previous random number which we don't actually use
            await stmt.fetchval(id_)
        await conn.executemany(WRITE_ROW_SQL, updates)

    return json_response(worlds)


async def plaintext(request):
    """
    Test 6
    """
    return Response(body=b'Hello, World!', content_type='text/plain')
