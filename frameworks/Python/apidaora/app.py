import asyncio
import asyncpg
import os
import jinja2
from apidaora import MethodType, Request, JSONResponse, HTMLResponse, PlainResponse, Route, asgi_app
from jsondaora import jsondaora
from random import randint
from operator import itemgetter
from http import HTTPStatus
from typing import List, TypedDict, Optional


READ_ROW_SQL = 'SELECT "randomnumber" FROM "world" WHERE id = $1'
WRITE_ROW_SQL = 'UPDATE "world" SET "randomnumber"=$1 WHERE id=$2'
ADDITIONAL_ROW = [0, 'Additional fortune added at request time.']



async def setup_database():
    global connection_pool
    connection_pool = await asyncpg.create_pool(
        user=os.getenv('PGUSER', 'benchmarkdbuser'),
        password=os.getenv('PGPASS', 'benchmarkdbpass'),
        database='hello_world',
        host='tfb-database',
        port=5432
    )


def load_fortunes_template():
    path = os.path.join('templates', 'fortune.html')
    with open(path, 'r') as template_file:
        template_text = template_file.read()
        return jinja2.Template(template_text)


def get_num_queries(queries):
    try:
        query_count = int(queries)
    except (ValueError, TypeError):
        return 1

    if query_count < 1:
        return 1
    if query_count > 500:
        return 500
    return query_count


connection_pool = None
sort_fortunes_key = itemgetter(1)
template = load_fortunes_template()
loop = asyncio.get_event_loop()
loop.run_until_complete(setup_database())


@jsondaora
class JsonBody(TypedDict):
    message: str


@jsondaora
class JsonResponse(JSONResponse):
    body: JsonBody


async def json_serialization(req: Request) -> JsonResponse:
    return JsonResponse(
        status_code=HTTPStatus.OK,
        body=JsonBody(message='Hello, world!')
    )


@jsondaora
class SingleDatabaseBody(TypedDict):
    id: int
    randomNumber: float


@jsondaora
class SingleDatabaseResponse(JSONResponse):
    body: SingleDatabaseBody


async def single_database_query(req: Request) -> SingleDatabaseResponse:
    row_id = randint(1, 10000)

    async with connection_pool.acquire() as connection:
        number = await connection.fetchval(READ_ROW_SQL, row_id)

    return SingleDatabaseResponse(
        status_code=HTTPStatus.OK,
        body=SingleDatabaseBody(id=row_id, randomNumber=number)
    )


@jsondaora
class MultipleDatabaseRequestQuery(TypedDict):
    queries: Optional[str] = None


@jsondaora
class MultipleDatabaseRequest(Request):
    query: MultipleDatabaseRequestQuery


@jsondaora
class MultipleDatabaseObject(TypedDict):
    id: int
    randomNumber: float


@jsondaora
class MultipleDatabaseBody(TypedDict):
    id: int
    randomNumber: float


@jsondaora
class MultipleDatabaseResponse(JSONResponse):
    body: List[MultipleDatabaseBody]


async def multiple_database_queries(
    req: MultipleDatabaseRequest
) -> MultipleDatabaseResponse:
    num_queries = get_num_queries(req.query['queries'])
    row_ids = [randint(1, 10000) for _ in range(num_queries)]
    worlds = []

    async with connection_pool.acquire() as connection:
        statement = await connection.prepare(READ_ROW_SQL)
        for row_id in row_ids:
            number = await statement.fetchval(row_id)
            worlds.append(
                MultipleDatabaseObject(
                    id=row_id,
                    randomNumber=number
                )
            )

    return MultipleDatabaseResponse(status_code=HTTPStatus.OK, body=worlds)


@jsondaora
class FortunesResponse(HTMLResponse):
    body: str


async def fortunes(req: Request) -> FortunesResponse:
    async with connection_pool.acquire() as connection:
        fortunes = await connection.fetch('SELECT * FROM Fortune')

    fortunes.append(ADDITIONAL_ROW)
    fortunes.sort(key=sort_fortunes_key)
    content = template.render(fortunes=fortunes)
    return FortunesResponse(
        status_code=HTTPStatus.OK,
        body=content
    )


async def database_updates(
    req: MultipleDatabaseRequest
) -> MultipleDatabaseResponse:
    num_queries = get_num_queries(req.query['queries'])
    updates = [(randint(1, 10000), randint(1, 10000)) for _ in range(num_queries)]
    worlds = [{'id': row_id, 'randomNumber': number} for row_id, number in updates]

    async with connection_pool.acquire() as connection:
        statement = await connection.prepare(READ_ROW_SQL)
        for row_id, number in updates:
            await statement.fetchval(row_id)
        await connection.executemany(WRITE_ROW_SQL, updates)

    return MultipleDatabaseResponse(
        status_code=HTTPStatus.OK,
        body=worlds
    )


@jsondaora
class TextResponse(PlainResponse):
    body: str


async def plaintext(req: Request) -> TextResponse:
    return TextResponse(
        status_code=HTTPStatus.OK,
        body='Hello, world!'
    )


routes = [
    Route('/json', MethodType.GET, json_serialization),
    Route('/db', MethodType.GET, single_database_query),
    Route('/queries', MethodType.GET, multiple_database_queries),
    Route('/fortunes', MethodType.GET, fortunes),
    Route('/updates', MethodType.GET, database_updates),
    Route('/plaintext', MethodType.GET, plaintext),
]
app = asgi_app(routes)
