import multiprocessing
import os
import pathlib
from operator import itemgetter
from random import randint, sample
from typing import Annotated, AsyncIterable, Optional

import asyncpg  # type: ignore
import jinja2  # type: ignore
import uvicorn  # type: ignore
from pydantic import BaseModel, Field
from starlette.responses import HTMLResponse, PlainTextResponse
from xpresso import App, Depends, Path, Response, FromQuery

READ_ROW_SQL = 'SELECT "randomnumber", "id" FROM "world" WHERE id = $1'
WRITE_ROW_SQL = 'UPDATE "world" SET "randomnumber"=$1 WHERE id=$2'
ADDITIONAL_ROW = (0, 'Additional fortune added at request time.')


sort_fortunes_key = itemgetter(1)

app_dir = pathlib.Path(__file__).parent
with (app_dir / "templates" / "fortune.html").open() as template_file:
    template = jinja2.Template(template_file.read())


async def get_db_pool() -> AsyncIterable[asyncpg.Pool]:
    async with asyncpg.create_pool(  # type: ignore
        user=os.getenv('PGUSER', 'benchmarkdbuser'),
        password=os.getenv('PGPASS', 'benchmarkdbpass'),
        database=os.getenv('PGDB', 'hello_world'),
        host=os.getenv('PGHOST', 'tfb-database'),
        port=5432,
    ) as pool:
        yield pool


DBPool = Annotated[asyncpg.Pool, Depends(get_db_pool, scope="app")]


def get_num_queries(queries: Optional[str]) -> int:
    if not queries:
        return 1
    try:
        queries_num = int(queries)
    except (ValueError, TypeError):
        return 1
    if queries_num < 1:
        return 1
    if queries_num > 500:
        return 500
    return queries_num



class Greeting(BaseModel):
    message: str


def json_serialization() -> Greeting:
    return Greeting(message="Hello, world!")


def plaintext() -> Response:
    return PlainTextResponse(b"Hello, world!")


class QueryResult(BaseModel):
    id: int
    randomNumber: int


async def single_database_query(pool: DBPool) -> QueryResult:
    row_id = randint(1, 10000)

    connection: "asyncpg.Connection"
    async with pool.acquire() as connection:  # type: ignore
        number: int = await connection.fetchval(READ_ROW_SQL, row_id)  # type: ignore

    return QueryResult.construct(id=row_id, randomNumber=number)


QueryCount = Annotated[str, Field(gt=0, le=500)]


async def multiple_database_queries(
    pool: DBPool,
    queries: FromQuery[str | None] = None,
) -> list[QueryResult]:
    num_queries = get_num_queries(queries)
    row_ids = sample(range(1, 10000), num_queries)

    connection: "asyncpg.Connection"
    async with pool.acquire() as connection:  # type: ignore
        statement = await connection.prepare(READ_ROW_SQL)  # type: ignore
        return [
            QueryResult.construct(
                id=row_id,
                randomNumber=await statement.fetchval(row_id),  # type: ignore
            )
            for row_id in row_ids
        ]


async def fortunes(pool: DBPool) -> Response:
    connection: "asyncpg.Connection"
    async with pool.acquire() as connection:  # type: ignore
        fortunes: "list[tuple[int, str]]" = await connection.fetch("SELECT * FROM Fortune")  # type: ignore

    fortunes.append(ADDITIONAL_ROW)
    fortunes.sort(key=sort_fortunes_key)
    content = template.render(fortunes=fortunes)  # type: ignore
    return HTMLResponse(content)


async def database_updates(
    pool: DBPool,
    queries: FromQuery[str | None] = None,
) -> list[QueryResult]:
    num_queries = get_num_queries(queries)

    updates = [(row_id, randint(1, 10000)) for row_id in sample(range(1, 10000), num_queries)]

    async with pool.acquire() as connection:
        statement = await connection.prepare(READ_ROW_SQL)
        for row_id, _ in updates:
            await statement.fetchval(row_id)
        await connection.executemany(WRITE_ROW_SQL, updates)  # type: ignore

    return [QueryResult.construct(id=row_id, randomNumber=number) for row_id, number in updates]


routes = (
    Path("/json", get=json_serialization),
    Path("/plaintext", get=plaintext),
    Path("/db", get=single_database_query),
    Path("/queries", get=multiple_database_queries),
    Path("/fortunes", get=fortunes),
    Path("/updates", get=database_updates),
)


app = App(routes=routes)


if __name__ == "__main__":
    workers = multiprocessing.cpu_count()
    if os.environ.get("TRAVIS") == "true":
        workers = 2
    uvicorn.run(  # type: ignore
        "main:app",
        host="0.0.0.0",
        port=8080,
        workers=workers,
        log_level="error",
        loop="uvloop",
        http="httptools",
    )
