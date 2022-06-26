from functools import partial
import multiprocessing
import os
import pathlib
from concurrent.futures import ProcessPoolExecutor, wait
from operator import itemgetter
from random import Random
from typing import Annotated, AsyncIterable

import anyio
import asyncpg  # type: ignore
import jinja2  # type: ignore
import uvicorn  # type: ignore
from pydantic import BaseModel, Field
from starlette.responses import HTMLResponse, JSONResponse, PlainTextResponse
from xpresso import App, Depends, Path, Response

READ_ROW_SQL = 'SELECT "randomnumber", "id" FROM "world" WHERE id = $1'
WRITE_ROW_SQL = 'UPDATE "world" SET "randomnumber"=$1 WHERE id=$2'
ADDITIONAL_ROW = (0, "Additional fortune added at request time.")


sort_fortunes_key = itemgetter(1)

with (pathlib.Path("templates") / "fortune.html").open() as template_file:
    template = jinja2.Template(template_file.read())


async def get_db_pool() -> AsyncIterable[asyncpg.Pool]:
    async with asyncpg.create_pool(  # type: ignore
        user=os.getenv("PGUSER", "benchmarkdbuser"),
        password=os.getenv("PGPASS", "benchmarkdbpass"),
        database="hello_world",
        host="tfb-database",
        port=5432,
    ) as pool:
        yield pool


DBPool = Annotated[asyncpg.Pool, Depends(get_db_pool, scope="app")]


async def json_serialization() -> Response:
    return JSONResponse({"message": "Hello, world!"})


async def plaintext() -> Response:
    return PlainTextResponse(b"Hello, world!")


class QueryResult(BaseModel):
    id: int
    randomNumber: int


async def single_database_query(pool: DBPool, random: Random) -> QueryResult:
    row_id = random.randint(1, 10000)

    connection: "asyncpg.Connection"
    async with pool.acquire() as connection:  # type: ignore
        number: int = await connection.fetchval(READ_ROW_SQL, row_id)  # type: ignore

    return QueryResult(id=row_id, randomNumber=number)


QueryCount = Annotated[int, Field(gt=0, le=500)]


async def multiple_database_queries(
    pool: DBPool,
    random: Random,
    queries: QueryCount | None = None,
) -> list[QueryResult]:
    num_queries = queries or 1
    row_ids = random.sample(range(1, 10000), num_queries)

    connection: "asyncpg.Connection"
    async with pool.acquire() as connection:  # type: ignore
        statement = await connection.prepare(READ_ROW_SQL)  # type: ignore
        return [
            QueryResult(
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
    random: Random,
    queries: QueryCount | None = None,
) -> list[QueryResult]:
    num_queries = queries or 1
    updates = [
        (row_id, random.randint(1, 10000))
        for row_id in random.sample(range(1, 10000), num_queries)
    ]

    connection: "asyncpg.Connection"
    async with pool.acquire() as connection:  # type: ignore
        await connection.executemany(WRITE_ROW_SQL, updates)  # type: ignore

    return [QueryResult(id=row_id, randomNumber=number) for row_id, number in updates]


routes = [
    Path("/json", get=json_serialization),
    Path("/plaintext", get=plaintext),
    Path("/db", get=single_database_query),
    Path("/queries", get=multiple_database_queries),
    Path("/fortunes", get=fortunes),
    Path("/updates", get=database_updates),
]


async def main() -> None:
    config = uvicorn.Config(
        App(routes=routes),
        host="0.0.0.0",
        port=8080,
        log_level="ERROR",
    )
    await uvicorn.Server(config).serve()


if __name__ == "__main__":
    run = partial(anyio.run, main, backend_options={"uvloop": True})
    workers = multiprocessing.cpu_count()
    if os.environ.get("TRAVIS") == "true":
        workers = 2
    with ProcessPoolExecutor(workers) as exec:
        wait(
            [
                exec.submit(run)
                for _ in range(workers)
            ]
        )
