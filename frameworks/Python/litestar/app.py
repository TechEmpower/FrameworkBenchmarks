import multiprocessing
import os
from contextlib import asynccontextmanager
from pathlib import Path
from random import randint, sample
from typing import Any

import asyncpg
import orjson
from litestar import Litestar, MediaType, Request, get, Response
from litestar.contrib.jinja import JinjaTemplateEngine
from litestar.response import Template
from litestar.template import TemplateConfig

READ_ROW_SQL = 'SELECT "id", "randomnumber" FROM "world" WHERE id = $1'
WRITE_ROW_SQL = 'UPDATE "world" SET "randomnumber"=$1 WHERE id=$2'
MAX_POOL_SIZE = 1000 // multiprocessing.cpu_count()
MIN_POOL_SIZE = max(int(MAX_POOL_SIZE / 2), 1)


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


async def setup_database():
	return await asyncpg.create_pool(
		user=os.getenv("PGUSER", "benchmarkdbuser"),
		password=os.getenv("PGPASS", "benchmarkdbpass"),
		database="hello_world",
		host="tfb-database",
		port=5432,
		min_size=MIN_POOL_SIZE,
		max_size=MAX_POOL_SIZE,
	)


@asynccontextmanager
async def lifespan(app: Litestar):
	# Set up the database connection pool
	app.state.connection_pool = await setup_database()
	yield
	# Close the database connection pool
	await app.state.connection_pool.close()


@get("/json")
async def json_serialization() -> Response:
	return Response(
		content=orjson.dumps({"message": "Hello, world!"}),
		media_type=MediaType.JSON,
	)


@get("/db")
async def single_database_query() -> Response:
	row_id = randint(1, 10000)
	async with app.state.connection_pool.acquire() as connection:
		number = await connection.fetchval(READ_ROW_SQL, row_id)

	return Response(
		content=orjson.dumps({"id": row_id, "randomNumber": number}),
		media_type=MediaType.JSON,
	)


@get("/queries")
async def multiple_database_queries(queries: Any = None) -> Response:
	num_queries = get_num_queries(queries)
	row_ids = sample(range(1, 10000), num_queries)
	worlds = []

	async with app.state.connection_pool.acquire() as connection:
		statement = await connection.prepare(READ_ROW_SQL)
		for row_id in row_ids:
			number = await statement.fetchval(row_id)
			worlds.append({"id": row_id, "randomNumber": number})

	return Response(
		content=orjson.dumps(worlds),
		media_type=MediaType.JSON,
	)


@get("/fortunes", media_type=MediaType.HTML)
async def fortunes(request: Request) -> Template:
	async with app.state.connection_pool.acquire() as connection:
		fortunes = await connection.fetch("SELECT * FROM Fortune")

	fortunes.append([0, 'Additional fortune added at request time.'])
	fortunes.sort(key=lambda row: row[1])
	return Template(
		"fortune.html",
		context={"fortunes": fortunes, "request": request},
		media_type=MediaType.HTML,
	)


@get("/updates")
async def database_updates(queries: Any = None) -> bytes:
	num_queries = get_num_queries(queries)
	# To avoid deadlock
	ids = sorted(sample(range(1, 10000 + 1), num_queries))
	numbers = sorted(sample(range(1, 10000), num_queries))
	updates = list(zip(ids, numbers, strict=False))

	worlds = [{"id": row_id, "randomNumber": number} for row_id, number in updates]

	async with app.state.connection_pool.acquire() as connection:
		statement = await connection.prepare(READ_ROW_SQL)
		for row_id, _ in updates:
			await statement.fetchval(row_id)
		await connection.executemany(WRITE_ROW_SQL, updates)

	return Response(
		content=orjson.dumps(worlds),
		media_type=MediaType.JSON,
	)


@get("/plaintext", media_type=MediaType.TEXT)
async def plaintext() -> bytes:
	return b"Hello, world!"


app = Litestar(
	lifespan=[lifespan],
	template_config=TemplateConfig(
		directory=Path("templates"),
		engine=JinjaTemplateEngine,
	),
	route_handlers=[
		json_serialization,
		single_database_query,
		multiple_database_queries,
		fortunes,
		database_updates,
		plaintext,
	],
)
