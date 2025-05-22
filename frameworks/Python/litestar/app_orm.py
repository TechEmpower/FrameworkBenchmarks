import logging
import multiprocessing
import os
from contextlib import asynccontextmanager
from operator import attrgetter
from pathlib import Path
from random import randint, sample
from typing import Any

import orjson
from litestar import Litestar, MediaType, Request, get, Response
from litestar.contrib.jinja import JinjaTemplateEngine
from litestar.response import Template
from litestar.template import TemplateConfig
from sqlalchemy import Column, Integer, String, select
from sqlalchemy.ext.asyncio import async_sessionmaker, create_async_engine
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy.orm.attributes import flag_modified

logger = logging.getLogger(__name__)

Base = declarative_base()


class World(Base):
	__tablename__ = "world"
	id = Column(Integer, primary_key=True)
	randomnumber = Column(Integer)

	def __json__(self):
		return {"id": self.id, "randomnumber": self.randomnumber}


sa_data = World.__table__


class Fortune(Base):
	__tablename__ = "fortune"
	id = Column(Integer, primary_key=True)
	message = Column(String)


sa_fortunes = Fortune.__table__

ADDITIONAL_FORTUNE = Fortune(id=0, message="Additional fortune added at request time.")
MAX_POOL_SIZE = 1000 // multiprocessing.cpu_count()

sort_fortunes_key = attrgetter("message")

template_path = os.path.join(os.path.dirname(os.path.realpath(__file__)), "templates")


async def setup_database():
	dsn = "postgresql+asyncpg://%s:%s@tfb-database:5432/hello_world" % (
		os.getenv("PGPASS", "benchmarkdbuser"),
		os.getenv("PGPASS", "benchmarkdbpass"),
	)

	engine = create_async_engine(
		dsn,
		future=True,
		pool_size=MAX_POOL_SIZE,
		connect_args={
			"ssl": False  # NEEDED FOR NGINX-UNIT OTHERWISE IT FAILS
		},
	)
	return async_sessionmaker(engine)


@asynccontextmanager
async def lifespan(app: Litestar):
	# Set up the database connection pool
	app.state.db_session = await setup_database()
	yield
	# Close the database connection pool
	await app.state.db_session().close()


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


@get("/json")
async def json_serialization() -> Response:
	return Response(
		content=orjson.dumps({"message": "Hello, world!"}),
		media_type=MediaType.JSON,
	)


@get("/db")
async def single_database_query() -> Response:
	id_ = randint(1, 10000)

	async with app.state.db_session() as sess:
		result = await sess.get(World, id_)

	return Response(
		content=orjson.dumps(result.__json__()),
		media_type=MediaType.JSON,
	)


@get("/queries")
async def multiple_database_queries(queries: Any = None) -> Response:
	num_queries = get_num_queries(queries)
	data = []

	async with app.state.db_session() as sess:
		for id_ in sample(range(1, 10001), num_queries):
			result = await sess.get(World, id_)
			data.append(result.__json__())

	return Response(
		content=orjson.dumps(data),
		media_type=MediaType.JSON,
	)


@get("/fortunes", media_type=MediaType.HTML)
async def fortunes(request: Request) -> Template:
	async with app.state.db_session() as sess:
		ret = await sess.execute(select(Fortune.id, Fortune.message))
		data = ret.all()

	data.append(ADDITIONAL_FORTUNE)
	data.sort(key=sort_fortunes_key)

	return Template(
		"fortune.jinja",
		context={"request": request, "fortunes": data},
		media_type=MediaType.HTML,
	)


@get("/updates")
async def database_updates(queries: Any = None) -> Response:
	num_queries = get_num_queries(queries)

	ids = sorted(sample(range(1, 10000 + 1), num_queries))
	data = []
	async with app.state.db_session.begin() as sess:
		for id_ in ids:
			world = await sess.get(World, id_, populate_existing=True)
			world.randomnumber = randint(1, 10000)
			# force sqlalchemy to UPDATE entry even if the value has not changed
			# doesn't make sense in a real application, added only for pass `tfb verify`
			flag_modified(world, "randomnumber")
			data.append(world.__json__())

	return Response(
		content=orjson.dumps(data),
		media_type=MediaType.JSON,
	)


@get("/plaintext", media_type=MediaType.TEXT)
async def plaintext() -> bytes:
	return b"Hello, world!"


app = Litestar(
	template_config=TemplateConfig(
		directory=Path("templates"),
		engine=JinjaTemplateEngine,
	),
	lifespan=[lifespan],
	route_handlers=[
		json_serialization,
		single_database_query,
		multiple_database_queries,
		fortunes,
		database_updates,
		plaintext,
	],
)
