import logging
import multiprocessing
import os
from contextlib import asynccontextmanager
from operator import attrgetter
from random import randint, sample

from sqlalchemy import Column, Integer, String, select
from sqlalchemy.ext.asyncio import AsyncSession, create_async_engine
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy.orm import sessionmaker
from sqlalchemy.orm.attributes import flag_modified

from fastapi import FastAPI, Request
from fastapi.responses import PlainTextResponse, UJSONResponse
from fastapi.templating import Jinja2Templates

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

ADDITIONAL_FORTUNE = Fortune(
    id=0, message="Additional fortune added at request time."
)
MAX_POOL_SIZE = 1000//multiprocessing.cpu_count()

sort_fortunes_key = attrgetter("message")

template_path = os.path.join(
    os.path.dirname(os.path.realpath(__file__)), "templates"
)
templates = Jinja2Templates(directory=template_path)


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
    return sessionmaker(engine, class_=AsyncSession)


@asynccontextmanager
async def lifespan(app: FastAPI):
    # Setup the database connection pool
    app.state.db_session = await setup_database()
    yield
    # Close the database connection pool
    await app.state.db_session.close()


app = FastAPI(lifespan=lifespan)


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


@app.get("/json")
async def json_serialization():
    return UJSONResponse({"message": "Hello, world!"})


@app.get("/db")
async def single_database_query():
    id_ = randint(1, 10000)

    async with app.state.db_session() as sess:
        result = await sess.get(World, id_)

    return UJSONResponse(result.__json__())


@app.get("/queries")
async def multiple_database_queries(queries=None):
    num_queries = get_num_queries(queries)
    data = []

    async with app.state.db_session() as sess:
        for id_ in sample(range(1, 10001), num_queries):
            result = await sess.get(World, id_)
            data.append(result.__json__())

    return UJSONResponse(data)


@app.get("/fortunes")
async def fortunes(request: Request):
    async with app.state.db_session() as sess:
        ret = await sess.execute(select(Fortune.id, Fortune.message))
        data = ret.all()

    data.append(ADDITIONAL_FORTUNE)
    data.sort(key=sort_fortunes_key)

    return templates.TemplateResponse(
        "fortune.jinja", {"request": request, "fortunes": data}
    )


@app.get("/updates")
async def database_updates(queries=None):
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

    return UJSONResponse(data)


@app.get("/plaintext")
async def plaintext():
    return PlainTextResponse(b"Hello, world!")
