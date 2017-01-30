import os
from pathlib import Path

import aiohttp_jinja2
import aiopg.sa
import asyncpg
import jinja2
from aiohttp import web
from sqlalchemy.engine.url import URL

from .views import (
    json,
    single_database_query_orm,
    multiple_database_queries_orm,
    fortunes,
    updates,
    plaintext,

    single_database_query_raw,
    multiple_database_queries_raw,
    fortunes_raw,
    updates_raw,
)

THIS_DIR = Path(__file__).parent


def pg_dsn() -> str:
    """
    :return: DSN url suitable for sqlalchemy and aiopg.
    """
    return str(URL(
        database='hello_world',
        password=os.getenv('PGPASS', 'benchmarkdbpass'),
        host=os.getenv('DBHOST', 'localhost'),
        port='5432',
        username=os.getenv('PGUSER', 'benchmarkdbuser'),
        drivername='postgres',
    ))


async def startup(app: web.Application):
    dsn = pg_dsn()
    app.update(
        aiopg_engine=await aiopg.sa.create_engine(dsn=dsn, minsize=10, maxsize=20, loop=app.loop),
        asyncpg_pool=await asyncpg.create_pool(dsn=dsn, min_size=10, max_size=20, loop=app.loop),
    )


async def cleanup(app: web.Application):
    app['aiopg_engine'].close()
    await app['aiopg_engine'].wait_closed()
    await app['asyncpg_pool'].close()


def setup_routes(app):
    app.router.add_get('/json', json)
    app.router.add_get('/db', single_database_query_orm)
    app.router.add_get('/queries', multiple_database_queries_orm)
    app.router.add_get('/fortunes', fortunes)
    app.router.add_get('/updates', updates)
    app.router.add_get('/plaintext', plaintext)

    app.router.add_get('/raw/db', single_database_query_raw)
    app.router.add_get('/raw/queries', multiple_database_queries_raw)
    app.router.add_get('/raw/fortunes', fortunes_raw)
    app.router.add_get('/raw/updates', updates_raw)


def create_app(loop):
    app = web.Application(loop=loop)

    jinja2_loader = jinja2.FileSystemLoader(str(THIS_DIR / 'templates'))
    aiohttp_jinja2.setup(app, loader=jinja2_loader)

    app.on_startup.append(startup)
    app.on_cleanup.append(cleanup)

    setup_routes(app)
    return app
