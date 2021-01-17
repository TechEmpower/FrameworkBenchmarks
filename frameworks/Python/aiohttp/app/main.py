import os
import multiprocessing
from pathlib import Path

import aiohttp_jinja2
import asyncpg
import jinja2
from aiohttp import web
from sqlalchemy.engine.url import URL
from sqlalchemy.ext.asyncio import AsyncSession, create_async_engine
from sqlalchemy.orm import sessionmaker

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

CONNECTION_ORM = os.getenv('CONNECTION', 'ORM').upper() == 'ORM'

THIS_DIR = Path(__file__).parent


def pg_dsn() -> str:
    """
    :return: DSN url suitable for sqlalchemy and aiopg.
    """
    return str(URL.create(
        database='hello_world',
        password=os.getenv('PGPASS', 'benchmarkdbpass'),
        host='tfb-database',
        port='5432',
        username=os.getenv('PGUSER', 'benchmarkdbuser'),
        drivername='postgresql',
    ))


async def db_ctx(app: web.Application):
    dsn = pg_dsn()
    # number of gunicorn workers = multiprocessing.cpu_count() as per gunicorn_conf.py
    # max_connections = 2000 as per toolset/setup/linux/databases/postgresql/postgresql.conf:64
    # give 10% leeway
    max_size = min(1800 / multiprocessing.cpu_count(), 160)
    max_size = max(int(max_size), 1)
    min_size = max(int(max_size / 2), 1)
    print(f'connection pool: min size: {min_size}, max size: {max_size}, orm: {CONNECTION_ORM}')
    if CONNECTION_ORM:
        engine = create_async_engine(dsn, future=True, pool_size=max_size)
        app['db_session'] = sessionmaker(engine, class_=AsyncSession)
    else:
        app['pg'] = await asyncpg.create_pool(dsn=dsn, min_size=min_size, max_size=max_size, loop=app.loop)

    yield

    if not CONNECTION_ORM:
        await app['pg'].close()


def setup_routes(app):
    if CONNECTION_ORM:
        app.router.add_get('/json', json)
        app.router.add_get('/db', single_database_query_orm)
        app.router.add_get('/queries/{queries:.*}', multiple_database_queries_orm)
        app.router.add_get('/fortunes', fortunes)
        app.router.add_get('/updates/{queries:.*}', updates)
        app.router.add_get('/plaintext', plaintext)
    else:
        app.router.add_get('/db', single_database_query_raw)
        app.router.add_get('/queries/{queries:.*}', multiple_database_queries_raw)
        app.router.add_get('/fortunes', fortunes_raw)
        app.router.add_get('/updates/{queries:.*}', updates_raw)


def create_app():
    app = web.Application()

    jinja2_loader = jinja2.FileSystemLoader(str(THIS_DIR / 'templates'))
    aiohttp_jinja2.setup(app, loader=jinja2_loader)

    app.cleanup_ctx.append(db_ctx)

    setup_routes(app)
    return app
