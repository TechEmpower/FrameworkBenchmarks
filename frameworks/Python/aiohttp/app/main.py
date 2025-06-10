import multiprocessing
import os
import platform

from aiohttp import web
from sqlalchemy.engine.url import URL
from sqlalchemy.ext.asyncio import async_sessionmaker, create_async_engine

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

if platform.python_implementation() != "PyPy":
    import asyncpg

    class NoResetConnection(asyncpg.Connection):
        __slots__ = ()
    
        def get_reset_query(self):
            return ""

CONNECTION_ORM = os.getenv('CONNECTION', 'ORM').upper() == 'ORM'


def pg_dsn(dialect=None) -> str:
    """
    :return: DSN url suitable for sqlalchemy and aiopg.
    """
    url = URL.create(
        database='hello_world',
        password=os.getenv('PGPASS', 'benchmarkdbpass'),
        host='tfb-database',
        port='5432',
        username=os.getenv('PGUSER', 'benchmarkdbuser'),
        drivername='postgresql+{}'.format(dialect) if dialect else 'postgresql',
    )
    return url.render_as_string(hide_password=False)

async def db_ctx(app: web.Application):
    # number of gunicorn workers = multiprocessing.cpu_count() as per gunicorn_conf.py
    # max_connections = 2000 as per toolset/setup/linux/databases/postgresql/postgresql.conf:64
    # since the world table contains only 10,000 rows, a large connection pool is unnecessary
    # the server hardware provides 56 CPU cores producing high concurrency
    # https://wiki.postgresql.org/wiki/Number_Of_Database_Connections
    max_size = 2
    min_size = 2
    print(f'connection pool: min size: {min_size}, max size: {max_size}, orm: {CONNECTION_ORM}')
    if CONNECTION_ORM:
        dsn = pg_dsn('asyncpg')
        engine = create_async_engine(dsn, pool_size=max_size)
        app['db_session'] = async_sessionmaker(engine)
    else:
        dsn = pg_dsn()
        app['pg'] = await asyncpg.create_pool(dsn=dsn, min_size=min_size, max_size=max_size, loop=app.loop, connection_class=NoResetConnection)

    yield

    if CONNECTION_ORM:
        await app['db_session'].dispose()
    else:
        await app['pg'].close()


def setup_routes(app):
    if CONNECTION_ORM:
        app.router.add_get('/db', single_database_query_orm)
        app.router.add_get('/queries/{queries:.*}', multiple_database_queries_orm)
        app.router.add_get('/fortunes', fortunes)
        app.router.add_get('/updates/{queries:.*}', updates)
    else:
        app.router.add_get('/json', json)
        app.router.add_get('/plaintext', plaintext)
        app.router.add_get('/db', single_database_query_raw)
        app.router.add_get('/queries/{queries:.*}', multiple_database_queries_raw)
        app.router.add_get('/fortunes', fortunes_raw)
        app.router.add_get('/updates/{queries:.*}', updates_raw)


def create_app():
    app = web.Application()
    if platform.python_implementation() != "PyPy":
        app.cleanup_ctx.append(db_ctx)
    setup_routes(app)
    return app
