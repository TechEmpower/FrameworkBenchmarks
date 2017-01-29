import os
from pathlib import Path

import aiohttp_jinja2
import jinja2
from aiohttp import web

from aiopg.sa import create_engine
from sqlalchemy.engine.url import URL

from .views import json, plaintext


THIS_DIR = Path(__file__).parent


def pg_dsn() -> str:
    """
    :return: DSN url suitable for sqlalchemy and aiopg.
    """
    return str(URL(
        database='hello_world',
        password='benchmarkdbpass',
        host=os.environ.get('DBHOST', ''),
        port='5432',
        username='benchmarkdbuser',
        drivername='postgres',
    ))


async def startup(app: web.Application):
    app['pg_engine'] = await create_engine(pg_dsn(), loop=app.loop)


async def cleanup(app: web.Application):
    app['pg_engine'].close()
    await app['pg_engine'].wait_closed()


def setup_routes(app):
    app.router.add_get('/json', json)
    app.router.add_get('/plaintext', plaintext)


def create_app(loop):
    app = web.Application(loop=loop)

    jinja2_loader = jinja2.FileSystemLoader(str(THIS_DIR / 'templates'))
    aiohttp_jinja2.setup(app, loader=jinja2_loader)

    # app.on_startup.append(startup)
    # app.on_cleanup.append(cleanup)

    setup_routes(app)
    return app
