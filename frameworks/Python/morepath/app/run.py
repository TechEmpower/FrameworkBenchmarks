import os

import morepath

from app import App
from .model import db


def setup_db():
    DBHOST = 'tfb-database'

    db.bind(
        'postgres',
        user='benchmarkdbuser',
        password='benchmarkdbpass',
        host=DBHOST,
        database='hello_world'
    )
    db.generate_mapping(create_tables=True)


def wsgi_factory():   # pragma: no cover
    morepath.autoscan()

    App.commit()
    setup_db()

    return App()


application = wsgi_factory()   # pragma: no cover
