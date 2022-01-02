"""
App config and initialization.
"""

import orjson
from pyramid.config import Configurator
from pyramid.renderers import JSON
from .models import get_engine, get_session_factory


def dbsession(request):
    sess = request.registry.dbsession_factory()

    def cleanup(request):
        sess.close()

    request.add_finished_callback(cleanup)
    return sess


def main(global_config, **settings):
    """This function returns a Pyramid WSGI application."""
    json_renderer = JSON(serializer=orjson.dumps)
    with Configurator(settings=settings) as config:
        config.include("pyramid_chameleon")
        config.add_renderer("json", json_renderer)
        config.add_route("test_1", "/json")
        config.add_route("test_2", "/db")
        config.add_route("test_3", "/queries")
        config.add_route("test_4", "/fortunes")
        config.add_route("test_5", "/updates")
        config.add_route("test_6", "/plaintext")
        config.set_default_csrf_options(require_csrf=False)

        engine = get_engine(settings)
        config.registry.dbsession_factory = get_session_factory(engine)
        config.add_request_method(dbsession, reify=True)

        config.scan()
    return config.make_wsgi_app()
