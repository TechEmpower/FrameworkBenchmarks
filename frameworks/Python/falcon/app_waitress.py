#!/usr/bin/env python
from waitress import serve
from app import wsgi
from gunicorn_conf import bind, keepalive, workers
import logging


if __name__ == "__main__":
    logging.basicConfig()
    logging.getLogger().setLevel(logging.CRITICAL)
    logging.disable(True)

    serve(
        app=wsgi,
        listen=bind,
        log_socket_errors=False,
        threads=workers,
        expose_tracebacks=False,
        connection_limit=128,
        channel_timeout=keepalive,
        _quiet=True)
