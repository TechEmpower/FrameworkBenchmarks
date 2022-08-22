#!/usr/bin/env python
import orjson
from falcon import media
from app import wsgi


# custom JSON handler
JSONHandler = media.JSONHandler(dumps=orjson.dumps, loads=orjson.loads)
extra_handlers = {
    "application/json": JSONHandler,
    "application/json; charset=UTF-8": JSONHandler
}
wsgi.req_options.media_handlers.update(extra_handlers)
wsgi.resp_options.media_handlers.update(extra_handlers)
