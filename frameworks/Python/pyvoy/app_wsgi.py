import orjson


JSON_HEADERS = [('content-type', 'application/json')]
PLAINTEXT_HEADERS = [('content-type', 'text/plain; charset=utf-8')]

json_dumps = orjson.dumps


def route_json(environ, proto):
    proto('200 OK', JSON_HEADERS)
    return [json_dumps({'message': 'Hello, world!'})]


def route_plaintext(environ, proto):
    proto('200 OK', PLAINTEXT_HEADERS)
    return [b'Hello, world!']


def handle_404(environ, proto):
    proto('404 NOT FOUND', PLAINTEXT_HEADERS)
    return [b"not found"]


routes = {
    '/json': route_json,
    '/plaintext': route_plaintext
}


def main(environ, proto):
    handler = routes.get(environ["PATH_INFO"], handle_404)
    return handler(environ, proto)
