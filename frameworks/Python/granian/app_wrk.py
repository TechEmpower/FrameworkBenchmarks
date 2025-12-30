import orjson

JSON_HEADERS = [('content-type', 'application/json')]
PLAINTEXT_HEADERS = [('content-type', 'text/plain; charset=utf-8')]

json_dumps = orjson.dumps


async def route_json(scope, proto):
    proto.response_bytes(
        200,
        JSON_HEADERS,
        json_dumps({'message': 'Hello, world!'})
    )


async def route_plaintext(scope, proto):
    proto.response_bytes(
        200,
        PLAINTEXT_HEADERS,
        b'Hello, world!'
    )


async def handle_404(scope, proto):
    proto.response_bytes(
        404,
        PLAINTEXT_HEADERS,
        b'Not found'
    )


routes = {
    '/json': route_json,
    '/plaintext': route_plaintext
}


def main(scope, proto):
    handler = routes.get(scope.path, handle_404)
    return handler(scope, proto)
