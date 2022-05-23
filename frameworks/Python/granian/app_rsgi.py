import orjson

from granian.rsgi import Response

JSON_HEADERS = {'content-type': 'application/json'}
PLAINTEXT_HEADERS = {'content-type': 'text/plain; charset=utf-8'}

json_dumps = orjson.dumps


async def route_json(scope, receive):
    return Response(
        1, 200, JSON_HEADERS,
        json_dumps({'message': 'Hello, world!'}), None, None
    )


async def route_plaintext(scope, receive):
    return Response(
        2, 200, PLAINTEXT_HEADERS,
        None, 'Hello, world!', None
    )


async def handle_404(scope, receive):
    return Response(
        2, 404, PLAINTEXT_HEADERS,
        None, 'Not found', None
    )


routes = {
    '/json': route_json,
    '/plaintext': route_plaintext
}


def main(scope, receive):
    handler = routes.get(scope.path, handle_404)
    return handler(scope, receive)
