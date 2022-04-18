import orjson

JSON_RESPONSE = {
    'type': 'http.response.start',
    'status': 200,
    'headers': [
        [b'content-type', b'application/json'],
    ]
}
PLAINTEXT_RESPONSE = {
    'type': 'http.response.start',
    'status': 200,
    'headers': [
        [b'content-type', b'text/plain; charset=utf-8'],
    ]
}

json_dumps = orjson.dumps


async def route_json(scope, receive, send):
    await send(JSON_RESPONSE)
    await send({
        'type': 'http.response.body',
        'body': json_dumps({'message': 'Hello, world!'}),
        'more_body': False
    })


async def route_plaintext(scope, receive, send):
    await send(PLAINTEXT_RESPONSE)
    await send({
        'type': 'http.response.body',
        'body': b'Hello, world!',
        'more_body': False
    })


async def handle_404(scope, receive, send):
    await send(PLAINTEXT_RESPONSE)
    await send({
        'type': 'http.response.body',
        'body': b'Not found',
        'more_body': False
    })


routes = {
    '/json': route_json,
    '/plaintext': route_plaintext
}


def main(scope, receive, send):
    handler = routes.get(scope['path'], handle_404)
    return handler(scope, receive, send)
