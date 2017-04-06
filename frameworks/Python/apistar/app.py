from apistar import App, Route, wsgi
import ujson as json


def json_view() -> wsgi.WSGIResponse:
    content = json.dumps({'message': 'Hello, world!'}).encode('utf-8')
    return wsgi.WSGIResponse(
        '200 OK',
        [
            ('Content-Type', 'application/json'),
            ('Content-Length', str(len(content)))
        ],
        [content]
    )


def plaintext_view() -> wsgi.WSGIResponse:
    content = b'Hello, world!'
    return wsgi.WSGIResponse(
        '200 OK',
        [
            ('Content-Type', 'text/plain'),
            ('Content-Length', str(len(content)))
        ],
        [content]
    )


app = App(routes=[
    Route('/json', 'GET', json_view),
    Route('/plaintext', 'GET', plaintext_view),
])
