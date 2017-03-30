from apistar import App, Route, wsgi
import ujson as json


def json_view() -> wsgi.WSGIResponse:
    content = json.dumps({'message': 'Welcome to API Star!'}).encode('utf-8')
    return wsgi.WSGIResponse(
        '200 OK',
        [('Content-Type', 'application/json')],
        [content]
    )


def plaintext_view() -> wsgi.WSGIResponse:
    return wsgi.WSGIResponse(
        '200 OK',
        [('Content-Type', 'text/plain')],
        [b'Hello, World!']
    )


app = App(routes=[
    Route('/json', 'GET', json_view),
    Route('/plaintext', 'GET', plaintext_view),
])
