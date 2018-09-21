from apistar import App, Route, http


def json_view() -> http.JSONResponse:
    content = {'message': 'Hello, world!'}
    return http.JSONResponse(content, status_code=200)


def plaintext_view() -> http.Response:

    content = 'Hello, world!'
    headers = {'Content-Type': 'text/plain'}
    return http.Response(content, headers=headers)

app = App(routes=[
    Route('/json', 'GET', json_view),
    Route('/plaintext', 'GET', plaintext_view),
])
