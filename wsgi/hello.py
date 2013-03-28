import ujson


def app(environ, start_response):
    response = {
      "message": "Hello, World!"
    }
    data = ujson.dumps(response)
    response_headers = [
        ('Content-type', 'text/plain'),
        ('Content-Length', str(len(data)))
    ]
    start_response('200 OK', response_headers)
    return [data]
