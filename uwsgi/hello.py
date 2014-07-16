import ujson


def application(environ, start_response):
    response = {
      "message": "Hello, World!"
    }
    data = ujson.dumps(response)
    response_headers = [
        ('Content-type', 'application/json'),
        ('Content-Length', str(len(data)))
    ]
    start_response('200 OK', response_headers)
    return [data]
