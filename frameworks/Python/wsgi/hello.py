import ujson
import sys

if sys.version_info[0] == 3:
    def encode(obj):
        return ujson.dumps(obj).encode('utf-8')
else:
    def encode(obj):
        return ujson.dumps(obj)

def json(environ, start_response):
    response = {"message": "Hello, World!"}
    data = encode(response)
    response_headers = [
        ('Content-type', 'application/json'),
        ('Content-Length', str(len(data)))
    ]
    start_response('200 OK', response_headers)
    return [data]

def plaintext(environ, start_response):
    data = b"Hello, World!"
    response_headers = [
        ('Content-type', 'text/plain'),
        ('Content-Length', str(len(data)))
    ]
    start_response('200 OK', response_headers)
    return [data]

def app(environ, start_response):
    path = environ['PATH_INFO']
    if path.startswith('/json'):
        return json(environ, start_response)
    else:
        return plaintext(environ, start_response)
