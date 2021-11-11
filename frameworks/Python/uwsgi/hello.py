import ujson
from email.utils import formatdate


def application(environ, start_response):
    response = {
      "message": "Hello, World!"
    }
    data = ujson.dumps(response)
    response_headers = [
        ('Server', 'uwsgi'),
        ('Date', formatdate(timeval=None, localtime=False, usegmt=True)),
        ('Content-Type', 'application/json'),
        ('Content-Length', str(len(data)))
    ]
    start_response('200 OK', response_headers)
    return [data]
