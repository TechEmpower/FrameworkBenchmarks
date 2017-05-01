from wsgiref.handlers import format_date_time

def generate_http_response(payload, content_type='application/json'):
    return ("""HTTP/1.1 200 OK
CONTENT-TYPE: %s
CONTENT-LENGTH: %s
CONNECTION: keep-alive
DATE: %s
SERVER: yocto_http/0.0.2

%s""" % (content_type, len(payload), format_date_time(None), payload)).encode('utf-8')
