import os
import fastwsgi

try:
    from ujson import dumps as jsonify
except:
    from json import dumps as jsonify


def app(environ, start_response):
    path = environ["PATH_INFO"]
    headers = [ ('Server', 'FastWSGI') ]
    
    if path == "/json":
        headers.append( ('Content-Type', 'application/json') )
        start_response('200 OK', headers)
        return [ jsonify( {"message":"Hello, World!"} ).encode('utf8') ]

    if path == "/plaintext":
        headers.append( ('Content-Type', 'text/plain') )
        start_response('200 OK', headers)
        return [ b'Hello, World!' ]

    start_response('400 Bad Request', headers)
    return [ b'' ]


if __name__ == "__main__":
    import multiprocessing

    _is_travis = os.environ.get('TRAVIS') == 'true'

    workers = int(multiprocessing.cpu_count())
    if _is_travis:
        workers = 2

    host = '0.0.0.0'
    port = 3000

    def run_app():
        fastwsgi.run(app, host, port, loglevel=0)

    def create_fork():
        n = os.fork()
        # n greater than 0 means parent process
        if not n > 0:
            run_app()

    # fork limiting the cpu count - 1
    for i in range(1, workers):
        create_fork()

    run_app()  # run app on the main process too :)
