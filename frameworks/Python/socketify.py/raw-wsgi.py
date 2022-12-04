from socketify import WSGI
import os

try:
    from ujson import dumps as json
except:
    from json import dumps as json
    
def app(environ, start_response):
    path = environ["PATH_INFO"]
    if path == "/plaintext":
        start_response('200 OK', [('Content-Type', 'text/plain')])
        yield b'Hello, World!\n'
        return
    if path == "/json":
        start_response('200 OK', [('Content-Type', 'application/json')])
        yield json({"message":"Hello, World!"}).encode('utf8')
        return
    
def run_app():
    WSGI(app).listen(3000, lambda config: print(f"Listening on port http://localhost:{config.port} now\n")).run()

def create_fork():
    n = os.fork()
    # n greater than 0 means parent process
    if not n > 0:
        run_app()

def get_worker_count():
    try:
        return int(os.environ["WORKER_COUNT"])
    except:
        return 2

WORKER_COUNT = get_worker_count() - 1

for index in range(WORKER_COUNT):
    create_fork()

run_app()