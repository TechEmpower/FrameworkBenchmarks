from socketify import ASGI
import os
try:
    from ujson import dumps as json
except:
    from json import dumps as json
    
async def app(scope, receive, send):
    assert scope['type'] == 'http'
    path = scope['path']
    if path == "/plaintext":
        await send({
            'type': 'http.response.start',
            'status': 200,
            'headers': [
                [b'content-type', b'text/plain'],
            ],
        })
        await send({
            'type': 'http.response.body',
            'body': b'Hello, world!',
        })
        return
    if path == "/json":
        await send({
            'type': 'http.response.start',
            'status': 200,
            'headers': [
                [b'content-type', b'application/json'],
            ],
        })
        await send({
            'type': 'http.response.body',
            'body': json({"message":"Hello, World!"}).encode('utf8'),
        })
        


def run_app():
    ASGI(app,lifespan=False).listen(3000, lambda config: print(f"Listening on port http://localhost:{config.port} now\n")).run()

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