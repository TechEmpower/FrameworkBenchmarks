import os
import multiprocessing
import logging
from socketify import ASGI
from emmett import App
from emmett.tools import service

app = App(__name__)


app.config.handle_static = False

@app.route()
@service.json
async def json():
    return {'message': 'Hello, World!'}

@app.route(output='bytes')
async def plaintext():
    return b'Hello, World!'



_is_travis = os.environ.get('TRAVIS') == 'true'

workers = int(multiprocessing.cpu_count())
if _is_travis:
    workers = 2

def run_app():
    ASGI(app).listen(8080, lambda config: logging.info(f"Listening on port http://localhost:{config.port} now\n")).run()


def create_fork():
    n = os.fork()
    # n greater than 0 means parent process
    if not n > 0:
        run_app()


# fork limiting the cpu count - 1
for i in range(1, workers):
    create_fork()

run_app()  # run app on the main process too :)
