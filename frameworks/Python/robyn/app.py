import multiprocessing
import os

from robyn import Robyn

app = Robyn(__file__)


@app.get('/plaintext')
async def plaintext() -> str:
    return "Hello, world!"


if __name__ == '__main__':
    _is_travis = os.environ.get('TRAVIS') == 'true'

    workers = multiprocessing.cpu_count() * 2 + 1
    if _is_travis:
        workers = 2

    app.processes = workers
    app.add_header("Server", "Robyn")
    app.add_header("Content-Type", "text/plain")

    app.start(url="0.0.0.0", port=8080)
