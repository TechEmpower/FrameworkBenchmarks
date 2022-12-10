import os
import multiprocessing
import logging
from fastapi import FastAPI, Request
from fastapi.responses import PlainTextResponse
from socketify import ASGI


try:
    import orjson
    from fastapi.responses import ORJSONResponse as JSONResponse
except ImportError:
    from fastapi.responses import JSONResponse as JSONResponse

app = FastAPI()

@app.get("/json")
async def json_serialization():
    return JSONResponse({"message": "Hello, world!"})

@app.get("/plaintext")
async def plaintext():
    return PlainTextResponse(b"Hello, world!")


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