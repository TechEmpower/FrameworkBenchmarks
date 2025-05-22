import os
import multiprocessing
import logging

import orjson
from litestar import Litestar, get, MediaType
from socketify import ASGI


@get("/json")
async def json_serialization():
	return orjson.dumps({"message": "Hello, world!"})


@get("/plaintext", media_type=MediaType.TEXT)
async def plaintext():
	return b"Hello, world!"


app = Litestar(
	route_handlers=[
		json_serialization,
		plaintext,
	]
)

_is_travis = os.environ.get("TRAVIS") == "true"

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
