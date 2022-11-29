
import threading
import time
import os

from ujson import dumps as json
from datetime import datetime

from socketify import App

def plaintext(res, req):
    res.write_header("Server", "socketify")
    res.write_header("Content-Type", "text/plain")
    res.end("Hello, World!")

def applicationjson(res, req):
    res.write_header("Server", "socketify")
    res.write_header("Content-Type", "application/json")
    res.end(json({"message":"Hello, World!"}))


def run_app():
    app = App(request_response_factory_max_itens=200_000)
    
    app.get("/", plaintext)
    app.get("/json", applicationjson)
    app.get("/plaintext", plaintext)
    app.listen(3000)
    app.run()
    
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

