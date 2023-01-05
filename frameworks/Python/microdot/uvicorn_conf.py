import multiprocessing
import os
import sys

_is_travis = os.environ.get("TRAVIS") == "true"

workers = int(multiprocessing.cpu_count())
if _is_travis:
    workers = 2

worker_class = 'uvicorn.workers.UvicornWorker'
bind = "0.0.0.0:8080"
keepalive = 120
errorlog = "-"
pidfile = "gunicorn.pid"
