import multiprocessing
import os

_is_travis = os.environ.get("TRAVIS") == "true"

workers = multiprocessing.cpu_count()

bind = "0.0.0.0:8080"
keepalive = 120
errorlog = "-"
pidfile = "/tmp/litestar.pid"
loglevel = "error"
