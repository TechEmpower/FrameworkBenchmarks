import multiprocessing
import os

_is_travis = os.environ.get('TRAVIS') == 'true'

workers = multiprocessing.cpu_count()
if _is_travis:
    workers = 2

bind = ["0.0.0.0:8080"]
keep_alive_timeout = 120
worker_class = "uvloop"
