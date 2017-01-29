import multiprocessing
import os

_is_travis = os.environ.get('TRAVIS') == 'true'

workers = multiprocessing.cpu_count() * 3
if _is_travis:
    workers = 2

bind = '0.0.0.0:8000'
keepalive = 120
errorlog = '-'
pidfile = 'gunicorn.pid'

worker_class = 'aiohttp.worker.GunicornUVLoopWebWorker'

