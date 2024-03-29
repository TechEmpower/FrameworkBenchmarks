import multiprocessing
import os
import sys


_is_pypy = hasattr(sys, 'pypy_version_info')
_is_travis = os.environ.get('TRAVIS') == 'true'
_is_asyncio = os.environ.get('ASYNCIO') == 'true'

workers = int(multiprocessing.cpu_count() * 1.5)
if _is_travis:
    workers = 2

bind = "0.0.0.0:8080"
keepalive = 60
errorlog = '-'
pidfile = 'gunicorn.pid'

if _is_pypy:
    worker_class = "sync"
elif _is_asyncio:
    worker_class = "uvicorn.workers.UvicornWorker"
else:
    worker_class = "meinheld.gmeinheld.MeinheldWorker"

    def post_fork(server, worker):
        # Disalbe access log
        import meinheld.server
        meinheld.server.set_access_logger(None)
