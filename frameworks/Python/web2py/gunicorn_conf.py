import multiprocessing
import os
import sys

_is_travis = os.environ.get('TRAVIS') == 'true'

workers = multiprocessing.cpu_count() * 3
if _is_travis:
    workers = 2

bind = "0.0.0.0:8080"
keepalive = 120
errorlog = '-'
pidfile = 'gunicorn.pid'
pythonpath = 'web2py'

worker_class = "meinheld.gmeinheld.MeinheldWorker"

def post_fork(server, worker):
    # Disable access log
    import meinheld.server
    meinheld.server.set_access_logger(None)
