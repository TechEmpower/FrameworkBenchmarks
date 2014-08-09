import multiprocessing
import sys

_is_pypy = hasattr(sys, 'pypy_version_info')


workers = multiprocessing.cpu_count() * 3
bind = "0.0.0.0:8080"
keepalive = 120
errorlog = '-'


if _is_pypy:
    worker_class = "tornado"
else:
    worker_class = "meinheld.gmeinheld.MeinheldWorker"

    def post_fork(server, worker):
        # Disalbe access log
        import meinheld.server
        meinheld.server.set_access_logger(None)

