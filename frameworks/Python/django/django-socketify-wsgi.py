import os
import sys
import multiprocessing
import logging
import socketify

app_dir = os.path.dirname(os.path.abspath(__file__)) + '/hello'
sys.path.append(app_dir)

from hello.wsgi import application as app


_is_travis = os.environ.get('TRAVIS') == 'true'

workers = int(multiprocessing.cpu_count())
if _is_travis:
    workers = 2


def run_app():
    msg = f"Listening on port 8080 now\n"
    socketify.WSGI(app).listen(8080, lambda config: logging.info(msg)).run()


def create_fork():
    n = os.fork()
    # n greater than 0 means parent process
    if not n > 0:
        run_app()


# fork limiting the cpu count - 1
for i in range(1, multiprocessing.cpu_count()):
    create_fork()

run_app()  # run app on the main process too :)
