from socketify import WSGI
import os
import multiprocessing
import logging

from hello_socketify.hello.wsgi import application


_is_travis = os.environ.get('TRAVIS') == 'true'

workers = int(multiprocessing.cpu_count())
if _is_travis:
    workers = 2


def run_app():
    WSGI(application).listen(8080, lambda config: logging.info(f"Listening on port http://localhost:{config.port} now\n")).run()


def create_fork():
    n = os.fork()
    # n greater than 0 means parent process
    if not n > 0:
        run_app()


# fork limiting the cpu count - 1
for i in range(1, multiprocessing.cpu_count()):
    create_fork()

run_app()  # run app on the main process too :)
