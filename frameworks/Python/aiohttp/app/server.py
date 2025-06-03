import multiprocessing
import os
import platform
import socket

from aiohttp import web

from .main import create_app

SERVERS_COUNT = multiprocessing.cpu_count()
BACKLOG = 2048
SOCKET_BACKLOG = BACKLOG * SERVERS_COUNT

def start_server(sock, cpu_id):
    if hasattr(os, "sched_setaffinity"):
        os.sched_setaffinity(0, {cpu_id})
    if platform.python_implementation() != "PyPy":
        import uvloop
        uvloop.install()
    app = create_app()

    web.run_app(app, sock=sock, backlog=BACKLOG, access_log=None)


def create_reusable_socket(host='0.0.0.0', port=8080):
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEPORT, 1)
    sock.setsockopt(socket.IPPROTO_TCP, socket.TCP_NODELAY, 1)

    sock.bind((host, port))
    sock.setblocking(False)
    sock.set_inheritable(True)

    sock.listen(SOCKET_BACKLOG)

    return sock


if __name__ == '__main__':
    sock = create_reusable_socket()
    workers = []
    for cpu_id in range(SERVERS_COUNT):
        worker = multiprocessing.Process(target=start_server, args=(sock, cpu_id))
        worker.daemon = True
        worker.start()
        workers.append(worker)

    for worker in workers:
        worker.join()
