import multiprocessing
import sys

from granian import Granian


if __name__ == '__main__':
    interface = sys.argv[1]
    runtime_mode = sys.argv[2]
    workers = multiprocessing.cpu_count()

    if interface == "rsgi":
        #: leave 25% cpu to the Rust runtime
        workers = round(workers * 0.75)

    blocking_threads = None
    if interface == "wsgi":
        #: we don't run any I/O in WSGI benches
        blocking_threads = 1

    Granian(
        f"app_{interface}:main",
        address="0.0.0.0",
        port=8080,
        workers=workers,
        runtime_mode=runtime_mode,
        blocking_threads=blocking_threads,
        backlog=16384,
        interface=interface,
        http="1",
        websockets=False
    ).serve()
