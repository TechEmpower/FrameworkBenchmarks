import multiprocessing
import sys

from granian import Granian


if __name__ == '__main__':
    interface = sys.argv[1]
    threading_mode = sys.argv[2]
    if threading_mode == "runtime":
        workers = multiprocessing.cpu_count()
        threads = 2
    else:
        workers = multiprocessing.cpu_count() // 2
        threads = 1

    Granian(
        f"app_{interface}:main",
        address="0.0.0.0",
        port=8080,
        workers=workers,
        threading_mode=threading_mode,
        threads=threads,
        backlog=2048,
        interface=interface,
        http="1",
        websockets=False
    ).serve()
