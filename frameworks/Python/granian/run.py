import multiprocessing
import sys

from granian import Granian


if __name__ == '__main__':
    interface = sys.argv[1]

    Granian(
        f"app_{interface}:main",
        address="0.0.0.0",
        port=8080,
        workers=multiprocessing.cpu_count(),
        backlog=2048,
        interface=interface
    ).serve()
