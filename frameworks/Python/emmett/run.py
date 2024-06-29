import multiprocessing

from emmett.server import run


if __name__ == "__main__":
    cpus = multiprocessing.cpu_count()

    run(
        "rsgi",
        ("app", "app"),
        host="0.0.0.0",
        port=8080,
        workers=cpus,
        backlog=16384,
        threading_mode='runtime',
        enable_websockets=False,
        log_level="warn"
    )
