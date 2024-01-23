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
        threads=2,
        backlog=2048,
        threading_mode='runtime',
        enable_websockets=False,
        log_level="warn"
    )
