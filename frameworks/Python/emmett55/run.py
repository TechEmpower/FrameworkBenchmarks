import multiprocessing

from emmett_core.server import run


if __name__ == "__main__":
    workers = multiprocessing.cpu_count()

    run(
        "rsgi",
        ("app", "app"),
        host="0.0.0.0",
        port=8080,
        workers=workers,
        backlog=16384,
        threading_mode="runtime",
        http="1",
        enable_websockets=False,
        log_level="warn"
    )
