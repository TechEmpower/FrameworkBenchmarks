import multiprocessing

from emmett.server import run


if __name__ == "__main__":
    workers = round(multiprocessing.cpu_count() / 2)

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
