from uvicorn.workers import UvicornWorker as _Worker


class UvicornWorker(_Worker):
    CONFIG_KWARGS = {
        "loop": "uvloop",
        "http": "httptools",
        "proxy_headers": False,
        "interface": "asgi3"
    }
