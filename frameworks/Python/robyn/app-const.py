import multiprocessing

from robyn import Robyn
from robyn.argument_parser import Config


class BenchConfig(Config):
    def __init__(self):
        super().__init__()
        self.workers = multiprocessing.cpu_count()
        self.processes = 1
        self.log_level = "WARN"


app = Robyn(__file__, config=BenchConfig())


@app.get("/plaintext", const=True)
def plaintext() -> str:
    return "Hello, world!"


@app.get("/json", const=True)
def json() -> dict:
    return {"message": "Hello, world!"}


if __name__ == "__main__":
    app.add_response_header("Server", "Robyn")
    app.start(host="0.0.0.0", port=8080)
