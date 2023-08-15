import multiprocessing
import os

from robyn import Robyn
from robyn.argument_parser import Config


class SpecialConfig(Config):
    def __init__(self):
        super().__init__()
        self.workers = 2
        self.processes = (os.cpu_count() * 2) + 1
        self.log_level = "WARN"


app = Robyn(__file__, config=SpecialConfig())


@app.get("/plaintext", const=True)
def plaintext() -> str:
    return "Hello, world!"


if __name__ == "__main__":
    app.add_response_header("Server", "Robyn")
    app.add_response_header("Content-Type", "text/plain")

    app.start(url="0.0.0.0", port=8080)
