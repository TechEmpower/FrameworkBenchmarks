import multiprocessing
import os

from robyn import Response, Robyn
from robyn.argument_parser import Config


class SpecialConfig(Config):
    def __init__(self):
        super().__init__()
        self.workers = (os.cpu_count() * 2) + 1
        self.processes = os.cpu_count()
        self.log_level = "WARN"


app = Robyn(__file__, config=SpecialConfig())


@app.get("/plaintext")
def plaintext() -> str:
    return "Hello, world!"


@app.get("/json")
def json() -> dict:
    return {
        "message": "Hello, world!"
    }

if __name__ == "__main__":
    app.add_response_header("Server", "Roby1n")
    app.start(host="0.0.0.0", port=8080)
