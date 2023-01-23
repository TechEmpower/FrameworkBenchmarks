
import os

from socketify import App


def plaintext(res, req):
    res.send(b'Hello, World!')

def applicationjson(res, req):
    res.send({"message":"Hello, World!"})


def run_app():
    app = App(None, 200_000, 0)
    app.get("/", plaintext)
    app.get("/json", applicationjson)
    app.get("/plaintext", plaintext)
    app.listen(3000, None)
    app.run()

def create_fork():
    n = os.fork()
    # n greater than 0 means parent process
    if not n > 0:
        run_app()

def get_worker_count():
    try:
        return int(os.environ["WORKER_COUNT"])
    except:
        return 2

WORKER_COUNT = get_worker_count() - 1

for index in range(WORKER_COUNT):
    create_fork()

run_app()
#sudo ./tfb --mode benchmark --test socketify.py --type plaintext
#sudo ./tfb --mode benchmark --test socketify.py --type json --network=tfb