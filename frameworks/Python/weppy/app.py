import sys
from functools import partial
from random import randint
from weppy import App, Pipe, request, response
from weppy.orm import Database, Model, Field, rowmethod
from weppy.tools import service
from email.utils import formatdate

_is_pypy = hasattr(sys, 'pypy_version_info')
if sys.version_info[0] == 3:
    xrange = range

DBHOSTNAME = 'tfb-database'

app = App(__name__)


class World(Model):
    tablename = "world"
    randomnumber = Field.int()

    @rowmethod('serialize')
    def _serialize(self, row):
        return {'id': row.id, 'randomNumber': row.randomnumber}


class Fortune(Model):
    tablename = "fortune"
    message = Field.string()

    @rowmethod('serialize')
    def _serialize(self, row):
        return {'id': row.id, 'message': row.message}


class DateHeaderPipe(Pipe):
    def open(self):
        response.headers["Date"] = formatdate(timeval=None, localtime=False, usegmt=True)


app.config.handle_static = False
app.config.db.adapter = 'postgres:psycopg2' \
    if not _is_pypy else 'postgres:pg8000'
app.config.db.host = DBHOSTNAME
app.config.db.user = 'benchmarkdbuser'
app.config.db.password = 'benchmarkdbpass'
app.config.db.database = 'hello_world'
app.config.db.pool_size = 100

app.pipeline = [DateHeaderPipe()]

db = Database(app, auto_migrate=False)
db.define_models(World, Fortune)


@app.route()
@service.json
def json():
    return {'message': 'Hello, World!'}


@app.route("/db", pipeline=[db.pipe])
@service.json
def get_random_world():
    return World.get(randint(1, 10000)).serialize()


def get_qparam():
    try:
        rv = int(request.query_params.queries)
        if rv < 1:
            rv = 1
        if rv > 500:
            rv = 500
    except:
        rv = 1
    return rv


@app.route("/queries", pipeline=[db.pipe])
@service.json
def get_random_worlds():
    num_queries = get_qparam()
    worlds = [
        World.get(randint(1, 10000)).serialize() for _ in xrange(num_queries)]
    return worlds


@app.route(pipeline=[db.pipe])
def fortunes():
    fortunes = Fortune.all().select()
    fortunes.append(
        Fortune.new(id=0, message="Additional fortune added at request time."))
    fortunes.sort(lambda m: m.message)
    return {'fortunes': fortunes}


@app.route(pipeline=[db.pipe])
@service.json
def updates():
    num_queries = get_qparam()
    worlds = []
    rp = partial(randint, 1, 10000)
    ids = [rp() for _ in xrange(num_queries)]
    ids.sort()  # To avoid deadlock
    for id in ids:
        world = World.get(id)
        world.update_record(randomnumber=rp())
        worlds.append(world.serialize())
    return worlds


@app.route()
def plaintext():
    response.headers["Content-Type"] = "text/plain"
    return 'Hello, World!'


try:
    import meinheld
    meinheld.server.set_access_logger(None)
    meinheld.set_keepalive(120)
except ImportError:
    pass

# entry point for debugging
if __name__ == "__main__":
    app.run(debug=True)
