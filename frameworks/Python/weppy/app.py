import cgi
import os
import sys
from functools import partial
from random import randint
from weppy import App, request, response
from weppy.dal import Model, Field, DAL, rowmethod
from weppy.tools import service

_is_pypy = hasattr(sys, 'pypy_version_info')
if sys.version_info[0] == 3:
    xrange = range

# NOTE on html escaping: weppy normally escapes every character to its ascii
# html representation. The 'fortunes' test seems not to like that, so we
# re-define the escaping only to the next characters
# _html_escape_table = {
#     "&": "&amp;",
#     '"': "&quot;",
#     "'": "&apos;",
#     ">": "&gt;",
#     "<": "&lt;",
# }


def light_html_escape(text):
    # return "".join(_html_escape_table.get(c, c) for c in text)
    return cgi.escape(text, True).replace("'", "&#x27;")


DBHOSTNAME = os.environ.get('DBHOST', 'localhost')


app = App(__name__)


class World(Model):
    tablename = "world"
    randomnumber = Field('int')

    @rowmethod('serialize')
    def _serialize(self, row):
        return {'id': row.id, 'randomNumber': row.randomnumber}


class Fortune(Model):
    tablename = "fortune"
    message = Field()

    @rowmethod('serialize')
    def _serialize(self, row):
        return {'id': row.id, 'message': row.message}


app.config.db.adapter = 'postgres:psycopg2' \
    if not _is_pypy else 'postgres:pg8000'
app.config.db.host = DBHOSTNAME
app.config.db.user = 'benchmarkdbuser'
app.config.db.password = 'benchmarkdbpass'
app.config.db.database = 'hello_world'
app.config.db.pool_size = 100

db = DAL(app, auto_migrate=False)
db.define_models(World, Fortune)


@app.route()
@service.json
def json():
    return {'message': 'Hello, World!'}


@app.route("/db", handlers=[db.handler])
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


@app.route("/queries", handlers=[db.handler])
@service.json
def get_random_worlds():
    num_queries = get_qparam()
    worlds = [
        World.get(randint(1, 10000)).serialize() for _ in xrange(num_queries)]
    return worlds


@app.route(handlers=[db.handler])
def fortunes():
    fortunes = Fortune.all().select()
    fortunes.append(
        Fortune.new(id=0, message="Additional fortune added at request time."))
    fortunes.sort(lambda m: m.message)
    return dict(fortunes=fortunes, escape=light_html_escape)


@app.route(handlers=[db.handler])
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
