
from random import randint, sample

from emmett import App, request
from emmett.orm import Database, Model, Field, rowmethod
from emmett.tools import service

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


app.config.handle_static = False
app.config.db.adapter = 'postgres:psycopg2'
app.config.db.host = 'tfb-database'
app.config.db.user = 'benchmarkdbuser'
app.config.db.password = 'benchmarkdbpass'
app.config.db.database = 'hello_world'
app.config.db.pool_size = 10

db = Database(app)
db.define_models(World, Fortune)


@app.route()
@service.json
async def json():
    return {'message': 'Hello, World!'}


@app.route("/db", pipeline=[db.pipe])
@service.json
async def get_random_world():
    return World.get(randint(1, 10000)).serialize()


def get_qparam():
    try:
        rv = int(request.query_params.queries or 1)
    except ValueError:
        return 1
    if rv < 1:
        return 1
    if rv > 500:
        return 500
    return rv


@app.route("/queries", pipeline=[db.pipe])
@service.json
async def get_random_worlds():
    num_queries = get_qparam()
    row_ids = sample(range(1, 10000), num_queries)
    worlds = [World.get(row_id).serialize() for row_id in row_ids]
    return worlds


@app.route(pipeline=[db.pipe], output='template')
async def fortunes():
    fortune = Fortune.new(message="Additional fortune added at request time.")
    fortune.id = 0
    fortunes = Fortune.all().select()
    fortunes.append(fortune)
    fortunes.sort(lambda m: m.message)
    return {'fortunes': fortunes}


@app.route(pipeline=[db.pipe])
@service.json
async def updates():
    num_queries = get_qparam()
    updates = list(zip(
        sample(range(1, 10000), num_queries),
        sorted(sample(range(1, 10000), num_queries))
    ))
    worlds = []
    for row_id, number in updates:
        world = World.get(row_id)
        world.update_record(randomnumber=number)
        worlds.append(world.serialize())
    return worlds


@app.route(output='bytes')
async def plaintext():
    return b'Hello, World!'
