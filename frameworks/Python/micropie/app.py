import asyncpg
import os
from micropie import App
from random import randint, sample
import uvloop
import asyncio

# Set uvloop as the event loop policy
uvloop.install()

READ_ROW_SQL = 'SELECT "randomnumber", "id" FROM "world" WHERE id = $1'
WRITE_ROW_SQL = 'UPDATE "world" SET "randomnumber"=$1 WHERE id=$2'
ADDITIONAL_ROW = [0, 'Additional fortune added at request time.']

async def setup_database():
    global connection_pool
    connection_pool = await asyncpg.create_pool(
        user=os.getenv('PGUSER', 'benchmarkdbuser'),
        password=os.getenv('PGPASS', 'benchmarkdbpass'),
        database='hello_world',
        host='tfb-database',
        port=5432
    )

def get_num_queries(request):
    try:
        query_count = int(request.query_params["queries"])
    except (KeyError, IndexError, ValueError):
        return 1
    if query_count < 1:
        return 1
    if query_count > 500:
        return 500
    return query_count

connection_pool = None

class Root(App):
    async def db(self):
        row_id = randint(1, 10000)
        async with connection_pool.acquire() as connection:
            number = await connection.fetchval(READ_ROW_SQL, row_id)
        return {'id': row_id, 'randomNumber': number}

    async def queries(self):
        num_queries = get_num_queries(self.request)
        row_ids = sample(range(1, 10000), num_queries)
        worlds = []
        async with connection_pool.acquire() as connection:
            statement = await connection.prepare(READ_ROW_SQL)
            for row_id in row_ids:
                number = await statement.fetchval(row_id)
                worlds.append({'id': row_id, 'randomNumber': number})
        return worlds

    async def fortunes(self):
        async with connection_pool.acquire() as connection:
            fortunes = await connection.fetch('SELECT * FROM Fortune')
        fortunes.append(ADDITIONAL_ROW)
        fortunes.sort(key=lambda row: row[1])
        return await self._render_template("fortune.html", fortunes=fortunes)

    async def updates(self):
        num_queries = get_num_queries(self.request)  # Fixed from previous issue
        updates = [(row_id, randint(1, 10000)) for row_id in sample(range(1, 10000), num_queries)]
        worlds = [{'id': row_id, 'randomNumber': number} for row_id, number in updates]
        async with connection_pool.acquire() as connection:
            statement = await connection.prepare(READ_ROW_SQL)
            for row_id, number in updates:
                await statement.fetchval(row_id)
            await connection.executemany(WRITE_ROW_SQL, updates)
        return worlds

    async def json(self):
        return {'message': 'Hello, world!'}

    async def plaintext(self):
        return 200, 'Hello, world!', [('Content-Type', 'text/plain; charset=utf-8')]

# Initialize the app with database setup
async def init_app():
    await setup_database()
    return Root()

# Run with uvloop
app = asyncio.run(init_app())
