import multiprocessing
import os
import random
from contextlib import asynccontextmanager

import asyncpg
from nexios import NexiosApp, MakeConfig



READ_ROW_SQL = 'SELECT "id", "randomnumber" FROM "world" WHERE id = $1'
WRITE_ROW_SQL = 'UPDATE "world" SET "randomnumber"=$1 WHERE id=$2'
ADDITIONAL_ROW = [0, "Additional fortune added at request time."]
MAX_POOL_SIZE = 1000 // multiprocessing.cpu_count()
MIN_POOL_SIZE = max(int(MAX_POOL_SIZE / 2), 1)

connection_pool = None


def get_num_queries(queries):
    try:
        query_count = int(queries)
    except (ValueError, TypeError):
        return 1

    if query_count < 1:
        return 1
    if query_count > 500:
        return 500
    return query_count


async def setup_database():
    return await asyncpg.create_pool(
        user=os.getenv("PGUSER", "benchmarkdbuser"),
        password=os.getenv("PGPASS", "benchmarkdbpass"),
        database="hello_world",
        host="tfb-database",
        port=5432,
        min_size=MIN_POOL_SIZE,
        max_size=MAX_POOL_SIZE,
    )


@asynccontextmanager
async def lifespan(app: NexiosApp):
    # Setup the database connection pool
    global connection_pool
    connection_pool = await setup_database()
    yield
    # Close the database connection pool
    await connection_pool.close()


# Create Nexios app with lifespan optimized for Granian
app = NexiosApp(
    config=MakeConfig({
        "debug": False,
        "openapi": {
            "enabled": False
        }
    }),
    lifespan=lifespan
)


@app.get("/json")
async def json_serialization(request, response):
    return response.json({"message": "Hello, world!"})


@app.get("/db")
async def single_database_query(request, response):
    row_id = random.randint(1, 10000)
    async with connection_pool.acquire() as connection:
        number = await connection.fetchval(READ_ROW_SQL, row_id)

    return response.json({"id": row_id, "randomNumber": number})


@app.get("/queries")
async def multiple_database_queries(request, response):
    queries = request.query_params.get("queries")
    num_queries = get_num_queries(queries)
    row_ids = random.sample(range(1, 10000), num_queries)
    worlds = []

    async with connection_pool.acquire() as connection:
        statement = await connection.prepare(READ_ROW_SQL)
        for row_id in row_ids:
            number = await statement.fetchval(row_id)
            worlds.append({"id": row_id, "randomNumber": number})

    return response.json(worlds)


@app.get("/fortunes")
async def fortunes(request, response):
    async with connection_pool.acquire() as connection:
        fortunes = await connection.fetch("SELECT * FROM Fortune")

    fortunes.append(ADDITIONAL_ROW)
    fortunes.sort(key=lambda row: row[1])
    
    # Render fortune template
    html_content = """
    <!DOCTYPE html>
    <html>
    <head><title>Fortunes</title></head>
    <body>
    <table>
    <tr><th>id</th><th>message</th></tr>
    """
    
    for fortune in fortunes:
        html_content += f"<tr><td>{fortune[0]}</td><td>{fortune[1]}</td></tr>"
    
    html_content += """
    </table>
    </body>
    </html>
    """
    
    return response.html(html_content)


@app.get("/updates")
async def database_updates(request, response):
    queries = request.query_params.get("queries")
    num_queries = get_num_queries(queries)
    # To avoid deadlock
    ids = sorted(random.sample(range(1, 10000 + 1), num_queries))
    numbers = sorted(random.sample(range(1, 10000), num_queries))
    updates = list(zip(ids, numbers))

    worlds = [
        {"id": row_id, "randomNumber": number} for row_id, number in updates
    ]

    async with connection_pool.acquire() as connection:
        statement = await connection.prepare(READ_ROW_SQL)
        for row_id, _ in updates:
            await statement.fetchval(row_id)
        await connection.executemany(WRITE_ROW_SQL, updates)

    return response.json(worlds)


@app.get("/plaintext")
async def plaintext(request, response):
    return response.text("Hello, world!")


if __name__ == "__main__":
    # Granian will be called from the command line
    # This is just for development/testing
    import uvicorn
    uvicorn.run(app, host="0.0.0.0", port=8080) 