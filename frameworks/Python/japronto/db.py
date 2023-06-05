import asyncio
from contextlib import asynccontextmanager
import asyncpg
import os


class Connection(asyncpg.Connection):
    async def reset(self, *, timeout=None):
        pass


class Pool:
    def __init__(self, connect_url, max_size=10, connection_class=None):
        self._connect_url = connect_url
        self._connection_class = connection_class or Connection
        self._queue = asyncio.LifoQueue(max_size)

    def __await__(self):
        return self._async_init__().__await__()

    async def _async_init__(self):
        for _ in range(self._queue.maxsize):
            self._queue.put_nowait(await asyncpg.connect(self._connect_url, connection_class=self._connection_class))
        return self

    @asynccontextmanager
    async def acquire(self):
        conn = await self._queue.get()
        try:
            yield conn
        finally:
            self._queue.put_nowait(conn)

    async def close(self):
        for _ in range(self._queue.maxsize):
            conn = await self._queue.get()
            await conn.close()


async def init_db(app):
    app.db_pool = await Pool("postgresql://%s:%s@tfb-database:5432/hello_world" % (os.getenv("PGUSER", "benchmarkdbuser"), os.getenv("PSPASS", "benchmarkdbpass")), connection_class=asyncpg.Connection)


async def close_db(app):
    await asyncio.wait_for(app.db_pool.close(), timeout=1)
