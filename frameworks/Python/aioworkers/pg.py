import logging
from operator import itemgetter
from random import randint

import asyncpg.exceptions
import jinja2
from aioworkers_pg.base import Connector

from aioworkers.core.base import AbstractEntity
from aioworkers.core.config import ValueExtractor
from aioworkers.net.uri import URI

READ_ROW_SQL = 'SELECT "randomnumber", "id" FROM "world" WHERE id = $1'
WRITE_ROW_SQL = 'UPDATE "world" SET "randomnumber"=$1 WHERE id=$2'
ADDITIONAL_ROW = [0, "Additional fortune added at request time."]
sort_fortunes_key = itemgetter(1)
logger = logging.getLogger(__name__)


class PG(Connector):
    def set_config(self, config: ValueExtractor) -> None:
        cfg = config.connection
        dsn: URI = cfg.get_uri("dsn").with_auth(
            username=cfg.get("username"),
            password=cfg.get("password"),
        )
        super().set_config(config.new_child(dsn=dsn))


class Templates(AbstractEntity):
    fortune: jinja2.Template

    def set_config(self, config):
        super().set_config(config)
        self.fortune = jinja2.Template(config.fortune)


def get_num_queries(request):
    query_count = request.url.query.get_int("queries")
    if query_count is None:
        return 1
    elif query_count < 1:
        return 1
    elif query_count > 500:
        return 500
    return query_count


async def single_database_query(context):
    row_id = randint(1, 10000)

    async with context.pg.pool.acquire() as connection:
        number = await connection.fetchval(READ_ROW_SQL, row_id)

    return {"id": row_id, "randomNumber": number}


async def multiple_database_queries(context, request):
    num_queries = get_num_queries(request)
    row_ids = [randint(1, 10000) for _ in range(num_queries)]
    worlds = []

    async with context.pg.pool.acquire() as connection:
        statement = await connection.prepare(READ_ROW_SQL)
        for row_id in row_ids:
            number = await statement.fetchval(row_id)
            worlds.append({"id": row_id, "randomNumber": number})

    return worlds


async def fortunes(context, request):
    async with context.pg.pool.acquire() as connection:
        fortunes = await connection.fetch("SELECT * FROM Fortune")

    fortunes.append(ADDITIONAL_ROW)
    fortunes.sort(key=sort_fortunes_key)
    content = context.templates.fortune.render(fortunes=fortunes)

    return request.response(
        content.encode(),
        headers=[
            ("Content-Type", "text/html; charset=utf-8"),
        ],
    )


async def database_updates(context, request):
    num_queries = get_num_queries(request)
    uniq = {randint(1, 10000) for _ in range(num_queries)}
    while len(uniq) < num_queries:
        uniq.add(randint(1, 10000))
    updates = [
        (row_id, randint(1, 10000)) for row_id in uniq
    ]
    worlds = [
        {"id": row_id, "randomNumber": number} for row_id, number in updates
    ]

    async with context.pg.pool.acquire() as connection:
        statement = await connection.prepare(READ_ROW_SQL)
        for row_id, number in updates:
            await statement.fetchval(row_id)
        for _ in range(99):
            try:
                await connection.executemany(WRITE_ROW_SQL, updates)
            except asyncpg.exceptions.DeadlockDetectedError as e:
                logger.debug('Deadlock %s', e)
            else:
                break
        else:
            worlds.clear()

    return worlds
