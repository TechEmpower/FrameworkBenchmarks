import logging
import asyncio

import aiopg
import os
import psycopg2.extras

import api_hour
import api_hour.aiorest

from . import endpoints


LOG = logging.getLogger(__name__)


class Container(api_hour.Container):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        # Servers
        self.servers['http'] = api_hour.aiorest.Application(*args, **kwargs)
        self.servers['http'].ah_container = self # keep a reference to Container
        # routes
        self.servers['http'].add_url('GET', '/json', endpoints.world.json)
        self.servers['http'].add_url('GET', '/db', endpoints.world.db)
        self.servers['http'].add_url('GET', '/queries', endpoints.world.queries)

    def make_servers(self):
        return [self.servers['http'].make_handler]

    @asyncio.coroutine
    def start(self):
        yield from super().start()
        LOG.info('Starting engines...')
        # Add your custom engine here, example with PostgreSQL:
        self.engines['pg'] = self.loop.create_task(aiopg.create_pool(host=os.environ.get('DBHOST', '127.0.0.1'),
                                                                     sslmode='disable',
                                                                     port=int(self.config['engines']['pg']['port']),
                                                                     dbname=self.config['engines']['pg']['dbname'],
                                                                     user=self.config['engines']['pg']['user'],
                                                                     password=self.config['engines']['pg']['password'],
                                                                     cursor_factory=psycopg2.extras.RealDictCursor,
                                                                     minsize=int(self.config['engines']['pg']['minsize']),
                                                                     maxsize=int(self.config['engines']['pg']['maxsize'])))
        yield from asyncio.wait([self.engines['pg']], return_when=asyncio.ALL_COMPLETED)

        LOG.info('All engines ready !')


    @asyncio.coroutine
    def stop(self):
        LOG.info('Stopping engines...')
        # Add your custom end here, example with PostgreSQL:
        if 'pg' in self.engines:
            if self.engines['pg'].done():
                self.engines['pg'].result().terminate()
                yield from self.engines['pg'].result().wait_closed()
            else:
                yield from self.engines['pg'].cancel()
        LOG.info('All engines stopped !')
        yield from super().stop()