import logging
import asyncio
import os

import aiopg
import jinja2
import psycopg2.extras
import asyncio_redis
from asyncio_redis.protocol import HiRedisProtocol
import aiohttp.web
import aiohttp_jinja2
import api_hour

from . import endpoints

LOG = logging.getLogger(__name__)


class Container(api_hour.Container):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        # Servers
        self.servers['http'] = aiohttp.web.Application(loop=kwargs['loop'])
        aiohttp_jinja2.setup(self.servers['http'], loader=jinja2.PackageLoader('hello'))
        self.servers['http'].ah_container = self # keep a reference to Container
        # routes
        self.servers['http'].router.add_route('GET', '/json', endpoints.world.json)
        self.servers['http'].router.add_route('GET', '/db', endpoints.world.db)
        self.servers['http'].router.add_route('GET', '/db_redis', endpoints.world.db_redis)
        self.servers['http'].router.add_route('GET', '/queries', endpoints.world.queries)
        self.servers['http'].router.add_route('GET', '/queries_redis', endpoints.world.queries_redis)
        self.servers['http'].router.add_route('GET', '/fortunes', endpoints.world.fortunes)
        self.servers['http'].router.add_route('GET', '/fortunes_redis', endpoints.world.fortunes_redis)
        self.servers['http'].router.add_route('GET', '/updates', endpoints.world.updates)
        self.servers['http'].router.add_route('GET', '/updates_redis', endpoints.world.updates_redis)
        self.servers['http'].router.add_route('GET', '/plaintext', endpoints.world.plaintext)

    def make_servers(self):
        return [self.servers['http'].make_handler(logger=self.worker.log,
                                                  debug=False,
                                                  keep_alive=0,
                                                  access_log=None,
                                                  access_log_format=self.worker.cfg.access_log_format)]

    @asyncio.coroutine
    def start(self):
        yield from super().start()
        LOG.info('Starting engines...')
        self.engines['pg'] = self.loop.create_task(aiopg.create_pool(host=os.environ.get('DBHOST', self.config['engines']['pg']['host']),
                                                                     port=int(self.config['engines']['pg']['port']),
                                                                     sslmode='disable',
                                                                     dbname=self.config['engines']['pg']['dbname'],
                                                                     user=self.config['engines']['pg']['user'],
                                                                     password=self.config['engines']['pg']['password'],
                                                                     cursor_factory=psycopg2.extras.RealDictCursor,
                                                                     minsize=int(self.config['engines']['pg']['minsize']),
                                                                     maxsize=int(self.config['engines']['pg']['maxsize']),
                                                                     loop=self.loop))
        yield from asyncio.wait([self.engines['pg']], return_when=asyncio.ALL_COMPLETED)
        self.engines['redis'] = yield from asyncio_redis.Pool.create(host=self.config['engines']['redis']['host'],
                                                                     port=self.config['engines']['redis']['port'],
                                                                     poolsize=self.config['engines']['redis']['poolsize'],
                                                                     loop=self.loop,
                                                                     protocol_class=HiRedisProtocol)

        LOG.info('All engines ready !')

    @asyncio.coroutine
    def stop(self):
        LOG.info('Stopping engines...')
        if 'pg' in self.engines:
            if self.engines['pg'].done():
                self.engines['pg'].result().terminate()
                yield from self.engines['pg'].result().wait_closed()
            else:
                yield from self.engines['pg'].cancel()
        if 'redis' in self.engines:
            self.engines['redis'].close()
            yield from asyncio.sleep(1) # wait redis close connection
        LOG.info('All engines stopped !')
        yield from super().stop()