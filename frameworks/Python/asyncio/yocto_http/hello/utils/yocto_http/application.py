import asyncio
import logging
from collections import OrderedDict

from .request import Request
from .utils import generate_http_response

log = logging.getLogger(__name__)

class Application(dict):

    def __init__(self, default_encoding='utf-8', decode_headers=False, loop=None):
        super(Application, self).__init__()
        self.default_encoding = default_encoding
        if loop is None:
            loop = asyncio.get_event_loop()
        self.decode_headers = decode_headers
        self.loop = loop
        self._route = OrderedDict()

    def add_route(self, path, endpoint, content_type='application/json'):
        assert callable(endpoint), endpoint
        if not asyncio.iscoroutinefunction(endpoint):
            endpoint = asyncio.coroutine(endpoint)
        endpoint.content_type = content_type
        self._route[path] = endpoint

    @asyncio.coroutine
    def handler(self, reader, writer):
        # while True:
            buffer = b''
            while b'\r\n\r\n' not in buffer:
                buffer += yield from reader.read(100)
            lines = buffer[:-2].decode(self.default_encoding).split('\r\n')

            url = lines[0].split(' ')[1].split('?')
            path = url[0]
            params = OrderedDict()
            if len(url) == 2:
                k, v = url[1].split('=', 1)  # @TODO: support several parameters
                params[k] = v

            # log.info('Received HTTP request from %r for "%s" route',
            #          writer.get_extra_info('peername'),
            #          uri)

            headers = {}
            if self.decode_headers:
                for line in lines:
                    k, v = line.split(': ', 1)
                    headers[k] = v
                log.debug("HTTP Headers: %r",
                          headers)

            if path in self._route:
                request = Request(app=self,
                                  path=path,
                                  params=params,
                                  headers=headers,
                                  reader=reader, writer=writer,
                                  encoding=self.default_encoding)
                try:
                    response = yield from self._route[path](request)
                    writer.write(generate_http_response(response, content_type=self._route[path].content_type))
                    try:
                        yield from writer.drain()
                    except ConnectionError:
                        pass
                except Exception as e:
                    log.exception(e)
            else:
                log.error('No route for the request "%s"', path)
                writer.write(generate_http_response(''))
                try:
                    yield from writer.drain()
                except ConnectionError:
                    pass
            writer.close()
