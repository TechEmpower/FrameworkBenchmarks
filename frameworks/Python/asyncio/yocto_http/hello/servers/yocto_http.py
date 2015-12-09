import asyncio

import ujson

from ..utils.yocto_http.utils import generate_http_response

class YoctoHttpJson(asyncio.Protocol):
    def connection_made(self, transport):
        self.transport = transport

    def data_received(self, data):
        # self.transport.write(data)
        payload = ujson.dumps({'message': 'Hello, World!'})
        self.transport.write(generate_http_response(payload))

class YoctoHttpText(asyncio.Protocol):
    def connection_made(self, transport):
        self.transport = transport

    def data_received(self, data):
        # self.transport.write(data)
        payload = 'Hello, World!'
        self.transport.write(generate_http_response(payload, 'text/plain; charset=UTF-8'))