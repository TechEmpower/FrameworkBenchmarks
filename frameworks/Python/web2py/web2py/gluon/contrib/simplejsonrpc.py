# -*- coding: utf-8 -*-
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as published by the
# Free Software Foundation; either version 3, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTIBILITY
# or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.

"Pythonic simple JSON RPC Client implementation"

__author__ = "Mariano Reingart (reingart@gmail.com)"
__copyright__ = "Copyright (C) 2011 Mariano Reingart"
__license__ = "LGPL 3.0"
__version__ = "0.05"


import urllib
from xmlrpclib import Transport, SafeTransport
from cStringIO import StringIO
import random
import sys
try:
    import gluon.contrib.simplejson as json     # try web2py json serializer
except ImportError:
    try:
        import json                             # try stdlib (py2.6)
    except:
        import simplejson as json               # try external module


class JSONRPCError(RuntimeError):
    "Error object for remote procedure call fail"
    def __init__(self, code, message, data=None):
        value = "%s: %s\n%s" % (code, message, '\n'.join(data))
        RuntimeError.__init__(self, value)
        self.code = code
        self.message = message
        self.data = data


class JSONDummyParser:
    "json wrapper for xmlrpclib parser interfase"
    def __init__(self):
        self.buf = StringIO()

    def feed(self, data):
        self.buf.write(data)

    def close(self):
        return self.buf.getvalue()


class JSONTransportMixin:
    "json wrapper for xmlrpclib transport interfase"

    def send_content(self, connection, request_body):
        connection.putheader("Content-Type", "application/json")
        connection.putheader("Content-Length", str(len(request_body)))
        connection.endheaders()
        if request_body:
            connection.send(request_body)
        # todo: add gzip compression

    def getparser(self):
        # get parser and unmarshaller
        parser = JSONDummyParser()
        return parser, parser


class JSONTransport(JSONTransportMixin, Transport):
    pass


class JSONSafeTransport(JSONTransportMixin, SafeTransport):
    pass


class ServerProxy(object):
    "JSON RPC Simple Client Service Proxy"

    def __init__(self, uri, transport=None, encoding=None, verbose=0,version=None):
        self.location = uri             # server location (url)
        self.trace = verbose            # show debug messages
        self.exceptions = True          # raise errors? (JSONRPCError)
        self.timeout = None
        self.json_request = self.json_response = ''
        self.version = version          # '2.0' for jsonrpc2

        type, uri = urllib.splittype(uri)
        if type not in ("http", "https"):
            raise IOError("unsupported JSON-RPC protocol")
        self.__host, self.__handler = urllib.splithost(uri)

        if transport is None:
            if type == "https":
                transport = JSONSafeTransport()
            else:
                transport = JSONTransport()
        self.__transport = transport
        self.__encoding = encoding
        self.__verbose = verbose

    def __getattr__(self, attr):
        "pseudo method that can be called"
        return lambda *args: self.call(attr, *args)

    def call(self, method, *args):
        "JSON RPC communication (method invocation)"

        # build data sent to the service
        request_id = random.randint(0, sys.maxint)
        data = {'id': request_id, 'method': method, 'params': args, }
        if self.version:
            data['jsonrpc'] = self.version #mandatory key/value for jsonrpc2 validation else err -32600
        request = json.dumps(data)

        # make HTTP request (retry if connection is lost)
        response = self.__transport.request(
            self.__host,
            self.__handler,
            request,
            verbose=self.__verbose
        )

        # store plain request and response for further debugging
        self.json_request = request
        self.json_response = response

        # parse json data coming from service
        # {'version': '1.1', 'id': id, 'result': result, 'error': None}
        response = json.loads(response)

        self.error = response.get('error', {})
        if self.error and self.exceptions:
            raise JSONRPCError(self.error.get('code', 0),
                               self.error.get('message', ''),
                               self.error.get('data', None))
        if response['id'] != request_id:
            raise JSONRPCError(0, "JSON Request ID != Response ID")

        return response.get('result')


ServiceProxy = ServerProxy


if __name__ == "__main__":
    # basic tests:
    location = "http://www.web2py.com.ar/webservices/sample/call/jsonrpc"
    client = ServerProxy(location, verbose='--verbose' in sys.argv,)
    print client.add(1, 2)
