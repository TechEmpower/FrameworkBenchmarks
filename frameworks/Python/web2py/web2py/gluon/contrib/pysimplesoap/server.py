#!/usr/bin/python
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

"""Pythonic simple SOAP Server implementation"""


from __future__ import unicode_literals
import sys
if sys.version > '3':
    unicode = str


import datetime
import sys
import logging
import warnings
import re
import traceback
try:
    from BaseHTTPServer import BaseHTTPRequestHandler, HTTPServer
except ImportError:
    from http.server import BaseHTTPRequestHandler, HTTPServer

from . import __author__, __copyright__, __license__, __version__
from .simplexml import SimpleXMLElement, TYPE_MAP, Date, Decimal

log = logging.getLogger(__name__)

# Deprecated?
NS_RX = re.compile(r'xmlns:(\w+)="(.+?)"')


class SoapDispatcher(object):
    """Simple Dispatcher for SOAP Server"""

    def __init__(self, name, documentation='', action='', location='',
                 namespace=None, prefix=False,
                 soap_uri="http://schemas.xmlsoap.org/soap/envelope/",
                 soap_ns='soap',
                 namespaces={},
                 pretty=False,
                 debug=False,
                 **kwargs):
        """
        :param namespace: Target namespace; xmlns=targetNamespace
        :param prefix: Prefix for target namespace; xmlns:prefix=targetNamespace
        :param namespaces: Specify additional namespaces; example: {'external': 'http://external.mt.moboperator'}
        :param pretty: Prettifies generated xmls
        :param debug: Use to add tracebacks in generated xmls.

        Multiple namespaces
        ===================

        It is possible to support multiple namespaces.
        You need to specify additional namespaces by passing `namespace` parameter.

        >>> dispatcher = SoapDispatcher(
        ...    name = "MTClientWS",
        ...    location = "http://localhost:8008/ws/MTClientWS",
        ...    action = 'http://localhost:8008/ws/MTClientWS', # SOAPAction
        ...    namespace = "http://external.mt.moboperator", prefix="external",
        ...    documentation = 'moboperator MTClientWS',
        ...    namespaces = {
        ...        'external': 'http://external.mt.moboperator',
        ...        'model': 'http://model.common.mt.moboperator'
        ...    },
        ...    ns = True)

        Now the registered method must return node names with namespaces' prefixes.

        >>> def _multi_ns_func(self, serviceMsisdn):
        ...    ret = {
        ...        'external:activateSubscriptionsReturn': [
        ...            {'model:code': '0'},
        ...            {'model:description': 'desc'},
        ...        ]}
        ...    return ret

        Our prefixes will be changed to those used by the client.
        """
        self.methods = {}
        self.name = name
        self.documentation = documentation
        self.action = action  # base SoapAction
        self.location = location
        self.namespace = namespace  # targetNamespace
        self.prefix = prefix
        self.soap_ns = soap_ns
        self.soap_uri = soap_uri
        self.namespaces = namespaces
        self.pretty = pretty
        self.debug = debug

    @staticmethod
    def _extra_namespaces(xml, ns):
        """Extends xml with extra namespaces.
        :param ns: dict with namespaceUrl:prefix pairs
        :param xml: XML node to modify
        """
        if ns:
            _tpl = 'xmlns:%s="%s"'
            _ns_str = " ".join([_tpl % (prefix, uri) for uri, prefix in ns.items() if uri not in xml])
            xml = xml.replace('/>', ' ' + _ns_str + '/>')
        return xml

    def register_function(self, name, fn, returns=None, args=None, doc=None):
        self.methods[name] = fn, returns, args, doc or getattr(fn, "__doc__", "")

    def dispatch(self, xml, action=None, fault=None):
        """Receive and process SOAP call, returns the xml"""
        # a dict can be sent in fault to expose it to the caller
        # default values:
        prefix = self.prefix
        ret = None
        if fault is None:
            fault = {}
        soap_ns, soap_uri = self.soap_ns, self.soap_uri
        soap_fault_code = 'VersionMismatch'
        name = None

        # namespaces = [('model', 'http://model.common.mt.moboperator'), ('external', 'http://external.mt.moboperator')]
        _ns_reversed = dict(((v, k) for k, v in self.namespaces.items()))  # Switch keys-values
        # _ns_reversed = {'http://external.mt.moboperator': 'external', 'http://model.common.mt.moboperator': 'model'}

        try:
            request = SimpleXMLElement(xml, namespace=self.namespace)

            # detect soap prefix and uri (xmlns attributes of Envelope)
            for k, v in request[:]:
                if v in ("http://schemas.xmlsoap.org/soap/envelope/",
                         "http://www.w3.org/2003/05/soap-env",):
                    soap_ns = request.attributes()[k].localName
                    soap_uri = request.attributes()[k].value

                # If the value from attributes on Envelope is in additional namespaces
                elif v in self.namespaces.values():
                    _ns = request.attributes()[k].localName
                    _uri = request.attributes()[k].value
                    _ns_reversed[_uri] = _ns  # update with received alias
                    # Now we change 'external' and 'model' to the received forms i.e. 'ext' and 'mod'
                # After that we know how the client has prefixed additional namespaces

            ns = NS_RX.findall(xml)
            for k, v in ns:
                if v in self.namespaces.values():
                    _ns_reversed[v] = k

            soap_fault_code = 'Client'

            # parse request message and get local method
            method = request('Body', ns=soap_uri).children()(0)
            if action:
                # method name = action
                name = action[len(self.action)+1:-1]
                prefix = self.prefix
            if not action or not name:
                # method name = input message name
                name = method.get_local_name()
                prefix = method.get_prefix()

            log.debug('dispatch method: %s', name)
            function, returns_types, args_types, doc = self.methods[name]
            log.debug('returns_types %s', returns_types)

            # de-serialize parameters (if type definitions given)
            if args_types:
                args = method.children().unmarshall(args_types)
            elif args_types is None:
                args = {'request': method}  # send raw request
            else:
                args = {}  # no parameters

            soap_fault_code = 'Server'
            # execute function
            ret = function(**args)
            log.debug('dispathed method returns: %s', ret)

        except Exception:  # This shouldn't be one huge try/except
            import sys
            etype, evalue, etb = sys.exc_info()
            log.error(traceback.format_exc())
            if self.debug:
                detail = ''.join(traceback.format_exception(etype, evalue, etb))
                detail += '\n\nXML REQUEST\n\n' + xml
            else:
                detail = None
            fault.update({'faultcode': "%s.%s" % (soap_fault_code, etype.__name__),
                     'faultstring': evalue,
                     'detail': detail})

        # build response message
        if not prefix:
            xml = """<%(soap_ns)s:Envelope xmlns:%(soap_ns)s="%(soap_uri)s"/>"""
        else:
            xml = """<%(soap_ns)s:Envelope xmlns:%(soap_ns)s="%(soap_uri)s"
                       xmlns:%(prefix)s="%(namespace)s"/>"""

        xml %= {    # a %= {} is a shortcut for a = a % {}
            'namespace': self.namespace,
            'prefix': prefix,
            'soap_ns': soap_ns,
            'soap_uri': soap_uri
        }

        # Now we add extra namespaces
        xml = SoapDispatcher._extra_namespaces(xml, _ns_reversed)

        # Change our namespace alias to that given by the client.
        # We put [('model', 'http://model.common.mt.moboperator'), ('external', 'http://external.mt.moboperator')]
        # mix it with {'http://external.mt.moboperator': 'ext', 'http://model.common.mt.moboperator': 'mod'}
        mapping = dict(((k, _ns_reversed[v]) for k, v in self.namespaces.items()))  # Switch keys-values and change value
        # and get {'model': u'mod', 'external': u'ext'}

        response = SimpleXMLElement(xml,
                                    namespace=self.namespace,
                                    namespaces_map=mapping,
                                    prefix=prefix)

        response['xmlns:xsi'] = "http://www.w3.org/2001/XMLSchema-instance"
        response['xmlns:xsd'] = "http://www.w3.org/2001/XMLSchema"

        body = response.add_child("%s:Body" % soap_ns, ns=False)

        if fault:
            # generate a Soap Fault (with the python exception)
            body.marshall("%s:Fault" % soap_ns, fault, ns=False)
        else:
            # return normal value
            res = body.add_child("%sResponse" % name, ns=self.namespace)
            if not prefix:
                res['xmlns'] = self.namespace  # add target namespace

            # serialize returned values (response) if type definition available
            if returns_types:
                # TODO: full sanity check of type structure (recursive)
                complex_type = isinstance(ret, dict)
                if complex_type:
                    # check if type mapping correlates with return value
                    types_ok = all([k in returns_types for k in ret.keys()])
                    if not types_ok:
                        warnings.warn("Return value doesn't match type structure: "
                                     "%s vs %s" % (str(returns_types), str(ret)))
                if not complex_type or not types_ok:
                    # backward compatibility for scalar and simple types
                    res.marshall(returns_types.keys()[0], ret, )
                else:
                    # new style for complex classes
                    for k, v in ret.items():
                        res.marshall(k, v)
            elif returns_types is None:
                # merge xmlelement returned
                res.import_node(ret)
            elif returns_types == {}:
                log.warning('Given returns_types is an empty dict.')

        return response.as_xml(pretty=self.pretty)

    # Introspection functions:

    def list_methods(self):
        """Return a list of aregistered operations"""
        return [(method, doc) for method, (function, returns, args, doc) in self.methods.items()]

    def help(self, method=None):
        """Generate sample request and response messages"""
        (function, returns, args, doc) = self.methods[method]
        xml = """
<soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
<soap:Body><%(method)s xmlns="%(namespace)s"/></soap:Body>
</soap:Envelope>""" % {'method': method, 'namespace': self.namespace}
        request = SimpleXMLElement(xml, namespace=self.namespace, prefix=self.prefix)
        if args:
            items = args.items()
        elif args is None:
            items = [('value', None)]
        else:
            items = []
        for k, v in items:
            request(method).marshall(k, v, add_comments=True, ns=False)

        xml = """
<soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
<soap:Body><%(method)sResponse xmlns="%(namespace)s"/></soap:Body>
</soap:Envelope>""" % {'method': method, 'namespace': self.namespace}
        response = SimpleXMLElement(xml, namespace=self.namespace, prefix=self.prefix)
        if returns:
            items = returns.items()
        elif args is None:
            items = [('value', None)]
        else:
            items = []
        for k, v in items:
            response('%sResponse' % method).marshall(k, v, add_comments=True, ns=False)

        return request.as_xml(pretty=True), response.as_xml(pretty=True), doc

    def wsdl(self):
        """Generate Web Service Description v1.1"""
        xml = """<?xml version="1.0"?>
<wsdl:definitions name="%(name)s"
          targetNamespace="%(namespace)s"
          xmlns:tns="%(namespace)s"
          xmlns:soap="http://schemas.xmlsoap.org/wsdl/soap/"
          xmlns:wsdl="http://schemas.xmlsoap.org/wsdl/"
          xmlns:xsd="http://www.w3.org/2001/XMLSchema">
    <wsdl:documentation xmlns:wsdl="http://schemas.xmlsoap.org/wsdl/">%(documentation)s</wsdl:documentation>

    <wsdl:types>
       <xsd:schema targetNamespace="%(namespace)s"
              elementFormDefault="qualified"
              xmlns:xsd="http://www.w3.org/2001/XMLSchema">
       </xsd:schema>
    </wsdl:types>

</wsdl:definitions>
""" % {'namespace': self.namespace, 'name': self.name, 'documentation': self.documentation}
        wsdl = SimpleXMLElement(xml)

        for method, (function, returns, args, doc) in self.methods.items():
            # create elements:

            def parse_element(name, values, array=False, complex=False):
                if not complex:
                    element = wsdl('wsdl:types')('xsd:schema').add_child('xsd:element')
                    complex = element.add_child("xsd:complexType")
                else:
                    complex = wsdl('wsdl:types')('xsd:schema').add_child('xsd:complexType')
                    element = complex
                element['name'] = name
                if values:
                    items = values
                elif values is None:
                    items = [('value', None)]
                else:
                    items = []
                if not array and items:
                    all = complex.add_child("xsd:all")
                elif items:
                    all = complex.add_child("xsd:sequence")
                for k, v in items:
                    e = all.add_child("xsd:element")
                    e['name'] = k
                    if array:
                        e[:] = {'minOccurs': "0", 'maxOccurs': "unbounded"}
                    if v in TYPE_MAP.keys():
                        t = 'xsd:%s' % TYPE_MAP[v]
                    elif v is None:
                        t = 'xsd:anyType'
                    elif isinstance(v, list):
                        n = "ArrayOf%s%s" % (name, k)
                        l = []
                        for d in v:
                            l.extend(d.items())
                        parse_element(n, l, array=True, complex=True)
                        t = "tns:%s" % n
                    elif isinstance(v, dict):
                        n = "%s%s" % (name, k)
                        parse_element(n, v.items(), complex=True)
                        t = "tns:%s" % n
                    else:
                        raise TypeError("unknonw type v for marshalling" % str(v))
                    e.add_attribute('type', t)

            parse_element("%s" % method, args and args.items())
            parse_element("%sResponse" % method, returns and returns.items())

            # create messages:
            for m, e in ('Input', ''), ('Output', 'Response'):
                message = wsdl.add_child('wsdl:message')
                message['name'] = "%s%s" % (method, m)
                part = message.add_child("wsdl:part")
                part[:] = {'name': 'parameters',
                           'element': 'tns:%s%s' % (method, e)}

        # create ports
        portType = wsdl.add_child('wsdl:portType')
        portType['name'] = "%sPortType" % self.name
        for method, (function, returns, args, doc) in self.methods.items():
            op = portType.add_child('wsdl:operation')
            op['name'] = method
            if doc:
                op.add_child("wsdl:documentation", doc)
            input = op.add_child("wsdl:input")
            input['message'] = "tns:%sInput" % method
            output = op.add_child("wsdl:output")
            output['message'] = "tns:%sOutput" % method

        # create bindings
        binding = wsdl.add_child('wsdl:binding')
        binding['name'] = "%sBinding" % self.name
        binding['type'] = "tns:%sPortType" % self.name
        soapbinding = binding.add_child('soap:binding')
        soapbinding['style'] = "document"
        soapbinding['transport'] = "http://schemas.xmlsoap.org/soap/http"
        for method in self.methods.keys():
            op = binding.add_child('wsdl:operation')
            op['name'] = method
            soapop = op.add_child('soap:operation')
            soapop['soapAction'] = self.action + method
            soapop['style'] = 'document'
            input = op.add_child("wsdl:input")
            ##input.add_attribute('name', "%sInput" % method)
            soapbody = input.add_child("soap:body")
            soapbody["use"] = "literal"
            output = op.add_child("wsdl:output")
            ##output.add_attribute('name', "%sOutput" % method)
            soapbody = output.add_child("soap:body")
            soapbody["use"] = "literal"

        service = wsdl.add_child('wsdl:service')
        service["name"] = "%sService" % self.name
        service.add_child('wsdl:documentation', text=self.documentation)
        port = service.add_child('wsdl:port')
        port["name"] = "%s" % self.name
        port["binding"] = "tns:%sBinding" % self.name
        soapaddress = port.add_child('soap:address')
        soapaddress["location"] = self.location
        return wsdl.as_xml(pretty=True)


class SOAPHandler(BaseHTTPRequestHandler):

    def do_GET(self):
        """User viewable help information and wsdl"""
        args = self.path[1:].split("?")
        if self.path != "/" and args[0] not in self.server.dispatcher.methods.keys():
            self.send_error(404, "Method not found: %s" % args[0])
        else:
            if self.path == "/":
                # return wsdl if no method supplied
                response = self.server.dispatcher.wsdl()
            else:
                # return supplied method help (?request or ?response messages)
                req, res, doc = self.server.dispatcher.help(args[0])
                if len(args) == 1 or args[1] == "request":
                    response = req
                else:
                    response = res
            self.send_response(200)
            self.send_header("Content-type", "text/xml")
            self.end_headers()
            self.wfile.write(response)

    def do_POST(self):
        """SOAP POST gateway"""
        request = self.rfile.read(int(self.headers.getheader('content-length')))
        fault = {}
        # execute the method
        response = self.server.dispatcher.dispatch(request, fault=fault)
        # check if fault dict was completed (faultcode, faultstring, detail)
        if fault:
            self.send_response(500)
        else:
            self.send_response(200)
        self.send_header("Content-type", "text/xml")
        self.end_headers()
        self.wfile.write(response)


class WSGISOAPHandler(object):

    def __init__(self, dispatcher):
        self.dispatcher = dispatcher

    def __call__(self, environ, start_response):
        return self.handler(environ, start_response)

    def handler(self, environ, start_response):
        if environ['REQUEST_METHOD'] == 'GET':
            return self.do_get(environ, start_response)
        elif environ['REQUEST_METHOD'] == 'POST':
            return self.do_post(environ, start_response)
        else:
            start_response('405 Method not allowed', [('Content-Type', 'text/plain')])
            return ['Method not allowed']

    def do_get(self, environ, start_response):
        path = environ.get('PATH_INFO').lstrip('/')
        query = environ.get('QUERY_STRING')
        if path != "" and path not in self.dispatcher.methods.keys():
            start_response('404 Not Found', [('Content-Type', 'text/plain')])
            return ["Method not found: %s" % path]
        elif path == "":
            # return wsdl if no method supplied
            response = self.dispatcher.wsdl()
        else:
            # return supplied method help (?request or ?response messages)
            req, res, doc = self.dispatcher.help(path)
            if len(query) == 0 or query == "request":
                response = req
            else:
                response = res
        start_response('200 OK', [('Content-Type', 'text/xml'), ('Content-Length', str(len(response)))])
        return [response]

    def do_post(self, environ, start_response):
        length = int(environ['CONTENT_LENGTH'])
        request = environ['wsgi.input'].read(length)
        response = self.dispatcher.dispatch(request)
        start_response('200 OK', [('Content-Type', 'text/xml'), ('Content-Length', str(len(response)))])
        return [response]


if __name__ == "__main__":

    dispatcher = SoapDispatcher(
        name="PySimpleSoapSample",
        location="http://localhost:8008/",
        action='http://localhost:8008/',  # SOAPAction
        namespace="http://example.com/pysimplesoapsamle/", prefix="ns0",
        documentation='Example soap service using PySimpleSoap',
        trace=True, debug=True,
        ns=True)

    def adder(p, c, dt=None):
        """Add several values"""
        dt = dt + datetime.timedelta(365)
        return {'ab': p['a'] + p['b'], 'dd': c[0]['d'] + c[1]['d'], 'dt': dt}

    def dummy(in0):
        """Just return input"""
        return in0

    def echo(request):
        """Copy request->response (generic, any type)"""
        return request.value

    dispatcher.register_function(
        'Adder', adder,
        returns={'AddResult': {'ab': int, 'dd': unicode, 'dt': datetime.date}},
        args={'p': {'a': int, 'b': int}, 'dt': Date, 'c': [{'d': Decimal}]}
    )

    dispatcher.register_function(
        'Dummy', dummy,
        returns={'out0': str},
        args={'in0': str}
    )

    dispatcher.register_function('Echo', echo)

    if '--local' in sys.argv:

        wsdl = dispatcher.wsdl()

        for method, doc in dispatcher.list_methods():
            request, response, doc = dispatcher.help(method)

    if '--serve' in sys.argv:
        log.info("Starting server...")
        httpd = HTTPServer(("", 8008), SOAPHandler)
        httpd.dispatcher = dispatcher
        httpd.serve_forever()

    if '--wsgi-serve' in sys.argv:
        log.info("Starting wsgi server...")
        from wsgiref.simple_server import make_server
        application = WSGISOAPHandler(dispatcher)
        wsgid = make_server('', 8008, application)
        wsgid.serve_forever()

    if '--consume' in sys.argv:
        from .client import SoapClient
        client = SoapClient(
            location="http://localhost:8008/",
            action='http://localhost:8008/',  # SOAPAction
            namespace="http://example.com/sample.wsdl",
            soap_ns='soap',
            trace=True,
            ns=False
        )
        p = {'a': 1, 'b': 2}
        c = [{'d': '1.20'}, {'d': '2.01'}]
        response = client.Adder(p=p, dt='2010-07-24', c=c)
        result = response.AddResult
        log.info(int(result.ab))
        log.info(str(result.dd))
        
    if '--consume-wsdl' in sys.argv:
        from .client import SoapClient
        client = SoapClient(
            wsdl="http://localhost:8008/",
        )
        p = {'a': 1, 'b': 2}
        c = [{'d': '1.20'}, {'d': '2.01'}]
        dt = datetime.date.today()
        response = client.Adder(p=p, dt=dt, c=c)
        result = response['AddResult']
        log.info(int(result['ab']))
        log.info(str(result['dd']))

