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

"""Pythonic simple SOAP Client helpers"""


from __future__ import unicode_literals
import sys
if sys.version > '3':
    basestring = unicode = str

import datetime
from decimal import Decimal
import os
import logging
import hashlib
import warnings

try:
    import urllib2
    from urlparse import urlsplit
except ImportError:
    from urllib import request as urllib2
    from urllib.parse import urlsplit

from . import __author__, __copyright__, __license__, __version__


log = logging.getLogger(__name__)


def fetch(url, http, cache=False, force_download=False, wsdl_basedir=''):
    """Download a document from a URL, save it locally if cache enabled"""

    # check / append a valid schema if not given:
    url_scheme, netloc, path, query, fragment = urlsplit(url)
    if not url_scheme in ('http', 'https', 'file'):
        for scheme in ('http', 'https', 'file'):
            try:
                if not url.startswith("/") and scheme in ('http', 'https'):
                    tmp_url = "%s://%s" % (scheme, os.path.join(wsdl_basedir, url))
                else:
                    tmp_url = "%s:%s" % (scheme, os.path.join(wsdl_basedir, url))
                log.debug('Scheme not found, trying %s' % scheme)
                return fetch(tmp_url, http, cache, force_download, wsdl_basedir)
            except Exception as e:
                log.error(e)
        raise RuntimeError('No scheme given for url: %s' % url)

    # make md5 hash of the url for caching...
    filename = '%s.xml' % hashlib.md5(url.encode('utf8')).hexdigest()
    if isinstance(cache, basestring):
        filename = os.path.join(cache, filename)
    if cache and os.path.exists(filename) and not force_download:
        log.info('Reading file %s' % filename)
        f = open(filename, 'r')
        xml = f.read()
        f.close()
    else:
        if url_scheme == 'file':
            log.info('Fetching url %s using urllib2' % url)
            f = urllib2.urlopen(url)
            xml = f.read()
        else:
            log.info('GET %s using %s' % (url, http._wrapper_version))
            response, xml = http.request(url, 'GET', None, {})
        if cache:
            log.info('Writing file %s' % filename)
            if not os.path.isdir(cache):
                os.makedirs(cache)
            f = open(filename, 'w')
            f.write(xml)
            f.close()
    return xml


def sort_dict(od, d):
    """Sort parameters (same order as xsd:sequence)"""
    if isinstance(od, dict):
        ret = OrderedDict()
        for k in od.keys():
            v = d.get(k)
            # don't append null tags!
            if v is not None:
                if isinstance(v, dict):
                    v = sort_dict(od[k], v)
                elif isinstance(v, list):
                    v = [sort_dict(od[k][0], v1) for v1 in v]
                ret[k] = v
        if hasattr(od, 'namespace'):
            ret.namespace = od.namespace
            ret.qualified = od.qualified
        return ret
    else:
        return d


def make_key(element_name, element_type, namespace):
    """Return a suitable key for elements"""
    # only distinguish 'element' vs other types
    if element_type in ('complexType', 'simpleType'):
        eltype = 'complexType'
    else:
        eltype = element_type
    if eltype not in ('element', 'complexType', 'simpleType'):
        raise RuntimeError("Unknown element type %s = %s" % (element_name, eltype))
    return (element_name, eltype, namespace)


def process_element(elements, element_name, node, element_type, xsd_uri, dialect, namespace, qualified=None,
                    soapenc_uri = 'http://schemas.xmlsoap.org/soap/encoding/'):
    """Parse and define simple element types"""

    log.debug('Processing element %s %s' % (element_name, element_type))
    for tag in node:
        if tag.get_local_name() in ('annotation', 'documentation'):
            continue
        elif tag.get_local_name() in ('element', 'restriction'):
            log.debug('%s has no children! %s' % (element_name, tag))
            children = tag  # element "alias"?
            alias = True
        elif tag.children():
            children = tag.children()
            alias = False
        else:
            log.debug('%s has no children! %s' % (element_name, tag))
            continue  # TODO: abstract?
        d = OrderedDict()
        d.namespace = namespace
        d.qualified = qualified
        for e in children:
            t = e['type']
            if not t:
                t = e['base']  # complexContent (extension)!
            if not t:
                t = e['ref']   # reference to another element
            if not t:
                # "anonymous" elements had no type attribute but children
                if e['name'] and e.children():
                    # create a type name to process the children
                    t = "%s_%s" % (element_name, e['name'])  
                    c = e.children()
                    et = c.get_local_name()
                    c = c.children()
                    process_element(elements, t, c, et, xsd_uri, dialect, namespace, qualified)
                else:
                    t = 'anyType'  # no type given!
            t = t.split(":")
            if len(t) > 1:
                ns, type_name = t
            else:
                ns, type_name = None, t[0]
            if element_name == type_name and not alias and len(children) > 1:
                continue   # abort to prevent infinite recursion
            uri = ns and e.get_namespace_uri(ns) or xsd_uri
            if uri in (xsd_uri, soapenc_uri) and type_name != 'Array':
                # look for the type, None == any
                fn = REVERSE_TYPE_MAP.get(type_name, None)
            elif uri == soapenc_uri and type_name == 'Array':
                # arrays of simple types (look at the attribute tags):
                fn = []
                for a in e.children():
                    for k, v in a[:]:
                        if k.endswith(":arrayType"):
                            type_name = v
                            if ":" in type_name:
                                type_name = type_name[type_name.index(":")+1:]
                            if "[]" in type_name:
                                type_name = type_name[:type_name.index("[]")]                                
                            fn.append(REVERSE_TYPE_MAP.get(type_name, None))
            else:
                fn = None

            if not fn:
                # simple / complex type, postprocess later
                if ns:
                    fn_namespace = uri       # use the specified namespace
                else:
                    fn_namespace = namespace # use parent namespace (default)
                for k, v in e[:]:
                    if k.startswith("xmlns:"):
                        # get the namespace uri from the element
                        fn_namespace = v        
                fn = elements.setdefault(make_key(type_name, 'complexType', fn_namespace), OrderedDict())

            if e['maxOccurs'] == 'unbounded' or (uri == soapenc_uri and type_name == 'Array'):
                # it's an array... TODO: compound arrays? and check ns uri!
                if isinstance(fn, OrderedDict):
                    if len(children) > 1 and dialect in ('jetty',):
                        # Jetty style support
                        # {'ClassName': [{'attr1': val1, 'attr2': val2}]
                        fn.array = True
                    else:
                        # .NET style support (backward compatibility)
                        # [{'ClassName': {'attr1': val1, 'attr2': val2}]
                        d.array = True
                else:
                    if dialect in ('jetty',):
                        # scalar support [{'attr1': [val1]}]
                        fn = [fn]
                    else:
                        d.array = True

            if (e['name'] is not None and not alias) or e['ref']:
                e_name = e['name'] or type_name  # for refs, use the type name
                d[e_name] = fn
            else:
                log.debug('complexContent/simpleType/element %s = %s' % (element_name, type_name))
                d[None] = fn
            if e is not None and e.get_local_name() == 'extension' and e.children():
                # extend base element:
                process_element(elements, element_name, e.children(), element_type, xsd_uri, dialect, namespace, qualified)
        elements.setdefault(make_key(element_name, element_type, namespace), OrderedDict()).update(d)


def postprocess_element(elements, processed):
    """Fix unresolved references (elements referenced before its definition, thanks .net)"""
    
    # avoid already processed elements:
    if elements in processed:
        return
    processed.append(elements)
    
    for k, v in elements.items():
        if isinstance(v, OrderedDict):
            if v != elements:  # TODO: fix recursive elements
                postprocess_element(v, processed)
            if None in v and v[None]:  # extension base?
                if isinstance(v[None], dict):
                    for i, kk in enumerate(v[None]):
                        # extend base -keep orginal order-
                        if v[None] is not None:
                            elements[k].insert(kk, v[None][kk], i)
                    del v[None]
                else:  # "alias", just replace
                    log.debug('Replacing %s = %s' % (k, v[None]))
                    elements[k] = v[None]
                    #break
            if v.array:
                elements[k] = [v]  # convert arrays to python lists
        if isinstance(v, list):
            for n in v:  # recurse list
                if isinstance(n, (OrderedDict, list)):
                    #if n != elements:  # TODO: fix recursive elements
                    postprocess_element(n, processed)


def get_message(messages, message_name, part_name):
    if part_name:
        # get the specific part of the message:
        return messages.get((message_name, part_name))
    else:
        # get the first part for the specified message:
        for (message_name_key, part_name_key), message in messages.items():
            if message_name_key == message_name:
                return message


get_local_name = lambda s: s and str((':' in s) and s.split(':')[1] or s)
get_namespace_prefix = lambda s: s and str((':' in s) and s.split(':')[0] or None)


def preprocess_schema(schema, imported_schemas, elements, xsd_uri, dialect, http, cache, force_download, wsdl_basedir, global_namespaces=None, qualified=False):
    """Find schema elements and complex types"""

    from .simplexml import SimpleXMLElement    # here to avoid recursive imports

    # analyze the namespaces used in this schema
    local_namespaces = {}
    for k, v in schema[:]:
        if k.startswith("xmlns"):
            local_namespaces[get_local_name(k)] = v
        if k == 'targetNamespace':
            # URI namespace reference for this schema
            if v == "urn:DefaultNamespace":
                v = global_namespaces[None]
            local_namespaces[None] = v
        if k == 'elementFormDefault':
            qualified = (v == "qualified")
    # add schema namespaces to the global namespace dict = {URI: ns prefix}
    for ns in local_namespaces.values():
        if ns not in global_namespaces:
            global_namespaces[ns] = 'ns%s' % len(global_namespaces)
            
    for element in schema.children() or []:
        if element.get_local_name() in ('import', 'include',):
            schema_namespace = element['namespace']
            schema_location = element['schemaLocation']
            if schema_location is None:
                log.debug('Schema location not provided for %s!' % schema_namespace)
                continue
            if schema_location in imported_schemas:
                log.debug('Schema %s already imported!' % schema_location)
                continue
            imported_schemas[schema_location] = schema_namespace
            log.debug('Importing schema %s from %s' % (schema_namespace, schema_location))
            # Open uri and read xml:
            xml = fetch(schema_location, http, cache, force_download, wsdl_basedir)

            # Parse imported XML schema (recursively):
            imported_schema = SimpleXMLElement(xml, namespace=xsd_uri)
            preprocess_schema(imported_schema, imported_schemas, elements, xsd_uri, dialect, http, cache, force_download, wsdl_basedir, global_namespaces, qualified)

        element_type = element.get_local_name()
        if element_type in ('element', 'complexType', "simpleType"):
            namespace = local_namespaces[None]          # get targetNamespace
            element_ns = global_namespaces[ns]          # get the prefix
            element_name = element['name']
            log.debug("Parsing Element %s: %s" % (element_type, element_name))
            if element.get_local_name() == 'complexType':
                children = element.children()
            elif element.get_local_name() == 'simpleType':
                children = element('restriction', ns=xsd_uri)
            elif element.get_local_name() == 'element' and element['type']:
                children = element
            else:
                children = element.children()
                if children:
                    children = children.children()
                elif element.get_local_name() == 'element':
                    children = element
            if children:
                process_element(elements, element_name, children, element_type, xsd_uri, dialect, namespace, qualified)


# simplexml utilities:

try:
    _strptime = datetime.datetime.strptime
except AttributeError:  # python2.4
    _strptime = lambda s, fmt: datetime.datetime(*(time.strptime(s, fmt)[:6]))


# Functions to serialize/deserialize special immutable types:
def datetime_u(s):
    fmt = "%Y-%m-%dT%H:%M:%S"
    try:
        return _strptime(s, fmt)
    except ValueError:
        try:
            # strip utc offset
            if s[-3] == ":" and s[-6] in (' ', '-', '+'):
                warnings.warn('removing unsupported UTC offset', RuntimeWarning)
                s = s[:-6]
            # parse microseconds
            try:
                return _strptime(s, fmt + ".%f")
            except:
                return _strptime(s, fmt)
        except ValueError:
            # strip microseconds (not supported in this platform)
            if "." in s:
                warnings.warn('removing unsuppported microseconds', RuntimeWarning)
                s = s[:s.index(".")]
            return _strptime(s, fmt)

datetime_m = lambda dt: dt.isoformat()
date_u = lambda s: _strptime(s[0:10], "%Y-%m-%d").date()
date_m = lambda d: d.strftime("%Y-%m-%d")
time_u = lambda s: _strptime(s, "%H:%M:%S").time()
time_m = lambda d: d.strftime("%H%M%S")
bool_u = lambda s: {'0': False, 'false': False, '1': True, 'true': True}[s]
bool_m = lambda s: {False: 'false', True: 'true'}[s]


# aliases:
class Alias(object):
    def __init__(self, py_type, xml_type):
        self.py_type, self.xml_type = py_type, xml_type

    def __call__(self, value):
        return self.py_type(value)

    def __repr__(self):
        return "<alias '%s' for '%s'>" % (self.xml_type, self.py_type)

if sys.version > '3':
    long = Alias(int, 'long')
byte = Alias(str, 'byte')
short = Alias(int, 'short')
double = Alias(float, 'double')
integer = Alias(long, 'integer')
DateTime = datetime.datetime
Date = datetime.date
Time = datetime.time

# Define convertion function (python type): xml schema type
TYPE_MAP = {
    unicode: 'string',
    bool: 'boolean',
    short: 'short',
    byte: 'byte',
    int: 'int',
    long: 'long',
    integer: 'integer',
    float: 'float',
    double: 'double',
    Decimal: 'decimal',
    datetime.datetime: 'dateTime',
    datetime.date: 'date',
}
TYPE_MARSHAL_FN = {
    datetime.datetime: datetime_m,
    datetime.date: date_m,
    bool: bool_m
}
TYPE_UNMARSHAL_FN = {
    datetime.datetime: datetime_u,
    datetime.date: date_u,
    bool: bool_u,
    str: unicode,
}

REVERSE_TYPE_MAP = dict([(v, k) for k, v in TYPE_MAP.items()])

REVERSE_TYPE_MAP.update({
    'base64Binary': str,
})

# insert str here to avoid collision in REVERSE_TYPE_MAP (i.e. decoding errors)
if str not in TYPE_MAP:
    TYPE_MAP[str] = 'string'    


class OrderedDict(dict):
    """Minimal ordered dictionary for xsd:sequences"""
    def __init__(self):
        self.__keys = []
        self.array = False
        self.namespace = None
        self.qualified = None

    def __setitem__(self, key, value):
        if key not in self.__keys:
            self.__keys.append(key)
        dict.__setitem__(self, key, value)

    def insert(self, key, value, index=0):
        if key not in self.__keys:
            self.__keys.insert(index, key)
        dict.__setitem__(self, key, value)

    def __delitem__(self, key):
        if key in self.__keys:
            self.__keys.remove(key)
        dict.__delitem__(self, key)

    def __iter__(self):
        return iter(self.__keys)

    def keys(self):
        return self.__keys

    def items(self):
        return [(key, self[key]) for key in self.__keys]

    def update(self, other):
        for k, v in other.items():
            self[k] = v
        # do not change if we are an array but the other is not:
        if isinstance(other, OrderedDict) and not self.array:
            self.array = other.array
        if isinstance(other, OrderedDict) and not self.namespace:
            self.namespace = other.namespace
            self.qualified = other.qualified

    def copy(self):
        "Make a duplicate"
        new = OrderedDict()
        new.update(self)
        return new

    def __str__(self):
        return "%s" % dict.__str__(self)

    def __repr__(self):
        s = "{%s}" % ", ".join(['%s: %s' % (repr(k), repr(v)) for k, v in self.items()])
        if self.array and False:
            s = "[%s]" % s
        return s
