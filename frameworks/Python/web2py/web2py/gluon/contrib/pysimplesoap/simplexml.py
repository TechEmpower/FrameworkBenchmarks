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

"""Simple XML manipulation"""


from __future__ import unicode_literals
import sys
if sys.version > '3':
    basestring = str
    unicode = str

import logging
import re
import time
import xml.dom.minidom

from . import __author__, __copyright__, __license__, __version__

# Utility functions used for marshalling, moved aside for readability
from .helpers import TYPE_MAP, TYPE_MARSHAL_FN, TYPE_UNMARSHAL_FN, \
                     REVERSE_TYPE_MAP, OrderedDict, Date, Decimal

log = logging.getLogger(__name__)


class SimpleXMLElement(object):
    """Simple XML manipulation (simil PHP)"""

    def __init__(self, text=None, elements=None, document=None,
                 namespace=None, prefix=None, namespaces_map={}, jetty=False):
        """
        :param namespaces_map: How to map our namespace prefix to that given by the client;
          {prefix: received_prefix}
        """
        self.__namespaces_map = namespaces_map
        _rx = "|".join(namespaces_map.keys())  # {'external': 'ext', 'model': 'mod'} -> 'external|model'
        self.__ns_rx = re.compile(r"^(%s):.*$" % _rx)  # And now we build an expression ^(external|model):.*$
                                                       # to find prefixes in all xml nodes i.e.: <model:code>1</model:code>
                                                       # and later change that to <mod:code>1</mod:code>
        self.__ns = namespace
        self.__prefix = prefix
        self.__jetty = jetty                           # special list support

        if text is not None:
            try:
                self.__document = xml.dom.minidom.parseString(text)
            except:
                log.error(text)
                raise
            self.__elements = [self.__document.documentElement]
        else:
            self.__elements = elements
            self.__document = document

    def add_child(self, name, text=None, ns=True):
        """Adding a child tag to a node"""
        if not ns or self.__ns is False:
            ##log.debug('adding %s without namespace', name)
            element = self.__document.createElement(name)
        else:
            ##log.debug('adding %s ns "%s" %s', name, self.__ns, ns)
            if isinstance(ns, basestring):
                element = self.__document.createElement(name)
                if ns:
                    element.setAttribute("xmlns", ns)
            elif self.__prefix:
                element = self.__document.createElementNS(self.__ns, "%s:%s" % (self.__prefix, name))
            else:
                element = self.__document.createElementNS(self.__ns, name)
        # don't append null tags!
        if text is not None:
            element.appendChild(self.__document.createTextNode(text))
        self._element.appendChild(element)
        return SimpleXMLElement(
            elements=[element],
            document=self.__document,
            namespace=self.__ns,
            prefix=self.__prefix,
            jetty=self.__jetty,
            namespaces_map=self.__namespaces_map
        )

    def __setattr__(self, tag, text):
        """Add text child tag node (short form)"""
        if tag.startswith("_"):
            object.__setattr__(self, tag, text)
        else:
            ##log.debug('__setattr__(%s, %s)', tag, text)
            self.add_child(tag, text)

    def __delattr__(self, tag):
        """Remove a child tag (non recursive!)"""
        elements = [__element for __element in self._element.childNodes
                    if __element.nodeType == __element.ELEMENT_NODE]
        for element in elements:
            self._element.removeChild(element)

    def add_comment(self, data):
        """Add an xml comment to this child"""
        comment = self.__document.createComment(data)
        self._element.appendChild(comment)

    def as_xml(self, filename=None, pretty=False):
        """Return the XML representation of the document"""
        if not pretty:
            return self.__document.toxml('UTF-8')
        else:
            return self.__document.toprettyxml(encoding='UTF-8')

    def __repr__(self):
        """Return the XML representation of this tag"""
        # NOTE: do not use self.as_xml('UTF-8') as it returns the whole xml doc
        return self._element.toxml('UTF-8')

    def get_name(self):
        """Return the tag name of this node"""
        return self._element.tagName

    def get_local_name(self):
        """Return the tag local name (prefix:name) of this node"""
        return self._element.localName

    def get_prefix(self):
        """Return the namespace prefix of this node"""
        return self._element.prefix

    def get_namespace_uri(self, ns):
        """Return the namespace uri for a prefix"""
        element = self._element
        while element is not None and element.attributes is not None:
            try:
                return element.attributes['xmlns:%s' % ns].value
            except KeyError:
                element = element.parentNode

    def attributes(self):
        """Return a dict of attributes for this tag"""
        #TODO: use slice syntax [:]?
        return self._element.attributes

    def __getitem__(self, item):
        """Return xml tag attribute value or a slice of attributes (iter)"""
        ##log.debug('__getitem__(%s)', item)
        if isinstance(item, basestring):
            if self._element.hasAttribute(item):
                return self._element.attributes[item].value
        elif isinstance(item, slice):
            # return a list with name:values
            return list(self._element.attributes.items())[item]
        else:
            # return element by index (position)
            element = self.__elements[item]
            return SimpleXMLElement(
                elements=[element],
                document=self.__document,
                namespace=self.__ns,
                prefix=self.__prefix,
                jetty=self.__jetty,
                namespaces_map=self.__namespaces_map
            )

    def add_attribute(self, name, value):
        """Set an attribute value from a string"""
        self._element.setAttribute(name, value)

    def __setitem__(self, item, value):
        """Set an attribute value"""
        if isinstance(item, basestring):
            self.add_attribute(item, value)
        elif isinstance(item, slice):
            # set multiple attributes at once
            for k, v in value.items():
                self.add_attribute(k, v)

    def __call__(self, tag=None, ns=None, children=False, root=False,
                 error=True, ):
        """Search (even in child nodes) and return a child tag by name"""
        try:
            if root:
                # return entire document
                return SimpleXMLElement(
                    elements=[self.__document.documentElement],
                    document=self.__document,
                    namespace=self.__ns,
                    prefix=self.__prefix,
                    jetty=self.__jetty,
                    namespaces_map=self.__namespaces_map
                )
            if tag is None:
                # if no name given, iterate over siblings (same level)
                return self.__iter__()
            if children:
                # future: filter children? by ns?
                return self.children()
            elements = None
            if isinstance(tag, int):
                # return tag by index
                elements = [self.__elements[tag]]
            if ns and not elements:
                for ns_uri in isinstance(ns, (tuple, list)) and ns or (ns, ):
                    ##log.debug('searching %s by ns=%s', tag, ns_uri)
                    elements = self._element.getElementsByTagNameNS(ns_uri, tag)
                    if elements:
                        break
            if self.__ns and not elements:
                ##log.debug('searching %s by ns=%s', tag, self.__ns)
                elements = self._element.getElementsByTagNameNS(self.__ns, tag)
            if not elements:
                ##log.debug('searching %s', tag)
                elements = self._element.getElementsByTagName(tag)
            if not elements:
                ##log.debug(self._element.toxml())
                if error:
                    raise AttributeError("No elements found")
                else:
                    return
            return SimpleXMLElement(
                elements=elements,
                document=self.__document,
                namespace=self.__ns,
                prefix=self.__prefix,
                jetty=self.__jetty,
                namespaces_map=self.__namespaces_map)
        except AttributeError as e:
            raise AttributeError("Tag not found: %s (%s)" % (tag, e))

    def __getattr__(self, tag):
        """Shortcut for __call__"""
        return self.__call__(tag)

    def __iter__(self):
        """Iterate over xml tags at this level"""
        try:
            for __element in self.__elements:
                yield SimpleXMLElement(
                    elements=[__element],
                    document=self.__document,
                    namespace=self.__ns,
                    prefix=self.__prefix,
                    jetty=self.__jetty,
                    namespaces_map=self.__namespaces_map)
        except:
            raise

    def __dir__(self):
        """List xml children tags names"""
        return [node.tagName for node
                in self._element.childNodes
                if node.nodeType != node.TEXT_NODE]

    def children(self):
        """Return xml children tags element"""
        elements = [__element for __element in self._element.childNodes
                    if __element.nodeType == __element.ELEMENT_NODE]
        if not elements:
            return None
            #raise IndexError("Tag %s has no children" % self._element.tagName)
        return SimpleXMLElement(
            elements=elements,
            document=self.__document,
            namespace=self.__ns,
            prefix=self.__prefix,
            jetty=self.__jetty,
            namespaces_map=self.__namespaces_map
        )

    def __len__(self):
        """Return element count"""
        return len(self.__elements)

    def __contains__(self, item):
        """Search for a tag name in this element or child nodes"""
        return self._element.getElementsByTagName(item)

    def __unicode__(self):
        """Returns the unicode text nodes of the current element"""
        if self._element.childNodes:
            rc = ""
            for node in self._element.childNodes:
                if node.nodeType == node.TEXT_NODE:
                    rc = rc + node.data
            return rc
        return ''

    def __str__(self):
        """Returns the str text nodes of the current element"""
        return self.__unicode__()

    def __int__(self):
        """Returns the integer value of the current element"""
        return int(self.__str__())

    def __float__(self):
        """Returns the float value of the current element"""
        try:
            return float(self.__str__())
        except:
            raise IndexError(self._element.toxml())

    _element = property(lambda self: self.__elements[0])

    def unmarshall(self, types, strict=True):
        #import pdb; pdb.set_trace()

        """Convert to python values the current serialized xml element"""
        # types is a dict of {tag name: convertion function}
        # strict=False to use default type conversion if not specified
        # example: types={'p': {'a': int,'b': int}, 'c': [{'d':str}]}
        #   expected xml: <p><a>1</a><b>2</b></p><c><d>hola</d><d>chau</d>
        #   returnde value: {'p': {'a':1,'b':2}, `'c':[{'d':'hola'},{'d':'chau'}]}
        d = {}
        for node in self():
            name = str(node.get_local_name())
            ref_name_type = None
            # handle multirefs: href="#id0"
            if 'href' in node.attributes().keys():
                href = node['href'][1:]
                for ref_node in self(root=True)("multiRef"):
                    if ref_node['id'] == href:
                        node = ref_node
                        ref_name_type = ref_node['xsi:type'].split(":")[1]
                        break             

            try:
                if isinstance(types, dict):
                    fn = types[name]
                    # custom array only in the response (not defined in the WSDL):
                    # <results soapenc:arrayType="xsd:string[199]>
                    if any([k for k,v in node[:] if 'arrayType' in k]) and not isinstance(fn, list):
                        fn = [fn]
                else:
                    fn = types
            except (KeyError, ) as e:
                if 'xsi:type' in node.attributes().keys():
                    xsd_type = node['xsi:type'].split(":")[1]
                    try:
                        fn = REVERSE_TYPE_MAP[xsd_type]
                    except:
                        fn = None  # ignore multirefs!
                elif strict:
                    raise TypeError("Tag: %s invalid (type not found)" % (name,))
                else:
                    # if not strict, use default type conversion
                    fn = str

            if isinstance(fn, list):
                # append to existing list (if any) - unnested dict arrays -
                value = d.setdefault(name, [])
                children = node.children()
                # TODO: check if this was really needed (get first child only)
                ##if len(fn[0]) == 1 and children:
                ##    children = children()
                if fn and not isinstance(fn[0], dict):
                    # simple arrays []
                    for child in (children or []):
                        tmp_dict = child.unmarshall(fn[0], strict)
                        value.extend(tmp_dict.values())
                elif (self.__jetty and len(fn[0]) > 1):
                    # Jetty array style support [{k, v}]
                    for parent in node:
                        tmp_dict = {}    # unmarshall each value & mix
                        for child in (node.children() or []):
                            tmp_dict.update(child.unmarshall(fn[0], strict))
                        value.append(tmp_dict)
                else:  # .Net / Java
                    for child in (children or []):
                        value.append(child.unmarshall(fn[0], strict))

            elif isinstance(fn, tuple):
                value = []
                _d = {}
                children = node.children()
                as_dict = len(fn) == 1 and isinstance(fn[0], dict)

                for child in (children and children() or []):  # Readability counts
                    if as_dict:
                        _d.update(child.unmarshall(fn[0], strict))  # Merging pairs
                    else:
                        value.append(child.unmarshall(fn[0], strict))
                if as_dict:
                    value.append(_d)

                if name in d:
                    _tmp = list(d[name])
                    _tmp.extend(value)
                    value = tuple(_tmp)
                else:
                    value = tuple(value)

            elif isinstance(fn, dict):
                ##if ref_name_type is not None:
                ##    fn = fn[ref_name_type]
                children = node.children()
                value = children and children.unmarshall(fn, strict)
            else:
                if fn is None:  # xsd:anyType not unmarshalled
                    value = node
                elif unicode(node) or (fn == str and unicode(node) != ''):
                    try:
                        # get special deserialization function (if any)
                        fn = TYPE_UNMARSHAL_FN.get(fn, fn)
                        if fn == str:
                            # always return an unicode object:
                            # (avoid encoding errors in py<3!)
                            value = unicode(node)
                        else:
                            value = fn(unicode(node))
                    except (ValueError, TypeError) as e:
                        raise ValueError("Tag: %s: %s" % (name, e))
                else:
                    value = None
            d[name] = value
        return d

    def _update_ns(self, name):
        """Replace the defined namespace alias with tohse used by the client."""
        pref = self.__ns_rx.search(name)
        if pref:
            pref = pref.groups()[0]
            try:
                name = name.replace(pref, self.__namespaces_map[pref])
            except KeyError:
                log.warning('Unknown namespace alias %s' % name)
        return name

    def marshall(self, name, value, add_child=True, add_comments=False,
                 ns=False, add_children_ns=True):
        """Analyze python value and add the serialized XML element using tag name"""
        # Change node name to that used by a client
        name = self._update_ns(name)

        if isinstance(value, dict):  # serialize dict (<key>value</key>)
            # for the first parent node, use the document target namespace
            # (ns==True) or use the namespace string uri if passed (elements)
            child = add_child and self.add_child(name, ns=ns) or self
            for k, v in value.items():
                if not add_children_ns:
                    ns = False
                else:
                    # for children, use the wsdl element target namespace:
                    ns = getattr(value, 'namespace', None)
                child.marshall(k, v, add_comments=add_comments, ns=ns)
        elif isinstance(value, tuple):  # serialize tuple (<key>value</key>)
            child = add_child and self.add_child(name, ns=ns) or self
            if not add_children_ns:
                ns = False
            for k, v in value:
                getattr(self, name).marshall(k, v, add_comments=add_comments, ns=ns)
        elif isinstance(value, list):  # serialize lists
            child = self.add_child(name, ns=ns)
            if not add_children_ns:
                ns = False
            if add_comments:
                child.add_comment("Repetitive array of:")
            for t in value:
                child.marshall(name, t, False, add_comments=add_comments, ns=ns)
        elif isinstance(value, basestring):  # do not convert strings or unicodes
            self.add_child(name, value, ns=ns)
        elif value is None:  # sent a empty tag?
            self.add_child(name, ns=ns)
        elif value in TYPE_MAP.keys():
            # add commented placeholders for simple tipes (for examples/help only)
            child = self.add_child(name, ns=ns)
            child.add_comment(TYPE_MAP[value])
        else:  # the rest of object types are converted to string
            # get special serialization function (if any)
            fn = TYPE_MARSHAL_FN.get(type(value), str)
            self.add_child(name, fn(value), ns=ns)

    def import_node(self, other):
        x = self.__document.importNode(other._element, True)  # deep copy
        self._element.appendChild(x)
