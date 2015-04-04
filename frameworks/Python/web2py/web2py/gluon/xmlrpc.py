#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
| This file is part of the web2py Web Framework
| Copyrighted by Massimo Di Pierro <mdipierro@cs.depaul.edu>
| License: LGPLv3 (http://www.gnu.org/licenses/lgpl.html)
"""

from SimpleXMLRPCServer import SimpleXMLRPCDispatcher


def handler(request, response, methods):
    response.session_id = None  # no sessions for xmlrpc
    dispatcher = SimpleXMLRPCDispatcher(allow_none=True, encoding=None)
    for method in methods:
        dispatcher.register_function(method)
    dispatcher.register_introspection_functions()
    response.headers['Content-Type'] = 'text/xml'
    dispatch = getattr(dispatcher, '_dispatch', None)
    return dispatcher._marshaled_dispatch(request.body.read(), dispatch)
