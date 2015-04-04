#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
| This file is part of the web2py Web Framework
| Copyrighted by Massimo Di Pierro <mdipierro@cs.depaul.edu>
| License: LGPLv3 (http://www.gnu.org/licenses/lgpl.html)

Restricted environment to execute application's code
-----------------------------------------------------
"""

import sys
try:
    import cPickle as pickle
except:
    import pickle
import traceback
import types
import os
import logging

from gluon.storage import Storage
from gluon.http import HTTP
from gluon.html import BEAUTIFY, XML
from gluon.settings import global_settings

logger = logging.getLogger("web2py")

__all__ = ['RestrictedError', 'restricted', 'TicketStorage', 'compile2']


class TicketStorage(Storage):

    """
    Defines the ticket object and the default values of its members (None)
    """

    def __init__(
        self,
        db=None,
        tablename='web2py_ticket'
    ):
        Storage.__init__(self)
        self.db = db
        self.tablename = tablename

    def store(self, request, ticket_id, ticket_data):
        """
        Stores the ticket. It will figure out if this must be on disk or in db
        """
        if self.db:
            self._store_in_db(request, ticket_id, ticket_data)
        else:
            self._store_on_disk(request, ticket_id, ticket_data)

    def _store_in_db(self, request, ticket_id, ticket_data):
        self.db._adapter.reconnect()
        try:
            table = self._get_table(self.db, self.tablename, request.application)
            table.insert(ticket_id=ticket_id,
                         ticket_data=pickle.dumps(ticket_data, pickle.HIGHEST_PROTOCOL),
                         created_datetime=request.now)
            self.db.commit()
            message = 'In FILE: %(layer)s\n\n%(traceback)s\n'
        except Exception:
            self.db.rollback()
            message =' Unable to store in FILE: %(layer)s\n\n%(traceback)s\n'
        self.db.close()
        logger.error(message % ticket_data)

    def _store_on_disk(self, request, ticket_id, ticket_data):
        ef = self._error_file(request, ticket_id, 'wb')
        try:
            pickle.dump(ticket_data, ef)
        finally:
            ef.close()

    def _error_file(self, request, ticket_id, mode, app=None):
        root = request.folder
        if app:
            root = os.path.join(os.path.join(root, '..'), app)
        errors_folder = os.path.abspath(
            os.path.join(root, 'errors'))  # .replace('\\', '/')
        return open(os.path.join(errors_folder, ticket_id), mode)

    def _get_table(self, db, tablename, app):
        tablename = tablename + '_' + app
        table = db.get(tablename)
        if not table:
            table = db.define_table(
                tablename,
                db.Field('ticket_id', length=100),
                db.Field('ticket_data', 'text'),
                db.Field('created_datetime', 'datetime'))
        return table

    def load(
        self,
        request,
        app,
        ticket_id,
    ):
        if not self.db:
            try:
                ef = self._error_file(request, ticket_id, 'rb', app)
            except IOError:
                return {}
            try:
                return pickle.load(ef)
            finally:
                ef.close()
        else:
            table = self._get_table(self.db, self.tablename, app)
            rows = self.db(table.ticket_id == ticket_id).select()
            return pickle.loads(rows[0].ticket_data) if rows else {}


class RestrictedError(Exception):
    """
    Class used to wrap an exception that occurs in the restricted environment
    below. The traceback is used to log the exception and generate a ticket.
    """

    def __init__(
        self,
        layer='',
        code='',
        output='',
        environment=None,
    ):
        """
        Layer here is some description of where in the system the exception
        occurred.
        """
        if environment is None:
            environment = {}
        self.layer = layer
        self.code = code
        self.output = output
        self.environment = environment
        if layer:
            try:
                self.traceback = traceback.format_exc()
            except:
                self.traceback = 'no traceback because template parsing error'
            try:
                self.snapshot = snapshot(context=10, code=code,
                                         environment=self.environment)
            except:
                self.snapshot = {}
        else:
            self.traceback = '(no error)'
            self.snapshot = {}

    def log(self, request):
        """
        Logs the exception.
        """
        try:
            d = {
                'layer': str(self.layer),
                'code': str(self.code),
                'output': str(self.output),
                'traceback': str(self.traceback),
                'snapshot': self.snapshot,
            }
            ticket_storage = TicketStorage(db=request.tickets_db)
            ticket_storage.store(request, request.uuid.split('/', 1)[1], d)
            cmd_opts = global_settings.cmd_options
            if cmd_opts and cmd_opts.print_errors:
                logger.error(self.traceback)
            return request.uuid
        except:
            logger.error(self.traceback)
            return None


    def load(self, request, app, ticket_id):
        """
        Loads a logged exception.
        """
        ticket_storage = TicketStorage(db=request.tickets_db)
        d = ticket_storage.load(request, app, ticket_id)

        self.layer = d.get('layer')
        self.code = d.get('code')
        self.output = d.get('output')
        self.traceback = d.get('traceback')
        self.snapshot = d.get('snapshot')

    def __str__(self):
        # safely show an useful message to the user
        try:
            output = self.output
            if isinstance(output, unicode):
                output = output.encode("utf8")
            elif not isinstance(output, str):
                output = str(output)
        except:
            output = ""
        return output


def compile2(code, layer):
    """
    The ``+'\\n'`` is necessary else compile fails when code ends in a comment.
    """

    return compile(code.rstrip().replace('\r\n', '\n') + '\n', layer, 'exec')


def restricted(code, environment=None, layer='Unknown'):
    """
    Runs code in environment and returns the output. If an exception occurs
    in code it raises a RestrictedError containing the traceback. Layer is
    passed to RestrictedError to identify where the error occurred.
    """
    if environment is None:
        environment = {}
    environment['__file__'] = layer
    environment['__name__'] = '__restricted__'
    try:
        if isinstance(code, types.CodeType):
            ccode = code
        else:
            ccode = compile2(code, layer)
        exec ccode in environment
    except HTTP:
        raise
    except RestrictedError:
        # do not encapsulate (obfuscate) the original RestrictedError
        raise
    except Exception, error:
        # extract the exception type and value (used as output message)
        etype, evalue, tb = sys.exc_info()
        # XXX Show exception in Wing IDE if running in debugger
        if __debug__ and 'WINGDB_ACTIVE' in os.environ:
            sys.excepthook(etype, evalue, tb)
        output = "%s %s" % (etype, evalue)
        raise RestrictedError(layer, code, output, environment)


def snapshot(info=None, context=5, code=None, environment=None):
    """Return a dict describing a given traceback (based on cgitb.text)."""
    import time
    import linecache
    import inspect
    import pydoc
    import cgitb

    # if no exception info given, get current:
    etype, evalue, etb = info or sys.exc_info()

    if isinstance(etype, types.ClassType):
        etype = etype.__name__

    # create a snapshot dict with some basic information
    s = {}
    s['pyver'] = 'Python ' + sys.version.split()[0] + ': ' + sys.executable + ' (prefix: %s)' % sys.prefix
    s['date'] = time.ctime(time.time())

    # start to process frames
    records = inspect.getinnerframes(etb, context)
    s['frames'] = []
    for frame, file, lnum, func, lines, index in records:
        file = file and os.path.abspath(file) or '?'
        args, varargs, varkw, locals = inspect.getargvalues(frame)
        call = ''
        if func != '?':
            call = inspect.formatargvalues(args, varargs, varkw, locals,
                                           formatvalue=lambda value: '=' + pydoc.text.repr(value))

        # basic frame information
        f = {'file': file, 'func': func, 'call': call, 'lines': {},
             'lnum': lnum}

        highlight = {}

        def reader(lnum=[lnum]):
            highlight[lnum[0]] = 1
            try:
                return linecache.getline(file, lnum[0])
            finally:
                lnum[0] += 1
        vars = cgitb.scanvars(reader, frame, locals)

        # if it is a view, replace with generated code
        if file.endswith('html'):
            lmin = lnum > context and (lnum - context) or 0
            lmax = lnum + context
            lines = code.split("\n")[lmin:lmax]
            index = min(context, lnum) - 1

        if index is not None:
            i = lnum - index
            for line in lines:
                f['lines'][i] = line.rstrip()
                i += 1

        # dump local variables (referenced in current line only)
        f['dump'] = {}
        for name, where, value in vars:
            if name in f['dump']:
                continue
            if value is not cgitb.__UNDEF__:
                if where == 'global':
                    name = 'global ' + name
                elif where != 'local':
                    name = where + name.split('.')[-1]
                f['dump'][name] = pydoc.text.repr(value)
            else:
                f['dump'][name] = 'undefined'

        s['frames'].append(f)

    # add exception type, value and attributes
    s['etype'] = str(etype)
    s['evalue'] = str(evalue)
    s['exception'] = {}
    if isinstance(evalue, BaseException):
        for name in dir(evalue):
            # prevent py26 DeprecatedWarning:
            if name != 'message' or sys.version_info < (2.6):
                value = pydoc.text.repr(getattr(evalue, name))
                s['exception'][name] = value

    # add all local values (of last frame) to the snapshot
    s['locals'] = {}
    for name, value in locals.items():
        s['locals'][name] = pydoc.text.repr(value)

    # add web2py environment variables
    for k, v in environment.items():
        if k in ('request', 'response', 'session'):
            s[k] = XML(str(BEAUTIFY(v)))

    return s
