#
# Copyright 2007 Google Inc.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#      http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# Modified by Massimo Di Pierro so it works with and without GAE with web2py
# the modified version of this file is still released under the original Apache license
# and it is not released under the web2py license.
#
# This should be compatible with the Apache license since it states:
# "For the purposes of this License, Derivative Works shall not include works
#   that remain separable from, or merely link (or bind by name) to the interfaces of,
#   the Work and Derivative Works thereof."
#
# In fact this file is Apache-licensed and it is separable from the rest of web2py.


"""
An interactive, stateful AJAX shell that runs Python code on the server.
"""

import logging
import new
try:
   import cPickle as pickle
except:
   import pickle
import sys
import traceback
import types
import StringIO
import threading
locker = threading.RLock()

# Set to True if stack traces should be shown in the browser, etc.
_DEBUG = True

# The entity kind for shell historys. Feel free to rename to suit your app.
_HISTORY_KIND = '_Shell_History'

# Types that can't be pickled.
UNPICKLABLE_TYPES = [
    types.ModuleType,
    types.TypeType,
    types.ClassType,
    types.FunctionType,
]

# Unpicklable statements to seed new historys with.
INITIAL_UNPICKLABLES = [
    'import logging',
    'import os',
    'import sys',
]


class History:
    """A shell history. Stores the history's globals.

    Each history globals is stored in one of two places:

    If the global is picklable, it's stored in the parallel globals and
    global_names list properties. (They're parallel lists to work around the
    unfortunate fact that the datastore can't store dictionaries natively.)

    If the global is not picklable (e.g. modules, classes, and functions), or if
    it was created by the same statement that created an unpicklable global,
    it's not stored directly. Instead, the statement is stored in the
    unpicklables list property. On each request, before executing the current
    statement, the unpicklable statements are evaluated to recreate the
    unpicklable globals.

    The unpicklable_names property stores all of the names of globals that were
    added by unpicklable statements. When we pickle and store the globals after
    executing a statement, we skip the ones in unpicklable_names.

    Using Text instead of string is an optimization. We don't query on any of
    these properties, so they don't need to be indexed.
    """
    global_names = []
    globals = []
    unpicklable_names = []
    unpicklables = []

    def set_global(self, name, value):
        """Adds a global, or updates it if it already exists.

        Also removes the global from the list of unpicklable names.

        Args:
            name: the name of the global to remove
            value: any picklable value
        """
        blob = pickle.dumps(value, pickle.HIGHEST_PROTOCOL)

        if name in self.global_names:
            index = self.global_names.index(name)
            self.globals[index] = blob
        else:
            self.global_names.append(name)
            self.globals.append(blob)

        self.remove_unpicklable_name(name)

    def remove_global(self, name):
        """Removes a global, if it exists.

        Args:
            name: string, the name of the global to remove
        """
        if name in self.global_names:
            index = self.global_names.index(name)
            del self.global_names[index]
            del self.globals[index]

    def globals_dict(self):
        """Returns a dictionary view of the globals.
        """
        return dict((name, cPickle.loads(val))
                    for name, val in zip(self.global_names, self.globals))

    def add_unpicklable(self, statement, names):
        """Adds a statement and list of names to the unpicklables.

        Also removes the names from the globals.

        Args:
            statement: string, the statement that created new unpicklable global(s).
            names: list of strings; the names of the globals created by the statement.
        """
        self.unpicklables.append(statement)

        for name in names:
            self.remove_global(name)
            if name not in self.unpicklable_names:
                self.unpicklable_names.append(name)

    def remove_unpicklable_name(self, name):
        """Removes a name from the list of unpicklable names, if it exists.

        Args:
            name: string, the name of the unpicklable global to remove
        """
        if name in self.unpicklable_names:
            self.unpicklable_names.remove(name)


def represent(obj):
    """Returns a string representing the given object's value, which should allow the
    code below to determine whether the object changes over time.
    """
    try:
        return pickle.dumps(obj, pickle.HIGHEST_PROTOCOL)
    except:
        return repr(obj)


def run(history, statement, env={}):
    """
    Evaluates a python statement in a given history and returns the result.
    """
    history.unpicklables = INITIAL_UNPICKLABLES

    # extract the statement to be run
    if not statement:
        return ''

    # the python compiler doesn't like network line endings
    statement = statement.replace('\r\n', '\n')

    # add a couple newlines at the end of the statement. this makes
    # single-line expressions such as 'class Foo: pass' evaluate happily.
    statement += '\n\n'

    # log and compile the statement up front
    try:
        logging.info('Compiling and evaluating:\n%s' % statement)
        compiled = compile(statement, '<string>', 'single')
    except:
        return str(traceback.format_exc())

    # create a dedicated module to be used as this statement's __main__
    statement_module = new.module('__main__')

    # use this request's __builtin__, since it changes on each request.
    # this is needed for import statements, among other things.
    import __builtin__
    statement_module.__builtins__ = __builtin__

    # load the history from the datastore
    history = History()

    # swap in our custom module for __main__. then unpickle the history
    # globals, run the statement, and re-pickle the history globals, all
    # inside it.
    old_main = sys.modules.get('__main__')
    output = StringIO.StringIO()
    try:
        sys.modules['__main__'] = statement_module
        statement_module.__name__ = '__main__'
        statement_module.__dict__.update(env)

        # re-evaluate the unpicklables
        for code in history.unpicklables:
            exec code in statement_module.__dict__

        # re-initialize the globals
        for name, val in history.globals_dict().items():
            try:
                statement_module.__dict__[name] = val
            except:
                msg = 'Dropping %s since it could not be unpickled.\n' % name
                output.write(msg)
                logging.warning(msg + traceback.format_exc())
                history.remove_global(name)

        # run!
        old_globals = dict((key, represent(
            value)) for key, value in statement_module.__dict__.items())
        try:
            old_stdout, old_stderr = sys.stdout, sys.stderr
            try:
                sys.stderr = sys.stdout = output
                locker.acquire()
                exec compiled in statement_module.__dict__
            finally:
                locker.release()
                sys.stdout, sys.stderr = old_stdout, old_stderr
        except:
            output.write(str(traceback.format_exc()))
            return output.getvalue()

        # extract the new globals that this statement added
        new_globals = {}
        for name, val in statement_module.__dict__.items():
            if name not in old_globals or represent(val) != old_globals[name]:
                new_globals[name] = val

        if True in [isinstance(val, tuple(UNPICKLABLE_TYPES))
                    for val in new_globals.values()]:
            # this statement added an unpicklable global. store the statement and
            # the names of all of the globals it added in the unpicklables.
            history.add_unpicklable(statement, new_globals.keys())
            logging.debug('Storing this statement as an unpicklable.')
        else:
            # this statement didn't add any unpicklables. pickle and store the
            # new globals back into the datastore.
            for name, val in new_globals.items():
                if not name.startswith('__'):
                    try:
                        history.set_global(name, val)
                    except (TypeError, pickle.PicklingError), ex:
                        UNPICKLABLE_TYPES.append(type(val))
                        history.add_unpicklable(statement, new_globals.keys())

    finally:
        sys.modules['__main__'] = old_main
    return output.getvalue()

if __name__ == '__main__':
    history = History()
    while True:
        print run(history, raw_input('>>> ')).rstrip()
