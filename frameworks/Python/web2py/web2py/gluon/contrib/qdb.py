#!/usr/bin/env python
# coding:utf-8

"Queues(Pipe)-based independent remote client-server Python Debugger"

__author__ = "Mariano Reingart (reingart@gmail.com)"
__copyright__ = "Copyright (C) 2011 Mariano Reingart"
__license__ = "LGPL 3.0"
__version__ = "1.01b"

# remote debugger queue-based (jsonrpc-like interface):
# - bidirectional communication (request - response calls in both ways)
# - request with id == null is a notification (do not send a response)
# - request with a value for id is a normal call, wait response
# based on idle, inspired by pythonwin implementation, taken many code from pdb

import bdb
import inspect
import linecache
import os
import sys
import traceback
import cmd
import pydoc
import threading


class Qdb(bdb.Bdb):
    "Qdb Debugger Backend"

    def __init__(self, pipe, redirect_stdio=True, allow_interruptions=False,
                 skip=[__name__]):
        kwargs = {}
        if sys.version_info > (2, 7):
            kwargs['skip'] = skip
        bdb.Bdb.__init__(self, **kwargs)
        self.frame = None
        self.i = 1  # sequential RPC call id
        self.waiting = False
        self.pipe = pipe  # for communication
        self._wait_for_mainpyfile = False
        self._wait_for_breakpoint = False
        self.mainpyfile = ""
        self._lineno = None     # last listed line numbre
        # replace system standard input and output (send them thru the pipe)
        if redirect_stdio:
            sys.stdin = self
            sys.stdout = self
            sys.stderr = self
        if allow_interruptions:
            # fake breakpoint to prevent removing trace_dispatch on set_continue
            self.breaks[None] = []
        self.allow_interruptions = allow_interruptions
        self.burst = 0          # do not send notifications ("burst" mode)
        self.params = {}        # optional parameters for interaction

    def pull_actions(self):
        # receive a remote procedure call from the frontend:
        # returns True if action processed
        #         None when 'run' notification is received (see 'startup')
        request = self.pipe.recv()
        if request.get("method") == 'run':
            return None
        response = {'version': '1.1', 'id': request.get('id'),
                    'result': None,
                    'error': None}
        try:
            # dispatch message (JSON RPC like)
            method = getattr(self, request['method'])
            response['result'] = method.__call__(*request['args'],
                                                 **request.get('kwargs', {}))
        except Exception, e:
            response['error'] = {'code': 0, 'message': str(e)}
        # send the result for normal method calls, not for notifications
        if request.get('id'):
            self.pipe.send(response)
        return True

    # Override Bdb methods

    def trace_dispatch(self, frame, event, arg):
        # check for non-interaction rpc (set_breakpoint, interrupt)
        while self.allow_interruptions and self.pipe.poll():
            self.pull_actions()
        # process the frame (see Bdb.trace_dispatch)
        if self.quitting:
            return  # None
        if event == 'line':
            return self.dispatch_line(frame)
        if event == 'call':
            return self.dispatch_call(frame, arg)
        if event == 'return':
            return self.dispatch_return(frame, arg)
        if event == 'exception':
            return self.dispatch_exception(frame, arg)
        return self.trace_dispatch

    def user_call(self, frame, argument_list):
        """This method is called when there is the remote possibility
        that we ever need to stop in this function."""
        if self._wait_for_mainpyfile or self._wait_for_breakpoint:
            return
        if self.stop_here(frame):
            self.interaction(frame, None)

    def user_line(self, frame):
        """This function is called when we stop or break at this line."""
        if self._wait_for_mainpyfile:
            if (not self.canonic(frame.f_code.co_filename).startswith(self.mainpyfile)
                    or frame.f_lineno <= 0):
                return
            self._wait_for_mainpyfile = 0
        if self._wait_for_breakpoint:
            if not self.break_here(frame):
                return
            self._wait_for_breakpoint = 0
        self.interaction(frame)

    def user_exception(self, frame, info):
        """This function is called if an exception occurs,
        but only if we are to stop at or just below this level."""
        if self._wait_for_mainpyfile or self._wait_for_breakpoint:
            return
        extype, exvalue, trace = info
        # pre-process stack trace as it isn't pickeable (cannot be sent pure)
        msg = ''.join(traceback.format_exception(extype, exvalue, trace))
        trace = traceback.extract_tb(trace)
        title = traceback.format_exception_only(extype, exvalue)[0]
        # send an Exception notification
        msg = {'method': 'exception',
               'args': (title, extype.__name__, exvalue, trace, msg),
               'id': None}
        self.pipe.send(msg)
        self.interaction(frame, info)

    def run(self, code, interp=None, *args, **kwargs):
        try:
            return bdb.Bdb.run(self, code, *args, **kwargs)
        finally:
            pass

    def runcall(self, function, interp=None, *args, **kwargs):
        try:
            self.interp = interp
            return bdb.Bdb.runcall(self, function, *args, **kwargs)
        finally:
            pass

    def _runscript(self, filename):
        # The script has to run in __main__ namespace (clear it)
        import __main__
        import imp
        __main__.__dict__.clear()
        __main__.__dict__.update({"__name__": "__main__",
                                  "__file__": filename,
                                  "__builtins__": __builtins__,
                                  "imp": imp,          # need for run
                                  })

        # avoid stopping before we reach the main script
        self._wait_for_mainpyfile = 1
        self.mainpyfile = self.canonic(filename)
        self._user_requested_quit = 0
        statement = 'imp.load_source("__main__", "%s")' % filename
        # notify and wait frontend to set initial params and breakpoints
        self.pipe.send({'method': 'startup', 'args': (__version__, )})
        while self.pull_actions() is not None:
            pass
        self.run(statement)

    # General interaction function

    def interaction(self, frame, info=None):
        # chache frame locals to ensure that modifications are not overwritten
        self.frame_locals = frame and frame.f_locals or {}
        # extract current filename and line number
        code, lineno = frame.f_code, frame.f_lineno
        filename = code.co_filename
        basename = os.path.basename(filename)
        message = "%s:%s" % (basename, lineno)
        if code.co_name != "?":
            message = "%s: %s()" % (message, code.co_name)

        # wait user events
        self.waiting = True
        self.frame = frame
        try:
            while self.waiting:
                #  sync_source_line()
                if frame and filename[:1] + filename[-1:] != "<>" and os.path.exists(filename):
                    line = linecache.getline(filename, self.frame.f_lineno,
                                             self.frame.f_globals)
                else:
                    line = ""
                # send the notification (debug event) - DOESN'T WAIT RESPONSE
                self.burst -= 1
                if self.burst < 0:
                    kwargs = {}
                    if self.params.get('call_stack'):
                        kwargs['call_stack'] = self.do_where()
                    if self.params.get('environment'):
                        kwargs['environment'] = self.do_environment()
                    self.pipe.send({'method': 'interaction', 'id': None,
                                    'args': (filename, self.frame.f_lineno, line),
                                    'kwargs': kwargs})

                self.pull_actions()

        finally:
            self.waiting = False
        self.frame = None

    def do_debug(self, mainpyfile=None, wait_breakpoint=1):
        self.reset()
        if not wait_breakpoint or mainpyfile:
            self._wait_for_mainpyfile = 1
            if not mainpyfile:
                frame = sys._getframe().f_back
                mainpyfile = frame.f_code.co_filename
            self.mainpyfile = self.canonic(mainpyfile)
        self._wait_for_breakpoint = wait_breakpoint
        sys.settrace(self.trace_dispatch)

    def set_trace(self, frame=None):
        # start debugger interaction immediatelly
        if frame is None:
            frame = sys._getframe().f_back
        self._wait_for_mainpyfile = frame.f_code.co_filename
        self._wait_for_breakpoint = 0
        bdb.Bdb.set_trace(self, frame)

    # Command definitions, called by interaction()

    def do_continue(self):
        self.set_continue()
        self.waiting = False

    def do_step(self):
        self.set_step()
        self.waiting = False

    def do_return(self):
        self.set_return(self.frame)
        self.waiting = False

    def do_next(self):
        self.set_next(self.frame)
        self.waiting = False

    def interrupt(self):
        self.set_step()

    def do_quit(self):
        self.set_quit()
        self.waiting = False

    def do_jump(self, lineno):
        arg = int(lineno)
        try:
            self.frame.f_lineno = arg
            return arg
        except ValueError, e:
            print '*** Jump failed:', e
            return False

    def do_list(self, arg):
        last = None
        if arg:
            if isinstance(arg, tuple):
                first, last = arg
            else:
                first = arg
        elif not self._lineno:
            first = max(1, self.frame.f_lineno - 5)
        else:
            first = self._lineno + 1
        if last is None:
            last = first + 10
        filename = self.frame.f_code.co_filename
        breaklist = self.get_file_breaks(filename)
        lines = []
        for lineno in range(first, last + 1):
            line = linecache.getline(filename, lineno,
                                     self.frame.f_globals)
            if not line:
                lines.append((filename, lineno, '', current, "<EOF>\n"))
                break
            else:
                breakpoint = "B" if lineno in breaklist else ""
                current = "->" if self.frame.f_lineno == lineno else ""
                lines.append((filename, lineno, breakpoint, current, line))
                self._lineno = lineno
        return lines

    def do_read(self, filename):
        return open(filename, "Ur").read()

    def do_set_breakpoint(self, filename, lineno, temporary=0, cond=None):
        return self.set_break(filename, int(lineno), temporary, cond)

    def do_list_breakpoint(self):
        breaks = []
        if self.breaks:  # There's at least one
            for bp in bdb.Breakpoint.bpbynumber:
                if bp:
                    breaks.append((bp.number, bp.file, bp.line,
                                   bp.temporary, bp.enabled, bp.hits, bp.cond, ))
        return breaks

    def do_clear_breakpoint(self, filename, lineno):
        self.clear_break(filename, lineno)

    def do_clear_file_breakpoints(self, filename):
        self.clear_all_file_breaks(filename)

    def do_clear(self, arg):
        # required by BDB to remove temp breakpoints!
        err = self.clear_bpbynumber(arg)
        if err:
            print '*** DO_CLEAR failed', err

    def do_eval(self, arg, safe=True):
        ret = eval(arg, self.frame.f_globals,
                   self.frame_locals)
        if safe:
            ret = pydoc.cram(repr(ret), 255)
        return ret

    def do_exec(self, arg):
        locals = self.frame_locals
        globals = self.frame.f_globals
        code = compile(arg + '\n', '<stdin>', 'single')
        save_displayhook = sys.displayhook
        self.displayhook_value = None
        try:
            sys.displayhook = self.displayhook
            exec code in globals, locals
        finally:
            sys.displayhook = save_displayhook
        return self.displayhook_value

    def do_where(self):
        "print_stack_trace"
        stack, curindex = self.get_stack(self.frame, None)
        lines = []
        for frame, lineno in stack:
            filename = frame.f_code.co_filename
            line = linecache.getline(filename, lineno)
            lines.append((filename, lineno, "", "", line, ))
        return lines

    def do_environment(self):
        "return current frame local and global environment"
        env = {'locals': {}, 'globals': {}}
        # converts the frame global and locals to a short text representation:
        if self.frame:
            for name, value in self.frame_locals.items():
                env['locals'][name] = pydoc.cram(repr(
                    value), 255), repr(type(value))
            for name, value in self.frame.f_globals.items():
                env['globals'][name] = pydoc.cram(repr(
                    value), 20), repr(type(value))
        return env

    def get_autocomplete_list(self, expression):
        "Return list of auto-completion options for expression"
        try:
            obj = self.do_eval(expression)
        except:
            return []
        else:
            return dir(obj)

    def get_call_tip(self, expression):
        "Return list of auto-completion options for expression"
        try:
            obj = self.do_eval(expression)
        except Exception, e:
            return ('', '', str(e))
        else:
            name = ''
            try:
                name = obj.__name__
            except AttributeError:
                pass
            argspec = ''
            drop_self = 0
            f = None
            try:
                if inspect.isbuiltin(obj):
                    pass
                elif inspect.ismethod(obj):
                    # Get the function from the object
                    f = obj.im_func
                    drop_self = 1
                elif inspect.isclass(obj):
                    # Get the __init__ method function for the class.
                    if hasattr(obj, '__init__'):
                        f = obj.__init__.im_func
                    else:
                        for base in object.__bases__:
                            if hasattr(base, '__init__'):
                                f = base.__init__.im_func
                                break
                    if f is not None:
                        drop_self = 1
                elif callable(obj):
                    # use the obj as a function by default
                    f = obj
                    # Get the __call__ method instead.
                    f = obj.__call__.im_func
                    drop_self = 0
            except AttributeError:
                pass
            if f:
                argspec = apply(inspect.formatargspec, inspect.getargspec(f))
            doc = ''
            if callable(obj):
                try:
                    doc = inspect.getdoc(obj)
                except:
                    pass
            return (name, argspec[1:-1], doc.strip())

    def set_burst(self, val):
        "Set burst mode -multiple command count- (shut up notifications)"
        self.burst = val

    def set_params(self, params):
        "Set parameters for interaction"
        self.params.update(params)

    def displayhook(self, obj):
        """Custom displayhook for the do_exec which prevents
        assignment of the _ variable in the builtins.
        """
        self.displayhook_value = repr(obj)

    def reset(self):
        bdb.Bdb.reset(self)
        self.waiting = False
        self.frame = None

    def post_mortem(self, t=None):
        # handling the default
        if t is None:
            # sys.exc_info() returns (type, value, traceback) if an exception is
            # being handled, otherwise it returns None
            t = sys.exc_info()[2]
            if t is None:
                raise ValueError("A valid traceback must be passed if no "
                                 "exception is being handled")
        self.reset()
        # get last frame:
        while t is not None:
            frame = t.tb_frame
            t = t.tb_next
            code, lineno = frame.f_code, frame.f_lineno
            filename = code.co_filename
            line = linecache.getline(filename, lineno)
            #(filename, lineno, "", current, line, )}

        self.interaction(frame)

    # console file-like object emulation
    def readline(self):
        "Replacement for stdin.readline()"
        msg = {'method': 'readline', 'args': (), 'id': self.i}
        self.pipe.send(msg)
        msg = self.pipe.recv()
        self.i += 1
        return msg['result']

    def readlines(self):
        "Replacement for stdin.readlines()"
        lines = []
        while lines[-1:] != ['\n']:
            lines.append(self.readline())
        return lines

    def write(self, text):
        "Replacement for stdout.write()"
        msg = {'method': 'write', 'args': (text, ), 'id': None}
        self.pipe.send(msg)

    def writelines(self, l):
        map(self.write, l)

    def flush(self):
        pass

    def isatty(self):
        return 0


class QueuePipe(object):
    "Simulated pipe for threads (using two queues)"

    def __init__(self, name, in_queue, out_queue):
        self.__name = name
        self.in_queue = in_queue
        self.out_queue = out_queue

    def send(self, data):
        self.out_queue.put(data, block=True)

    def recv(self, count=None, timeout=None):
        data = self.in_queue.get(block=True, timeout=timeout)
        return data

    def poll(self, timeout=None):
        return not self.in_queue.empty()

    def close(self):
        pass


class RPCError(RuntimeError):
    "Remote Error (not user exception)"
    pass


class Frontend(object):
    "Qdb generic Frontend interface"

    def __init__(self, pipe):
        self.i = 1
        self.pipe = pipe
        self.notifies = []
        self.read_lock = threading.RLock()
        self.write_lock = threading.RLock()

    def recv(self):
        self.read_lock.acquire()
        try:
            return self.pipe.recv()
        finally:
            self.read_lock.release()

    def send(self, data):
        self.write_lock.acquire()
        try:
            return self.pipe.send(data)
        finally:
            self.write_lock.release()

    def startup(self):
        self.send({'method': 'run', 'args': (), 'id': None})

    def interaction(self, filename, lineno, line, *kwargs):
        raise NotImplementedError

    def exception(self, title, extype, exvalue, trace, request):
        "Show a user_exception"
        raise NotImplementedError

    def write(self, text):
        "Console output (print)"
        raise NotImplementedError

    def readline(self, text):
        "Console input/rawinput"
        raise NotImplementedError

    def run(self):
        "Main method dispatcher (infinite loop)"
        if self.pipe:
            if not self.notifies:
                # wait for a message...
                request = self.recv()
            else:
                # process an asyncronus notification received earlier
                request = self.notifies.pop(0)
            return self.process_message(request)

    def process_message(self, request):
        if request:
            result = None
            if request.get("error"):
                # it is not supposed to get an error here
                # it should be raised by the method call
                raise RPCError(res['error']['message'])
            elif request.get('method') == 'interaction':
                self.interaction(*request.get("args"), **request.get("kwargs"))
            elif request.get('method') == 'startup':
                self.startup()
            elif request.get('method') == 'exception':
                self.exception(*request['args'])
            elif request.get('method') == 'write':
                self.write(*request.get("args"))
            elif request.get('method') == 'readline':
                result = self.readline()
            if result:
                response = {'version': '1.1', 'id': request.get('id'),
                            'result': result,
                            'error': None}
                self.send(response)
            return True

    def call(self, method, *args):
        "Actually call the remote method (inside the thread)"
        req = {'method': method, 'args': args, 'id': self.i}
        self.send(req)
        self.i += 1  # increment the id
        while 1:
            # wait until command acknowledge (response id match the request)
            res = self.recv()
            if 'id' not in res or not res['id']:
                # nested notification received (i.e. write)! process it!
                self.process_message(res)
            elif 'result' not in res:
                # nested request received (i.e. readline)! process it!
                self.process_message(res)
            elif long(req['id']) != long(res['id']):
                print "DEBUGGER wrong packet received: expecting id", req[
                    'id'], res['id']
                # protocol state is unknown
            elif 'error' in res and res['error']:
                raise RPCError(res['error']['message'])
            else:
                return res['result']

    def do_step(self, arg=None):
        "Execute the current line, stop at the first possible occasion"
        self.call('do_step')

    def do_next(self, arg=None):
        "Execute the current line, do not stop at function calls"
        self.call('do_next')

    def do_continue(self, arg=None):
        "Continue execution, only stop when a breakpoint is encountered."
        self.call('do_continue')

    def do_return(self, arg=None):
        "Continue execution until the current function returns"
        self.call('do_return')

    def do_jump(self, arg):
        "Set the next line that will be executed."
        res = self.call('do_jump', arg)
        print res

    def do_where(self, arg=None):
        "Print a stack trace, with the most recent frame at the bottom."
        return self.call('do_where')

    def do_quit(self, arg=None):
        "Quit from the debugger. The program being executed is aborted."
        self.call('do_quit')

    def do_eval(self, expr):
        "Inspect the value of the expression"
        return self.call('do_eval', expr)

    def do_environment(self):
        "List all the locals and globals variables (string representation)"
        return self.call('do_environment')

    def do_list(self, arg=None):
        "List source code for the current file"
        return self.call('do_list', arg)

    def do_read(self, filename):
        "Read and send a local filename"
        return self.call('do_read', filename)

    def do_set_breakpoint(self, filename, lineno, temporary=0, cond=None):
        "Set a breakpoint at filename:breakpoint"
        self.call('do_set_breakpoint', filename, lineno, temporary, cond)

    def do_clear_breakpoint(self, filename, lineno):
        "Remove a breakpoint at filename:breakpoint"
        self.call('do_clear_breakpoint', filename, lineno)

    def do_clear_file_breakpoints(self, filename):
        "Remove all breakpoints at filename"
        self.call('do_clear_breakpoints', filename, lineno)

    def do_list_breakpoint(self):
        "List all breakpoints"
        return self.call('do_list_breakpoint')

    def do_exec(self, statement):
        return self.call('do_exec', statement)

    def get_autocomplete_list(self, expression):
        return self.call('get_autocomplete_list', expression)

    def get_call_tip(self, expression):
        return self.call('get_call_tip', expression)

    def interrupt(self):
        "Immediately stop at the first possible occasion (outside interaction)"
        # this is a notification!, do not expect a response
        req = {'method': 'interrupt', 'args': ()}
        self.send(req)

    def set_burst(self, value):
        req = {'method': 'set_burst', 'args': (value, )}
        self.send(req)

    def set_params(self, params):
        req = {'method': 'set_params', 'args': (params, )}
        self.send(req)


class Cli(Frontend, cmd.Cmd):
    "Qdb Front-end command line interface"

    def __init__(self, pipe, completekey='tab', stdin=None, stdout=None, skip=None):
        cmd.Cmd.__init__(self, completekey, stdin, stdout)
        Frontend.__init__(self, pipe)

    # redefine Frontend methods:

    def run(self):
        while 1:
            try:
                Frontend.run(self)
            except KeyboardInterrupt:
                print "Interupting..."
                self.interrupt()

    def interaction(self, filename, lineno, line):
        print "> %s(%d)\n-> %s" % (filename, lineno, line),
        self.filename = filename
        self.cmdloop()

    def exception(self, title, extype, exvalue, trace, request):
        print "=" * 80
        print "Exception", title
        print request
        print "-" * 80

    def write(self, text):
        print text,

    def readline(self):
        return raw_input()

    def postcmd(self, stop, line):
        return not line.startswith("h")  # stop

    do_h = cmd.Cmd.do_help

    do_s = Frontend.do_step
    do_n = Frontend.do_next
    do_c = Frontend.do_continue
    do_r = Frontend.do_return
    do_j = Frontend.do_jump
    do_q = Frontend.do_quit

    def do_eval(self, args):
        "Inspect the value of the expression"
        print Frontend.do_eval(self, args)

    def do_list(self, args):
        "List source code for the current file"
        lines = Frontend.do_list(self, eval(args, {}, {}) if args else None)
        self.print_lines(lines)

    def do_where(self, args):
        "Print a stack trace, with the most recent frame at the bottom."
        lines = Frontend.do_where(self)
        self.print_lines(lines)

    def do_environment(self, args=None):
        env = Frontend.do_environment(self)
        for key in env:
            print "=" * 78
            print key.capitalize()
            print "-" * 78
            for name, value in env[key].items():
                print "%-12s = %s" % (name, value)

    def do_list_breakpoint(self, arg=None):
        "List all breakpoints"
        breaks = Frontend.do_list_breakpoint(self)
        print "Num File                          Line Temp Enab Hits Cond"
        for bp in breaks:
            print '%-4d%-30s%4d %4s %4s %4d %s' % bp
        print

    def do_set_breakpoint(self, arg):
        "Set a breakpoint at filename:breakpoint"
        if arg:
            if ':' in arg:
                args = arg.split(":")
            else:
                args = (self.filename, arg)
            Frontend.do_set_breakpoint(self, *args)
        else:
            self.do_list_breakpoint()

    do_b = do_set_breakpoint
    do_l = do_list
    do_p = do_eval
    do_w = do_where
    do_e = do_environment

    def default(self, line):
        "Default command"
        if line[:1] == '!':
            print self.do_exec(line[1:])
        else:
            print "*** Unknown command: ", line

    def print_lines(self, lines):
        for filename, lineno, bp, current, source in lines:
            print "%s:%4d%s%s\t%s" % (filename, lineno, bp, current, source),
        print


def test():
    def f(pipe):
        print "creating debugger"
        qdb = Qdb(pipe=pipe, redirect_stdio=False)
        print "set trace"

        my_var = "Mariano!"
        qdb.set_trace()
        print "hello world!"
        print "good by!"
        saraza

    if '--process' in sys.argv:
        from multiprocessing import Process, Pipe
        pipe, child_conn = Pipe()
        p = Process(target=f, args=(child_conn,))
    else:
        from threading import Thread
        from Queue import Queue
        parent_queue, child_queue = Queue(), Queue()
        front_conn = QueuePipe("parent", parent_queue, child_queue)
        child_conn = QueuePipe("child", child_queue, parent_queue)
        p = Thread(target=f, args=(child_conn,))

    p.start()
    import time

    class Test(Frontend):
        def interaction(self, *args):
            print "interaction!", args

        def exception(self, *args):
            print "exception", args
            #raise RuntimeError("exception %s" % repr(args))

    qdb = Test(front_conn)
    time.sleep(5)

    while 1:
        print "running..."
        Frontend.run(qdb)
        time.sleep(1)
        print "do_next"
        qdb.do_next()
    p.join()


def connect(host="localhost", port=6000, authkey='secret password'):
    "Connect to a running debugger backend"

    address = (host, port)
    from multiprocessing.connection import Client

    print "qdb debugger fronted: waiting for connection to", address
    conn = Client(address, authkey=authkey)
    try:
        Cli(conn).run()
    except EOFError:
        pass
    finally:
        conn.close()


def main(host='localhost', port=6000, authkey='secret password'):
    "Debug a script and accept a remote frontend"

    if not sys.argv[1:] or sys.argv[1] in ("--help", "-h"):
        print "usage: pdb.py scriptfile [arg] ..."
        sys.exit(2)

    mainpyfile = sys.argv[1]     # Get script filename
    if not os.path.exists(mainpyfile):
        print 'Error:', mainpyfile, 'does not exist'
        sys.exit(1)

    del sys.argv[0]         # Hide "pdb.py" from argument list

    # Replace pdb's dir with script's dir in front of module search path.
    sys.path[0] = os.path.dirname(mainpyfile)

    from multiprocessing.connection import Listener
    address = (host, port)     # family is deduced to be 'AF_INET'
    listener = Listener(address, authkey=authkey)
    print "qdb debugger backend: waiting for connection at", address
    conn = listener.accept()
    print 'qdb debugger backend: connected to', listener.last_accepted

    # create the backend
    qdb = Qdb(conn, redirect_stdio=True, allow_interruptions=True)
    try:
        print "running", mainpyfile
        qdb._runscript(mainpyfile)
        print "The program finished"
    except SystemExit:
        # In most cases SystemExit does not warrant a post-mortem session.
        print "The program exited via sys.exit(). Exit status: ",
        print sys.exc_info()[1]
        raise
    except:
        raise

    conn.close()
    listener.close()


qdb = None


def set_trace(host='localhost', port=6000, authkey='secret password'):
    "Simplified interface to debug running programs"
    global qdb, listener, conn

    from multiprocessing.connection import Listener
    # only create it if not currently instantiated
    if not qdb:
        address = (host, port)     # family is deduced to be 'AF_INET'
        listener = Listener(address, authkey=authkey)
        conn = listener.accept()

        # create the backend
        qdb = Qdb(conn)
    # start debugger backend:
    qdb.set_trace()


def quit():
    "Remove trace and quit"
    global qdb, listener, conn
    if qdb:
        sys.settrace(None)
        qdb = None
    if conn:
        conn.close()
        conn = None
    if listener:
        listener.close()
        listener = None

if __name__ == '__main__':
    # When invoked as main program:
    if '--test' in sys.argv:
        test()
    # Check environment for configuration parameters:
    kwargs = {}
    for param in 'host', 'port', 'authkey':
        if 'QDB_%s' % param.upper() in os.environ:
            kwargs[param] = os.environ['QDB_%s' % param.upper()]

    if not sys.argv[1:]:
        # connect to a remote debbuger
        connect(**kwargs)
    else:
        # start the debugger on a script
        # reimport as global __main__ namespace is destroyed
        import qdb
        qdb.main(**kwargs)
