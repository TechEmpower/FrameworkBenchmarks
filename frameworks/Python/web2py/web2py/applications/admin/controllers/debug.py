import os
import sys
import cStringIO
import gluon.contrib.shell
import gluon.dal
import gluon.html
import gluon.validators
import code
import thread
from gluon.debug import communicate, web_debugger, qdb_debugger
import pydoc


if DEMO_MODE or MULTI_USER_MODE:
    session.flash = T('disabled in demo mode')
    redirect(URL('default', 'site'))

FE = 10 ** 9


def index():
    app = request.args(0) or 'admin'
    reset()
    # read buffer
    data = communicate()
    return dict(app=app, data=data)


def callback():
    app = request.args[0]
    command = request.vars.statement
    session['debug_commands:' + app].append(command)
    output = communicate(command)
    k = len(session['debug_commands:' + app]) - 1
    return '[%i] %s%s\n' % (k + 1, command, output)


def reset():
    app = request.args(0) or 'admin'
    session['debug_commands:' + app] = []
    return 'done'


# new implementation using qdb

def interact():
    app = request.args(0) or 'admin'
    reset()

    # process all pending messages in the frontend
    web_debugger.run()

    # if debugging, filename and lineno should have valid values
    filename = web_debugger.filename
    lineno = web_debugger.lineno
    if filename:
        # prevent IOError 2 on some circuntances (EAFP instead of os.access)
        try:
            lines = open(filename).readlines()
        except:
            lines = ""
        lines = dict([(i + 1, l) for (i, l) in enumerate(
            [l.strip("\n").strip("\r") for l in lines])])
        filename = os.path.basename(filename)
    else:
        lines = {}

    if filename:
        web_debugger.set_burst(2)
        env = web_debugger.do_environment()
        f_locals = env['locals']
        f_globals = {}
        for name, value in env['globals'].items():
            if name not in gluon.html.__all__ and \
                name not in gluon.validators.__all__:
                f_globals[name] = pydoc.text.repr(value)
    else:
        f_locals = {}
        f_globals = {}
        response.headers['refresh'] = "3"

    if web_debugger.exception_info:
        response.flash = T('"User Exception" debug mode. '
                           'An error ticket could be issued!')

    return dict(app=app, data="",
                filename=web_debugger.filename, lines=lines, lineno=lineno,
                f_globals=f_globals, f_locals=f_locals,
                exception=web_debugger.exception_info)


def step():
    web_debugger.do_step()
    redirect(URL("interact"))


def next():
    web_debugger.do_next()
    redirect(URL("interact"))


def cont():
    web_debugger.do_continue()
    redirect(URL("interact"))


def ret():
    web_debugger.do_return()
    redirect(URL("interact"))


def stop():
    web_debugger.do_quit()
    redirect(URL("interact"))


def execute():
    app = request.args[0]
    command = request.vars.statement
    session['debug_commands:' + app].append(command)
    try:
        output = web_debugger.do_exec(command)
        if output is None:
            output = ""
    except Exception, e:
        output = T("Exception %s") % str(e)
    k = len(session['debug_commands:' + app]) - 1
    return '[%i] %s%s\n' % (k + 1, command, output)


def breakpoints():
    "Add or remove breakpoints"

    # Get all .py files
    files = listdir(apath('', r=request), '.*\.py$')
    files = [filename for filename in files
             if filename and 'languages' not in filename
             and not filename.startswith("admin")
             and not filename.startswith("examples")]

    form = SQLFORM.factory(
        Field('filename', requires=IS_IN_SET(files), label=T("Filename")),
        Field('lineno', 'integer', label=T("Line number"),
              requires=IS_NOT_EMPTY()),
        Field('temporary', 'boolean', label=T("Temporary"),
              comment=T("deleted after first hit")),
        Field('condition', 'string', label=T("Condition"),
              comment=T("honored only if the expression evaluates to true")),
    )

    if form.accepts(request.vars, session):
        filename = os.path.join(request.env['applications_parent'],
                                'applications', form.vars.filename)
        err = qdb_debugger.do_set_breakpoint(filename,
                                             form.vars.lineno,
                                             form.vars.temporary,
                                             form.vars.condition)
        response.flash = T("Set Breakpoint on %s at line %s: %s") % (
            filename, form.vars.lineno, err or T('successful'))

    for item in request.vars:
        if item[:7] == 'delete_':
            qdb_debugger.do_clear(item[7:])

    breakpoints = [{'number': bp[0], 'filename': os.path.basename(bp[1]),
                    'path': bp[1], 'lineno': bp[2],
                    'temporary': bp[3], 'enabled': bp[4], 'hits': bp[5],
                    'condition': bp[6]}
                   for bp in qdb_debugger.do_list_breakpoint()]

    return dict(breakpoints=breakpoints, form=form)


def toggle_breakpoint():
    "Set or clear a breakpoint"

    lineno = None
    ok = None
    try:
        filename = os.path.join(request.env['applications_parent'],
                                'applications', request.vars.filename)
        # normalize path name: replace slashes, references, etc...
        filename = os.path.normpath(os.path.normcase(filename))
        if not request.vars.data:
            # ace send us the line number!
            lineno = int(request.vars.sel_start) + 1
        else:
            # editarea send us the offset, manually check the cursor pos
            start = 0
            sel_start = int(request.vars.sel_start)
            for lineno, line in enumerate(request.vars.data.split("\n")):
                if sel_start <= start:
                    break
                start += len(line) + 1
            else:
                lineno = None
        if lineno is not None:
            for bp in qdb_debugger.do_list_breakpoint():
                no, bp_filename, bp_lineno, temporary, enabled, hits, cond = bp
                # normalize path name: replace slashes, references, etc...
                bp_filename = os.path.normpath(os.path.normcase(bp_filename))
                if filename == bp_filename and lineno == bp_lineno:
                    err = qdb_debugger.do_clear_breakpoint(filename, lineno)
                    response.flash = T("Removed Breakpoint on %s at line %s", (
                        filename, lineno))
                    ok = False
                    break
            else:
                err = qdb_debugger.do_set_breakpoint(filename, lineno)
                response.flash = T("Set Breakpoint on %s at line %s: %s") % (
                    filename, lineno, err or T('successful'))
                ok = True
        else:
            response.flash = T("Unable to determine the line number!")
    except Exception, e:
        session.flash = str(e)
    return response.json({'ok': ok, 'lineno': lineno})

def list_breakpoints():
    "Return a list of linenumbers for current breakpoints"

    breakpoints = []
    ok = None
    try:
        filename = os.path.join(request.env['applications_parent'],
                                'applications', request.vars.filename)
        # normalize path name: replace slashes, references, etc...
        filename = os.path.normpath(os.path.normcase(filename))
        for bp in qdb_debugger.do_list_breakpoint():
            no, bp_filename, bp_lineno, temporary, enabled, hits, cond = bp
            # normalize path name: replace slashes, references, etc...
            bp_filename = os.path.normpath(os.path.normcase(bp_filename))
            if filename == bp_filename:
                breakpoints.append(bp_lineno)
        ok = True
    except Exception, e:
        session.flash = str(e)
        ok = False
    return response.json({'ok': ok, 'breakpoints': breakpoints})
