# -*- coding: utf-8 -*-

EXPERIMENTAL_STUFF = True
MAXNFILES = 1000

if EXPERIMENTAL_STUFF:
    if is_mobile:
        response.view = response.view.replace('default/', 'default.mobile/')
        response.menu = []

import re
from gluon.admin import *
from gluon.fileutils import abspath, read_file, write_file
from gluon.utils import web2py_uuid
from gluon.tools import Config
from gluon.compileapp import find_exposed_functions
from glob import glob
import shutil
import platform

try:
    import git
    if git.__version__ < '0.3.1':
        raise ImportError("Your version of git is %s. Upgrade to 0.3.1 or better." % git.__version__)
    have_git = True
except ImportError, e:
    have_git = False
    GIT_MISSING = 'Requires gitpython module, but not installed or incompatible version: %s' % e

from gluon.languages import (read_possible_languages, read_dict, write_dict,
                             read_plural_dict, write_plural_dict)


if DEMO_MODE and request.function in ['change_password', 'pack',
'pack_custom','pack_plugin', 'upgrade_web2py', 'uninstall',
'cleanup', 'compile_app', 'remove_compiled_app', 'delete',
'delete_plugin', 'create_file', 'upload_file', 'update_languages',
'reload_routes', 'git_push', 'git_pull', 'install_plugin']:
    session.flash = T('disabled in demo mode')
    redirect(URL('site'))

if is_gae and request.function in ('edit', 'edit_language',
'edit_plurals', 'update_languages', 'create_file', 'install_plugin'):
    session.flash = T('disabled in GAE mode')
    redirect(URL('site'))

if not is_manager() and request.function in ['change_password', 'upgrade_web2py']:
    session.flash = T('disabled in multi user mode')
    redirect(URL('site'))

if FILTER_APPS and request.args(0) and not request.args(0) in FILTER_APPS:
    session.flash = T('disabled in demo mode')
    redirect(URL('site'))


if not session.token:
    session.token = web2py_uuid()


def count_lines(data):
    return len([line for line in data.split('\n') if line.strip() and not line.startswith('#')])


def log_progress(app, mode='EDIT', filename=None, progress=0):
    progress_file = os.path.join(apath(app, r=request), 'progress.log')
    now = str(request.now)[:19]
    if not os.path.exists(progress_file):
        safe_open(progress_file, 'w').write('[%s] START\n' % now)
    if filename:
        safe_open(progress_file, 'a').write(
            '[%s] %s %s: %s\n' % (now, mode, filename, progress))


def safe_open(a, b):
    if (DEMO_MODE or is_gae) and ('w' in b or 'a' in b):
        class tmp:
            def write(self, data):
                pass
            def close(self):
                pass
        return tmp()
    return open(a, b)


def safe_read(a, b='r'):
    safe_file = safe_open(a, b)
    try:
        return safe_file.read()
    finally:
        safe_file.close()


def safe_write(a, value, b='w'):
    safe_file = safe_open(a, b)
    try:
        safe_file.write(value)
    finally:
        safe_file.close()


def get_app(name=None):
    app = name or request.args(0)
    if (app and os.path.exists(apath(app, r=request)) and
        (not MULTI_USER_MODE or is_manager() or
         db(db.app.name == app)(db.app.owner == auth.user.id).count())):
        return app
    session.flash = T('App does not exist or you are not authorized')
    redirect(URL('site'))


def index():
    """ Index handler """

    send = request.vars.send
    if DEMO_MODE:
        session.authorized = True
        session.last_time = t0
    if not send:
        send = URL('site')
    if session.authorized:
        redirect(send)
    elif request.vars.password:
        if verify_password(request.vars.password[:1024]):
            session.authorized = True
            login_record(True)

            if CHECK_VERSION:
                session.check_version = True
            else:
                session.check_version = False

            session.last_time = t0
            if isinstance(send, list):  # ## why does this happen?
                send = str(send[0])

            redirect(send)
        else:
            times_denied = login_record(False)
            if times_denied >= allowed_number_of_attempts:
                response.flash = \
                    T('admin disabled because too many invalid login attempts')
            elif times_denied == allowed_number_of_attempts - 1:
                response.flash = \
                    T('You have one more login attempt before you are locked out')
            else:
                response.flash = T('invalid password.')
    return dict(send=send)


def check_version():
    """ Checks if web2py is up to date """

    session.forget()
    session._unlock(response)

    new_version, version = check_new_version(request.env.web2py_version,
                                             WEB2PY_VERSION_URL)

    if new_version == -1:
        return A(T('Unable to check for upgrades'), _href=WEB2PY_URL)
    elif new_version != True:
        return A(T('web2py is up to date'), _href=WEB2PY_URL)
    elif platform.system().lower() in ('windows', 'win32', 'win64') and os.path.exists("web2py.exe"):
        return SPAN('You should upgrade to %s' % version.split('(')[0])
    else:
        return sp_button(URL('upgrade_web2py'), T('upgrade now to %s') % version.split('(')[0])


def logout():
    """ Logout handler """
    session.authorized = None
    if MULTI_USER_MODE:
        redirect(URL('user/logout'))
    redirect(URL('index'))


def change_password():

    if session.pam_user:
        session.flash = T(
            'PAM authenticated user, cannot change password here')
        redirect(URL('site'))
    form = SQLFORM.factory(Field('current_admin_password', 'password'),
                           Field('new_admin_password',
                                 'password', requires=IS_STRONG()),
                           Field('new_admin_password_again', 'password'),
                           _class="span4 well")
    if form.accepts(request.vars):
        if not verify_password(request.vars.current_admin_password):
            form.errors.current_admin_password = T('invalid password')
        elif form.vars.new_admin_password != form.vars.new_admin_password_again:
            form.errors.new_admin_password_again = T('no match')
        else:
            path = abspath('parameters_%s.py' % request.env.server_port)
            safe_write(path, 'password="%s"' % CRYPT()(
                request.vars.new_admin_password)[0])
            session.flash = T('password changed')
            redirect(URL('site'))
    return dict(form=form)


def site():
    """ Site handler """

    myversion = request.env.web2py_version

    # Shortcut to make the elif statements more legible
    file_or_appurl = 'file' in request.vars or 'appurl' in request.vars

    class IS_VALID_APPNAME(object):
        def __call__(self, value):
            if not re.compile('^\w+$').match(value):
                return (value, T('Invalid application name'))
            if not request.vars.overwrite and \
                    os.path.exists(os.path.join(apath(r=request), value)):
                return (value, T('Application exists already'))
            return (value, None)

    is_appname = IS_VALID_APPNAME()
    form_create = SQLFORM.factory(Field('name', requires=is_appname),
                                  table_name='appcreate')
    form_update = SQLFORM.factory(Field('name', requires=is_appname),
                                  Field('file', 'upload', uploadfield=False),
                                  Field('url'),
                                  Field('overwrite', 'boolean'),
                                  table_name='appupdate')
    form_create.process()
    form_update.process()

    if DEMO_MODE:
        pass

    elif form_create.accepted:
        # create a new application
        appname = cleanpath(form_create.vars.name)
        created, error = app_create(appname, request, info=True)
        if created:
            if MULTI_USER_MODE:
                db.app.insert(name=appname, owner=auth.user.id)
            log_progress(appname)
            session.flash = T('new application "%s" created', appname)
            redirect(URL('design', args=appname))
        else:
            session.flash = \
                DIV(T('unable to create application "%s"', appname),
                    PRE(error))
        redirect(URL(r=request))

    elif form_update.accepted:
        if (form_update.vars.url or '').endswith('.git'):
            if not have_git:
                session.flash = GIT_MISSING
                redirect(URL(r=request))
            target = os.path.join(apath(r=request), form_update.vars.name)
            try:
                new_repo = git.Repo.clone_from(form_update.vars.url, target)
                session.flash = T('new application "%s" imported',
                                  form_update.vars.name)
            except git.GitCommandError, err:
                session.flash = T('Invalid git repository specified.')
            redirect(URL(r=request))

        elif form_update.vars.url:
            # fetch an application via URL or file upload
            try:
                f = urllib.urlopen(form_update.vars.url)
                if f.code == 404:
                    raise Exception("404 file not found")
            except Exception, e:
                session.flash = \
                    DIV(T('Unable to download app because:'), PRE(str(e)))
                redirect(URL(r=request))
            fname = form_update.vars.url

        elif form_update.accepted and form_update.vars.file:
            fname = request.vars.file.filename
            f = request.vars.file.file

        else:
            session.flash = 'No file uploaded and no URL specified'
            redirect(URL(r=request))

        if f:
            appname = cleanpath(form_update.vars.name)
            installed = app_install(appname, f,
                                    request, fname,
                                    overwrite=form_update.vars.overwrite)
        if f and installed:
            msg = 'application %(appname)s installed with md5sum: %(digest)s'
            if MULTI_USER_MODE:
                db.app.insert(name=appname, owner=auth.user.id)
            log_progress(appname)
            session.flash = T(msg, dict(appname=appname,
                                        digest=md5_hash(installed)))
        elif f and form_update.vars.overwrite:
            msg = 'unable to install application "%(appname)s"'
            session.flash = T(msg, dict(appname=form_update.vars.name))
        else:
            msg = 'unable to install application "%(appname)s"'
            session.flash = T(msg, dict(appname=form_update.vars.name))
        redirect(URL(r=request))

    regex = re.compile('^\w+$')

    if is_manager():
        apps = [f for f in os.listdir(apath(r=request)) if regex.match(f)]
    else:
        apps = [f.name for f in db(db.app.owner == auth.user_id).select()]

    if FILTER_APPS:
        apps = [f for f in apps if f in FILTER_APPS]

    apps = sorted(apps, lambda a, b: cmp(a.upper(), b.upper()))
    myplatform = platform.python_version()
    return dict(app=None, apps=apps, myversion=myversion, myplatform=myplatform,
                form_create=form_create, form_update=form_update)


def report_progress(app):
    import datetime
    progress_file = os.path.join(apath(app, r=request), 'progress.log')
    regex = re.compile('\[(.*?)\][^\:]+\:\s+(\-?\d+)')
    if not os.path.exists(progress_file):
        return []
    matches = regex.findall(open(progress_file, 'r').read())
    events, counter = [], 0
    for m in matches:
        if not m:
            continue
        days = -(request.now - datetime.datetime.strptime(m[0],
                 '%Y-%m-%d %H:%M:%S')).days
        counter += int(m[1])
        events.append([days, counter])
    return events


def pack():
    app = get_app()

    try:
        if len(request.args) == 1:
            fname = 'web2py.app.%s.w2p' % app
            filename = app_pack(app, request, raise_ex=True)
        else:
            fname = 'web2py.app.%s.compiled.w2p' % app
            filename = app_pack_compiled(app, request, raise_ex=True)
    except Exception, e:
        filename = None

    if filename:
        response.headers['Content-Type'] = 'application/w2p'
        disposition = 'attachment; filename=%s' % fname
        response.headers['Content-Disposition'] = disposition
        return safe_read(filename, 'rb')
    else:
        session.flash = T('internal error: %s', e)
        redirect(URL('site'))

def pack_plugin():
    app = get_app()
    if len(request.args) == 2:
        fname = 'web2py.plugin.%s.w2p' % request.args[1]
        filename = plugin_pack(app, request.args[1], request)
    if filename:
        response.headers['Content-Type'] = 'application/w2p'
        disposition = 'attachment; filename=%s' % fname
        response.headers['Content-Disposition'] = disposition
        return safe_read(filename, 'rb')
    else:
        session.flash = T('internal error')
        redirect(URL('plugin', args=request.args))

def pack_custom():
    app = get_app()
    base = apath(app, r=request)
    if request.post_vars.file:
        files = request.post_vars.file
        files = [files] if not isinstance(files,list) else files
        fname = 'web2py.app.%s.w2p' % app
        try:
            filename = app_pack(app, request, raise_ex=True, filenames=files)
        except Exception, e:
            filename = None
        if filename:
            response.headers['Content-Type'] = 'application/w2p'
            disposition = 'attachment; filename=%s' % fname
            response.headers['Content-Disposition'] = disposition
            return safe_read(filename, 'rb')
        else:
            session.flash = T('internal error: %s', e)
            redirect(URL(args=request.args))
    def ignore(fs):
        return [f for f in fs if not (
                f[:1] in '#' or f.endswith('~') or f.endswith('.bak'))]
    files = {}
    for (r,d,f) in os.walk(base):
        files[r] = {'folders':ignore(d),'files':ignore(f)}
    return locals()


def upgrade_web2py():
    dialog = FORM.confirm(T('Upgrade'),
                          {T('Cancel'): URL('site')})
    if dialog.accepted:
        (success, error) = upgrade(request)
        if success:
            session.flash = T('web2py upgraded; please restart it')
        else:
            session.flash = T('unable to upgrade because "%s"', error)
        redirect(URL('site'))
    return dict(dialog=dialog)


def uninstall():
    app = get_app()

    dialog = FORM.confirm(T('Uninstall'),
                          {T('Cancel'): URL('site')})
    dialog['_id'] = 'confirm_form'
    dialog['_class'] = 'well'
    for component in dialog.components:
        component['_class'] = 'btn'

    if dialog.accepted:
        if MULTI_USER_MODE:
            if is_manager() and db(db.app.name == app).delete():
                pass
            elif db(db.app.name == app)(db.app.owner == auth.user.id).delete():
                pass
            else:
                session.flash = T('no permission to uninstall "%s"', app)
                redirect(URL('site'))
        try:
            filename = app_pack(app, request, raise_ex=True)
        except:
            session.flash = T('unable to uninstall "%s"', app)
        else:
            if app_uninstall(app, request):
                session.flash = T('application "%s" uninstalled', app)
            else:
                session.flash = T('unable to uninstall "%s"', app)
        redirect(URL('site'))
    return dict(app=app, dialog=dialog)


def cleanup():
    app = get_app()
    clean = app_cleanup(app, request)
    if not clean:
        session.flash = T("some files could not be removed")
    else:
        session.flash = T('cache, errors and sessions cleaned')

    redirect(URL('site'))


def compile_app():
    app = get_app()
    c = app_compile(app, request)
    if not c:
        session.flash = T('application compiled')
    else:
        session.flash = DIV(T('Cannot compile: there are errors in your app:'),
                            CODE(c))
    redirect(URL('site'))


def remove_compiled_app():
    """ Remove the compiled application """
    app = get_app()
    remove_compiled_application(apath(app, r=request))
    session.flash = T('compiled application removed')
    redirect(URL('site'))


def delete():
    """ Object delete handler """
    app = get_app()
    filename = '/'.join(request.args)
    sender = request.vars.sender

    if isinstance(sender, list):  # ## fix a problem with Vista
        sender = sender[0]

    dialog = FORM.confirm(T('Delete'),
                          {T('Cancel'): URL(sender, anchor=request.vars.id)})

    if dialog.accepted:
        try:
            full_path = apath(filename, r=request)
            lineno = count_lines(open(full_path, 'r').read())
            os.unlink(full_path)
            log_progress(app, 'DELETE', filename, progress=-lineno)
            session.flash = T('file "%(filename)s" deleted',
                              dict(filename=filename))
        except Exception:
            session.flash = T('unable to delete file "%(filename)s"',
                              dict(filename=filename))
        redirect(URL(sender, anchor=request.vars.id2))
    return dict(dialog=dialog, filename=filename)


def enable():
    app = get_app()
    filename = os.path.join(apath(app, r=request), 'DISABLED')
    if is_gae:
        return SPAN(T('Not supported'), _style='color:yellow')
    elif os.path.exists(filename):
        os.unlink(filename)
        return SPAN(T('Disable'), _style='color:green')
    else:
        safe_open(filename, 'wb').write('disabled: True\ntime-disabled: %s' % request.now)
        return SPAN(T('Enable'), _style='color:red')

def peek():
    """ Visualize object code """
    app = get_app(request.vars.app)
    filename = '/'.join(request.args)
    if request.vars.app:
        path = abspath(filename)
    else:
        path = apath(filename, r=request)
    try:
        data = safe_read(path).replace('\r', '')
    except IOError:
        session.flash = T('file does not exist')
        redirect(URL('site'))

    extension = filename[filename.rfind('.') + 1:].lower()

    return dict(app=app,
                filename=filename,
                data=data,
                extension=extension)


def test():
    """ Execute controller tests """
    app = get_app()
    if len(request.args) > 1:
        file = request.args[1]
    else:
        file = '.*\.py'

    controllers = listdir(
        apath('%s/controllers/' % app, r=request), file + '$')

    return dict(app=app, controllers=controllers)


def keepalive():
    return ''


def search():
    keywords = request.vars.keywords or ''
    app = get_app()

    def match(filename, keywords):
        filename = os.path.join(apath(app, r=request), filename)
        if keywords in read_file(filename, 'rb'):
            return True
        return False
    path = apath(request.args[0], r=request)
    files1 = glob(os.path.join(path, '*/*.py'))
    files2 = glob(os.path.join(path, '*/*.html'))
    files3 = glob(os.path.join(path, '*/*/*.html'))
    files = [x[len(path) + 1:].replace(
        '\\', '/') for x in files1 + files2 + files3 if match(x, keywords)]
    return response.json(dict(files=files, message=T.M('Searching: **%s** %%{file}', len(files))))


def edit():
    """ File edit handler """
    # Load json only if it is ajax edited...
    app = get_app(request.vars.app)
    app_path = apath(app, r=request)
    preferences={'theme':'web2py', 'editor': 'default', 'closetag': 'true', 'codefolding': 'false', 'tabwidth':'4', 'indentwithtabs':'false', 'linenumbers':'true', 'highlightline':'true'}
    config = Config(os.path.join(request.folder, 'settings.cfg'),
                    section='editor', default_values={})
    preferences.update(config.read())

    if not(request.ajax) and not(is_mobile):
        # return the scaffolding, the rest will be through ajax requests
        response.title = T('Editing %s') % app
        return response.render ('default/edit.html', dict(app=app, editor_settings=preferences))

    # show settings tab and save prefernces
    if 'settings' in request.vars:
        if request.post_vars:        #save new preferences
            post_vars = request.post_vars.items()
            # Since unchecked checkbox are not serialized, we must set them as false by hand to store the correct preference in the settings
            post_vars+= [(opt, 'false') for opt in preferences if opt not in request.post_vars ]
            if config.save(post_vars):
                response.headers["web2py-component-flash"] = T('Preferences saved correctly')
            else:
                response.headers["web2py-component-flash"] = T('Preferences saved on session only')
            response.headers["web2py-component-command"] = "update_editor(%s);$('a[href=#editor_settings] button.close').click();" % response.json(config.read())
            return
        else:
            details = {'realfilename':'settings', 'filename':'settings', 'id':'editor_settings', 'force': False}
            details['plain_html'] = response.render('default/editor_settings.html', {'editor_settings':preferences})
            return response.json(details)

    """ File edit handler """
    # Load json only if it is ajax edited...
    app = get_app(request.vars.app)
    filename = '/'.join(request.args)
    realfilename = request.args[-1]
    if request.vars.app:
        path = abspath(filename)
    else:
        path = apath(filename, r=request)
    # Try to discover the file type
    if filename[-3:] == '.py':
        filetype = 'python'
    elif filename[-5:] == '.html':
        filetype = 'html'
    elif filename[-5:] == '.load':
        filetype = 'html'
    elif filename[-4:] == '.css':
        filetype = 'css'
    elif filename[-3:] == '.js':
        filetype = 'javascript'
    else:
        filetype = 'html'

    # ## check if file is not there
    if ('revert' in request.vars) and os.path.exists(path + '.bak'):
        try:
            data = safe_read(path + '.bak')
            data1 = safe_read(path)
        except IOError:
            session.flash = T('Invalid action')
            if 'from_ajax' in request.vars:
                return response.json({'error': str(T('Invalid action'))})
            else:
                redirect(URL('site'))

        safe_write(path, data)
        file_hash = md5_hash(data)
        saved_on = time.ctime(os.stat(path)[stat.ST_MTIME])
        safe_write(path + '.bak', data1)
        response.flash = T('file "%s" of %s restored', (filename, saved_on))
    else:
        try:
            data = safe_read(path)
        except IOError:
            session.flash = T('Invalid action')
            if 'from_ajax' in request.vars:
                return response.json({'error': str(T('Invalid action'))})
            else:
                redirect(URL('site'))

        lineno_old = count_lines(data)
        file_hash = md5_hash(data)
        saved_on = time.ctime(os.stat(path)[stat.ST_MTIME])

        if request.vars.file_hash and request.vars.file_hash != file_hash:
            session.flash = T('file changed on disk')
            data = request.vars.data.replace('\r\n', '\n').strip() + '\n'
            safe_write(path + '.1', data)
            if 'from_ajax' in request.vars:
                return response.json({'error': str(T('file changed on disk')),
                                      'redirect': URL('resolve',
                                                      args=request.args)})
            else:
                redirect(URL('resolve', args=request.args))
        elif request.vars.data:
            safe_write(path + '.bak', data)
            data = request.vars.data.replace('\r\n', '\n').strip() + '\n'
            safe_write(path, data)
            lineno_new = count_lines(data)
            log_progress(
                app, 'EDIT', filename, progress=lineno_new - lineno_old)
            file_hash = md5_hash(data)
            saved_on = time.ctime(os.stat(path)[stat.ST_MTIME])
            response.flash = T('file saved on %s', saved_on)

    data_or_revert = (request.vars.data or request.vars.revert)

    # Check compile errors
    highlight = None
    if filetype == 'python' and request.vars.data:
        import _ast
        try:
            code = request.vars.data.rstrip().replace('\r\n', '\n') + '\n'
            compile(code, path, "exec", _ast.PyCF_ONLY_AST)
        except Exception, e:
            # offset calculation is only used for textarea (start/stop)
            start = sum([len(line) + 1 for l, line
                         in enumerate(request.vars.data.split("\n"))
                         if l < e.lineno - 1])
            if e.text and e.offset:
                offset = e.offset - (len(e.text) - len(
                    e.text.splitlines()[-1]))
            else:
                offset = 0
            highlight = {'start': start, 'end': start +
                         offset + 1, 'lineno': e.lineno, 'offset': offset}
            try:
                ex_name = e.__class__.__name__
            except:
                ex_name = 'unknown exception!'
            response.flash = DIV(T('failed to compile file because:'), BR(),
                                 B(ex_name), ' ' + T('at line %s', e.lineno),
                                 offset and ' ' +
                                 T('at char %s', offset) or '',
                                 PRE(str(e)))
    if data_or_revert and request.args[1] == 'modules':
        # Lets try to reload the modules
        try:
            mopath = '.'.join(request.args[2:])[:-3]
            exec 'import applications.%s.modules.%s' % (
                request.args[0], mopath)
            reload(sys.modules['applications.%s.modules.%s'
                               % (request.args[0], mopath)])
        except Exception, e:
            response.flash = DIV(
                T('failed to reload module because:'), PRE(str(e)))

    edit_controller = None
    editviewlinks = None
    view_link = None
    if filetype == 'html' and len(request.args) >= 3:
        cfilename = os.path.join(request.args[0], 'controllers',
                                 request.args[2] + '.py')
        if os.path.exists(apath(cfilename, r=request)):
            edit_controller = URL('edit', args=[cfilename.replace(os.sep, "/")])
            view = request.args[3].replace('.html', '')
            view_link = URL(request.args[0], request.args[2], view)
    elif filetype == 'python' and request.args[1] == 'controllers':
        ## it's a controller file.
        ## Create links to all of the associated view files.
        app = get_app()
        viewname = os.path.splitext(request.args[2])[0]
        viewpath = os.path.join(app, 'views', viewname)
        aviewpath = apath(viewpath, r=request)
        viewlist = []
        if os.path.exists(aviewpath):
            if os.path.isdir(aviewpath):
                viewlist = glob(os.path.join(aviewpath, '*.html'))
        elif os.path.exists(aviewpath + '.html'):
            viewlist.append(aviewpath + '.html')
        if len(viewlist):
            editviewlinks = []
            for v in viewlist:
                vf = os.path.split(v)[-1]
                vargs = "/".join([viewpath.replace(os.sep, "/"), vf])
                editviewlinks.append(A(vf.split(".")[0],
                                       _class="editor_filelink",
                                       _href=URL('edit', args=[vargs])))

    if len(request.args) > 2 and request.args[1] == 'controllers':
        controller = (request.args[2])[:-3]
        functions = find_exposed_functions(data)
    else:
        (controller, functions) = (None, None)

    if 'from_ajax' in request.vars:
        return response.json({'file_hash': file_hash, 'saved_on': saved_on, 'functions': functions, 'controller': controller, 'application': request.args[0], 'highlight': highlight})
    else:
        file_details = dict(app=request.args[0],
                    lineno=request.vars.lineno or 1,
                    editor_settings=preferences,
                    filename=filename,
                    realfilename=realfilename,
                    filetype=filetype,
                    data=data,
                    edit_controller=edit_controller,
                    file_hash=file_hash,
                    saved_on=saved_on,
                    controller=controller,
                    functions=functions,
                    view_link=view_link,
                    editviewlinks=editviewlinks,
                    id=IS_SLUG()(filename)[0],
                    force= True if (request.vars.restore or
                                    request.vars.revert) else False)
        plain_html = response.render('default/edit_js.html', file_details)
        file_details['plain_html'] = plain_html
        if is_mobile:
            return response.render('default.mobile/edit.html',
                                   file_details, editor_settings=preferences)
        else:
            return response.json(file_details)

def todolist():
    """ Returns all TODO of the requested app
    """
    app = request.vars.app or ''
    app_path = apath('%(app)s' % {'app':app}, r=request)
    dirs=['models', 'controllers', 'modules', 'private' ]
    def listfiles(app, dir, regexp='.*\.py$'):
        files = sorted( listdir(apath('%(app)s/%(dir)s/' % {'app':app, 'dir':dir}, r=request), regexp))
        files = [x.replace(os.path.sep, '/') for x in files if not x.endswith('.bak')]
        return files

    pattern = '#\s*(todo)+\s+(.*)'
    regex = re.compile(pattern, re.IGNORECASE)

    output = []
    for d in dirs:
        for f in listfiles(app, d):
            matches = []
            filename= apath(os.path.join(app, d, f), r=request)
            with open(filename, 'r') as f_s:
                src = f_s.read()
                for m in regex.finditer(src):
                    start = m.start()
                    lineno = src.count('\n', 0, start) + 1
                    matches.append({'text':m.group(0), 'lineno':lineno})
            if len(matches) != 0:
                output.append({'filename':f,'matches':matches, 'dir':d})

    return {'todo':output, 'app': app}

def editor_sessions():
    config = Config(os.path.join(request.folder, 'settings.cfg'),
                    section='editor_sessions', default_values={})
    preferences = config.read()

    if request.vars.session_name and request.vars.files:
        session_name = request.vars.session_name
        files = request.vars.files
        preferences.update({session_name:','.join(files)})
        if config.save(preferences.items()):
            response.headers["web2py-component-flash"] = T('Session saved correctly')
        else:
            response.headers["web2py-component-flash"] = T('Session saved on session only')

    return response.render('default/editor_sessions.html', {'editor_sessions':preferences})

def resolve():
    """
    """

    filename = '/'.join(request.args)
    # ## check if file is not there
    path = apath(filename, r=request)
    a = safe_read(path).split('\n')
    try:
        b = safe_read(path + '.1').split('\n')
    except IOError:
        session.flash = 'Other file, no longer there'
        redirect(URL('edit', args=request.args))

    d = difflib.ndiff(a, b)

    def leading(line):
        """  """

        # TODO: we really need to comment this
        z = ''
        for (k, c) in enumerate(line):
            if c == ' ':
                z += '&nbsp;'
            elif c == ' \t':
                z += '&nbsp;'
            elif k == 0 and c == '?':
                pass
            else:
                break

        return XML(z)

    def getclass(item):
        """ Determine item class """

        if item[0] == ' ':
            return 'normal'
        if item[0] == '+':
            return 'plus'
        if item[0] == '-':
            return 'minus'

    if request.vars:
        c = '\n'.join([item[2:].rstrip() for (i, item) in enumerate(d) if item[0]
                       == ' ' or 'line%i' % i in request.vars])
        safe_write(path, c)
        session.flash = 'files merged'
        redirect(URL('edit', args=request.args))
    else:
        # Making the short circuit compatible with <= python2.4
        gen_data = lambda index, item: not item[:1] in ['+', '-'] and "" \
            or INPUT(_type='checkbox',
                     _name='line%i' % index,
                     value=item[0] == '+')

        diff = TABLE(*[TR(TD(gen_data(i, item)),
                          TD(item[0]),
                          TD(leading(item[2:]),
                          TT(item[2:].rstrip())),
                          _class=getclass(item))
                       for (i, item) in enumerate(d) if item[0] != '?'])

    return dict(diff=diff, filename=filename)


def edit_language():
    """ Edit language file """
    app = get_app()
    filename = '/'.join(request.args)
    response.title = request.args[-1]
    strings = read_dict(apath(filename, r=request))

    if '__corrupted__' in strings:
        form = SPAN(strings['__corrupted__'], _class='error')
        return dict(filename=filename, form=form)

    keys = sorted(strings.keys(), lambda x, y: cmp(
        unicode(x, 'utf-8').lower(), unicode(y, 'utf-8').lower()))
    rows = []
    rows.append(H2(T('Original/Translation')))

    for key in keys:
        name = md5_hash(key)
        s = strings[key]
        (prefix, sep, key) = key.partition('\x01')
        if sep:
            prefix = SPAN(prefix + ': ', _class='tm_ftag')
            k = key
        else:
            (k, prefix) = (prefix, '')

        _class = 'untranslated' if k == s else 'translated'

        if len(s) <= 40:
            elem = INPUT(_type='text', _name=name, value=s,
                         _size=70, _class=_class)
        else:
            elem = TEXTAREA(_name=name, value=s, _cols=70,
                            _rows=5, _class=_class)

        # Making the short circuit compatible with <= python2.4
        k = (s != k) and k or B(k)

        new_row = DIV(LABEL(prefix, k, _style="font-weight:normal;"),
                      CAT(elem, '\n', TAG.BUTTON(
                    T('delete'),
                    _onclick='return delkey("%s")' % name,
                    _class='btn')), _id=name, _class='span6 well well-small')

        rows.append(DIV(new_row,_class="row-fluid"))
    rows.append(DIV(INPUT(_type='submit', _value=T('update'), _class="btn btn-primary"), _class='controls'))
    form = FORM(*rows)
    if form.accepts(request.vars, keepvalues=True):
        strs = dict()
        for key in keys:
            name = md5_hash(key)
            if form.vars[name] == chr(127):
                continue
            strs[key] = form.vars[name]
        write_dict(apath(filename, r=request), strs)
        session.flash = T('file saved on %(time)s', dict(time=time.ctime()))
        redirect(URL(r=request, args=request.args))
    return dict(app=request.args[0], filename=filename, form=form)


def edit_plurals():
    """ Edit plurals file """
    app = get_app()
    filename = '/'.join(request.args)
    plurals = read_plural_dict(
        apath(filename, r=request))  # plural forms dictionary
    nplurals = int(request.vars.nplurals) - 1  # plural forms quantity
    xnplurals = xrange(nplurals)

    if '__corrupted__' in plurals:
        # show error message and exit
        form = SPAN(plurals['__corrupted__'], _class='error')
        return dict(filename=filename, form=form)

    keys = sorted(plurals.keys(), lambda x, y: cmp(
        unicode(x, 'utf-8').lower(), unicode(y, 'utf-8').lower()))
    tab_rows = []
    for key in keys:
        name = md5_hash(key)
        forms = plurals[key]

        if len(forms) < nplurals:
            forms.extend(None for i in xrange(nplurals - len(forms)))
        tab_col1 = DIV(CAT(LABEL(T("Singular Form")), B(key,
                                                        _class='fake-input')))
        tab_inputs = [SPAN(LABEL(T("Plural Form #%s", n + 1)), INPUT(_type='text', _name=name + '_' + str(n), value=forms[n], _size=20), _class='span6') for n in xnplurals]
        tab_col2 = DIV(CAT(*tab_inputs))
        tab_col3 = DIV(CAT(LABEL(XML('&nbsp;')), TAG.BUTTON(T('delete'), _onclick='return delkey("%s")' % name, _class='btn'), _class='span6'))
        tab_row = DIV(DIV(tab_col1, '\n', tab_col2, '\n', tab_col3, _class='well well-small'), _id=name, _class='row-fluid tab_row')
        tab_rows.append(tab_row)

    tab_rows.append(DIV(TAG['button'](T('update'), _type='submit',
                                      _class='btn btn-primary'),
                        _class='controls'))
    tab_container = DIV(*tab_rows, **dict(_class="row-fluid"))

    form = FORM(tab_container)
    if form.accepts(request.vars, keepvalues=True):
        new_plurals = dict()
        for key in keys:
            name = md5_hash(key)
            if form.vars[name + '_0'] == chr(127):
                continue
            new_plurals[key] = [form.vars[name + '_' + str(n)]
                                for n in xnplurals]
        write_plural_dict(apath(filename, r=request), new_plurals)
        session.flash = T('file saved on %(time)s', dict(time=time.ctime()))
        redirect(URL(r=request, args=request.args, vars=dict(
            nplurals=request.vars.nplurals)))
    return dict(app=request.args[0], filename=filename, form=form)


def about():
    """ Read about info """
    app = get_app()
    # ## check if file is not there
    about = safe_read(apath('%s/ABOUT' % app, r=request))
    license = safe_read(apath('%s/LICENSE' % app, r=request))
    return dict(app=app, about=MARKMIN(about), license=MARKMIN(license), progress=report_progress(app))


def design():
    """ Application design handler """
    app = get_app()

    if not response.flash and app == request.application:
        msg = T('ATTENTION: you cannot edit the running application!')
        response.flash = msg

    if request.vars and not request.vars.token == session.token:
        redirect(URL('logout'))

    if request.vars.pluginfile is not None and not isinstance(request.vars.pluginfile, str):
        filename = os.path.basename(request.vars.pluginfile.filename)
        if plugin_install(app, request.vars.pluginfile.file,
                          request, filename):
            session.flash = T('new plugin installed')
            redirect(URL('design', args=app))
        else:
            session.flash = \
                T('unable to create application "%s"', request.vars.filename)
        redirect(URL(r=request))
    elif isinstance(request.vars.pluginfile, str):
        session.flash = T('plugin not specified')
        redirect(URL(r=request))

    # If we have only pyc files it means that
    # we cannot design
    if os.path.exists(apath('%s/compiled' % app, r=request)):
        session.flash = \
            T('application is compiled and cannot be designed')
        redirect(URL('site'))

    # Get all models
    models = listdir(apath('%s/models/' % app, r=request), '.*\.py$')
    models = [x.replace('\\', '/') for x in models]
    defines = {}
    for m in models:
        data = safe_read(apath('%s/models/%s' % (app, m), r=request))
        defines[m] = regex_tables.findall(data)
        defines[m].sort()

    # Get all controllers
    controllers = sorted(
        listdir(apath('%s/controllers/' % app, r=request), '.*\.py$'))
    controllers = [x.replace('\\', '/') for x in controllers]
    functions = {}
    for c in controllers:
        data = safe_read(apath('%s/controllers/%s' % (app, c), r=request))
        items = find_exposed_functions(data)
        functions[c] = items

    # Get all views
    views = sorted(
        listdir(apath('%s/views/' % app, r=request), '[\w/\-]+(\.\w+)+$'))
    views = [x.replace('\\', '/') for x in views if not x.endswith('.bak')]
    extend = {}
    include = {}
    for c in views:
        data = safe_read(apath('%s/views/%s' % (app, c), r=request))
        items = regex_extend.findall(data)

        if items:
            extend[c] = items[0][1]

        items = regex_include.findall(data)
        include[c] = [i[1] for i in items]

    # Get all modules
    modules = listdir(apath('%s/modules/' % app, r=request), '.*\.py$')
    modules = modules = [x.replace('\\', '/') for x in modules]
    modules.sort()

    # Get all private files
    privates = listdir(apath('%s/private/' % app, r=request), '[^\.#].*')
    privates = [x.replace('\\', '/') for x in privates]
    privates.sort()

    # Get all static files
    statics = listdir(apath('%s/static/' % app, r=request), '[^\.#].*',
                      maxnum = MAXNFILES)
    statics = [x.replace(os.path.sep, '/') for x in statics]
    statics.sort()

    # Get all languages
    langpath = os.path.join(apath(app, r=request),'languages')
    languages = dict([(lang, info) for lang, info
                      in read_possible_languages(langpath).iteritems()
                      if info[2] != 0])  # info[2] is langfile_mtime:
                                         # get only existed files

    #Get crontab
    cronfolder = apath('%s/cron' % app, r=request)
    crontab = apath('%s/cron/crontab' % app, r=request)
    if not is_gae:
        if not os.path.exists(cronfolder):
            os.mkdir(cronfolder)
        if not os.path.exists(crontab):
            safe_write(crontab, '#crontab')

    plugins = []

    def filter_plugins(items, plugins):
        plugins += [item[7:].split('/')[0].split(
            '.')[0] for item in items if item.startswith('plugin_')]
        plugins[:] = list(set(plugins))
        plugins.sort()
        return [item for item in items if not item.startswith('plugin_')]

    return dict(app=app,
                models=filter_plugins(models, plugins),
                defines=defines,
                controllers=filter_plugins(controllers, plugins),
                functions=functions,
                views=filter_plugins(views, plugins),
                modules=filter_plugins(modules, plugins),
                extend=extend,
                include=include,
                privates=filter_plugins(privates, plugins),
                statics=filter_plugins(statics, plugins),
                languages=languages,
                crontab=crontab,
                plugins=plugins)


def delete_plugin():
    """ Object delete handler """
    app = request.args(0)
    plugin = request.args(1)
    plugin_name = 'plugin_' + plugin

    dialog = FORM.confirm(
        T('Delete'),
        {T('Cancel'): URL('design', args=app)})

    if dialog.accepted:
        try:
            for folder in ['models', 'views', 'controllers', 'static', 'modules', 'private']:
                path = os.path.join(apath(app, r=request), folder)
                for item in os.listdir(path):
                    if item.rsplit('.', 1)[0] == plugin_name:
                        filename = os.path.join(path, item)
                        if os.path.isdir(filename):
                            shutil.rmtree(filename)
                        else:
                            os.unlink(filename)
            session.flash = T('plugin "%(plugin)s" deleted',
                              dict(plugin=plugin))
        except Exception:
            session.flash = T('unable to delete file plugin "%(plugin)s"',
                              dict(plugin=plugin))
        redirect(URL('design', args=request.args(0), anchor=request.vars.id2))
    return dict(dialog=dialog, plugin=plugin)


def plugin():
    """ Application design handler """
    app = get_app()
    plugin = request.args(1)

    if not response.flash and app == request.application:
        msg = T('ATTENTION: you cannot edit the running application!')
        response.flash = msg

    # If we have only pyc files it means that
    # we cannot design
    if os.path.exists(apath('%s/compiled' % app, r=request)):
        session.flash = \
            T('application is compiled and cannot be designed')
        redirect(URL('site'))

    # Get all models
    models = listdir(apath('%s/models/' % app, r=request), '.*\.py$')
    models = [x.replace('\\', '/') for x in models]
    defines = {}
    for m in models:
        data = safe_read(apath('%s/models/%s' % (app, m), r=request))
        defines[m] = regex_tables.findall(data)
        defines[m].sort()

    # Get all controllers
    controllers = sorted(
        listdir(apath('%s/controllers/' % app, r=request), '.*\.py$'))
    controllers = [x.replace('\\', '/') for x in controllers]
    functions = {}
    for c in controllers:
        data = safe_read(apath('%s/controllers/%s' % (app, c), r=request))
        items = find_exposed_functions(data)
        functions[c] = items

    # Get all views
    views = sorted(
        listdir(apath('%s/views/' % app, r=request), '[\w/\-]+\.\w+$'))
    views = [x.replace('\\', '/') for x in views]
    extend = {}
    include = {}
    for c in views:
        data = safe_read(apath('%s/views/%s' % (app, c), r=request))
        items = regex_extend.findall(data)
        if items:
            extend[c] = items[0][1]

        items = regex_include.findall(data)
        include[c] = [i[1] for i in items]

    # Get all modules
    modules = listdir(apath('%s/modules/' % app, r=request), '.*\.py$')
    modules = modules = [x.replace('\\', '/') for x in modules]
    modules.sort()

    # Get all private files
    privates = listdir(apath('%s/private/' % app, r=request), '[^\.#].*')
    privates = [x.replace('\\', '/') for x in privates]
    privates.sort()

    # Get all static files
    statics = listdir(apath('%s/static/' % app, r=request), '[^\.#].*',
                      maxnum = MAXNFILES)
    statics = [x.replace(os.path.sep, '/') for x in statics]
    statics.sort()

    # Get all languages
    languages = sorted([lang + '.py' for lang, info in
                        T.get_possible_languages_info().iteritems()
                        if info[2] != 0])  # info[2] is langfile_mtime:
                                    # get only existed files

    #Get crontab
    crontab = apath('%s/cron/crontab' % app, r=request)
    if not os.path.exists(crontab):
        safe_write(crontab, '#crontab')

    def filter_plugins(items):
        regex = re.compile('^plugin_' + plugin + '(/.*|\..*)?$')
        return [item for item in items if item and regex.match(item)]

    return dict(app=app,
                models=filter_plugins(models),
                defines=defines,
                controllers=filter_plugins(controllers),
                functions=functions,
                views=filter_plugins(views),
                modules=filter_plugins(modules),
                extend=extend,
                include=include,
                privates=filter_plugins(privates),
                statics=filter_plugins(statics),
                languages=languages,
                crontab=crontab)

def create_file():
    """ Create files handler """
    if request.vars and not request.vars.token == session.token:
        redirect(URL('logout'))
    try:
        anchor = '#' + request.vars.id if request.vars.id else ''
        if request.vars.app:
            app = get_app(request.vars.app)
            path = abspath(request.vars.location)
        else:
            if request.vars.dir:
                    request.vars.location += request.vars.dir + '/'
            app = get_app(name=request.vars.location.split('/')[0])
            path = apath(request.vars.location, r=request)
        filename = re.sub('[^\w./-]+', '_', request.vars.filename)
        if path[-7:] == '/rules/':
            # Handle plural rules files
            if len(filename) == 0:
                raise SyntaxError
            if not filename[-3:] == '.py':
                filename += '.py'
            lang = re.match('^plural_rules-(.*)\.py$', filename).group(1)
            langinfo = read_possible_languages(apath(app, r=request))[lang]
            text = dedent("""
                   #!/usr/bin/env python
                   # -*- coding: utf-8 -*-
                   # Plural-Forms for %(lang)s (%(langname)s)

                   nplurals=2  # for example, English language has 2 forms:
                               # 1 singular and 1 plural

                   # Determine plural_id for number *n* as sequence of positive
                   # integers: 0,1,...
                   # NOTE! For singular form ALWAYS return plural_id = 0
                   get_plural_id = lambda n: int(n != 1)

                   # Construct and return plural form of *word* using
                   # *plural_id* (which ALWAYS>0). This function will be executed
                   # for words (or phrases) not found in plural_dict dictionary.
                   # By default this function simply returns word in singular:
                   construct_plural_form = lambda word, plural_id: word
                   """)[1:] % dict(lang=langinfo[0], langname=langinfo[1])

        elif path[-11:] == '/languages/':
            # Handle language files
            if len(filename) == 0:
                raise SyntaxError
            if not filename[-3:] == '.py':
                filename += '.py'
            path = os.path.join(apath(app, r=request), 'languages', filename)
            if not os.path.exists(path):
                safe_write(path, '')
            # create language xx[-yy].py file:
            findT(apath(app, r=request), filename[:-3])
            session.flash = T('language file "%(filename)s" created/updated',
                              dict(filename=filename))
            redirect(request.vars.sender + anchor)

        elif path[-8:] == '/models/':
            # Handle python models
            if not filename[-3:] == '.py':
                filename += '.py'

            if len(filename) == 3:
                raise SyntaxError

            text = '# -*- coding: utf-8 -*-\n'

        elif path[-13:] == '/controllers/':
            # Handle python controllers
            if not filename[-3:] == '.py':
                filename += '.py'

            if len(filename) == 3:
                raise SyntaxError

            text = '# -*- coding: utf-8 -*-\n# %s\ndef index(): return dict(message="hello from %s")'
            text = text % (T('try something like'), filename)

        elif path[-7:] == '/views/':
            if request.vars.plugin and not filename.startswith('plugin_%s/' % request.vars.plugin):
                filename = 'plugin_%s/%s' % (request.vars.plugin, filename)
            # Handle template (html) views
            if filename.find('.') < 0:
                filename += '.html'
            extension = filename.split('.')[-1].lower()

            if len(filename) == 5:
                raise SyntaxError

            msg = T(
                'This is the %(filename)s template', dict(filename=filename))
            if extension == 'html':
                text = dedent("""
                   {{extend 'layout.html'}}
                   <h1>%s</h1>
                   {{=BEAUTIFY(response._vars)}}""" % msg)[1:]
            else:
                generic = os.path.join(path, 'generic.' + extension)
                if os.path.exists(generic):
                    text = read_file(generic)
                else:
                    text = ''

        elif path[-9:] == '/modules/':
            if request.vars.plugin and not filename.startswith('plugin_%s/' % request.vars.plugin):
                filename = 'plugin_%s/%s' % (request.vars.plugin, filename)
            # Handle python module files
            if not filename[-3:] == '.py':
                filename += '.py'

            if len(filename) == 3:
                raise SyntaxError

            text = dedent("""
                   #!/usr/bin/env python
                   # -*- coding: utf-8 -*-
                   from gluon import *\n""")[1:]

        elif (path[-8:] == '/static/') or (path[-9:] == '/private/'):
            if (request.vars.plugin and
                not filename.startswith('plugin_%s/' % request.vars.plugin)):
                filename = 'plugin_%s/%s' % (request.vars.plugin, filename)
            text = ''

        else:
            redirect(request.vars.sender + anchor)

        full_filename = os.path.join(path, filename)
        dirpath = os.path.dirname(full_filename)

        if not os.path.exists(dirpath):
            os.makedirs(dirpath)

        if os.path.exists(full_filename):
            raise SyntaxError

        safe_write(full_filename, text)
        log_progress(app, 'CREATE', filename)
        if request.vars.dir:
            result = T('file "%(filename)s" created',
                          dict(filename=full_filename[len(path):]))
        else:
            session.flash = T('file "%(filename)s" created',
                          dict(filename=full_filename[len(path):]))
        vars = {}
        if request.vars.id:
            vars['id'] = request.vars.id
        if request.vars.app:
            vars['app'] = request.vars.app
        redirect(URL('edit',
                 args=[os.path.join(request.vars.location, filename)], vars=vars))

    except Exception, e:
        if not isinstance(e, HTTP):
            session.flash = T('cannot create file')

    if request.vars.dir:
        response.flash = result
        response.headers['web2py-component-content'] = 'append'
        response.headers['web2py-component-command'] = "%s %s %s" % (
            "$.web2py.invalidate('#files_menu');",
            "load_file('%s');" % URL('edit', args=[app,request.vars.dir,filename]),
            "$.web2py.enableElement($('#form form').find($.web2py.formInputClickSelector));")
        return ''
    else:
        redirect(request.vars.sender + anchor)


def listfiles(app, dir, regexp='.*\.py$'):
        files = sorted(
         listdir(apath('%(app)s/%(dir)s/' % {'app':app, 'dir':dir}, r=request), regexp))
        files = [x.replace('\\', '/') for x in files if not x.endswith('.bak')]
        return files

def editfile(path,file,vars={}, app = None):
        args=(path,file) if 'app' in vars else (app,path,file)
        url = URL('edit', args=args, vars=vars)
        return A(file, _class='editor_filelink', _href=url, _style='word-wrap: nowrap;')

def files_menu():
        app = request.vars.app or 'welcome'
        dirs=[{'name':'models', 'reg':'.*\.py$'},
              {'name':'controllers', 'reg':'.*\.py$'},
              {'name':'views', 'reg':'[\w/\-]+(\.\w+)+$'},
              {'name':'modules', 'reg':'.*\.py$'},
              {'name':'static', 'reg': '[^\.#].*'},
              {'name':'private', 'reg':'.*\.py$'}]
        result_files = []
        for dir in dirs:
                result_files.append(TAG[''](LI(dir['name'], _class="nav-header component", _onclick="collapse('" + dir['name'] + "_files');"),
                                      LI(UL(*[LI(editfile(dir['name'], f, dict(id=dir['name'] + f.replace('.','__')), app), _style="overflow:hidden", _id=dir['name']+"__"+f.replace('.','__'))
                                                      for f in listfiles(app, dir['name'], regexp=dir['reg'])],
                                                      _class="nav nav-list small-font"),
                                               _id=dir['name'] + '_files', _style="display: none;")))
        return dict(result_files = result_files)

def upload_file():
    """ File uploading handler """
    if request.vars and not request.vars.token == session.token:
        redirect(URL('logout'))
    try:
        filename = None
        app = get_app(name=request.vars.location.split('/')[0])
        path = apath(request.vars.location, r=request)

        if request.vars.filename:
            filename = re.sub('[^\w\./]+', '_', request.vars.filename)
        else:
            filename = os.path.split(request.vars.file.filename)[-1]

        if path[-8:] == '/models/' and not filename[-3:] == '.py':
            filename += '.py'

        if path[-9:] == '/modules/' and not filename[-3:] == '.py':
            filename += '.py'

        if path[-13:] == '/controllers/' and not filename[-3:] == '.py':
            filename += '.py'

        if path[-7:] == '/views/' and not filename[-5:] == '.html':
            filename += '.html'

        if path[-11:] == '/languages/' and not filename[-3:] == '.py':
            filename += '.py'

        filename = os.path.join(path, filename)
        dirpath = os.path.dirname(filename)

        if not os.path.exists(dirpath):
            os.makedirs(dirpath)

        data = request.vars.file.file.read()
        lineno = count_lines(data)
        safe_write(filename, data, 'wb')
        log_progress(app, 'UPLOAD', filename, lineno)
        session.flash = T('file "%(filename)s" uploaded',
                          dict(filename=filename[len(path):]))
    except Exception:
        if filename:
            d = dict(filename=filename[len(path):])
        else:
            d = dict(filename='unkown')
        session.flash = T('cannot upload file "%(filename)s"', d)

    redirect(request.vars.sender)


def errors():
    """ Error handler """
    import operator
    import os
    import pickle
    import hashlib

    app = get_app()
    if is_gae:
        method = 'dbold' if ('old' in
                     (request.args(1) or '')) else 'dbnew'
    else:
        method = request.args(1) or 'new'
    db_ready = {}
    db_ready['status'] = get_ticket_storage(app)
    db_ready['errmessage'] = T(
        "No ticket_storage.txt found under /private folder")
    db_ready['errlink'] = "http://web2py.com/books/default/chapter/29/13#Collecting-tickets"

    if method == 'new':
        errors_path = apath('%s/errors' % app, r=request)

        delete_hashes = []
        for item in request.vars:
            if item[:7] == 'delete_':
                delete_hashes.append(item[7:])

        hash2error = dict()

        for fn in listdir(errors_path, '^[a-fA-F0-9.\-]+$'):
            fullpath = os.path.join(errors_path, fn)
            if not os.path.isfile(fullpath):
                continue
            try:
                fullpath_file = open(fullpath, 'r')
                try:
                    error = pickle.load(fullpath_file)
                finally:
                    fullpath_file.close()
            except IOError:
                continue
            except EOFError:
                continue

            hash = hashlib.md5(error['traceback']).hexdigest()

            if hash in delete_hashes:
                os.unlink(fullpath)
            else:
                try:
                    hash2error[hash]['count'] += 1
                except KeyError:
                    error_lines = error['traceback'].split("\n")
                    last_line = error_lines[-2] if len(error_lines)>1 else 'unknown'
                    error_causer = os.path.split(error['layer'])[1]
                    hash2error[hash] = dict(count=1, pickel=error,
                                            causer=error_causer,
                                            last_line=last_line,
                                            hash=hash, ticket=fn)

        decorated = [(x['count'], x) for x in hash2error.values()]
        decorated.sort(key=operator.itemgetter(0), reverse=True)

        return dict(errors=[x[1] for x in decorated], app=app, method=method, db_ready=db_ready)

    elif method == 'dbnew':
        errors_path = apath('%s/errors' % app, r=request)
        tk_db, tk_table = get_ticket_storage(app)

        delete_hashes = []
        for item in request.vars:
            if item[:7] == 'delete_':
                delete_hashes.append(item[7:])

        hash2error = dict()

        for fn in tk_db(tk_table.id > 0).select():
            try:
                error = pickle.loads(fn.ticket_data)
                hash = hashlib.md5(error['traceback']).hexdigest()

                if hash in delete_hashes:
                    tk_db(tk_table.id == fn.id).delete()
                    tk_db.commit()
                else:
                    try:
                        hash2error[hash]['count'] += 1
                    except KeyError:
                        error_lines = error['traceback'].split("\n")
                        last_line = error_lines[-2]
                        error_causer = os.path.split(error['layer'])[1]
                        hash2error[hash] = dict(count=1,
                            pickel=error, causer=error_causer,
                            last_line=last_line, hash=hash,
                            ticket=fn.ticket_id)
            except AttributeError, e:
                tk_db(tk_table.id == fn.id).delete()
                tk_db.commit()

        decorated = [(x['count'], x) for x in hash2error.values()]
        decorated.sort(key=operator.itemgetter(0), reverse=True)
        return dict(errors=[x[1] for x in decorated], app=app,
                    method=method, db_ready=db_ready)

    elif method == 'dbold':
        tk_db, tk_table = get_ticket_storage(app)
        for item in request.vars:
            if item[:7] == 'delete_':
                tk_db(tk_table.ticket_id == item[7:]).delete()
                tk_db.commit()
        tickets_ = tk_db(tk_table.id > 0).select(tk_table.ticket_id,
            tk_table.created_datetime,
            orderby=~tk_table.created_datetime)
        tickets = [row.ticket_id for row in tickets_]
        times = dict([(row.ticket_id, row.created_datetime) for
            row in tickets_])
        return dict(app=app, tickets=tickets, method=method,
                    times=times, db_ready=db_ready)

    else:
        for item in request.vars:
            # delete_all rows doesn't contain any ticket
            # Remove anything else as requested
            if item[:7] == 'delete_' and (not item == "delete_all}"):
                os.unlink(apath('%s/errors/%s' % (app, item[7:]), r=request))
        func = lambda p: os.stat(apath('%s/errors/%s' %
                                       (app, p), r=request)).st_mtime
        tickets = sorted(
            listdir(apath('%s/errors/' % app, r=request), '^\w.*'),
            key=func,
            reverse=True)

        return dict(app=app, tickets=tickets, method=method, db_ready=db_ready)


def get_ticket_storage(app):
    private_folder = apath('%s/private' % app, r=request)
    ticket_file = os.path.join(private_folder, 'ticket_storage.txt')
    if os.path.exists(ticket_file):
        db_string = open(ticket_file).read()
        db_string = db_string.strip().replace('\r', '').replace('\n', '')
    elif is_gae:
        # use Datastore as fallback if there is no ticket_file
        db_string = "google:datastore"
    else:
        return False
    tickets_table = 'web2py_ticket'
    tablename = tickets_table + '_' + app
    db_path = apath('%s/databases' % app, r=request)
    ticketsdb = DAL(db_string, folder=db_path, auto_import=True)
    if not ticketsdb.get(tablename):
        table = ticketsdb.define_table(
            tablename,
            Field('ticket_id', length=100),
            Field('ticket_data', 'text'),
            Field('created_datetime', 'datetime'),
        )
    return ticketsdb, ticketsdb.get(tablename)


def make_link(path):
    """ Create a link from a path """
    tryFile = path.replace('\\', '/')

    if os.path.isabs(tryFile) and os.path.isfile(tryFile):
        (folder, filename) = os.path.split(tryFile)
        (base, ext) = os.path.splitext(filename)
        app = get_app()

        editable = {'controllers': '.py', 'models': '.py', 'views': '.html'}
        for key in editable.keys():
            check_extension = folder.endswith("%s/%s" % (app, key))
            if ext.lower() == editable[key] and check_extension:
                return A('"' + tryFile + '"',
                         _href=URL(r=request,
                         f='edit/%s/%s/%s' % (app, key, filename))).xml()
    return ''


def make_links(traceback):
    """ Make links using the given traceback """

    lwords = traceback.split('"')

    # Making the short circuit compatible with <= python2.4
    result = (len(lwords) != 0) and lwords[0] or ''

    i = 1

    while i < len(lwords):
        link = make_link(lwords[i])

        if link == '':
            result += '"' + lwords[i]
        else:
            result += link

            if i + 1 < len(lwords):
                result += lwords[i + 1]
                i = i + 1

        i = i + 1

    return result


class TRACEBACK(object):
    """ Generate the traceback """

    def __init__(self, text):
        """ TRACEBACK constructor """

        self.s = make_links(CODE(text).xml())

    def xml(self):
        """ Returns the xml """

        return self.s


def ticket():
    """ Ticket handler """

    if len(request.args) != 2:
        session.flash = T('invalid ticket')
        redirect(URL('site'))

    app = get_app()
    myversion = request.env.web2py_version
    ticket = request.args[1]
    e = RestrictedError()
    e.load(request, app, ticket)

    return dict(app=app,
                ticket=ticket,
                output=e.output,
                traceback=(e.traceback and TRACEBACK(e.traceback)),
                snapshot=e.snapshot,
                code=e.code,
                layer=e.layer,
                myversion=myversion)


def ticketdb():
    """ Ticket handler """

    if len(request.args) != 2:
        session.flash = T('invalid ticket')
        redirect(URL('site'))

    app = get_app()
    myversion = request.env.web2py_version
    ticket = request.args[1]
    e = RestrictedError()
    request.tickets_db = get_ticket_storage(app)[0]
    e.load(request, app, ticket)
    response.view = 'default/ticket.html'
    return dict(app=app,
                ticket=ticket,
                output=e.output,
                traceback=(e.traceback and TRACEBACK(e.traceback)),
                snapshot=e.snapshot,
                code=e.code,
                layer=e.layer,
                myversion=myversion)


def error():
    """ Generate a ticket (for testing) """
    raise RuntimeError('admin ticket generator at your service')


def update_languages():
    """ Update available languages """

    app = get_app()
    update_all_languages(apath(app, r=request))
    session.flash = T('Language files (static strings) updated')
    redirect(URL('design', args=app, anchor='languages'))


def user():
    if MULTI_USER_MODE:
        if not db(db.auth_user).count():
            auth.settings.registration_requires_approval = False
        return dict(form=auth())
    else:
        return dict(form=T("Disabled"))


def reload_routes():
    """ Reload routes.py """
    import gluon.rewrite
    gluon.rewrite.load()
    redirect(URL('site'))


def manage_students():
    if not (MULTI_USER_MODE and is_manager()):
        session.flash = T('Not Authorized')
        redirect(URL('site'))
    db.auth_user.registration_key.writable = True
    grid = SQLFORM.grid(db.auth_user)
    return locals()


def bulk_register():
    if not (MULTI_USER_MODE and is_manager()):
        session.flash = T('Not Authorized')
        redirect(URL('site'))
    form = SQLFORM.factory(Field('emails', 'text'))
    if form.process().accepted:
        emails = [x.strip() for x in form.vars.emails.split('\n') if x.strip()]
        n = 0
        for email in emails:
            if not db.auth_user(email=email):
                n += db.auth_user.insert(email=email) and 1 or 0
        session.flash = T('%s students registered', n)
        redirect(URL('site'))
    return locals()

### Begin experimental stuff need fixes:
# 1) should run in its own process - cannot os.chdir
# 2) should not prompt user at console
# 3) should give option to force commit and not reuqire manual merge


def git_pull():
    """ Git Pull handler """
    app = get_app()
    if not have_git:
        session.flash = GIT_MISSING
        redirect(URL('site'))
    dialog = FORM.confirm(T('Pull'),
                          {T('Cancel'): URL('site')})
    if dialog.accepted:
        try:
            repo = git.Repo(os.path.join(apath(r=request), app))
            origin = repo.remotes.origin
            origin.fetch()
            origin.pull()
            session.flash = T("Application updated via git pull")
            redirect(URL('site'))

        except git.CheckoutError:
            session.flash = T("Pull failed, certain files could not be checked out. Check logs for details.")
            redirect(URL('site'))
        except git.UnmergedEntriesError:
            session.flash = T("Pull is not possible because you have unmerged files. Fix them up in the work tree, and then try again.")
            redirect(URL('site'))
        except git.GitCommandError:
            session.flash = T(
                "Pull failed, git exited abnormally. See logs for details.")
            redirect(URL('site'))
        except AssertionError:
            session.flash = T("Pull is not possible because you have unmerged files. Fix them up in the work tree, and then try again.")
            redirect(URL('site'))
    elif 'cancel' in request.vars:
        redirect(URL('site'))
    return dict(app=app, dialog=dialog)


def git_push():
    """ Git Push handler """
    app = get_app()
    if not have_git:
        session.flash = GIT_MISSING
        redirect(URL('site'))
    form = SQLFORM.factory(Field('changelog', requires=IS_NOT_EMPTY()))
    form.element('input[type=submit]')['_value'] = T('Push')
    form.add_button(T('Cancel'), URL('site'))
    form.process()
    if form.accepted:
        try:
            repo = git.Repo(os.path.join(apath(r=request), app))
            index = repo.index
            index.add([apath(r=request) + app + '/*'])
            new_commit = index.commit(form.vars.changelog)
            origin = repo.remotes.origin
            origin.push()
            session.flash = T(
                "Git repo updated with latest application changes.")
            redirect(URL('site'))
        except git.UnmergedEntriesError:
            session.flash = T("Push failed, there are unmerged entries in the cache. Resolve merge issues manually and try again.")
            redirect(URL('site'))
    return dict(app=app, form=form)

def plugins():
    app = request.args(0)
    from serializers import loads_json
    if not session.plugins:
        try:
            rawlist = urllib.urlopen("http://www.web2pyslices.com/" +
                                     "public/api.json/action/list/content/Package?package" +
                                     "_type=plugin&search_index=false").read()
            session.plugins = loads_json(rawlist)
        except:
            response.flash = T('Unable to download the list of plugins')
            session.plugins = []
    return dict(plugins=session.plugins["results"], app=request.args(0))

def install_plugin():
    app = request.args(0)
    source = request.vars.source
    plugin = request.vars.plugin
    if not (source and app):
        raise HTTP(500, T("Invalid request"))
    form = SQLFORM.factory()
    result = None
    if form.process().accepted:
        # get w2p plugin
        if "web2py.plugin." in source:
            filename = "web2py.plugin.%s.w2p" % \
                source.split("web2py.plugin.")[-1].split(".w2p")[0]
        else:
            filename = "web2py.plugin.%s.w2p" % cleanpath(plugin)
        if plugin_install(app, urllib.urlopen(source),
                          request, filename):
            session.flash = T('New plugin installed: %s', filename)
        else:
            session.flash = \
                T('unable to install plugin "%s"', filename)
        redirect(URL(f="plugins", args=[app,]))
    return dict(form=form, app=app, plugin=plugin, source=source)
