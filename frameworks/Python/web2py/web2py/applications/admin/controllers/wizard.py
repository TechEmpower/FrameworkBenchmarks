# -*- coding: utf-8 -*-

import os
import uuid
import re
import pickle
import urllib
import glob
from gluon.admin import app_create, plugin_install
from gluon.fileutils import abspath, read_file, write_file


def reset(session):
    session.app = {
        'name': '',
        'params': [('title', 'My New App'),
                   ('subtitle', 'powered by web2py'),
                   ('author', 'you'),
                   ('author_email', 'you@example.com'),
                   ('keywords', ''),
                   ('description', ''),
                   ('layout_theme', 'Default'),
                   ('database_uri', 'sqlite://storage.sqlite'),
                   ('security_key', str(uuid.uuid4())),
                   ('email_server', 'localhost'),
                   ('email_sender', 'you@example.com'),
                   ('email_login', ''),
                   ('login_method', 'local'),
                   ('login_config', ''),
                   ('plugins', [])],
        'tables': ['auth_user'],
        'table_auth_user': ['username', 'first_name',
                            'last_name', 'email', 'password'],
        'pages': ['index', 'error'],
        'page_index': '# Welcome to my new app',
        'page_error': '# Error: the document does not exist',
    }

if not session.app:
    reset(session)


def listify(x):
    if not isinstance(x, (list, tuple)):
        return x and [x] or []
    return x


def clean(name):
    return re.sub('\W+', '_', name.strip().lower())


def index():
    response.view = 'wizard/step.html'
    reset(session)
    apps = os.listdir(os.path.join(request.folder, '..'))
    form = SQLFORM.factory(Field('name', requires=[IS_NOT_EMPTY(),
                                                   IS_ALPHANUMERIC()]), _class='span5 well well-small')
    if form.accepts(request.vars):
        app = form.vars.name
        session.app['name'] = app
        if MULTI_USER_MODE and db(db.app.name == app)(db.app.owner != auth.user.id).count():
            session.flash = 'App belongs already to other user'
        elif app in apps:
            meta = os.path.normpath(
                os.path.join(os.path.normpath(request.folder),
                             '..', app, 'wizard.metadata'))
            if os.path.exists(meta):
                try:
                    metafile = open(meta, 'rb')
                    try:
                        session.app = pickle.load(metafile)
                    finally:
                        metafile.close()
                    session.flash = T("The app exists, was created by wizard, continue to overwrite!")
                except:
                    session.flash = T("The app exists, was NOT created by wizard, continue to overwrite!")
        redirect(URL('step1'))
    return dict(step='Start', form=form)


def step1():
    from gluon.contrib.simplejson import loads
    import urllib
    if not session.themes:
        #url = LAYOUTS_APP + '/default/layouts.json'
        #try:
        #    data = urllib.urlopen(url).read()
        #    session.themes = ['Default'] + loads(data)['layouts']
        #except:
        session.themes = ['Default']
    themes = session.themes
    if not session.plugins:
        #url = PLUGINS_APP + '/default/plugins.json'
        #try:
        #    data = urllib.urlopen(url).read()
        #    session.plugins = loads(data)['plugins']
        #except:
        session.plugins = []
    plugins = [x.split('.')[2] for x in session.plugins]
    response.view = 'wizard/step.html'
    params = dict(session.app['params'])
    form = SQLFORM.factory(
        Field('title', default=params.get('title', None),
              requires=IS_NOT_EMPTY()),
        Field('subtitle', default=params.get('subtitle', None)),
        Field('author', default=params.get('author', None)),
        Field(
            'author_email', default=params.get('author_email', None)),
        Field('keywords', default=params.get('keywords', None)),
        Field('description', 'text',
              default=params.get('description', None)),
        Field('layout_theme', requires=IS_IN_SET(themes),
              default=params.get('layout_theme', themes[0])),
        Field(
            'database_uri', default=params.get('database_uri', None)),
        Field(
            'security_key', default=params.get('security_key', None)),
        Field(
            'email_server', default=params.get('email_server', None)),
        Field(
            'email_sender', default=params.get('email_sender', None)),
        Field('email_login', default=params.get('email_login', None)),
        Field('login_method', requires=IS_IN_SET(('local', 'janrain')),
              default=params.get('login_method', 'local')),
        Field(
            'login_config', default=params.get('login_config', None)),
        Field('plugins', 'list:string', requires=IS_IN_SET(plugins, multiple=True)),
        _class='span7 well well-small')

    if form.accepts(request.vars):
        session.app['params'] = [(key, form.vars.get(key, None))
                                 for key, value in session.app['params']]
        redirect(URL('step2')  + '/#xwizard_form')
    return dict(step='1: Setting Parameters', form=form)


def step2():
    response.view = 'wizard/step.html'
    form = SQLFORM.factory(Field('table_names', 'list:string',
                                 default=session.app['tables']), _class="span7 well well-small")
    if form.accepts(request.vars):
        table_names = [clean(t) for t in listify(form.vars.table_names)
                       if t.strip()]
        if [t for t in table_names if t.startswith('auth_') and
                not t == 'auth_user']:
            form.error.table_names = \
                T('invalid table names (auth_* tables already defined)')
        else:
            session.app['tables'] = table_names
            for table in session.app['tables']:
                if not 'table_' + table in session.app:
                    session.app['table_' + table] = ['name']
                if not table == 'auth_user':
                    name = table + '_manage'
                    if not name in session.app['pages']:
                        session.app['pages'].append(name)
                        session.app['page_' + name] = \
                            '## Manage %s\n\n{{=form}}' % (table)
            if session.app['tables']:
                redirect(URL('step3', args=0) + '/#xwizard_form')
            else:
                redirect(URL('step4') + '/#xwizard_form')
    return dict(step='2: Tables', form=form)


def step3():
    response.view = 'wizard/step.html'
    n = int(request.args(-1) or 0)
    m = len(session.app['tables'])
    if n >= m:
        redirect(URL('step2'))
    table = session.app['tables'][n]
    form = SQLFORM.factory(Field('field_names', 'list:string',
                                 default=session.app.get('table_' + table, [])), _class="span7 well well-small")
    if form.accepts(request.vars) and form.vars.field_names:
        fields = listify(form.vars.field_names)
        if table == 'auth_user':
            for field in ['first_name', 'last_name', 'username', 'email', 'password']:
                if not field in fields:
                    fields.append(field)
        session.app['table_' + table] = [t.strip().lower()
                                         for t in listify(form.vars.field_names)
                                         if t.strip()]
        try:
            tables = sort_tables(session.app['tables'])
        except RuntimeError:
            response.flash = T('invalid circular reference')
        else:
            if n < m - 1:
                redirect(URL('step3', args=n + 1) + '/#xwizard_form')
            else:
                redirect(URL('step4') + '/#xwizard_form')
    return dict(step='3: Fields for table "%s" (%s of %s)'
                % (table, n + 1, m), table=table, form=form)


def step4():
    response.view = 'wizard/step.html'
    form = SQLFORM.factory(Field('pages', 'list:string',
                                 default=session.app['pages']), _class="span7 well well-small")
    if form.accepts(request.vars):
        session.app['pages'] = [clean(t)
                                for t in listify(form.vars.pages)
                                if t.strip()]
        if session.app['pages']:
            redirect(URL('step5', args=0) + '/#xwizard_form')
        else:
            redirect(URL('step6') + '/#xwizard_form')
    return dict(step='4: Pages', form=form)


def step5():
    response.view = 'wizard/step.html'
    n = int(request.args(-1) or 0)
    m = len(session.app['pages'])
    if n >= m:
        redirect(URL('step4'))
    page = session.app['pages'][n]
    markmin_url = 'http://web2py.com/examples/static/markmin.html'
    form = SQLFORM.factory(Field('content', 'text',
                                 default=session.app.get('page_' + page, []),
                                 comment=A('use markmin',
                                           _href=markmin_url, _target='_blank')),
                           formstyle='table2cols', _class="span7 well well-small")
    if form.accepts(request.vars):
        session.app['page_' + page] = form.vars.content
        if n < m - 1:
            redirect(URL('step5', args=n + 1) + '/#xwizard_form')
        else:
            redirect(URL('step6') + '/#xwizard_form')
    return dict(step='5: View for page "%s" (%s of %s)' % (page, n + 1, m), form=form)


def step6():
    response.view = 'wizard/step.html'
    params = dict(session.app['params'])
    app = session.app['name']
    form = SQLFORM.factory(
        Field('generate_model', 'boolean', default=True),
        Field('generate_controller', 'boolean', default=True),
        Field('generate_views', 'boolean', default=True),
        Field('generate_menu', 'boolean', default=True),
        Field('apply_layout', 'boolean', default=True),
        Field('erase_database', 'boolean', default=True),
        Field('populate_database', 'boolean', default=True),
        _id="generate_form", _class="form-horizontal span7 well well-small")
    if form.accepts(request.vars):
        if DEMO_MODE:
            session.flash = T('Application cannot be generated in demo mode')
            redirect(URL('index'))
        create(form.vars)
        session.flash = 'Application %s created' % app
        redirect(URL('generated'))
    return dict(step='6: Generate app "%s"' % app, form=form)


def generated():
    return dict(app=session.app['name'])


def sort_tables(tables):
    import re
    regex = re.compile('(%s)' % '|'.join(tables))
    is_auth_user = 'auth_user' in tables
    d = {}
    for table in tables:
        d[table] = []
        for field in session.app['table_%s' % table]:
            d[table] += regex.findall(field)
    tables = []
    if is_auth_user:
        tables.append('auth_user')

    def append(table, trail=[]):
        if table in trail:
            raise RuntimeError
        for t in d[table]:
            # if not t==table: (problem, no dropdown for self references)
            append(t, trail=trail + [table])
        if not table in tables:
            tables.append(table)
    for table in d:
        append(table)
    return tables


def make_table(table, fields):
    rawtable = table
    if table != 'auth_user':
        table = 't_' + table
    s = ''
    s += '\n' + '#' * 40 + '\n'
    s += "db.define_table('%s',\n" % table
    first_field = 'id'
    for field in fields:
        items = [x.lower() for x in field.split()]
        has = {}
        keys = []
        for key in ['notnull', 'unique', 'integer', 'double', 'boolean', 'float',
                    'boolean', 'date', 'time', 'datetime', 'text', 'wiki',
                    'html', 'file', 'upload', 'image', 'true',
                    'hidden', 'readonly', 'writeonly', 'multiple',
                    'notempty', 'required']:
            if key in items[1:]:
                keys.append(key)
                has[key] = True
        tables = session.app['tables']
        refs = [t for t in tables if t in items]
        items = items[:1] + [x for x in items[1:]
                             if not x in keys and not x in tables]
        barename = name = '_'.join(items)
        if table[:2] == 't_': name = 'f_' + name
        if first_field == 'id':
            first_field = name

        ### determine field type
        ftype = 'string'
        deftypes = {'integer': 'integer', 'double': 'double', 'boolean': 'boolean',
                    'float': 'double', 'bool': 'boolean',
                    'date': 'date', 'time': 'time', 'datetime': 'datetime',
                    'text': 'text', 'file': 'upload', 'image': 'upload',
                    'upload': 'upload', 'wiki': 'text', 'html': 'text'}
        for key, t in deftypes.items():
            if key in has:
                ftype = t
        if refs:
            key = refs[0]
            if not key == 'auth_user':
                key = 't_' + key
            if 'multiple' in has:
                ftype = 'list:reference %s' % key
            else:
                ftype = 'reference %s' % key
        if ftype == 'string' and 'multiple' in has:
            ftype = 'list:string'
        elif ftype == 'integer' and 'multiple' in has:
            ftype = 'list:integer'
        elif name == 'password':
            ftype = 'password'
        s += "    Field('%s', type='%s'" % (name, ftype)

        ### determine field attributes
        if 'notnull' in has or 'notempty' in has or 'required' in has:
            s += ', notnull=True'
        if 'unique' in has:
            s += ', unique=True'
        if ftype == 'boolean' and 'true' in has:
            s += ",\n          default=True"

        ### determine field representation
        elif 'wiki' in has:
            s += ",\n          represent=lambda x, row: MARKMIN(x)"
            s += ",\n          comment='WIKI (markmin)'"
        elif 'html' in has:
            s += ",\n          represent=lambda x, row: XML(x,sanitize=True)"
            s += ",\n          comment='HTML (sanitized)'"
        ### determine field access
        if name == 'password' or 'writeonly' in has:
            s += ",\n          readable=False"
        elif 'hidden' in has:
            s += ",\n          writable=False, readable=False"
        elif 'readonly' in has:
            s += ",\n          writable=False"

        ### make up a label
        s += ",\n          label=T('%s')),\n" % \
            ' '.join(x.capitalize() for x in barename.split('_'))
    if table == 'auth_user':
        s += "    Field('created_on','datetime',default=request.now,\n"
        s += "          label=T('Created On'),writable=False,readable=False),\n"
        s += "    Field('modified_on','datetime',default=request.now,\n"
        s += "          label=T('Modified On'),writable=False,readable=False,\n"
        s += "          update=request.now),\n"
        s += "    Field('registration_key',default='',\n"
        s += "          writable=False,readable=False),\n"
        s += "    Field('reset_password_key',default='',\n"
        s += "          writable=False,readable=False),\n"
        s += "    Field('registration_id',default='',\n"
        s += "          writable=False,readable=False),\n"
    elif 'auth_user' in session.app['tables']:
        s += "    auth.signature,\n"
    s += "    format='%(" + first_field + ")s',\n"
    s += "    migrate=settings.migrate)\n\n"
    if table == 'auth_user':
        s += """
db.auth_user.first_name.requires = IS_NOT_EMPTY(
    error_message=auth.messages.is_empty)
db.auth_user.last_name.requires = IS_NOT_EMPTY(
    error_message=auth.messages.is_empty)
db.auth_user.password.requires = CRYPT(
    key=auth.settings.hmac_key, min_length=4)
db.auth_user.username.requires = IS_NOT_IN_DB(db, db.auth_user.username)
db.auth_user.email.requires = (
    IS_EMAIL(error_message=auth.messages.invalid_email),
                               IS_NOT_IN_DB(db, db.auth_user.email))
"""
    else:
        s += "db.define_table('%s_archive',db.%s,Field('current_record','reference %s',readable=False,writable=False))\n" % (table, table, table)
    return s


def fix_db(filename):
    params = dict(session.app['params'])
    content = read_file(filename, 'rb')
    if 'auth_user' in session.app['tables']:
        auth_user = make_table('auth_user', session.app['table_auth_user'])
        content = content.replace('sqlite://storage.sqlite',
                                  params['database_uri'])
        content = content.replace('auth.define_tables()',
                                  auth_user + 'auth.define_tables(migrate = settings.migrate)')
    content += """
mail.settings.server = settings.email_server
mail.settings.sender = settings.email_sender
mail.settings.login = settings.email_login
"""
    if params['login_method'] == 'janrain':
        content += """
from gluon.contrib.login_methods.rpx_account import RPXAccount
auth.settings.actions_disabled=['register','change_password',
    'request_reset_password']
auth.settings.login_form = RPXAccount(request,
    api_key = settings.login_config.split(':')[-1],
    domain = settings.login_config.split(':')[0],
    url = "http://%s/%s/default/user/login" % (request.env.http_host,request.application))
"""
    write_file(filename, content, 'wb')


def make_menu(pages):
    s = ''
    s += 'response.title = settings.title\n'
    s += 'response.subtitle = settings.subtitle\n'
    s += "response.meta.author = '%(author)s <%(author_email)s>' % settings\n"
    s += 'response.meta.keywords = settings.keywords\n'
    s += 'response.meta.description = settings.description\n'
    s += 'response.menu = [\n'
    for page in pages:
        if not page.startswith('error'):
            if page.endswith('_manage'):
                page_name = page[:-7]
            else:
                page_name = page
            page_name = ' '.join(x.capitalize() for x in page_name.split('_'))
            s += "(T('%s'),URL('default','%s')==URL(),URL('default','%s'),[]),\n" \
                % (page_name, page, page)
    s += ']'
    return s


def make_page(page, contents):
    if 'auth_user' in session.app['tables'] and not page in ('index', 'error'):
        s = "@auth.requires_login()\ndef %s():\n" % page
    else:
        s = "def %s():\n" % page
    items = page.rsplit('_', 1)
    if items[0] in session.app['tables'] and len(items) == 2 and items[1] == 'manage':
        s += "    form = SQLFORM.smartgrid(db.t_%s,onupdate=auth.archive)\n" % items[0]
        s += "    return locals()\n\n"
    else:
        s += "    return dict()\n\n"
    return s


def make_view(page, contents):
    s = "{{extend 'layout.html'}}\n\n"
    s += str(MARKMIN(contents))
    return s


def populate(tables):
    s = 'from gluon.contrib.populate import populate\n'
    s += 'if db(db.auth_user).isempty():\n'
    for table in sort_tables(tables):
        t = table == 'auth_user' and 'auth_user' or 't_' + table
        s += "     populate(db.%s,10)\n" % t
    return s


def create(options):
    if DEMO_MODE:
        session.flash = T('disabled in demo mode')
        redirect(URL('step6'))
    params = dict(session.app['params'])
    app = session.app['name']
    if app_create(app, request, force=True, key=params['security_key']):
        if MULTI_USER_MODE:
            db.app.insert(name=app, owner=auth.user.id)
    else:
        session.flash = 'Failure to create application'
        redirect(URL('step6'))

    ### save metadata in newapp/wizard.metadata
    try:
        meta = os.path.join(request.folder, '..', app, 'wizard.metadata')
        file = open(meta, 'wb')
        pickle.dump(session.app, file)
        file.close()
    except IOError:
        session.flash = 'Failure to write wizard metadata'
        redirect(URL('step6'))

    ### apply theme
    if options.apply_layout and params['layout_theme'] != 'Default':
        try:
            fn = 'web2py.plugin.layout_%s.w2p' % params['layout_theme']
            theme = urllib.urlopen(
                LAYOUTS_APP + '/static/plugin_layouts/plugins/' + fn)
            plugin_install(app, theme, request, fn)
        except:
            session.flash = T("unable to download layout")

    ### apply plugins
    for plugin in params['plugins']:
        try:
            plugin_name = 'web2py.plugin.' + plugin + '.w2p'
            stream = urllib.urlopen(PLUGINS_APP + '/static/' + plugin_name)
            plugin_install(app, stream, request, plugin_name)
        except Exception, e:
            session.flash = T("unable to download plugin: %s" % plugin)

    ### write configuration file into newapp/models/0.py
    model = os.path.join(request.folder, '..', app, 'models', '0.py')
    file = open(model, 'wb')
    try:
        file.write("from gluon.storage import Storage\n")
        file.write("settings = Storage()\n\n")
        file.write("settings.migrate = True\n")
        for key, value in session.app['params']:
            file.write("settings.%s = %s\n" % (key, repr(value)))
    finally:
        file.close()

    ### write configuration file into newapp/models/menu.py
    if options.generate_menu:
        model = os.path.join(request.folder, '..', app, 'models', 'menu.py')
        file = open(model, 'wb')
        try:
            file.write(make_menu(session.app['pages']))
        finally:
            file.close()

    ### customize the auth_user table
    model = os.path.join(request.folder, '..', app, 'models', 'db.py')
    fix_db(model)

    ### create newapp/models/db_wizard.py
    if options.generate_model:
        model = os.path.join(
            request.folder, '..', app, 'models', 'db_wizard.py')
        file = open(model, 'wb')
        try:
            file.write('### we prepend t_ to tablenames and f_ to fieldnames for disambiguity\n\n')
            tables = sort_tables(session.app['tables'])
            for table in tables:
                if table == 'auth_user':
                    continue
                file.write(make_table(table, session.app['table_' + table]))
        finally:
            file.close()

    model = os.path.join(request.folder, '..', app,
                         'models', 'db_wizard_populate.py')
    if os.path.exists(model):
        os.unlink(model)
    if options.populate_database and session.app['tables']:
        file = open(model, 'wb')
        try:
            file.write(populate(session.app['tables']))
        finally:
            file.close()

    ### create newapp/controllers/default.py
    if options.generate_controller:
        controller = os.path.join(
            request.folder, '..', app, 'controllers', 'default.py')
        file = open(controller, 'wb')
        try:
            file.write("""# -*- coding: utf-8 -*-
### required - do no delete
def user(): return dict(form=auth())
def download(): return response.download(request,db)
def call(): return service()
### end requires
""")
            for page in session.app['pages']:
                file.write(
                    make_page(page, session.app.get('page_' + page, '')))
        finally:
            file.close()

    ### create newapp/views/default/*.html
    if options.generate_views:
        for page in session.app['pages']:
            view = os.path.join(
                request.folder, '..', app, 'views', 'default', page + '.html')
            file = open(view, 'wb')
            try:
                file.write(
                    make_view(page, session.app.get('page_' + page, '')))
            finally:
                file.close()

    if options.erase_database:
        path = os.path.join(request.folder, '..', app, 'databases', '*')
        for file in glob.glob(path):
            os.unlink(file)
