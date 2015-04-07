# -*- coding: utf-8 -*-

# ##########################################################
# ## make sure administrator is on localhost
# ###########################################################

import os
import socket
import datetime
import copy
import gluon.contenttype
import gluon.fileutils

try:
    import pygraphviz as pgv
except ImportError:
    pgv = None

is_gae = request.env.web2py_runtime_gae or False

# ## critical --- make a copy of the environment

global_env = copy.copy(globals())
global_env['datetime'] = datetime

http_host = request.env.http_host.split(':')[0]
remote_addr = request.env.remote_addr
try:
    hosts = (http_host, socket.gethostname(),
             socket.gethostbyname(http_host),
             '::1', '127.0.0.1', '::ffff:127.0.0.1')
except:
    hosts = (http_host, )

if request.is_https:
    session.secure()
elif (remote_addr not in hosts) and (remote_addr != "127.0.0.1") and \
    (request.function != 'manage'):
    raise HTTP(200, T('appadmin is disabled because insecure channel'))

if request.function == 'manage':
    if not 'auth' in globals() or not request.args:
        redirect(URL(request.controller, 'index'))
    manager_action = auth.settings.manager_actions.get(request.args(0), None)
    if manager_action is None and request.args(0) == 'auth':
        manager_action = dict(role=auth.settings.auth_manager_role,
                              heading=T('Manage Access Control'),
                              tables=[auth.table_user(),
                                      auth.table_group(),
                                      auth.table_permission()])
    manager_role = manager_action.get('role', None) if manager_action else None
    auth.requires_membership(manager_role)(lambda: None)()
    menu = False
elif (request.application == 'admin' and not session.authorized) or \
        (request.application != 'admin' and not gluon.fileutils.check_credentials(request)):
    redirect(URL('admin', 'default', 'index',
                 vars=dict(send=URL(args=request.args, vars=request.vars))))
else:
    response.subtitle = T('Database Administration (appadmin)')
    menu = True

ignore_rw = True
response.view = 'appadmin.html'
if menu:
    response.menu = [[T('design'), False, URL('admin', 'default', 'design',
                 args=[request.application])], [T('db'), False,
                 URL('index')], [T('state'), False,
                 URL('state')], [T('cache'), False,
                 URL('ccache')]]

# ##########################################################
# ## auxiliary functions
# ###########################################################

if False and request.tickets_db:
    from gluon.restricted import TicketStorage
    ts = TicketStorage()
    ts._get_table(request.tickets_db, ts.tablename, request.application)

def get_databases(request):
    dbs = {}
    for (key, value) in global_env.items():
        cond = False
        try:
            cond = isinstance(value, GQLDB)
        except:
            cond = isinstance(value, SQLDB)
        if cond:
            dbs[key] = value
    return dbs

databases = get_databases(None)

def eval_in_global_env(text):
    exec ('_ret=%s' % text, {}, global_env)
    return global_env['_ret']


def get_database(request):
    if request.args and request.args[0] in databases:
        return eval_in_global_env(request.args[0])
    else:
        session.flash = T('invalid request')
        redirect(URL('index'))

def get_table(request):
    db = get_database(request)
    if len(request.args) > 1 and request.args[1] in db.tables:
        return (db, request.args[1])
    else:
        session.flash = T('invalid request')
        redirect(URL('index'))


def get_query(request):
    try:
        return eval_in_global_env(request.vars.query)
    except Exception:
        return None


def query_by_table_type(tablename, db, request=request):
    keyed = hasattr(db[tablename], '_primarykey')
    if keyed:
        firstkey = db[tablename][db[tablename]._primarykey[0]]
        cond = '>0'
        if firstkey.type in ['string', 'text']:
            cond = '!=""'
        qry = '%s.%s.%s%s' % (
            request.args[0], request.args[1], firstkey.name, cond)
    else:
        qry = '%s.%s.id>0' % tuple(request.args[:2])
    return qry


# ##########################################################
# ## list all databases and tables
# ###########################################################
def index():
    return dict(databases=databases)


# ##########################################################
# ## insert a new record
# ###########################################################


def insert():
    (db, table) = get_table(request)
    form = SQLFORM(db[table], ignore_rw=ignore_rw)
    if form.accepts(request.vars, session):
        response.flash = T('new record inserted')
    return dict(form=form, table=db[table])


# ##########################################################
# ## list all records in table and insert new record
# ###########################################################


def download():
    import os
    db = get_database(request)
    return response.download(request, db)


def csv():
    import gluon.contenttype
    response.headers['Content-Type'] = \
        gluon.contenttype.contenttype('.csv')
    db = get_database(request)
    query = get_query(request)
    if not query:
        return None
    response.headers['Content-disposition'] = 'attachment; filename=%s_%s.csv'\
        % tuple(request.vars.query.split('.')[:2])
    return str(db(query, ignore_common_filters=True).select())


def import_csv(table, file):
    table.import_from_csv_file(file)


def select():
    import re
    db = get_database(request)
    dbname = request.args[0]
    try:
        is_imap = db._uri.startswith("imap://")
    except (KeyError, AttributeError, TypeError):
        is_imap = False
    regex = re.compile('(?P<table>\w+)\.(?P<field>\w+)=(?P<value>\d+)')
    if len(request.args) > 1 and hasattr(db[request.args[1]], '_primarykey'):
        regex = re.compile('(?P<table>\w+)\.(?P<field>\w+)=(?P<value>.+)')
    if request.vars.query:
        match = regex.match(request.vars.query)
        if match:
            request.vars.query = '%s.%s.%s==%s' % (request.args[0],
                                                   match.group('table'), match.group('field'),
                                                   match.group('value'))
    else:
        request.vars.query = session.last_query
    query = get_query(request)
    if request.vars.start:
        start = int(request.vars.start)
    else:
        start = 0
    nrows = 0

    step = 100
    fields = []

    if is_imap:
        step = 3
 
    stop = start + step

    table = None
    rows = []
    orderby = request.vars.orderby
    if orderby:
        orderby = dbname + '.' + orderby
        if orderby == session.last_orderby:
            if orderby[0] == '~':
                orderby = orderby[1:]
            else:
                orderby = '~' + orderby
    session.last_orderby = orderby
    session.last_query = request.vars.query
    form = FORM(TABLE(TR(T('Query:'), '', INPUT(_style='width:400px',
                _name='query', _value=request.vars.query or '',
                requires=IS_NOT_EMPTY(
                    error_message=T("Cannot be empty")))), TR(T('Update:'),
                INPUT(_name='update_check', _type='checkbox',
                value=False), INPUT(_style='width:400px',
                _name='update_fields', _value=request.vars.update_fields
                                    or '')), TR(T('Delete:'), INPUT(_name='delete_check',
                _class='delete', _type='checkbox', value=False), ''),
                TR('', '', INPUT(_type='submit', _value=T('submit')))),
                _action=URL(r=request, args=request.args))

    tb = None
    if form.accepts(request.vars, formname=None):
        regex = re.compile(request.args[0] + '\.(?P<table>\w+)\..+')
        match = regex.match(form.vars.query.strip())
        if match:
            table = match.group('table')
        try:
            nrows = db(query, ignore_common_filters=True).count()
            if form.vars.update_check and form.vars.update_fields:
                db(query, ignore_common_filters=True).update(
                    **eval_in_global_env('dict(%s)' % form.vars.update_fields))
                response.flash = T('%s %%{row} updated', nrows)
            elif form.vars.delete_check:
                db(query, ignore_common_filters=True).delete()
                response.flash = T('%s %%{row} deleted', nrows)
            nrows = db(query, ignore_common_filters=True).count()

            if is_imap:
                fields = [db[table][name] for name in
                    ("id", "uid", "created", "to",
                     "sender", "subject")]
            if orderby:
                rows = db(query, ignore_common_filters=True).select(
                              *fields, limitby=(start, stop),
                              orderby=eval_in_global_env(orderby))
            else:
                rows = db(query, ignore_common_filters=True).select(
                    *fields, limitby=(start, stop))
        except Exception, e:
            import traceback
            tb = traceback.format_exc()
            (rows, nrows) = ([], 0)
            response.flash = DIV(T('Invalid Query'), PRE(str(e)))
    # begin handle upload csv
    csv_table = table or request.vars.table
    if csv_table:
        formcsv = FORM(str(T('or import from csv file')) + " ",
                       INPUT(_type='file', _name='csvfile'),
                       INPUT(_type='hidden', _value=csv_table, _name='table'),
                       INPUT(_type='submit', _value=T('import')))
    else:
        formcsv = None
    if formcsv and formcsv.process().accepted:
        try:
            import_csv(db[request.vars.table],
                       request.vars.csvfile.file)
            response.flash = T('data uploaded')
        except Exception, e:
            response.flash = DIV(T('unable to parse csv file'), PRE(str(e)))
    # end handle upload csv

    return dict(
        form=form,
        table=table,
        start=start,
        stop=stop,
        step=step,
        nrows=nrows,
        rows=rows,
        query=request.vars.query,
        formcsv=formcsv,
        tb=tb
    )


# ##########################################################
# ## edit delete one record
# ###########################################################


def update():
    (db, table) = get_table(request)
    keyed = hasattr(db[table], '_primarykey')
    record = None
    db[table]._common_filter = None
    if keyed:
        key = [f for f in request.vars if f in db[table]._primarykey]
        if key:
            record = db(db[table][key[0]] == request.vars[key[
                        0]]).select().first()
    else:
        record = db(db[table].id == request.args(
            2)).select().first()

    if not record:
        qry = query_by_table_type(table, db)
        session.flash = T('record does not exist')
        redirect(URL('select', args=request.args[:1],
                     vars=dict(query=qry)))

    if keyed:
        for k in db[table]._primarykey:
            db[table][k].writable = False

    form = SQLFORM(
        db[table], record, deletable=True, delete_label=T('Check to delete'),
        ignore_rw=ignore_rw and not keyed,
        linkto=URL('select',
                   args=request.args[:1]), upload=URL(r=request,
                                                      f='download', args=request.args[:1]))

    if form.accepts(request.vars, session):
        session.flash = T('done!')
        qry = query_by_table_type(table, db)
        redirect(URL('select', args=request.args[:1],
                 vars=dict(query=qry)))
    return dict(form=form, table=db[table])


# ##########################################################
# ## get global variables
# ###########################################################


def state():
    return dict()


def ccache():
    if is_gae:
        form = FORM(
            P(TAG.BUTTON(T("Clear CACHE?"), _type="submit", _name="yes", _value="yes")))
    else:
        cache.ram.initialize()
        cache.disk.initialize()

        form = FORM(
            P(TAG.BUTTON(
                T("Clear CACHE?"), _type="submit", _name="yes", _value="yes")),
            P(TAG.BUTTON(
                T("Clear RAM"), _type="submit", _name="ram", _value="ram")),
            P(TAG.BUTTON(
                T("Clear DISK"), _type="submit", _name="disk", _value="disk")),
        )

    if form.accepts(request.vars, session):
        session.flash = ""
        if is_gae:
            if request.vars.yes:
                cache.ram.clear()
                session.flash += T("Cache Cleared")
        else:
            clear_ram = False
            clear_disk = False
            if request.vars.yes:
                clear_ram = clear_disk = True
            if request.vars.ram:
                clear_ram = True
            if request.vars.disk:
                clear_disk = True
            if clear_ram:
                cache.ram.clear()
                session.flash += T("Ram Cleared")
            if clear_disk:
                cache.disk.clear()
                session.flash += T("Disk Cleared")
        redirect(URL(r=request))

    try:
        from guppy import hpy
        hp = hpy()
    except ImportError:
        hp = False

    import shelve
    import os
    import copy
    import time
    import math
    from gluon import portalocker

    ram = {
        'entries': 0,
        'bytes': 0,
        'objects': 0,
        'hits': 0,
        'misses': 0,
        'ratio': 0,
        'oldest': time.time(),
        'keys': []
    }

    disk = copy.copy(ram)
    total = copy.copy(ram)
    disk['keys'] = []
    total['keys'] = []

    def GetInHMS(seconds):
        hours = math.floor(seconds / 3600)
        seconds -= hours * 3600
        minutes = math.floor(seconds / 60)
        seconds -= minutes * 60
        seconds = math.floor(seconds)

        return (hours, minutes, seconds)

    if is_gae:
        gae_stats = cache.ram.client.get_stats()
        try:
            gae_stats['ratio'] = ((gae_stats['hits'] * 100) /
                (gae_stats['hits'] + gae_stats['misses']))
        except ZeroDivisionError:
            gae_stats['ratio'] = T("?")
        gae_stats['oldest'] = GetInHMS(time.time() - gae_stats['oldest_item_age'])
        total.update(gae_stats)
    else:
        for key, value in cache.ram.storage.iteritems():
            if isinstance(value, dict):
                ram['hits'] = value['hit_total'] - value['misses']
                ram['misses'] = value['misses']
                try:
                    ram['ratio'] = ram['hits'] * 100 / value['hit_total']
                except (KeyError, ZeroDivisionError):
                    ram['ratio'] = 0
            else:
                if hp:
                    ram['bytes'] += hp.iso(value[1]).size
                    ram['objects'] += hp.iso(value[1]).count
                ram['entries'] += 1
                if value[0] < ram['oldest']:
                    ram['oldest'] = value[0]
                ram['keys'].append((key, GetInHMS(time.time() - value[0])))

        for key in cache.disk.storage:
            value = cache.disk.storage[key]
            if isinstance(value, dict):
                disk['hits'] = value['hit_total'] - value['misses']
                disk['misses'] = value['misses']
                try:
                    disk['ratio'] = disk['hits'] * 100 / value['hit_total']
                except (KeyError, ZeroDivisionError):
                    disk['ratio'] = 0
            else:
                if hp:
                    disk['bytes'] += hp.iso(value[1]).size
                    disk['objects'] += hp.iso(value[1]).count
                disk['entries'] += 1
                if value[0] < disk['oldest']:
                    disk['oldest'] = value[0]
                disk['keys'].append((key, GetInHMS(time.time() - value[0])))

        total['entries'] = ram['entries'] + disk['entries']
        total['bytes'] = ram['bytes'] + disk['bytes']
        total['objects'] = ram['objects'] + disk['objects']
        total['hits'] = ram['hits'] + disk['hits']
        total['misses'] = ram['misses'] + disk['misses']
        total['keys'] = ram['keys'] + disk['keys']
        try:
            total['ratio'] = total['hits'] * 100 / (total['hits'] +
                                                total['misses'])
        except (KeyError, ZeroDivisionError):
            total['ratio'] = 0

        if disk['oldest'] < ram['oldest']:
            total['oldest'] = disk['oldest']
        else:
            total['oldest'] = ram['oldest']

        ram['oldest'] = GetInHMS(time.time() - ram['oldest'])
        disk['oldest'] = GetInHMS(time.time() - disk['oldest'])
        total['oldest'] = GetInHMS(time.time() - total['oldest'])

    def key_table(keys):
        return TABLE(
            TR(TD(B(T('Key'))), TD(B(T('Time in Cache (h:m:s)')))),
            *[TR(TD(k[0]), TD('%02d:%02d:%02d' % k[1])) for k in keys],
            **dict(_class='cache-keys',
                   _style="border-collapse: separate; border-spacing: .5em;"))

    if not is_gae:
        ram['keys'] = key_table(ram['keys'])
        disk['keys'] = key_table(disk['keys'])
        total['keys'] = key_table(total['keys'])

    return dict(form=form, total=total,
                ram=ram, disk=disk, object_stats=hp != False)


def table_template(table):
    from gluon.html import TR, TD, TABLE, TAG

    def FONT(*args, **kwargs):
        return TAG.font(*args, **kwargs)

    def types(field):
        f_type = field.type
        if not isinstance(f_type,str):
            return ' '
        elif f_type == 'string':
            return field.length
        elif f_type == 'id':
            return B('pk')
        elif f_type.startswith('reference') or \
                f_type.startswith('list:reference'):
            return B('fk')
        else:
            return ' '

    # This is horribe HTML but the only one graphiz understands
    rows = []
    cellpadding = 4
    color = "#000000"
    bgcolor = "#FFFFFF"
    face = "Helvetica"
    face_bold = "Helvetica Bold"
    border = 0

    rows.append(TR(TD(FONT(table, _face=face_bold, _color=bgcolor),
                           _colspan=3, _cellpadding=cellpadding,
                           _align="center", _bgcolor=color)))
    for row in db[table]:
        rows.append(TR(TD(FONT(row.name, _color=color, _face=face_bold),
                              _align="left", _cellpadding=cellpadding,
                              _border=border),
                       TD(FONT(row.type, _color=color, _face=face),
                               _align="left", _cellpadding=cellpadding,
                               _border=border),
                       TD(FONT(types(row), _color=color, _face=face),
                               _align="center", _cellpadding=cellpadding,
                               _border=border)))
    return "< %s >" % TABLE(*rows, **dict(_bgcolor=bgcolor, _border=1,
                                          _cellborder=0, _cellspacing=0)
                             ).xml()


def bg_graph_model():
    graph = pgv.AGraph(layout='dot',  directed=True,  strict=False,  rankdir='LR')

    subgraphs = dict()
    for tablename in db.tables:
        if hasattr(db[tablename],'_meta_graphmodel'):
            meta_graphmodel = db[tablename]._meta_graphmodel
        else:
            meta_graphmodel = dict(group=request.application, color='#ECECEC')

        group = meta_graphmodel['group'].replace(' ', '')
        if not subgraphs.has_key(group):
            subgraphs[group] = dict(meta=meta_graphmodel, tables=[])
            subgraphs[group]['tables'].append(tablename)
        else:
            subgraphs[group]['tables'].append(tablename)

        graph.add_node(tablename, name=tablename, shape='plaintext',
                       label=table_template(tablename))

    for n, key in enumerate(subgraphs.iterkeys()):
        graph.subgraph(nbunch=subgraphs[key]['tables'],
                    name='cluster%d' % n,
                    style='filled',
                    color=subgraphs[key]['meta']['color'],
                    label=subgraphs[key]['meta']['group'])

    for tablename in db.tables:
        for field in db[tablename]:
            f_type = field.type
            if isinstance(f_type,str) and (
                f_type.startswith('reference') or
                f_type.startswith('list:reference')):
                referenced_table = f_type.split()[1].split('.')[0]
                n1 = graph.get_node(tablename)
                n2 = graph.get_node(referenced_table)
                graph.add_edge(n1, n2, color="#4C4C4C", label='')

    graph.layout()
    if not request.args:
        response.headers['Content-Type'] = 'image/png'
        return graph.draw(format='png', prog='dot')
    else:
        response.headers['Content-Disposition']='attachment;filename=graph.%s'%request.args(0)
        if request.args(0) == 'dot':
            return graph.string()
        else:
            return graph.draw(format=request.args(0), prog='dot')

def graph_model():
    return dict(databases=databases, pgv=pgv)

def manage():
    tables = manager_action['tables']
    if isinstance(tables[0], str):
        db = manager_action.get('db', auth.db)
        db = globals()[db] if isinstance(db, str) else db
        tables = [db[table] for table in tables]
    if request.args(0) == 'auth':
        auth.table_user()._plural = T('Users')
        auth.table_group()._plural = T('Roles')
        auth.table_membership()._plural = T('Memberships')
        auth.table_permission()._plural = T('Permissions')
    if request.extension != 'load':
        return dict(heading=manager_action.get('heading',
                    T('Manage %(action)s') % dict(action=request.args(0).replace('_', ' ').title())),
                    tablenames=[table._tablename for table in tables],
                    labels=[table._plural.title() for table in tables])

    table = tables[request.args(1, cast=int)]
    formname = '%s_grid' % table._tablename
    linked_tables = orderby = None
    if request.args(0) == 'auth':
        auth.table_group()._id.readable = \
        auth.table_membership()._id.readable = \
        auth.table_permission()._id.readable = False
        auth.table_membership().user_id.label = T('User')
        auth.table_membership().group_id.label = T('Role')
        auth.table_permission().group_id.label = T('Role')
        auth.table_permission().name.label = T('Permission')
        if table == auth.table_user():
            linked_tables=[auth.settings.table_membership_name]
        elif table == auth.table_group():
            orderby = 'role' if not request.args(3) or '.group_id' not in request.args(3) else None
        elif table == auth.table_permission():
            orderby = 'group_id'
    kwargs = dict(user_signature=True, maxtextlength=1000,
                  orderby=orderby, linked_tables=linked_tables)
    smartgrid_args = manager_action.get('smartgrid_args', {})
    kwargs.update(**smartgrid_args.get('DEFAULT', {}))
    kwargs.update(**smartgrid_args.get(table._tablename, {}))
    grid = SQLFORM.smartgrid(table, args=request.args[:2], formname=formname, **kwargs)
    return grid

def hooks():
    import functools
    import inspect
    list_op=['_%s_%s' %(h,m) for h in ['before', 'after'] for m in ['insert','update','delete']]
    tables=[]
    with_build_it=False
    for db_str in sorted(databases):
        db = databases[db_str]
        for t in db.tables:
            method_hooks=[]
            for op in list_op:
                functions = []
                for f in getattr(db[t], op):
                    if hasattr(f, '__call__'):
                        try:
                            if isinstance(f, (functools.partial)):
                                f = f.func
                            filename = inspect.getsourcefile(f)
                            details = {'funcname':f.__name__,
                                       'filename':filename[len(request.folder):] if request.folder in filename else None,
                                       'lineno': inspect.getsourcelines(f)[1]}
                            if details['filename']: # Built in functions as delete_uploaded_files are not editable
                                details['url'] = URL(a='admin',c='default',f='edit', args=[request['application'], details['filename']],vars={'lineno':details['lineno']})
                            if details['filename'] or with_build_it:
                                functions.append(details)
                        # compiled app and windows build don't support code inspection
                        except:
                            pass
                if len(functions):
                    method_hooks.append({'name':op, 'functions':functions})
            if len(method_hooks):
                tables.append({'name':"%s.%s" % (db_str,t), 'slug': IS_SLUG()("%s.%s" % (db_str,t))[0], 'method_hooks':method_hooks})
    # Render
    ul_main = UL(_class='nav nav-list')
    for t in tables:
        ul_main.append(A(t['name'], _onclick="collapse('a_%s')" % t['slug']))
        ul_t = UL(_class='nav nav-list', _id="a_%s" % t['slug'], _style='display:none')
        for op in t['method_hooks']:
            ul_t.append(LI (op['name']))
            ul_t.append(UL([LI(A(f['funcname'], _class="editor_filelink", _href=f['url']if 'url' in f else None, **{'_data-lineno':f['lineno']-1})) for f in op['functions']]))
        ul_main.append(ul_t)
    return ul_main
