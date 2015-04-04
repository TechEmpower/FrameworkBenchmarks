# -*- coding: utf-8 -*-
# this file is released under public domain and you can use without limitations

if MULTI_USER_MODE:
    db = DAL('sqlite://storage.sqlite')       # if not, use SQLite or other DB
    from gluon.tools import *
    auth = Auth(
        globals(), db)                      # authentication/authorization
    crud = Crud(
        globals(), db)                      # for CRUD helpers using auth
    service = Service(
        globals())                   # for json, xml, jsonrpc, xmlrpc, amfrpc
    plugins = PluginManager()

    mail = auth.settings.mailer
    mail.settings.server = EMAIL_SERVER
    mail.settings.sender = EMAIL_SENDER
    mail.settings.login = EMAIL_LOGIN

    auth.settings.extra_fields['auth_user'] = \
        [Field('is_manager', 'boolean', default=False, writable=False)]
    auth.define_tables()                           # creates all needed tables
    auth.settings.registration_requires_verification = False
    auth.settings.registration_requires_approval = True
    auth.settings.reset_password_requires_verification = True

    db.define_table('app', Field('name'), Field('owner', db.auth_user))

if not session.authorized and MULTI_USER_MODE:
    if auth.user and not request.function == 'user':
        session.authorized = True
    elif not request.function == 'user':
        redirect(URL('default', 'user/login'))


def is_manager():
    if not MULTI_USER_MODE:
        return True
    elif auth.user and (auth.user.id == 1 or auth.user.is_manager):
        return True
    else:
        return False
