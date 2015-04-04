#!/usr/bin/env python
# coding: utf8

"""
    OpenID authentication for web2py

    Allowed using OpenID login together with web2py built-in login.

    By default, to support OpenID login, put this in your db.py

    >>> from gluon.contrib.login_methods.openid_auth import OpenIDAuth
    >>> auth.settings.login_form = OpenIDAuth(auth)

    To show OpenID list in user profile, you can add the following code
    before the end of function user() of your_app/controllers/default.py

    +     if (request.args and request.args(0) == "profile"):
    +         form = DIV(form, openid_login_form.list_user_openids())
        return dict(form=form, login_form=login_form, register_form=register_form, self_registration=self_registration)

    More detail in the description of the class OpenIDAuth.

    Requirements:
        python-openid version 2.2.5 or later

    Reference:
        * w2p openID
          http://w2popenid.appspot.com/init/default/wiki/w2popenid
        * RPX and web2py auth module
          http://www.web2pyslices.com/main/slices/take_slice/28
        * built-in file: gluon/contrib/login_methods/rpx_account.py
        * built-in file: gluon/tools.py (Auth class)
"""
import time
from datetime import datetime, timedelta

from gluon import *
from gluon.storage import Storage, Messages

try:
    import openid.consumer.consumer
    from openid.association import Association
    from openid.store.interface import OpenIDStore
    from openid.extensions.sreg import SRegRequest, SRegResponse
    from openid.store import nonce
    from openid.consumer.discover import DiscoveryFailure
except ImportError, err:
    raise ImportError("OpenIDAuth requires python-openid package")

DEFAULT = lambda: None


class OpenIDAuth(object):
    """
    OpenIDAuth

    It supports the logout_url, implementing the get_user and login_form
    for cas usage of gluon.tools.Auth.

    It also uses the ExtendedLoginForm to allow the OpenIDAuth login_methods
    combined with the standard logon/register procedure.

    It uses OpenID Consumer when render the form and begins the OpenID
    authentication.

    Example: (put these code after auth.define_tables() in your models.)

    auth = Auth(globals(), db)                # authentication/authorization
    ...
    auth.define_tables()                      # creates all needed tables
    ...

    #include in your model after auth has been defined
    from gluon.contrib.login_methods.openid_auth import OpenIDAuth
    openid_login_form = OpenIDAuth(request, auth, db)

    from gluon.contrib.login_methods.extended_login_form import ExtendedLoginForm
    extended_login_form = ExtendedLoginForm(request, auth, openid_login_form,
                                            signals=['oid','janrain_nonce'])

    auth.settings.login_form = extended_login_form
    """

    def __init__(self, auth):
        self.auth = auth
        self.db = auth.db

        request = current.request
        self.nextvar = '_next'
        self.realm = 'http://%s' % request.env.http_host
        self.login_url = URL(r=request, f='user', args=['login'])
        self.return_to_url = self.realm + self.login_url

        self.table_alt_logins_name = "alt_logins"
        if not auth.settings.table_user:
            raise
        self.table_user = self.auth.settings.table_user
        self.openid_expiration = 15  # minutes

        self.messages = self._define_messages()

        if not self.table_alt_logins_name in self.db.tables:
            self._define_alt_login_table()

    def _define_messages(self):
        messages = Messages(current.T)
        messages.label_alt_login_username = 'Sign-in with OpenID: '
        messages.label_add_alt_login_username = 'Add a new OpenID: '
        messages.submit_button = 'Sign in'
        messages.submit_button_add = 'Add'
        messages.a_delete = 'Delete'
        messages.comment_openid_signin = 'What is OpenID?'
        messages.comment_openid_help_title = 'Start using your OpenID'
        messages.comment_openid_help_url = 'http://openid.net/get-an-openid/start-using-your-openid/'
        messages.openid_fail_discover = 'Failed to discover OpenID service. Check your OpenID or "More about OpenID"?'
        messages.flash_openid_expired = 'OpenID expired. Please login or authenticate OpenID again. Sorry for the inconvenient.'
        messages.flash_openid_associated = 'OpenID associated'
        messages.flash_associate_openid = 'Please login or register an account for this OpenID.'
        messages.p_openid_not_registered = "This Open ID haven't be registered. " \
            + "Please login to associate with it or register an account for it."
        messages.flash_openid_authenticated = 'OpenID authenticated successfully.'
        messages.flash_openid_fail_authentication = 'OpenID authentication failed. (Error message: %s)'
        messages.flash_openid_canceled = 'OpenID authentication canceled by user.'
        messages.flash_openid_need_setup = 'OpenID authentication needs to be setup by the user with the provider first.'
        messages.h_openid_login = 'OpenID Login'
        messages.h_openid_list = 'OpenID List'
        return messages

    def _define_alt_login_table(self):
        """
        Define the OpenID login table.
        Note: oidtype is what I used for our project.
              We're going to support 'fackbook' and
              'plurk' alternate login methods.
              Otherwise it's always 'openid' and you
              may not need it. This should be easy to changed.
              (Just remove the field of "type" and remove the
              "and db.alt_logins.oidtype == type_"
              in _find_matched_openid function)
        """
        db = self.db
        table = db.define_table(
            self.table_alt_logins_name,
            Field('username', length=512, default=''),
            Field('oidtype', length=128, default='openid', readable=False),
            Field('oiduser', self.table_user, readable=False),
        )
        table.username.requires = IS_NOT_IN_DB(db, table.username)
        self.table_alt_logins = table

    def logout_url(self, next):
        """
        Delete the w2popenid record in session as logout
        """
        if current.session.w2popenid:
            del(current.session.w2popenid)
        return next

    def login_form(self):
        """
        Start to process the OpenID response if 'janrain_nonce' in request parameters
        and not processed yet. Else return the OpenID form for login.
        """
        request = current.request
        if 'janrain_nonce' in request.vars and not self._processed():
            self._process_response()
            return self.auth()
        return self._form()

    def get_user(self):
        """
        It supports the logout_url, implementing the get_user and login_form
        for cas usage of gluon.tools.Auth.
        """
        request = current.request
        args = request.args

        if args[0] == 'logout':
            return True  # Let logout_url got called

        if current.session.w2popenid:
            w2popenid = current.session.w2popenid
            db = self.db
            if (w2popenid.ok is True and w2popenid.oid):  # OpenID authenticated
                if self._w2popenid_expired(w2popenid):
                    del(current.session.w2popenid)
                    flash = self.messages.flash_openid_expired
                    current.session.warning = flash
                    redirect(self.auth.settings.login_url)
                oid = self._remove_protocol(w2popenid.oid)
                alt_login = self._find_matched_openid(db, oid)

                nextvar = self.nextvar
                # This OpenID not in the database. If user logged in then add it
                # into database, else ask user to login or register.
                if not alt_login:
                    if self.auth.is_logged_in():
                        # TODO: ask first maybe
                        self._associate_user_openid(self.auth.user, oid)
                        if current.session.w2popenid:
                            del(current.session.w2popenid)
                        current.session.flash = self.messages.flash_openid_associated
                        if nextvar in request.vars:
                            redirect(request.vars[nextvar])
                        redirect(self.auth.settings.login_next)

                    if nextvar not in request.vars:
                        # no next var, add it and do login again
                        # so if user login or register can go back here to associate the OpenID
                        redirect(URL(r=request,
                                     args=['login'],
                                     vars={nextvar: self.login_url}))
                    self.login_form = self._form_with_notification()
                    current.session.flash = self.messages.flash_associate_openid
                    return None  # need to login or register to associate this openid

                # Get existed OpenID user
                user = db(
                    self.table_user.id == alt_login.oiduser).select().first()
                if user:
                    if current.session.w2popenid:
                        del(current.session.w2popenid)
                if 'username' in self.table_user.fields():
                    username = 'username'
                elif 'email' in self.table_user.fields():
                    username = 'email'
                return {username: user[username]} if user else None  # login success (almost)

        return None  # just start to login

    def _find_matched_openid(self, db, oid, type_='openid'):
        """
        Get the matched OpenID for given
        """
        query = (
            (db.alt_logins.username == oid) & (db.alt_logins.oidtype == type_))
        alt_login = db(query).select().first()  # Get the OpenID record
        return alt_login

    def _associate_user_openid(self, user, oid):
        """
        Associate the user logged in with given OpenID
        """
        # print "[DB] %s authenticated" % oid
        self.db.alt_logins.insert(username=oid, oiduser=user.id)

    def _form_with_notification(self):
        """
        Render the form for normal login with a notice of OpenID authenticated
        """
        form = DIV()
        # TODO: check when will happen
        if self.auth.settings.login_form in (self.auth, self):
            self.auth.settings.login_form = self.auth
            form = DIV(self.auth())

        register_note = DIV(P(self.messages.p_openid_not_registered))
        form.components.append(register_note)
        return lambda: form

    def _remove_protocol(self, oid):
        """
        Remove https:// or http:// from oid url
        """
        protocol = 'https://'
        if oid.startswith(protocol):
            oid = oid[len(protocol):]
            return oid
        protocol = 'http://'
        if oid.startswith(protocol):
            oid = oid[len(protocol):]
            return oid
        return oid

    def _init_consumerhelper(self):
        """
        Initialize the ConsumerHelper
        """
        if not hasattr(self, "consumerhelper"):
            self.consumerhelper = ConsumerHelper(current.session,
                                                 self.db)
        return self.consumerhelper

    def _form(self, style=None):
        form = DIV(H3(self.messages.h_openid_login), self._login_form(style))
        return form

    def _login_form(self,
                    openid_field_label=None,
                    submit_button=None,
                    _next=None,
                    style=None):
        """
        Render the form for OpenID login
        """
        def warning_openid_fail(session):
            session.warning = messages.openid_fail_discover

        style = style or """
background-attachment: scroll;
background-repeat: no-repeat;
background-image: url("http://wiki.openid.net/f/openid-16x16.gif");
background-position: 0% 50%;
background-color: transparent;
padding-left: 18px;
width: 400px;
"""
        style = style.replace("\n", "")

        request = current.request
        session = current.session
        messages = self.messages
        hidden_next_input = ""
        if _next == 'profile':
            profile_url = URL(r=request, f='user', args=['profile'])
            hidden_next_input = INPUT(
                _type="hidden", _name="_next", _value=profile_url)
        form = FORM(
            openid_field_label or self.messages.label_alt_login_username,
            INPUT(_type="input", _name="oid",
                  requires=IS_NOT_EMPTY(
                  error_message=messages.openid_fail_discover),
                  _style=style),
            hidden_next_input,
            INPUT(_type="submit",
                  _value=submit_button or messages.submit_button),
            " ",
            A(messages.comment_openid_signin,
              _href=messages.comment_openid_help_url,
              _title=messages.comment_openid_help_title,
              _class='openid-identifier',
              _target="_blank"),
            _action=self.login_url
        )
        if form.accepts(request.vars, session):
            oid = request.vars.oid
            consumerhelper = self._init_consumerhelper()
            url = self.login_url
            return_to_url = self.return_to_url
            if not oid:
                warning_openid_fail(session)
                redirect(url)
            try:
                if '_next' in request.vars:
                    return_to_url = self.return_to_url + \
                        '?_next=' + request.vars._next
                url = consumerhelper.begin(oid, self.realm, return_to_url)
            except DiscoveryFailure:
                warning_openid_fail(session)
            redirect(url)
        return form

    def _processed(self):
        """
        Check if w2popenid authentication is processed.
        Return True if processed else False.
        """
        processed = (hasattr(current.session, 'w2popenid') and
                     current.session.w2popenid.ok is True)
        return processed

    def _set_w2popenid_expiration(self, w2popenid):
        """
        Set expiration for OpenID authentication.
        """
        w2popenid.expiration = datetime.now(
        ) + timedelta(minutes=self.openid_expiration)

    def _w2popenid_expired(self, w2popenid):
        """
        Check if w2popenid authentication is expired.
        Return True if expired else False.
        """
        return (not w2popenid.expiration) or (datetime.now() > w2popenid.expiration)

    def _process_response(self):
        """
        Process the OpenID by ConsumerHelper.
        """
        request = current.request
        request_vars = request.vars
        consumerhelper = self._init_consumerhelper()
        process_status = consumerhelper.process_response(
            request_vars, self.return_to_url)
        if process_status == "success":
            w2popenid = current.session.w2popenid
            user_data = self.consumerhelper.sreg()
            current.session.w2popenid.ok = True
            self._set_w2popenid_expiration(w2popenid)
            w2popenid.user_data = user_data
            current.session.flash = self.messages.flash_openid_authenticated
        elif process_status == "failure":
            flash = self.messages.flash_openid_fail_authentication % consumerhelper.error_message
            current.session.warning = flash
        elif process_status == "cancel":
            current.session.warning = self.messages.flash_openid_canceled
        elif process_status == "setup_needed":
            current.session.warning = self.messages.flash_openid_need_setup

    def list_user_openids(self):
        messages = self.messages
        request = current.request
        if 'delete_openid' in request.vars:
            self.remove_openid(request.vars.delete_openid)

        query = self.db.alt_logins.oiduser == self.auth.user.id
        alt_logins = self.db(query).select()
        l = []
        for alt_login in alt_logins:
            username = alt_login.username
            delete_href = URL(r=request, f='user',
                              args=['profile'],
                              vars={'delete_openid': username})
            delete_link = A(messages.a_delete, _href=delete_href)
            l.append(LI(username, " ", delete_link))

        profile_url = URL(r=request, f='user', args=['profile'])
        #return_to_url = self.return_to_url + '?' + self.nextvar + '=' + profile_url
        openid_list = DIV(H3(messages.h_openid_list), UL(l),
                          self._login_form(
                              _next='profile',
                              submit_button=messages.submit_button_add,
                              openid_field_label=messages.label_add_alt_login_username)
                          )
        return openid_list

    def remove_openid(self, openid):
        query = self.db.alt_logins.username == openid
        self.db(query).delete()


class ConsumerHelper(object):
    """
    ConsumerHelper knows the python-openid and
    """

    def __init__(self, session, db):
        self.session = session
        store = self._init_store(db)
        self.consumer = openid.consumer.consumer.Consumer(session, store)

    def _init_store(self, db):
        """
        Initialize Web2pyStore
        """
        if not hasattr(self, "store"):
            store = Web2pyStore(db)
            session = self.session
            if 'w2popenid' not in session:
                session.w2popenid = Storage()
            self.store = store
        return self.store

    def begin(self, oid, realm, return_to_url):
        """
        Begin the OpenID authentication
        """
        w2popenid = self.session.w2popenid
        w2popenid.oid = oid
        auth_req = self.consumer.begin(oid)
        auth_req.addExtension(SRegRequest(required=['email', 'nickname']))
        url = auth_req.redirectURL(return_to=return_to_url, realm=realm)
        return url

    def process_response(self, request_vars, return_to_url):
        """
        Complete the process and
        """
        resp = self.consumer.complete(request_vars, return_to_url)
        if resp:
            if resp.status == openid.consumer.consumer.SUCCESS:
                self.resp = resp
                if hasattr(resp, "identity_url"):
                    self.session.w2popenid.oid = resp.identity_url
                return "success"
            if resp.status == openid.consumer.consumer.FAILURE:
                self.error_message = resp.message
                return "failure"
            if resp.status == openid.consumer.consumer.CANCEL:
                return "cancel"
            if resp.status == openid.consumer.consumer.SETUP_NEEDED:
                return "setup_needed"
        return "no resp"

    def sreg(self):
        """
        Try to get OpenID Simple Registation
        http://openid.net/specs/openid-simple-registration-extension-1_0.html
        """
        if self.resp:
            resp = self.resp
            sreg_resp = SRegResponse.fromSuccessResponse(resp)
            return sreg_resp.data if sreg_resp else None
        else:
            return None


class Web2pyStore(OpenIDStore):
    """
    Web2pyStore

    This class implements the OpenIDStore interface. OpenID stores take care
    of persisting nonces and associations. The Janrain Python OpenID library
    comes with implementations for file and memory storage. Web2pyStore uses
    the web2py db abstration layer. See the source code docs of OpenIDStore
    for a comprehensive description of this interface.
    """

    def __init__(self, database):
        self.database = database
        self.table_oid_associations_name = 'oid_associations'
        self.table_oid_nonces_name = 'oid_nonces'
        self._initDB()

    def _initDB(self):

        if self.table_oid_associations_name not in self.database:
            self.database.define_table(self.table_oid_associations_name,
                                       Field('server_url',
                                             'string', length=2047, required=True),
                                       Field('handle',
                                             'string', length=255, required=True),
                                       Field('secret', 'blob', required=True),
                                       Field('issued',
                                             'integer', required=True),
                                       Field('lifetime',
                                             'integer', required=True),
                                       Field('assoc_type',
                                             'string', length=64, required=True)
                                       )
        if self.table_oid_nonces_name not in self.database:
            self.database.define_table(self.table_oid_nonces_name,
                                       Field('server_url',
                                             'string', length=2047, required=True),
                                       Field('itimestamp',
                                             'integer', required=True),
                                       Field('salt', 'string',
                                             length=40, required=True)
                                       )

    def storeAssociation(self, server_url, association):
        """
        Store associations. If there already is one with the same
        server_url and handle in the table replace it.
        """

        db = self.database
        query = (db.oid_associations.server_url == server_url) & (
            db.oid_associations.handle == association.handle)
        db(query).delete()
        db.oid_associations.insert(server_url=server_url,
                                   handle=association.handle,
                                   secret=association.secret,
                                   issued=association.issued,
                                   lifetime=association.lifetime,
                                   assoc_type=association.assoc_type), 'insert ' * 10

    def getAssociation(self, server_url, handle=None):
        """
        Return the association for server_url and handle. If handle is
        not None return the latests associations for that server_url.
        Return None if no association can be found.
        """

        db = self.database
        query = (db.oid_associations.server_url == server_url)
        if handle:
            query &= (db.oid_associations.handle == handle)
        rows = db(query).select(orderby=db.oid_associations.issued)
        keep_assoc, _ = self._removeExpiredAssocations(rows)
        if len(keep_assoc) == 0:
            return None
        else:
            assoc = keep_assoc.pop(
            )  # pop the last one as it should be the latest one
            return Association(assoc['handle'],
                               assoc['secret'],
                               assoc['issued'],
                               assoc['lifetime'],
                               assoc['assoc_type'])

    def removeAssociation(self, server_url, handle):
        db = self.database
        query = (db.oid_associations.server_url == server_url) & (
            db.oid_associations.handle == handle)
        return db(query).delete() is not None

    def useNonce(self, server_url, timestamp, salt):
        """
        This method returns Falase if a nonce has been used before or its
        timestamp is not current.
        """

        db = self.database
        if abs(timestamp - time.time()) > nonce.SKEW:
            return False
        query = (db.oid_nonces.server_url == server_url) & (db.oid_nonces.itimestamp == timestamp) & (db.oid_nonces.salt == salt)
        if db(query).count() > 0:
            return False
        else:
            db.oid_nonces.insert(server_url=server_url,
                                 itimestamp=timestamp,
                                 salt=salt)
            return True

    def _removeExpiredAssocations(self, rows):
        """
        This helper function is not part of the interface. Given a list of
        association rows it checks which associations have expired and
        deletes them from the db. It returns a tuple of the form
        ([valid_assoc], no_of_expired_assoc_deleted).
        """

        db = self.database
        keep_assoc = []
        remove_assoc = []
        t1970 = time.time()
        for r in rows:
            if r['issued'] + r['lifetime'] < t1970:
                remove_assoc.append(r)
            else:
                keep_assoc.append(r)
        for r in remove_assoc:
            del db.oid_associations[r['id']]
        return (keep_assoc, len(remove_assoc))  # return tuple (list of valid associations, number of deleted associations)

    def cleanupNonces(self):
        """
        Remove expired nonce entries from DB and return the number
        of entries deleted.
        """

        db = self.database
        query = (db.oid_nonces.itimestamp < time.time() - nonce.SKEW)
        return db(query).delete()

    def cleanupAssociations(self):
        """
        Remove expired associations from db and return the number
        of entries deleted.
        """

        db = self.database
        query = (db.oid_associations.id > 0)
        return self._removeExpiredAssocations(db(query).select())[1]  # return number of assoc removed

    def cleanup(self):
        """
        This method should be run periodically to free the db from
        expired nonce and association entries.
        """

        return self.cleanupNonces(), self.cleanupAssociations()
