import base64
import os
import time
from gluon import portalocker
from gluon.admin import apath
from gluon.fileutils import read_file
# ###########################################################
# ## make sure administrator is on localhost or https
# ###########################################################


http_host = request.env.http_host.split(':')[0]

if request.env.web2py_runtime_gae:
    session_db = DAL('gae')
    session.connect(request, response, db=session_db)
    hosts = (http_host, )
    is_gae = True
else:
    is_gae = False

if request.is_https:
    session.secure()
elif not request.is_local and not DEMO_MODE:
    raise HTTP(200, T('Admin is disabled because insecure channel'))

try:
    _config = {}
    port = int(request.env.server_port or 0)
    restricted(
        read_file(apath('../parameters_%i.py' % port, request)), _config)

    if not 'password' in _config or not _config['password']:
        raise HTTP(200, T('admin disabled because no admin password'))
except IOError:
    import gluon.fileutils
    if is_gae:
        if gluon.fileutils.check_credentials(request):
            session.authorized = True
            session.last_time = time.time()
        else:
            raise HTTP(200,
                       T('admin disabled because not supported on google app engine'))
    else:
        raise HTTP(
            200, T('admin disabled because unable to access password file'))


def verify_password(password):
    session.pam_user = None
    if DEMO_MODE:
        return True
    elif not _config.get('password'):
        return False
    elif _config['password'].startswith('pam_user:'):
        session.pam_user = _config['password'][9:].strip()
        import gluon.contrib.pam
        return gluon.contrib.pam.authenticate(session.pam_user, password)
    else:
        return _config['password'] == CRYPT()(password)[0]


# ###########################################################
# ## handle brute-force login attacks
# ###########################################################

deny_file = os.path.join(request.folder, 'private', 'hosts.deny')
allowed_number_of_attempts = 5
expiration_failed_logins = 3600


def read_hosts_deny():
    import datetime
    hosts = {}
    if os.path.exists(deny_file):
        hosts = {}
        f = open(deny_file, 'r')
        portalocker.lock(f, portalocker.LOCK_SH)
        for line in f.readlines():
            if not line.strip() or line.startswith('#'):
                continue
            fields = line.strip().split()
            if len(fields) > 2:
                hosts[fields[0].strip()] = (  # ip
                    int(fields[1].strip()),  # n attemps
                    int(fields[2].strip())   # last attempts
                    )
        portalocker.unlock(f)
        f.close()
    return hosts


def write_hosts_deny(denied_hosts):
    f = open(deny_file, 'w')
    portalocker.lock(f, portalocker.LOCK_EX)
    for key, val in denied_hosts.items():
        if time.time() - val[1] < expiration_failed_logins:
            line = '%s %s %s\n' % (key, val[0], val[1])
            f.write(line)
    portalocker.unlock(f)
    f.close()


def login_record(success=True):
    denied_hosts = read_hosts_deny()
    val = (0, 0)
    if success and request.client in denied_hosts:
        del denied_hosts[request.client]
    elif not success and not request.is_local:
        val = denied_hosts.get(request.client, (0, 0))
        if time.time() - val[1] < expiration_failed_logins \
            and val[0] >= allowed_number_of_attempts:
            return val[0]  # locked out
        time.sleep(2 ** val[0])
        val = (val[0] + 1, int(time.time()))
        denied_hosts[request.client] = val
    write_hosts_deny(denied_hosts)
    return val[0]


# ###########################################################
# ## session expiration
# ###########################################################

t0 = time.time()
if session.authorized:

    if session.last_time and session.last_time < t0 - EXPIRATION:
        session.flash = T('session expired')
        session.authorized = False
    else:
        session.last_time = t0


if request.vars.is_mobile in ('true', 'false', 'auto'):
    session.is_mobile = request.vars.is_mobile or 'auto'
if request.controller == 'default' and request.function == 'index':
    if not request.vars.is_mobile:
        session.is_mobile = 'auto'
if not session.is_mobile:
    session.is_mobile = 'auto'
if session.is_mobile == 'true':
    is_mobile = True
elif session.is_mobile == 'false':
    is_mobile = False
else:
    is_mobile = request.user_agent().get('is_mobile',False)

if DEMO_MODE:
    session.authorized = True
    session.forget()

if request.controller == "webservices":
    basic = request.env.http_authorization
    if not basic or not basic[:6].lower() == 'basic ':
        raise HTTP(401, "Wrong credentials")
    (username, password) = base64.b64decode(basic[6:]).split(':')
    if not verify_password(password) or MULTI_USER_MODE:
        time.sleep(10)
        raise HTTP(403, "Not authorized")
elif not session.authorized and not \
    (request.controller + '/' + request.function in
     ('default/index', 'default/user', 'plugin_jqmobile/index', 'plugin_jqmobile/about')):

    if request.env.query_string:
        query_string = '?' + request.env.query_string
    else:
        query_string = ''

    if request.env.web2py_original_uri:
        url = request.env.web2py_original_uri
    else:
        url = request.env.path_info + query_string
    redirect(URL(request.application, 'default', 'index', vars=dict(send=url)))
elif session.authorized and \
     request.controller == 'default' and \
     request.function == 'index':
    redirect(URL(request.application, 'default', 'site'))

if request.controller == 'appadmin' and DEMO_MODE:
    session.flash = 'Appadmin disabled in demo mode'
    redirect(URL('default', 'sites'))
