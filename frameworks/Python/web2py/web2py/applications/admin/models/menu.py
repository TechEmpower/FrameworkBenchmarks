# ###########################################################
# ## generate menu
# ###########################################################

_a = request.application
_c = request.controller
_f = request.function
response.title = '%s %s' % (_f, '/'.join(request.args))
response.subtitle = 'admin'
response.menu = [(T('Site'), _f == 'site', URL(_a, 'default', 'site'))]

if request.vars.app or request.args:
    _t = request.vars.app or request.args[0]
    response.menu.append((T('Edit'), _c == 'default' and _f == 'design',
                         URL(_a, 'default', 'design', args=_t)))
    response.menu.append((T('About'), _c == 'default' and _f == 'about',
                         URL(_a, 'default', 'about', args=_t,)))
    response.menu.append((T('Errors'), _c == 'default' and _f == 'errors',
                         URL(_a, 'default', 'errors', args=_t)))
    response.menu.append((T('Versioning'),
                          _c == 'mercurial' and _f == 'commit',
                          URL(_a, 'mercurial', 'commit', args=_t)))

if os.path.exists('applications/examples'):
    response.menu.append(
        (T('Help'), False, URL('examples', 'default', 'documentation')))
else:
    response.menu.append((T('Help'), False, 'http://web2py.com/examples/default/documentation'))

if not session.authorized:
    response.menu = [(T('Login'), True, URL('site'))]
else:
    response.menu.append((T('Logout'), False,
                          URL(_a, 'default', f='logout')))
    response.menu.append((T('Debug'), False,
                          URL(_a, 'debug', 'interact')))
