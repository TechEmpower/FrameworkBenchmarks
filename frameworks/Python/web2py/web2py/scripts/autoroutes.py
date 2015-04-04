# -*- coding: utf-8 -*-
'''
autoroutes writes routes for you based on a simpler routing
configuration file called routes.conf. Example:

----- BEGIN routes.conf-------
127.0.0.1   /examples/default
domain1.com /app1/default
domain2.com /app2/default
domain3.com /app3/default
----- END ----------

It maps a domain (the left-hand side) to an app (one app per domain),
and shortens the URLs for the app by removing the listed path prefix. That means:

http://domain1.com/index is mapped to /app1/default/index
http://domain2.com/index is mapped to /app2/default/index

It preserves admin, appadmin, static files, favicon.ico and robots.txt:

http://domain1.com/favicon.ico  /welcome/static/favicon.ico
http://domain1.com/robots.txt   /welcome/static/robots.txt
http://domain1.com/admin/...    /admin/...
http://domain1.com/appadmin/... /app1/appadmin/...
http://domain1.com/static/...   /app1/static/...

and vice-versa.

To use, cp scripts/autoroutes.py routes.py

and either edit the config string below, or set config = "" and edit routes.conf
'''

config = '''
127.0.0.1   /examples/default
domain1.com /app1/default
domain2.com /app2/default
domain3.com /app3/defcon3
'''
if not config.strip():
    try:
        config_file = open('routes.conf', 'r')
        try:
            config = config_file.read()
        finally:
            config_file.close()
    except:
        config = ''


def auto_in(apps):
    routes = [
        ('/robots.txt', '/welcome/static/robots.txt'),
        ('/favicon.ico', '/welcome/static/favicon.ico'),
        ('/admin$anything', '/admin$anything'),
    ]
    for domain, path in [x.strip().split() for x in apps.split('\n') if x.strip() and not x.strip().startswith('#')]:
        if not path.startswith('/'):
            path = '/' + path
        if path.endswith('/'):
            path = path[:-1]
        app = path.split('/')[1]
        routes += [
            ('.*:https?://(.*\.)?%s:$method /' % domain, '%s' % path),
            ('.*:https?://(.*\.)?%s:$method /static/$anything' %
             domain, '/%s/static/$anything' % app),
            ('.*:https?://(.*\.)?%s:$method /appadmin/$anything' %
             domain, '/%s/appadmin/$anything' % app),
            ('.*:https?://(.*\.)?%s:$method /$anything' %
             domain, '%s/$anything' % path),
        ]
    return routes


def auto_out(apps):
    routes = []
    for domain, path in [x.strip().split() for x in apps.split('\n') if x.strip() and not x.strip().startswith('#')]:
        if not path.startswith('/'):
            path = '/' + path
        if path.endswith('/'):
            path = path[:-1]
        app = path.split('/')[1]
        routes += [
            ('/%s/static/$anything' % app, '/static/$anything'),
            ('/%s/appadmin/$anything' % app, '/appadmin/$anything'),
            ('%s/$anything' % path, '/$anything'),
        ]
    return routes

routes_in = auto_in(config)
routes_out = auto_out(config)


def __routes_doctest():
    '''
    Dummy function for doctesting autoroutes.py.

    Use filter_url() to test incoming or outgoing routes;
    filter_err() for error redirection.

    filter_url() accepts overrides for method and remote host:
        filter_url(url, method='get', remote='0.0.0.0', out=False)

    filter_err() accepts overrides for application and ticket:
        filter_err(status, application='app', ticket='tkt')

    >>> filter_url('http://domain1.com/favicon.ico')
    'http://domain1.com/welcome/static/favicon.ico'
    >>> filter_url('https://domain2.com/robots.txt')
    'https://domain2.com/welcome/static/robots.txt'
    >>> filter_url('http://domain3.com/fcn')
    'http://domain3.com/app3/defcon3/fcn'
    >>> filter_url('http://127.0.0.1/fcn')
    'http://127.0.0.1/examples/default/fcn'
    >>> filter_url('HTTP://DOMAIN.COM/app/ctr/fcn')
    'http://domain.com/app/ctr/fcn'
    >>> filter_url('http://domain.com/app/ctr/fcn?query')
    'http://domain.com/app/ctr/fcn?query'
    >>> filter_url('http://otherdomain.com/fcn')
    'http://otherdomain.com/fcn'
    >>> regex_filter_out('/app/ctr/fcn')
    '/app/ctr/fcn'
    >>> regex_filter_out('/app1/ctr/fcn')
    '/app1/ctr/fcn'
    >>> filter_url('https://otherdomain.com/app1/default/fcn', out=True)
    '/fcn'
    >>> filter_url('http://otherdomain.com/app2/ctr/fcn', out=True)
    '/app2/ctr/fcn'
    >>> filter_url('http://domain1.com/app1/default/fcn?query', out=True)
    '/fcn?query'
    >>> filter_url('http://domain2.com/app3/defcon3/fcn#anchor', out=True)
    '/fcn#anchor'
    '''
    pass

if __name__ == '__main__':
    try:
        import gluon.main
    except ImportError:
        import sys
        import os
        os.chdir(os.path.dirname(os.path.dirname(__file__)))
        sys.path.append(os.path.dirname(os.path.dirname(__file__)))
        import gluon.main
    from gluon.rewrite import regex_select, load, filter_url, regex_filter_out
    regex_select()          # use base routing parameters
    load(routes=__file__)   # load this file

    import doctest
    doctest.testmod()
