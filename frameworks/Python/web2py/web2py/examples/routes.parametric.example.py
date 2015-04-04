# -*- coding: utf-8 -*-

#  routers are dictionaries of URL routing parameters.
#
#  For each request, the effective router is:
#    the built-in default base router (shown below),
#    updated by the BASE router in routes.py routers,
#    updated by the app-specific router in routes.py routers (if any),
#    updated by the app-specific router from applications/app/routes.py routers (if any)
#
#
#  Router members:
#
#  default_application: default application name
#  applications: list of all recognized applications, or 'ALL' to use all currently installed applications
#       Names in applications are always treated as an application names when they appear first in an incoming URL.
#       Set applications=None to disable the removal of application names from outgoing URLs.
#  domains: optional dict mapping domain names to application names
#       The domain name can include a port number: domain.com:8080
#       The application name can include a controller:  appx/ctlrx
#           or a controller and a function: appx/ctlrx/fcnx
#       Example:
#       domains = {   "domain.com" : "app",
#                  "x.domain.com" : "appx",
#                 },
#  path_prefix: a path fragment that is prefixed to all outgoing URLs and stripped from all incoming URLs
#
#  Note: default_application, applications, domains & path_prefix are permitted only in the BASE router,
#        and domain makes sense only in an application-specific router.
#        The remaining members can appear in the BASE router (as defaults for all applications)
#        or in application-specific routers.
#
#  default_controller: name of default controller
#  default_function: name of default function (in all controllers) or dictionary of default functions
#       by controller
#  controllers: list of valid controllers in selected app
#       or "DEFAULT" to use all controllers in the selected app plus 'static'
#       or None to disable controller-name removal.
#      Names in controllers are always treated as controller names when they appear in an incoming URL after
#      the (optional) application and language names.
#  functions: list of valid functions in the default controller (default None) or dictionary of valid
#       functions by controller.
#       If present, the default function name will be omitted when the controller is the default controller
#       and the first arg does not create an ambiguity.
#  languages: list of all supported languages
#       Names in languages are always treated as language names when they appear in an incoming URL after
#       the (optional) application name.
#  default_language
#       The language code (for example: en, it-it) optionally appears in the URL following
#       the application (which may be omitted). For incoming URLs, the code is copied to
#       request.uri_language; for outgoing URLs it is taken from request.uri_language.
#       If languages=None, language support is disabled.
#       The default_language, if any, is omitted from the URL.
#       To use the incoming language in your application, add this line to one of your models files:
#           if request.uri_language: T.force(request.uri_language)
#  root_static: list of static files accessed from root (by default, favicon.ico & robots.txt)
#       (mapped to the default application's static/ directory)
#       Each default (including domain-mapped) application has its own root-static files.
#  domain: the domain that maps to this application (alternative to using domains in the BASE router)
#  exclusive_domain: If True (default is False), an exception is raised if an attempt is made to generate
#       an outgoing URL with a different application without providing an explicit host.
#  map_hyphen: If True (default is False), hyphens in incoming /a/c/f fields are converted
#       to underscores, and back to hyphens in outgoing URLs.
#       Language, args and the query string are not affected.
#  map_static: By default (None), the default application is not stripped from static URLs.
#       Set map_static=True to override this policy.
#       Set map_static=False to map lang/static/file to static/lang/file
#  acfe_match: regex for valid application, controller, function, extension /a/c/f.e
#  file_match: regex for valid subpath (used for static file paths)
#              if file_match does not contain '/', it is uses to validate each element of a static file subpath,
#              rather than the entire subpath.
#  args_match: regex for valid args
#       This validation provides a measure of security.
#       If it is changed, the application perform its own validation.
#
#
#  The built-in default router supplies default values (undefined members are None):
#
#     default_router = dict(
#         default_application = 'init',
#             applications = 'ALL',
#         default_controller = 'default',
#             controllers = 'DEFAULT',
#         default_function = 'index',
#             functions = None,
#         default_language = None,
#             languages = None,
#         root_static = ['favicon.ico', 'robots.txt'],
#         map_static = None,
#         domains = None,
#         map_hyphen = False,
#         acfe_match = r'\w+$',                 # legal app/ctlr/fcn/ext
#         file_match = r'([-+=@$%\w]|(?<=[-+=@$%\w])[./])*$',   # legal static subpath
#         args_match = r'([\w@ -]|(?<=[\w@ -])[.=])*$',         # legal arg in args
#     )
#
#  See rewrite.map_url_in() and rewrite.map_url_out() for implementation details.


#  This simple router set overrides only the default application name,
#  but provides full rewrite functionality.

routers = dict(

    # base router
    BASE=dict(
        default_application='welcome',
    ),
)

# Specify log level for rewrite's debug logging
# Possible values: debug, info, warning, error, critical (loglevels),
#                  off, print (print uses print statement rather than logging)
# GAE users may want to use 'off' to suppress routine logging.
#
logging = 'debug'

# Error-handling redirects all HTTP errors (status codes >= 400) to a specified
# path.  If you wish to use error-handling redirects, uncomment the tuple
# below.  You can customize responses by adding a tuple entry with the first
# value in 'appName/HTTPstatusCode' format. ( Only HTTP codes >= 400 are
# routed. ) and the value as a path to redirect the user to.  You may also use
# '*' as a wildcard.
#
# The error handling page is also passed the error code and ticket as
# variables.  Traceback information will be stored in the ticket.
#
# routes_onerror = [
#     (r'init/400', r'/init/default/login')
#    ,(r'init/*', r'/init/static/fail.html')
#    ,(r'*/404', r'/init/static/cantfind.html')
#    ,(r'*/*', r'/init/error/index')
# ]

# specify action in charge of error handling
#
# error_handler = dict(application='error',
#                      controller='default',
#                      function='index')

# In the event that the error-handling page itself returns an error, web2py will
# fall back to its old static responses.  You can customize them here.
# ErrorMessageTicket takes a string format dictionary containing (only) the
# "ticket" key.

# error_message = '<html><body><h1>%s</h1></body></html>'
# error_message_ticket = '<html><body><h1>Internal error</h1>Ticket issued: <a href="/admin/default/ticket/%(ticket)s" target="_blank">%(ticket)s</a></body></html>'


def __routes_doctest():
    '''
    Dummy function for doctesting routes.py.

    Use filter_url() to test incoming or outgoing routes;
    filter_err() for error redirection.

    filter_url() accepts overrides for method and remote host:
        filter_url(url, method='get', remote='0.0.0.0', out=False)

    filter_err() accepts overrides for application and ticket:
        filter_err(status, application='app', ticket='tkt')

    >>> import os
    >>> import gluon.main
    >>> from gluon.rewrite import load, filter_url, filter_err, get_effective_router
    >>> load(routes=os.path.basename(__file__))

    >>> filter_url('http://domain.com/abc', app=True)
    'welcome'
    >>> filter_url('http://domain.com/welcome', app=True)
    'welcome'
    >>> os.path.relpath(filter_url('http://domain.com/favicon.ico'))
    'applications/welcome/static/favicon.ico'
    >>> filter_url('http://domain.com/abc')
    '/welcome/default/abc'
    >>> filter_url('http://domain.com/index/abc')
    "/welcome/default/index ['abc']"
    >>> filter_url('http://domain.com/default/abc.css')
    '/welcome/default/abc.css'
    >>> filter_url('http://domain.com/default/index/abc')
    "/welcome/default/index ['abc']"
    >>> filter_url('http://domain.com/default/index/a bc')
    "/welcome/default/index ['a bc']"

    >>> filter_url('https://domain.com/app/ctr/fcn', out=True)
    '/app/ctr/fcn'
    >>> filter_url('https://domain.com/welcome/ctr/fcn', out=True)
    '/ctr/fcn'
    >>> filter_url('https://domain.com/welcome/default/fcn', out=True)
    '/fcn'
    >>> filter_url('https://domain.com/welcome/default/index', out=True)
    '/'
    >>> filter_url('https://domain.com/welcome/appadmin/index', out=True)
    '/appadmin'
    >>> filter_url('http://domain.com/welcome/default/fcn?query', out=True)
    '/fcn?query'
    >>> filter_url('http://domain.com/welcome/default/fcn#anchor', out=True)
    '/fcn#anchor'
    >>> filter_url('http://domain.com/welcome/default/fcn?query#anchor', out=True)
    '/fcn?query#anchor'

    >>> filter_err(200)
    200
    >>> filter_err(399)
    399
    >>> filter_err(400)
    400
    '''
    pass

if __name__ == '__main__':
    import doctest
    doctest.testmod()
