"""
web2py handler for isapi-wsgi for IIS. Requires:
http://code.google.com/p/isapi-wsgi/
"""
# The entry point for the ISAPI extension.


def __ExtensionFactory__():
    import os
    import sys
    path = os.path.dirname(os.path.abspath(__file__))
    os.chdir(path)
    if not os.path.isdir('applications'):
        raise RuntimeError('Running from the wrong folder')
    sys.path = [path] + [p for p in sys.path if not p == path]
    import gluon.main
    import isapi_wsgi
    application = gluon.main.wsgibase
    return isapi_wsgi.ISAPIThreadPoolHandler(application)

# ISAPI installation:
if __name__ == '__main__':
    import sys
    if len(sys.argv) < 2:
        print "USAGE: python isapiwsgihandler.py install --server=Sitename"
        sys.exit(0)
    from isapi.install import ISAPIParameters
    from isapi.install import ScriptMapParams
    from isapi.install import VirtualDirParameters
    from isapi.install import HandleCommandLine

    params = ISAPIParameters()
    sm = [ScriptMapParams(Extension="*", Flags=0)]
    vd = VirtualDirParameters(Name="appname",
                              Description="Web2py in Python",
                              ScriptMaps=sm,
                              ScriptMapUpdate="replace")
    params.VirtualDirs = [vd]
    HandleCommandLine(params)
