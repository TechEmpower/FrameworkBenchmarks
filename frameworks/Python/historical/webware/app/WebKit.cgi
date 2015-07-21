#!/usr/bin/python

# If the Webware installation is located somewhere else,
# then set the webwareDir variable to point to it here:
webwareDir = '/home/knewman/Development/tfb-benchmark/webware/Webware'

# If you used the MakeAppWorkDir.py script to make a separate
# application working directory, specify it here:
workDir = '/home/knewman/Development/tfb-benchmark/webware/Webware/WebwareTest'

try:
    import os, sys
    if not webwareDir:
        webwareDir = os.path.dirname(os.path.dirname(os.getcwd()))
    sys.path.insert(1, webwareDir)
    webKitDir = os.path.join(webwareDir, 'WebKit')
    if workDir is None:
        workDir = webKitDir
    else:
        sys.path.insert(1, workDir)
    try:
        import WebKit.Adapters.CGIAdapter
    except ImportError:
        if os.path.exists(webwareDir) and os.path.exists(webKitDir):
            cgiAdapter = os.path.join(webKitDir, 'Adapters', 'CGIAdapter.py')
            if os.path.exists(cgiAdapter):
                raise
            msg = "CGIAdapter module at <code>%s</code> cannot be loaded" % cgiAdapter
        else:
            msg = "Webware installation not found at <code>%s</code>" % webwareDir
        sys.stdout.write('''Content-Type: text/html\n
<html><head><title>WebKit CGI Error</title><body>
<h3>WebKit CGI Error</h3>
<p>%s.</p>
<p>You may need to edit the WebKit.cgi script so that <code>webwareDir</code>
points to the actual Webware installation directory.</p>
<p>You may also need to modify permissions of the Webware installation
with <code>chmod</code> so that the CGIAdapter module can be imported.</p>
</body></html>\n''' % msg)
    else:
        WebKit.Adapters.CGIAdapter.main(workDir)
except:
    import sys, traceback
    from time import asctime, localtime, time
    sys.stderr.write('[%s] [error] WebKit: Error in adapter\n' % asctime(localtime(time())))
    sys.stderr.write('Error while executing script\n')
    traceback.print_exc(file=sys.stderr)
    output = ''.join(traceback.format_exception(*sys.exc_info()))
    output = output.replace('&', '&amp;').replace(
        '<', '&lt;').replace('>', '&gt;').replace('"', '&quot;')
    sys.stdout.write('''Content-Type: text/html\n
<html><head><title>WebKit CGI Error</title><body>
<h3>WebKit CGI Error</h3>
<pre>%s</pre>
</body></html>\n''' % output)
