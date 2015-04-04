import os
from gluon.settings import global_settings, read_file
#


def index():
    app = request.args(0)
    return dict(app=app)


def profiler():
    """
    to use the profiler start web2py with -F profiler.log
    """
    KEY = 'web2py_profiler_size'
    filename = global_settings.cmd_options.profiler_filename
    data = 'profiler disabled'
    if filename:
        if  KEY in request.cookies:
            size = int(request.cookies[KEY].value)
        else:
            size = 0
        if os.path.exists(filename):
            data = read_file('profiler.log', 'rb')
            if size < len(data):
                data = data[size:]
            else:
                size = 0
            size += len(data)
            response.cookies[KEY] = size
    return data
