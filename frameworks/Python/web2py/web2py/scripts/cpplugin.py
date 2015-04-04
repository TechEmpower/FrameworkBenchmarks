import sys
import glob
import os
import shutil
name = sys.argv[1]
app = sys.argv[2]
dest = sys.argv[3]
a = glob.glob(
    'applications/%(app)s/*/plugin_%(name)s.*' % dict(app=app, name=name))
b = glob.glob(
    'applications/%(app)s/*/plugin_%(name)s/*' % dict(app=app, name=name))

for f in a:
    print 'cp %s ...' % f,
    shutil.copyfile(f, os.path.join('applications', dest, *f.split('/')[2:]))
    print 'done'

for f in b:
    print 'cp %s ...' % f,
    path = f.split('/')
    for i in range(3, len(path)):
        try:
            os.mkdir(os.path.join('applications', dest, *path[2:i]))
        except:
            pass
    path = os.path.join('applications', dest, *f.split('/')[2:])
    if os.path.isdir(f):
        if not os.path.exists(path):
            shutil.copytree(f, path)
    else:
        shutil.copyfile(f, path)
    print 'done'
