import os
import glob
import zipfile
import urllib
import tempfile
import shutil

def copytree(src, dst):
    names = os.listdir(src)
    ignored_names = set()
    errors = []
    if not os.path.exists(dst):
        os.makedirs(dst)
    for name in names:
        srcname = os.path.join(src, name)
        dstname = os.path.join(dst, name)
        if os.path.isdir(srcname):
            copytree(srcname, dstname)
        else:
            shutil.copy2(srcname, dstname)

class W2PInstance(object):

    SOURCES = {'stable':'http://web2py.com/examples/static/web2py_src.zip',
               'nightly':'http://web2py.com/examples/static/nightly/web2py_src.zip',
               'trunk':'https://github.com/web2py/web2py/archive/master.zip'}

    def __init__(self,path):
        self.path = path

    def warn(self,message="system going down soon"):
        apps = glob.glob(os.path.join(self.path,'applications','*'))
        for app in apps:
            if os.path.isdir(app):
                open(os.path.join(app,'notifications.txt'),'w').write(message)

    def install(self,source='stable'):
        if not os.path.exists(self.path):
            os.mkdir(self.path)
        tmpdir = tempfile.mkdtemp()
        link = self.SOURCES[source]
        srcfile = os.path.join(tmpdir,'web2py_src.zip')
        print 'downloading...'
        open(srcfile,'wb').write(urllib.urlopen(link).read())
        print 'extracing...'
        zipfile.ZipFile(srcfile,'r').extractall(tmpdir)
        print 'copying...'
        copytree(os.path.join(tmpdir,'web2py'),self.path)

    def upgrade(self,source='stable'):
        self.install(source)

    def upgrade_tmp(self,source,common=False):
        tmpdir = tempfile.mkdtemp()
        link = self.SOURCES[source]
        srcfile = os.path.join(tmpdir,'web2py_src.zip')
        print 'copying production...'
        copytree(self.path,os.path.join(tmpdir,'web2py'))
        tmpdir_web2py = os.path.join(tmpdir,'web2py')
        tmp_web2py = W2PInstance(tempdir_web2py)
        tmp_web2py.clear_sessions()
        tmp_web2py.clear_cache()
        tmp_web2py.clear_error()
        print 'downloading...'
        open(srcfile,'wb').write(urllib.urlopen(link).read())
        print 'extracing...'
        zipfile.ZipFile(srcfile,'r').extractall(tmpdir)
        print 'running tests...'
        try:
            olddir = os.getcwd()
            os.chdir(tempdir_web2py)
            ret = os.system("PYTHONPATH=. python -m unittest -v gluon.tests")
            # eventually start web2py and run functional tests
        finally:
            os.chrid(olddir)
        if ret:
            sys.exit(ret and 1)
        copytree(os.path.join(tmpdir,'web2py'),self.path)

    def clear_sessions(self):
        files = glob.glob(os.path.join(self.path,'applications','*','sessions','*'))
        for file in files:
            try:
                os.unlink(file)
            except:
                pass

    def clear_cache(self):
        files = glob.glob(os.path.join(self.path,'applications','*','cache','*'))
        for file in files:
            try:
                os.unlink(file)
            except:
                pass

    def clear_errors(self):
        files = glob.glob(os.path.join(self.path,'applications','*','errors','*'))
        for file in files:
            try:
                os.unlink(file)
            except:
                pass

web2py = W2PInstance('/Users/massimodipierro/Downloads/web2py')
#web2py.install()
web2py.clear_sessions()

"""
{{
import os
_notifications = os.path.join(request.folder,'notifications.txt')
if os.path.exixts(_notifications):
   response.flash = response.flash or open(_notifications).read()
pass
}}
"""
