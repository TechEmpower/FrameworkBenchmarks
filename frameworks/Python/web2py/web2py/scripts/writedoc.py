import os
import sys
import glob
sys.path.append(os.path.join(os.path.split(__file__)[0],'..'))
from gluon.html import CODE

def main(path):
    models = glob.glob(os.path.join(path,'models','*.py'))
    controllers = glob.glob(os.path.join(path,'controllers','*.py'))
    views = glob.glob(os.path.join(path,'views','*.html'))
    modules = glob.glob(os.path.join(path,'modules','*.py'))
    models.sort()
    controllers.sort()
    views.sort()
    modules.sort()
    print '<html><body>'
    print '<h1>Models</h1>'
    for filename in models:
        print '<h2>%s</h2>' % filename[len(path):]
        print CODE(open(filename).read(),language='web2py').xml()
    print '<h1>Layout Views</h1>'
    for filename in views:
        print '<h2>%s</h2>' % filename[len(path):]
        print CODE(open(filename).read(),language='html').xml()
    print '<h1>Controllers and Views</h1>'
    for filename in controllers:
        print '<h2>%s</h2>' % filename[len(path):]
        print CODE(open(filename).read(),language='web2py')
        views = glob.glob(os.path.join(path,'views','*','*.html'))
        views.sort()
        for filename in views:
            print '<h2>%s</h2>' % filename[len(path):]
            print CODE(open(filename).read(),language='html').xml()
    print '<h1>Modules</h1>'
    for filename in modules:
        print '<h2>%s</h2>' % filename[len(path):]
        print CODE(open(filename).read(),language='python').xml()
    print '</body></html>'

if __name__=='__main__':
    main(sys.argv[1])
