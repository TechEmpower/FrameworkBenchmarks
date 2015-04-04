import os
import sys
import glob
import shutil
import re

regex_link = re.compile("""(href|src)\s*=\s*("|')(.+?)("|')""")

def getname(filename):
    return re.compile('\W').sub('',filename.split('/')[-1].rsplit('.',1)[0])

def make_controller(html_files):
    controller = ''
    for filename in html_files:
        name = getname(filename)
        controller += 'def %s():\n    return locals()\n\n' % name
    return controller

def fix_links(html,prefix):
    def fix(match):
        href,link = match.group(1), match.group(3)
        if not '://' in link:
            if link.lower().endswith('.html') and not '/' in link:
                link = "{{=URL('%s','%s')}}" % (prefix, getname(link))
            elif link.startswith('./'):
                link = "{{=URL('static','%s/%s')}}" % (prefix,link[2:])
            elif link.startswith('/'):
                link = "{{=URL('static','%s/%s')}}" % (prefix,link[1:])
            else:
                link = "{{=URL('static','%s/%s')}}" % (prefix,link)
        return '%s="%s"' % (href,link)
    return regex_link.sub(fix,html)

def make_views(html_files,prefix):
    views = {}
    layout_name = os.path.join(prefix,'layout.html')
    extend = "{{extend '%s'}}" % layout_name
    for filename in html_files:
        html = open(filename).read()
        name = getname(filename)
        views[os.path.join(prefix,name+'.html')] = fix_links(html,prefix)
    start = stop = None
    k = 0
    while start is None or stop is None:
        try:
            if start is None:
                if len(set(v[k] for v in views.values()))>1:
                    start=k
            if stop is None:
                if len(set(v[len(v)-k] for v in views.values()))>1:
                    stop=k
        except:
            if start is None:
                start = k
            if stop is None:
                stop = k
        k+=1
    header = footer = ''
    for name in views:
        html = views[name]
        n = len(html)
        header, views[name], footer = \
            html[:start], extend+html[start:n-stop], html[n-stop:]
    layout_html = header+'{{include}}'+footer
    views[layout_name] = layout_html
    return views

def recursive_overwrite(src, dest, ignore=None):
    if os.path.isdir(src):
        if not os.path.isdir(dest):
            os.makedirs(dest)
        files = os.listdir(src)
        if ignore is not None:
            ignored = ignore(src, files)
        else:
            ignored = set()
        for f in files:
            if f not in ignored:
                recursive_overwrite(os.path.join(src, f),
                                    os.path.join(dest, f),
                                    ignore)
    else:
        shutil.copyfile(src, dest)

def convert(source, destination,prefix='imported'):
    html_files = glob.glob(os.path.join(source,'*.html'))
    static_folder = os.path.join(destination,'static',prefix)
    recursive_overwrite(source,static_folder)
    controller = make_controller(html_files)
    views = make_views(html_files,prefix)
    controller_filename = os.path.join(destination,'controllers',prefix+'.py')

    open(controller_filename,'w').write(controller)
    for name in views:
        fullname = os.path.join(destination,'views',name)
        if not os.path.exists(os.path.split(fullname)[0]):
            os.makedirs(os.path.split(fullname)[0])
        open(fullname,'w').write(views[name])



convert(sys.argv[1],sys.argv[2])
