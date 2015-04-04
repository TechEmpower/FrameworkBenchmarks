import glob
import os
import zipfile
import sys
import re
from BeautifulSoup import BeautifulSoup as BS


def head(styles):
    title = '<title>{{=response.title or request.application}}</title>'
    items = '\n'.join(["{{response.files.append(URL(request.application,'static','%s'))}}" % (style) for style in styles])
    loc = """<style>
div.flash {
    position: absolute;
    float: right;
    padding: 10px;
    top: 0px;
    right: 0px;
    opacity: 0.75;
    margin: 10px 10px 10px 10px;
    text-align: center;
    clear: both;
    color: #fff;
    font-size: 11pt;
    text-align: center;
    vertical-align: middle;
    cursor: pointer;
    background: black;
    border: 2px solid #fff;
    -moz-border-radius: 5px;
    -webkit-border-radius: 5px;
    z-index: 2;
}
div.error {
    -moz-border-radius: 5px;
    -webkit-border-radius: 5px;
    background-color: red;
    color: white;
    padding: 3px;
    border: 1px solid #666;
}
</style>"""
    return "\n%s\n%s\n{{include 'web2py_ajax.html'}}\n%s" % (title, items, loc)


def content():
    return """<div class="flash">{{=response.flash or ''}}</div>{{include}}"""


def process(folder):
    indexfile = open(os.path.join(folder, 'index.html'), 'rb')
    try:
        soup = BS(indexfile.read())
    finally:
        indexfile.close()
    styles = [x['href'] for x in soup.findAll('link')]
    soup.find('head').contents = BS(head(styles))
    try:
        soup.find(
            'h1').contents = BS('{{=response.title or request.application}}')
        soup.find('h2').contents = BS(
            "{{=response.subtitle or '=response.subtitle'}}")
    except:
        pass
    for match in (soup.find('div', id='menu'),
                  soup.find('div', {'class': 'menu'}),
                  soup.find('div', id='nav'),
                  soup.find('div', {'class': 'nav'})):
        if match:
            match.contents = BS('{{=MENU(response.menu)}}')
            break
    done = False
    for match in (soup.find('div', id='content'),
                  soup.find('div', {'class': 'content'}),
                  soup.find('div', id='main'),
                  soup.find('div', {'class': 'main'})):
        if match:
            match.contents = BS(content())
            done = True
            break
    if done:
        page = soup.prettify()
        page = re.compile("\s*\{\{=response\.flash or ''\}\}\s*", re.MULTILINE)\
            .sub("{{=response.flash or ''}}", page)
        print page
    else:
        raise Exception("Unable to convert")

if __name__ == '__main__':
    if len(sys.argv) < 2:
        print """USAGE:
1) start a new web2py application
2) Download a sample free layout from the web into the static/ folder of
   your web2py application (make sure a sample index.html is there)
3) run this script with

   python layout_make.py /path/to/web2py/applications/app/static/
     > /path/to/web2py/applications/app/views/layout.html
"""
    elif not os.path.exists(sys.argv[1]):
        print 'Folder %s does not exist' % sys.argv[1]
    else:
        process(sys.argv[1])
