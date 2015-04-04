"""
Developed by Massimo Di Pierro
Released under the web2py license (LGPL)

What does it do?

if html is a variable containing HTML text and urls in the text, when you call

    html = expend_html(html)

it automatically converts the url to links but when possible it embeds the object being linked.
In particular it can embed images, videos, audio files, documents (it uses the google code player),
as well as pages to a oembed service.


Google Doc Support
==================
Microsoft Word (.DOC, .DOCX)
Microsoft Excel (.XLS and .XLSX)
Microsoft PowerPoint 2007 / 2010 (.PPTX)
Apple Pages (.PAGES)
Adobe PDF (.PDF)
Adobe Illustrator (.AI)
Adobe Photoshop (.PSD)
Autodesk AutoCad (.DXF)
Scalable Vector Graphics (.SVG)
PostScript (.EPS, .PS)
TrueType (.TTF)
XML Paper Specification (.XPS)

Oembed Support
==============
flickr.com
youtube.com
hulu.com
vimeo.com
slideshare.net
qik.com
polleverywhere.com
wordpress.com
revision3.com
viddler.com
"""

import re
import cgi
import sys
from simplejson import loads
import urllib
import uuid
try:
    from BeautifulSoup import BeautifulSoup, Comment
    have_soup = True
except ImportError:
    have_soup = False

regex_link = re.compile('https?://\S+')

EMBED_MAPS = [
    (re.compile('http://\S*?flickr.com/\S*'),
     'http://www.flickr.com/services/oembed/'),
    (re.compile('http://\S*.youtu(\.be|be\.com)/watch\S*'),
     'http://www.youtube.com/oembed'),
    (re.compile('http://www.hulu.com/watch/\S*'),
     'http://www.hulu.com/api/oembed.json'),
    (re.compile('http://vimeo.com/\S*'),
     'http://vimeo.com/api/oembed.json'),
    (re.compile('http://www.slideshare.net/[^\/]+/\S*'),
     'http://www.slideshare.net/api/oembed/2'),
    (re.compile('http://qik.com/\S*'),
     'http://qik.com/api/oembed.json'),
    (re.compile('http://www.polleverywhere.com/\w+/\S+'),
     'http://www.polleverywhere.com/services/oembed/'),
    (re.compile('http://\S+.wordpress.com/\S+'),
     'http://public-api.wordpress.com/oembed/'),
    (re.compile('http://*.revision3.com/\S+'),
     'http://revision3.com/api/oembed/'),
    (re.compile('http://\S+.viddler.com/\S+'),
     'http://lab.viddler.com/services/oembed/'),
]


def image(url):
    return '<img src="%s" style="max-width:100%%"/>' % url


def audio(url):
    return '<audio controls="controls" style="max-width:100%%"><source src="%s" /></audio>' % url


def video(url):
    return '<video controls="controls" style="max-width:100%%"><source src="%s" /></video>' % url


def googledoc_viewer(url):
    return '<iframe src="http://docs.google.com/viewer?url=%s&embedded=true" style="max-width:100%%"></iframe>' % urllib.quote(url)


def web2py_component(url):
    code = str(uuid.uuid4())
    return '<div id="%s"></div><script>\nweb2py_component("%s","%s");\n</script>' % (code, url, code)

EXTENSION_MAPS = {
    'png': image,
    'gif': image,
    'jpg': image,
    'jpeg': image,
    'wav': audio,
    'ogg': audio,
    'mp3': audio,
    'mov': video,
    'mpe': video,
    'mp4': video,
    'mpg': video,
    'mpg2': video,
    'mpeg': video,
    'mpeg4': video,
    'movie': video,
    'wmv': video,
    'load': web2py_component,
    'pdf': googledoc_viewer,
    'doc': googledoc_viewer,
    'docx': googledoc_viewer,
    'ppt': googledoc_viewer,
    'pptx': googledoc_viewer,
    'xls': googledoc_viewer,
    'xlsx': googledoc_viewer,
    'pages': googledoc_viewer,
    'ai': googledoc_viewer,
    'psd': googledoc_viewer,
    'xdf': googledoc_viewer,
    'svg': googledoc_viewer,
    'ttf': googledoc_viewer,
    'xps': googledoc_viewer,
}


class VimeoURLOpener(urllib.FancyURLopener):
    "Vimeo blocks the urllib user agent for some reason"
    version = "Mozilla/4.0"
urllib._urlopener = VimeoURLOpener()


def oembed(url):
    for k, v in EMBED_MAPS:
        if k.match(url):
            oembed = v + '?format=json&url=' + cgi.escape(url)
            try:
                data = urllib.urlopen(oembed).read()
                return loads(data)  # json!
            except:
                pass
    return {}


def extension(url):
    return url.split('?')[0].split('.')[-1].lower()


def expand_one(url, cdict):
    # try ombed but first check in cache
    if '@' in url and not '://'in url:
        return '<a href="mailto:%s">%s</a>' % (url, url)
    if cdict and url in cdict:
        r = cdict[url]
    else:
        r = oembed(url)
        if isinstance(cdict, dict):
            cdict[url] = r
    # if oembed service
    if 'html' in r:
        html = r['html'].encode('utf8')
        if html.startswith('<object'):
            return '<embed style="max-width:100%%">%s</embed>' % html
        else:
            return html
    elif 'url' in r:
        url = r['url'].encode('utf8')
    # embed images, video, audio files
    ext = extension(url)
    if ext in EXTENSION_MAPS:
        return EXTENSION_MAPS[ext](url)
    # else regular link
    return '<a href="%(u)s">%(u)s</a>' % dict(u=url)


def expand_html(html, cdict=None):
    if not have_soup:
        raise RuntimeError("Missing BeautifulSoup")
    soup = BeautifulSoup(html)
    comments = soup.findAll(text=lambda text: isinstance(text, Comment))
    [comment.extract() for comment in comments]
    for txt in soup.findAll(text=True):
        if not txt.parent.name in ('a', 'script', 'pre', 'code', 'embed', 'object', 'audio', 'video'):
            ntxt = regex_link.sub(
                lambda match: expand_one(match.group(0), cdict), txt)
            txt.replaceWith(BeautifulSoup(ntxt))
    return str(soup)


def test():
    example = """
<h3>Fringilla nisi parturient nullam</h3>
<p>http://www.youtube.com/watch?v=IWBFiI5RrA0</p>
<p>http://www.web2py.com/examples/static/images/logo_bw.png</p>
<p>http://www.web2py.com/examples/default/index.load</p>
<p>http://www.web2py.com/examples/static/web2py_manual_cutl.pdf</p>
<p>Elementum sodales est varius magna leo sociis erat. Nascetur pretium non
ultricies gravida. Condimentum at nascetur tempus. Porttitor viverra ipsum
accumsan neque aliquet. Ultrices vestibulum tempor quisque eget sem eget.
Ornare malesuada tempus dolor dolor magna consectetur. Nisl dui non curabitur
laoreet tortor.</p>
"""
    return expand_html(example)

if __name__ == "__main__":
    if len(sys.argv) > 1:
        print expand_html(open(sys.argv[1]).read())
    else:
        print test()
