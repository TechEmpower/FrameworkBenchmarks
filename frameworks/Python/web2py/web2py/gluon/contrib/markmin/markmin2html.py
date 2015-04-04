#!/usr/bin/env python
# -*- coding: utf-8 -*-
# created by Massimo Di Pierro
# recreated by Vladyslav Kozlovskyy
# license MIT/BSD/GPL
import re
import urllib
from cgi import escape
from string import maketrans
try:
   from ast import parse as ast_parse
   import ast
except ImportError: # python 2.5
    from compiler import parse
    import compiler.ast as ast

"""
TODO: next version should use MathJax

<script type="text/javascript" src="http://cdn.mathjax.org/mathjax/latest/MathJax.js">
MathJax.Hub.Config({
 extensions: ["tex2jax.js","TeX/AMSmath.js","TeX/AMSsymbols.js"],
 jax: ["input/TeX", "output/HTML-CSS"],
 tex2jax: {
     inlineMath: [ ['$','$'], ["\\(","\\)"] ],
     displayMath: [ ['$$','$$'], ["\\[","\\]"] ],
 },
 "HTML-CSS": { availableFonts: ["TeX"] }
});
</script>
"""

__all__ = ['render', 'markmin2html', 'markmin_escape']

__doc__ = """
# Markmin markup language

## About

This is a new markup language that we call markmin designed to produce high quality scientific papers and books and also put them online. We provide serializers for html, latex and pdf. It is implemented in the ``markmin2html`` function in the ``markmin2html.py``.

Example of usage:

``
m = "Hello **world** [[link http://web2py.com]]"
from markmin2html import markmin2html
print markmin2html(m)
from markmin2latex import markmin2latex
print markmin2latex(m)
from markmin2pdf import markmin2pdf # requires pdflatex
print markmin2pdf(m)
``
====================
# This is a test block
  with new features:
This is a blockquote with
a list with tables in it:
-----------
  This is a paragraph before list.
  You can continue paragraph on the
  next lines.

  This is an ordered list with tables:
  + Item 1
  + Item 2
  + --------
    aa|bb|cc
    11|22|33
    --------:tableclass1[tableid1]
  + Item 4
    -----------
     T1| T2| t3
    ===========
    aaa|bbb|ccc
    ddd|fff|ggg
    123|0  |5.0
    -----------:tableclass1
-----------:blockquoteclass[blockquoteid]

This this a new paragraph
with a followed table.
Table has header, footer, sections,
odd and even rows:
-------------------------------
**Title 1**|**Title 2**|**Title 3**
==============================
data 1     | data 2    |  2.00
data 3     |data4(long)| 23.00
           |data 5     | 33.50
==============================
New section|New data   |  5.00
data 1     |data2(long)|100.45
           |data 3     | 12.50
data 4     | data 5    |   .33
data 6     |data7(long)|  8.01
           |data 8     |   514
==============================
Total:     | 9 items   |698,79
------------------------------:tableclass1[tableid2]

## Multilevel
   lists

Now lists can be multilevel:

+ Ordered item 1 on level 1.
  You can continue item text on
  next strings

. paragraph in an item

++. Ordered item 1 of sublevel 2 with
    a paragraph (paragraph can start
    with point after plus or minus
    characters, e.g. **++.** or **--.**)

++. This is another item. But with 3 paragraphs,
    blockquote and sublists:

.. This is the second paragraph in the item. You
   can add paragraphs to an item, using point
   notation, where first characters in the string
   are sequence of points with space between
   them and another string. For example, this
   paragraph (in sublevel 2) starts with two points:
   ``.. This is the second paragraph...``

.. ----------
     ### this is a blockquote in a list

     You can use blockquote with headers, paragraphs,
     tables and lists in it:

     Tables can have or have not header and footer.
     This table is defined without any header
     and footer in it:
     ---------------------
     red  |fox     | 0
     blue |dolphin | 1000
     green|leaf    | 10000
     ---------------------
   ----------

.. This is yet another paragraph in the item.

--- This is an item of unordered list **(sublevel 3)**
--- This is the second item of the unordered list ''(sublevel 3)''

++++++ This is a single item of ordered list in sublevel 6
.... and this is a paragraph in sublevel 4
---. This is a new item with paragraph in sublevel 3.
++++ Start ordered list in sublevel 4 with code block: ``
line 1
  line 2
     line 3
``
++++. Yet another item with code block (we need to indent \`\` to add code block as part of item):
 ``
  line 1
line 2
  line 3
``
 This item finishes with this paragraph.

... Item in sublevel 3 can be continued with paragraphs.

... ``
  this is another
code block
    in the
  sublevel 3 item
``

+++ The last item in sublevel 3
.. This is a continuous paragraph for item 2 in sublevel 2.
   You can use such structure to create difficult structured
   documents.

++ item 3 in sublevel 2
-- item 1 in sublevel 2 (new unordered list)
-- item 2 in sublevel 2
-- item 3 in sublevel 2

++ item 1 in sublevel 2 (new ordered list)
++ item 2 in sublevel 2
++ item 3 in sublevle 2

+ item 2 in level 1
+ item 3 in level 1
- new unordered list (item 1 in level 1)
- level 2 in level 1

- level 3 in level 1
- level 4 in level 1
## This is the last section of the test

Single paragraph with '----' in it will be turned into separator:

-----------

And this is the last paragraph in
the test. Be happy!

====================

## Why?

We wanted a markup language with the following requirements:
- less than 300 lines of functional code
- easy to read
- secure
- support table, ul, ol, code
- support html5 video and audio elements (html serialization only)
- can align images and resize them
- can specify class for tables, blockquotes and code elements
- can add anchors
- does not use _ for markup (since it creates odd behavior)
- automatically links urls
- fast
- easy to extend
- supports latex and pdf including references
- allows to describe the markup in the markup (this document is generated from markmin syntax)

(results depend on text but in average for text ~100K markmin is 30% faster than markdown, for text ~10K it is 10x faster)

The [[web2py book http://www.lulu.com/product/paperback/web2py-%283rd-edition%29/12822827]] published by lulu, for example, was entirely generated with markmin2pdf from the online [[web2py wiki http://www.web2py.com/book]]

## Download

- http://web2py.googlecode.com/hg/gluon/contrib/markmin/markmin2html.py
- http://web2py.googlecode.com/hg/gluon/contrib/markmin/markmin2latex.py
- http://web2py.googlecode.com/hg/gluon/contrib/markmin/markmin2pdf.py

markmin2html.py and markmin2latex.py are single files and have no web2py dependence. Their license is BSD.

## Examples

### Bold, italic, code and links

------------------------------------------------------------------------------
**SOURCE**                                    | **OUTPUT**
==============================================================================
``# title``                                   | **title**
``## section``                                | **section**
``### subsection``                            | **subsection**
``**bold**``                                  | **bold**
``''italic''``                                | ''italic''
``~~strikeout~~``                             | ~~strikeout~~
``!`!`verbatim`!`!``                          | ``verbatim``
``\`\`color with **bold**\`\`:red``           | ``color with **bold**``:red
``\`\`many colors\`\`:color[blue:#ffff00]``   | ``many colors``:color[blue:#ffff00]
``http://google.com``                         | http://google.com
``[[**click** me #myanchor]]``                | [[**click** me #myanchor]]
``[[click me [extra info] #myanchor popup]]`` | [[click me [extra info] #myanchor popup]]
-------------------------------------------------------------------------------

### More on links

The format is always ``[[title link]]`` or ``[[title [extra] link]]``. Notice you can nest bold, italic, strikeout and code inside the link ``title``.

### Anchors [[myanchor]]

You can place an anchor anywhere in the text using the syntax ``[[name]]`` where ''name'' is the name of the anchor.
You can then link the anchor with [[link #myanchor]], i.e. ``[[link #myanchor]]`` or [[link with an extra info [extra info] #myanchor]], i.e.
``[[link with an extra info [extra info] #myanchor]]``.

### Images

[[alt-string for the image [the image title] http://www.web2py.com/examples/static/web2py_logo.png right 200px]]
This paragraph has an image aligned to the right with a width of 200px. Its is placed using the code

``[[alt-string for the image [the image title] http://www.web2py.com/examples/static/web2py_logo.png right 200px]]``.

### Unordered Lists

``
- Dog
- Cat
- Mouse
``

is rendered as
- Dog
- Cat
- Mouse

Two new lines between items break the list in two lists.

### Ordered Lists

``
+ Dog
+ Cat
+ Mouse
``

is rendered as
+ Dog
+ Cat
+ Mouse


### Multilevel Lists

``
+ Dogs
 -- red
 -- brown
 -- black
+ Cats
 -- fluffy
 -- smooth
 -- bald
+ Mice
 -- small
 -- big
 -- huge
``

is rendered as
+ Dogs
 -- red
 -- brown
 -- black
+ Cats
 -- fluffy
 -- smooth
 -- bald
+ Mice
 -- small
 -- big
 -- huge


### Tables (with optional header and/or footer)

Something like this
``
-----------------
**A**|**B**|**C**
=================
  0  |  0  |  X
  0  |  X  |  0
  X  |  0  |  0
=================
**D**|**F**|**G**
-----------------:abc[id]
``
is a table and is rendered as
-----------------
**A**|**B**|**C**
=================
0 | 0 | X
0 | X | 0
X | 0 | 0
=================
**D**|**F**|**G**
-----------------:abc[id]
Four or more dashes delimit the table and | separates the columns.
The ``:abc``, ``:id[abc_1]`` or ``:abc[abc_1]`` at the end sets the class and/or id for the table and it is optional.

### Blockquote

A table with a single cell is rendered as a blockquote:

-----
Hello world
-----

Blockquote can contain headers, paragraphs, lists and tables:

``
-----
  This is a paragraph in a blockquote

  + item 1
  + item 2
  -- item 2.1
  -- item 2.2
  + item 3

  ---------
  0 | 0 | X
  0 | X | 0
  X | 0 | 0
  ---------:tableclass1
-----
``

is rendered as:
-----
  This is a paragraph in a blockquote

  + item 1
  + item 2
  -- item 2.1
  -- item 2.2
  + item 3

  ---------
  0 | 0 | X
  0 | X | 0
  X | 0 | 0
  ---------:tableclass1
-----


### Code, ``<code>``, escaping and extra stuff

``
def test():
    return "this is Python code"
``:python

Optionally a ` inside a ``!`!`...`!`!`` block can be inserted escaped with !`!.

**NOTE:** You can escape markmin constructions (\\'\\',\`\`,\*\*,\~\~,\[,\{,\]\},\$,\@) with '\\\\' character:
 so \\\\`\\\\` can replace !`!`! escape string

The ``:python`` after the markup is also optional. If present, by default, it is used to set the class of the <code> block.
The behavior can be overridden by passing an argument ``extra`` to the ``render`` function. For example:

``
markmin2html("!`!!`!aaa!`!!`!:custom",
             extra=dict(custom=lambda text: 'x'+text+'x'))
``:python

generates

``'xaaax'``:python

(the ``!`!`...`!`!:custom`` block is rendered by the ``custom=lambda`` function passed to ``render``).

### Line breaks

``[[NEWLINE]]`` tag is used to break lines:
``
#### Multiline [[NEWLINE]]
   title
paragraph [[NEWLINE]]
with breaks[[NEWLINE]]in it
``
generates:

#### Multiline [[NEWLINE]]
   title
paragraph [[NEWLINE]]
with breaks[[NEWLINE]]in it


### Html5 support

Markmin also supports the <video> and <audio> html5 tags using the notation:
``
[[message link video]]
[[message link audio]]

[[message [title] link video]]
[[message [title] link audio]]
``
where ``message`` will be shown in browsers without HTML5 video/audio tags support.

### Latex and other extensions

Formulas can be embedded into HTML with ''\$\$``formula``\$\$''.
You can use Google charts to render the formula:

``
LATEX = '<img src="http://chart.apis.google.com/chart?cht=tx&chl=%s" />'
markmin2html(text,{'latex':lambda code: LATEX % urllib.quote(code)})
``

### Code with syntax highlighting

This requires a syntax highlighting tool, such as the web2py CODE helper.

``
extra={'code_cpp':lambda text: CODE(text,language='cpp').xml(),
       'code_java':lambda text: CODE(text,language='java').xml(),
       'code_python':lambda text: CODE(text,language='python').xml(),
       'code_html':lambda text: CODE(text,language='html').xml()}
``
or simple:
``
extra={'code':lambda text,lang='python': CODE(text,language=lang).xml()}
``
``
markmin2html(text,extra=extra)
``

Code can now be marked up as in this example:
``
!`!`
<html><body>example</body></html>
!`!`:code_html
``
OR
``
!`!`
<html><body>example</body></html>
!`!`:code[html]
``

### Citations and References

Citations are treated as internal links in html and proper citations in latex if there is a final section called "References". Items like

``
- [[key]] value
``

in the References will be translated into Latex

``
\\bibitem{key} value
``

Here is an example of usage:

``
As shown in Ref.!`!`mdipierro`!`!:cite

## References

- [[mdipierro]] web2py Manual, 3rd Edition, lulu.com
``

### Caveats

``<ul/>``, ``<ol/>``, ``<code/>``, ``<table/>``, ``<blockquote/>``, ``<h1/>``, ..., ``<h6/>`` do not have ``<p>...</p>`` around them.

"""
html_colors=['aqua', 'black', 'blue', 'fuchsia', 'gray', 'green',
             'lime', 'maroon', 'navy', 'olive', 'purple', 'red',
             'silver', 'teal', 'white', 'yellow']

META = '\x06'
LINK = '\x07'
DISABLED_META = '\x08'
LATEX = '<img src="http://chart.apis.google.com/chart?cht=tx&chl=%s" />'
regex_URL=re.compile(r'@/(?P<a>\w*)/(?P<c>\w*)/(?P<f>\w*(\.\w+)?)(/(?P<args>[\w\.\-/]+))?')
regex_env2=re.compile(r'@\{(?P<a>[\w\-\.]+?)(\:(?P<b>.*?))?\}')
regex_expand_meta = re.compile('('+META+'|'+DISABLED_META+'|````)')
regex_dd=re.compile(r'\$\$(?P<latex>.*?)\$\$')
regex_code = re.compile('('+META+'|'+DISABLED_META+r'|````)|(``(?P<t>.+?)``(?::(?P<c>[a-zA-Z][_a-zA-Z\-\d]*)(?:\[(?P<p>[^\]]*)\])?)?)',re.S)
regex_strong=re.compile(r'\*\*(?P<t>[^\s*]+( +[^\s*]+)*)\*\*')
regex_del=re.compile(r'~~(?P<t>[^\s*]+( +[^\s*]+)*)~~')
regex_em=re.compile(r"''(?P<t>([^\s']| |'(?!'))+)''")
regex_num=re.compile(r"^\s*[+-]?((\d+(\.\d*)?)|\.\d+)([eE][+-]?[0-9]+)?\s*$")
regex_list=re.compile('^(?:(?:(#{1,6})|(?:(\.+|\++|\-+)(\.)?))\s*)?(.*)$')
regex_bq_headline=re.compile('^(?:(\.+|\++|\-+)(\.)?\s+)?(-{3}-*)$')
regex_tq=re.compile('^(-{3}-*)(?::(?P<c>[a-zA-Z][_a-zA-Z\-\d]*)(?:\[(?P<p>[a-zA-Z][_a-zA-Z\-\d]*)\])?)?$')
regex_proto = re.compile(r'(?<!["\w>/=])(?P<p>\w+):(?P<k>\w+://[\w\d\-+=?%&/:.]+)', re.M)
regex_auto = re.compile(r'(?<!["\w>/=])(?P<k>\w+://[\w\d\-+_=?%&/:.,;#]+\w|[\w\-.]+@[\w\-.]+)',re.M)
regex_link=re.compile(r'('+LINK+r')|\[\[(?P<s>.+?)\]\]',re.S)
regex_link_level2=re.compile(r'^(?P<t>\S.*?)?(?:\s+\[(?P<a>.+?)\])?(?:\s+(?P<k>\S+))?(?:\s+(?P<p>popup))?\s*$',re.S)
regex_media_level2=re.compile(r'^(?P<t>\S.*?)?(?:\s+\[(?P<a>.+?)\])?(?:\s+(?P<k>\S+))?\s+(?P<p>img|IMG|left|right|center|video|audio|blockleft|blockright)(?:\s+(?P<w>\d+px))?\s*$',re.S)

regex_markmin_escape = re.compile(r"(\\*)(['`:*~\\[\]{}@\$+\-.#\n])")
regex_backslash = re.compile(r"\\(['`:*~\\[\]{}@\$+\-.#\n])")
ttab_in  = maketrans("'`:*~\\[]{}@$+-.#\n", '\x0b\x0c\x0e\x0f\x10\x11\x12\x13\x14\x15\x16\x17\x18\x19\x1a\x1b\x05')
ttab_out = maketrans('\x0b\x0c\x0e\x0f\x10\x11\x12\x13\x14\x15\x16\x17\x18\x19\x1a\x1b\x05',"'`:*~\\[]{}@$+-.#\n")
regex_quote = re.compile('(?P<name>\w+?)\s*\=\s*')

def make_dict(b):
    return '{%s}' % regex_quote.sub("'\g<name>':",b)
    
def safe_eval(node_or_string, env):
    """
    Safely evaluate an expression node or a string containing a Python
    expression.  The string or node provided may only consist of the following
    Python literal structures: strings, numbers, tuples, lists, dicts, booleans,
    and None.
    """
    _safe_names = {'None': None, 'True': True, 'False': False}
    _safe_names.update(env)
    if isinstance(node_or_string, basestring):
        node_or_string = ast_parse(node_or_string, mode='eval')
    if isinstance(node_or_string, ast.Expression):
        node_or_string = node_or_string.body
    def _convert(node):
        if isinstance(node, ast.Str):
            return node.s
        elif isinstance(node, ast.Num):
            return node.n
        elif isinstance(node, ast.Tuple):
            return tuple(map(_convert, node.elts))
        elif isinstance(node, ast.List):
            return list(map(_convert, node.elts))
        elif isinstance(node, ast.Dict):
            return dict((_convert(k), _convert(v)) for k, v
                        in zip(node.keys, node.values))
        elif isinstance(node, ast.Name):
            if node.id in _safe_names:
                return _safe_names[node.id]
        elif isinstance(node, ast.BinOp) and \
             isinstance(node.op, (Add, Sub)) and \
             isinstance(node.right, Num) and \
             isinstance(node.right.n, complex) and \
             isinstance(node.left, Num) and \
             isinstance(node.left.n, (int, long, float)):
            left = node.left.n
            right = node.right.n
            if isinstance(node.op, Add):
                return left + right
            else:
                return left - right
        raise ValueError('malformed string')
    return _convert(node_or_string)

def markmin_escape(text):
    """ insert \\ before markmin control characters: '`:*~[]{}@$ """
    return regex_markmin_escape.sub(
       lambda m: '\\'+m.group(0).replace('\\','\\\\'), text)

def replace_autolinks(text,autolinks):
    return regex_auto.sub(lambda m: autolinks(m.group('k')), text)

def replace_at_urls(text,url):
    # this is experimental @{function/args}
    def u1(match,url=url):
        a,c,f,args = match.group('a','c','f','args')
        return url(a=a or None,c=c or None,f = f or None,
                   args=(args or '').split('/'), scheme=True, host=True)
    return regex_URL.sub(u1,text)

def replace_components(text,env):
    # not perfect but acceptable
    def u2(match, env=env):
        f = env.get(match.group('a'), match.group(0))
        if callable(f):
            b = match.group('b')
            try:
                b = safe_eval(make_dict(b),env)
            except:
                pass
            try:
                f = f(**b) if isinstance(b,dict) else f(b)
            except Exception, e:
                f = 'ERROR: %s' % e
            return str(f)
    text = regex_env2.sub(u2, text)
    return text

def autolinks_simple(url):
    """
    it automatically converts the url to link,
    image, video or audio tag
    """
    u_url=url.lower()
    if '@' in url and not '://' in url:
        return '<a href="mailto:%s">%s</a>' % (url, url)
    elif u_url.endswith(('.jpg','.jpeg','.gif','.png')):
        return '<img src="%s" controls />' % url
    elif u_url.endswith(('.mp4','.mpeg','.mov','.ogv')):
        return '<video src="%s" controls></video>' % url
    elif u_url.endswith(('.mp3','.wav','.ogg')):
        return '<audio src="%s" controls></audio>' % url
    return '<a href="%s">%s</a>' % (url,url)

def protolinks_simple(proto, url):
    """
    it converts url to html-string using appropriate proto-prefix:
    Uses for construction "proto:url", e.g.:
        "iframe:http://www.example.com/path" will call protolinks()
        with parameters:
            proto="iframe"
            url="http://www.example.com/path"
    """
    if proto in ('iframe','embed'): #== 'iframe':
        return '<iframe src="%s" frameborder="0" allowfullscreen></iframe>'%url
    #elif proto == 'embed':  # NOTE: embed is a synonym to iframe now
    #    return '<a href="%s" class="%sembed">%s></a>'%(url,class_prefix,url)
    elif proto == 'qr':
        return '<img style="width:100px" src="http://chart.apis.google.com/chart?cht=qr&chs=100x100&chl=%s&choe=UTF-8&chld=H" alt="QR Code" title="QR Code" />'%url
    return proto+':'+url

def email_simple(email):
   return '<a href="mailto:%s">%s</a>' % (email, email)

def render(text,
           extra={},
           allowed={},
           sep='p',
           URL=None,
           environment=None,
           latex='google',
           autolinks='default',
           protolinks='default',
           class_prefix='',
           id_prefix='markmin_',
           pretty_print=False):
    """
    Arguments:
    - text is the text to be processed
    - extra is a dict like extra=dict(custom=lambda value: value) that process custom code
      as in " ``this is custom code``:custom "
    - allowed is a dictionary of list of allowed classes like
      allowed = dict(code=('python','cpp','java'))
    - sep can be 'p' to separate text in <p>...</p>
      or can be 'br' to separate text using <br />
    - URL -
    - environment is a dictionary of environment variables (can be accessed with @{variable}
    - latex -
    - autolinks is a function to convert auto urls to html-code (default is autolinks(url) )
    - protolinks is a function to convert proto-urls (e.g."proto:url") to html-code
      (default is protolinks(proto,url))
    - class_prefix is a prefix for ALL classes in markmin text. E.g. if class_prefix='my_'
      then for ``test``:cls class will be changed to "my_cls" (default value is '')
    - id_prefix is prefix for ALL ids in markmin text (default value is 'markmin_'). E.g.:
        -- [[id]] will be converted to <span class="anchor" id="markmin_id"></span>
        -- [[link #id]] will be converted to <a href="#markmin_id">link</a>
        -- ``test``:cls[id] will be converted to <code class="cls" id="markmin_id">test</code>

    >>> render('this is\\n# a section\\n\\nparagraph')
    '<p>this is</p><h1>a section</h1><p>paragraph</p>'
    >>> render('this is\\n## a subsection\\n\\nparagraph')
    '<p>this is</p><h2>a subsection</h2><p>paragraph</p>'
    >>> render('this is\\n### a subsubsection\\n\\nparagraph')
    '<p>this is</p><h3>a subsubsection</h3><p>paragraph</p>'
    >>> render('**hello world**')
    '<p><strong>hello world</strong></p>'
    >>> render('``hello world``')
    '<code>hello world</code>'
    >>> render('``hello world``:python')
    '<code class="python">hello world</code>'
    >>> render('``\\nhello\\nworld\\n``:python')
    '<pre><code class="python">hello\\nworld</code></pre>'
    >>> render('``hello world``:python[test_id]')
    '<code class="python" id="markmin_test_id">hello world</code>'
    >>> render('``hello world``:id[test_id]')
    '<code id="markmin_test_id">hello world</code>'
    >>> render('``\\nhello\\nworld\\n``:python[test_id]')
    '<pre><code class="python" id="markmin_test_id">hello\\nworld</code></pre>'
    >>> render('``\\nhello\\nworld\\n``:id[test_id]')
    '<pre><code id="markmin_test_id">hello\\nworld</code></pre>'
    >>> render("''hello world''")
    '<p><em>hello world</em></p>'
    >>> render('** hello** **world**')
    '<p>** hello** <strong>world</strong></p>'

    >>> render('- this\\n- is\\n- a list\\n\\nand this\\n- is\\n- another')
    '<ul><li>this</li><li>is</li><li>a list</li></ul><p>and this</p><ul><li>is</li><li>another</li></ul>'

    >>> render('+ this\\n+ is\\n+ a list\\n\\nand this\\n+ is\\n+ another')
    '<ol><li>this</li><li>is</li><li>a list</li></ol><p>and this</p><ol><li>is</li><li>another</li></ol>'

    >>> render("----\\na | b\\nc | d\\n----\\n")
    '<table><tbody><tr class="first"><td>a</td><td>b</td></tr><tr class="even"><td>c</td><td>d</td></tr></tbody></table>'

    >>> render("----\\nhello world\\n----\\n")
    '<blockquote>hello world</blockquote>'

    >>> render('[[myanchor]]')
    '<p><span class="anchor" id="markmin_myanchor"></span></p>'

    >>> render('[[ http://example.com]]')
    '<p><a href="http://example.com">http://example.com</a></p>'

    >>> render('[[bookmark [http://example.com] ]]')
    '<p><span class="anchor" id="markmin_bookmark"><a href="http://example.com">http://example.com</a></span></p>'

    >>> render('[[this is a link http://example.com]]')
    '<p><a href="http://example.com">this is a link</a></p>'

    >>> render('[[this is an image http://example.com left]]')
    '<p><img src="http://example.com" alt="this is an image" style="float:left" /></p>'

    >>> render('[[this is an image http://example.com left 200px]]')
    '<p><img src="http://example.com" alt="this is an image" style="float:left;width:200px" /></p>'

    >>> render("[[Your browser doesn't support <video> HTML5 tag http://example.com video]]")
    '<p><video controls="controls"><source src="http://example.com" />Your browser doesn\\'t support &lt;video&gt; HTML5 tag</video></p>'

    >>> render("[[Your browser doesn't support <audio> HTML5 tag http://example.com audio]]")
    '<p><audio controls="controls"><source src="http://example.com" />Your browser doesn\\'t support &lt;audio&gt; HTML5 tag</audio></p>'

    >>> render("[[Your\\nbrowser\\ndoesn't\\nsupport\\n<audio> HTML5 tag http://exam\\\\\\nple.com\\naudio]]")
    '<p><audio controls="controls"><source src="http://example.com" />Your browser doesn\\'t support &lt;audio&gt; HTML5 tag</audio></p>'

    >>> render('[[this is a **link** http://example.com]]')
    '<p><a href="http://example.com">this is a <strong>link</strong></a></p>'

    >>> render("``aaa``:custom", extra=dict(custom=lambda text: 'x'+text+'x'))
    'xaaax'

    >>> print render(r"$$\int_a^b sin(x)dx$$")
    <img src="http://chart.apis.google.com/chart?cht=tx&chl=%5Cint_a%5Eb%20sin%28x%29dx" />

    >>> markmin2html(r"use backslash: \[\[[[mess\[[ag\]]e link]]\]]")
    '<p>use backslash: [[<a href="link">mess[[ag]]e</a>]]</p>'

    >>> markmin2html("backslash instead of exclamation sign: \``probe``")
    '<p>backslash instead of exclamation sign: ``probe``</p>'

    >>> render(r"simple image: [[\[[this is an image\]] http://example.com IMG]]!!!")
    '<p>simple image: <img src="http://example.com" alt="[[this is an image]]" />!!!</p>'

    >>> render(r"simple link no anchor with popup: [[ http://example.com popup]]")
    '<p>simple link no anchor with popup: <a href="http://example.com" target="_blank">http://example.com</a></p>'

    >>> render("auto-url: http://example.com")
    '<p>auto-url: <a href="http://example.com">http://example.com</a></p>'

    >>> render("auto-image: (http://example.com/image.jpeg)")
    '<p>auto-image: (<img src="http://example.com/image.jpeg" controls />)</p>'

    >>> render("qr: (qr:http://example.com/image.jpeg)")
    '<p>qr: (<img style="width:100px" src="http://chart.apis.google.com/chart?cht=qr&chs=100x100&chl=http://example.com/image.jpeg&choe=UTF-8&chld=H" alt="QR Code" title="QR Code" />)</p>'

    >>> render("embed: (embed:http://example.com/page)")
    '<p>embed: (<iframe src="http://example.com/page" frameborder="0" allowfullscreen></iframe>)</p>'

    >>> render("iframe: (iframe:http://example.com/page)")
    '<p>iframe: (<iframe src="http://example.com/page" frameborder="0" allowfullscreen></iframe>)</p>'

    >>> render("title1: [[test message [simple \[test\] title] http://example.com ]] test")
    '<p>title1: <a href="http://example.com" title="simple [test] title">test message</a> test</p>'

    >>> render("title2: \[\[[[test message [simple title] http://example.com popup]]\]]")
    '<p>title2: [[<a href="http://example.com" title="simple title" target="_blank">test message</a>]]</p>'

    >>> render("title3: [[ [link w/o anchor but with title] http://www.example.com ]]")
    '<p>title3: <a href="http://www.example.com" title="link w/o anchor but with title">http://www.example.com</a></p>'

    >>> render("title4: [[ [simple title] http://www.example.com popup]]")
    '<p>title4: <a href="http://www.example.com" title="simple title" target="_blank">http://www.example.com</a></p>'

    >>> render("title5: [[test message [simple title] http://example.com IMG]]")
    '<p>title5: <img src="http://example.com" alt="test message" title="simple title" /></p>'

    >>> render("title6: [[[test message w/o title] http://example.com IMG]]")
    '<p>title6: <img src="http://example.com" alt="[test message w/o title]" /></p>'

    >>> render("title7: [[[this is not a title] [this is a title] http://example.com IMG]]")
    '<p>title7: <img src="http://example.com" alt="[this is not a title]" title="this is a title" /></p>'

    >>> render("title8: [[test message [title] http://example.com center]]")
    '<p>title8: <p style="text-align:center"><img src="http://example.com" alt="test message" title="title" /></p></p>'

    >>> render("title9: [[test message [title] http://example.com left]]")
    '<p>title9: <img src="http://example.com" alt="test message" title="title" style="float:left" /></p>'

    >>> render("title10: [[test message [title] http://example.com right 100px]]")
    '<p>title10: <img src="http://example.com" alt="test message" title="title" style="float:right;width:100px" /></p>'

    >>> render("title11: [[test message [title] http://example.com center 200px]]")
    '<p>title11: <p style="text-align:center"><img src="http://example.com" alt="test message" title="title" style="width:200px" /></p></p>'

    >>> render(r"\\[[probe]]")
    '<p>[[probe]]</p>'

    >>> render(r"\\\\[[probe]]")
    '<p>\\\\<span class="anchor" id="markmin_probe"></span></p>'

    >>> render(r"\\\\\\[[probe]]")
    '<p>\\\\[[probe]]</p>'

    >>> render(r"\\\\\\\\[[probe]]")
    '<p>\\\\\\\\<span class="anchor" id="markmin_probe"></span></p>'

    >>> render(r"\\\\\\\\\[[probe]]")
    '<p>\\\\\\\\[[probe]]</p>'

    >>> render(r"\\\\\\\\\\\[[probe]]")
    '<p>\\\\\\\\\\\\<span class="anchor" id="markmin_probe"></span></p>'

    >>> render("``[[ [\\[[probe\]\\]] URL\\[x\\]]]``:red[dummy_params]")
    '<span style="color: red"><a href="URL[x]" title="[[probe]]">URL[x]</a></span>'

    >>> render("the \\**text**")
    '<p>the **text**</p>'

    >>> render("the \\``text``")
    '<p>the ``text``</p>'

    >>> render("the \\\\''text''")
    "<p>the ''text''</p>"

    >>> render("the [[link [**with** ``<b>title</b>``:red] http://www.example.com]]")
    '<p>the <a href="http://www.example.com" title="**with** ``&lt;b&gt;title&lt;/b&gt;``:red">link</a></p>'

    >>> render("the [[link \\[**without** ``<b>title</b>``:red\\] http://www.example.com]]")
    '<p>the <a href="http://www.example.com">link [<strong>without</strong> <span style="color: red">&lt;b&gt;title&lt;/b&gt;</span>]</a></p>'

    >>> render("aaa-META-``code``:text[]-LINK-[[link http://www.example.com]]-LINK-[[image http://www.picture.com img]]-end")
    '<p>aaa-META-<code class="text">code</code>-LINK-<a href="http://www.example.com">link</a>-LINK-<img src="http://www.picture.com" alt="image" />-end</p>'

    >>> render("[[<a>test</a> [<a>test2</a>] <a>text3</a>]]")
    '<p><a href="&lt;a&gt;text3&lt;/a&gt;" title="&lt;a&gt;test2&lt;/a&gt;">&lt;a&gt;test&lt;/a&gt;</a></p>'

    >>> render("[[<a>test</a> [<a>test2</a>] <a>text3</a> IMG]]")
    '<p><img src="&lt;a&gt;text3&lt;/a&gt;" alt="&lt;a&gt;test&lt;/a&gt;" title="&lt;a&gt;test2&lt;/a&gt;" /></p>'

    >>> render("**bold** ''italic'' ~~strikeout~~")
    '<p><strong>bold</strong> <em>italic</em> <del>strikeout</del></p>'

    >>> render("this is ``a red on yellow text``:c[#FF0000:#FFFF00]")
    '<p>this is <span style="color: #FF0000;background-color: #FFFF00;">a red on yellow text</span></p>'

    >>> render("this is ``a text with yellow background``:c[:yellow]")
    '<p>this is <span style="background-color: yellow;">a text with yellow background</span></p>'

    >>> render("this is ``a colored text (RoyalBlue)``:color[rgb(65,105,225)]")
    '<p>this is <span style="color: rgb(65,105,225);">a colored text (RoyalBlue)</span></p>'

    >>> render("this is ``a green text``:color[green:]")
    '<p>this is <span style="color: green;">a green text</span></p>'

    >>> render("**@{probe:1}**", environment=dict(probe=lambda t:"test %s" % t))
    '<p><strong>test 1</strong></p>'

    >>> render("**@{probe:t=a}**", environment=dict(probe=lambda t:"test %s" % t, a=1))
    '<p><strong>test 1</strong></p>'

    >>> render('[[id1 [span **messag** in ''markmin''] ]] ... [[**link** to id [link\\\'s title] #mark1]]')
    '<p><span class="anchor" id="markmin_id1">span <strong>messag</strong> in markmin</span> ... <a href="#markmin_mark1" title="link\\\'s title"><strong>link</strong> to id</a></p>'

    >>> render('# Multiline[[NEWLINE]]\\n title\\nParagraph[[NEWLINE]]\\nwith breaks[[NEWLINE]]\\nin it')
    '<h1>Multiline<br /> title</h1><p>Paragraph<br /> with breaks<br /> in it</p>'

    >>> render("anchor with name 'NEWLINE': [[NEWLINE [ ] ]]")
    '<p>anchor with name \\'NEWLINE\\': <span class="anchor" id="markmin_NEWLINE"></span></p>'

    >>> render("anchor with name 'NEWLINE': [[NEWLINE [newline] ]]")
    '<p>anchor with name \\'NEWLINE\\': <span class="anchor" id="markmin_NEWLINE">newline</span></p>'
    """
    if autolinks=="default": autolinks = autolinks_simple
    if protolinks=="default": protolinks = protolinks_simple
    pp='\n' if pretty_print else ''
    if isinstance(text,unicode):
        text = text.encode('utf8')
    text = str(text or '')
    text = regex_backslash.sub(lambda m: m.group(1).translate(ttab_in), text)
    text = text.replace('\x05','').replace('\r\n', '\n') # concatenate strings separeted by \\n

    if URL is not None:
        text = replace_at_urls(text,URL)

    if latex == 'google':
        text = regex_dd.sub('``\g<latex>``:latex ', text)

    #############################################################
    # replace all blocks marked with ``...``:class[id] with META
    # store them into segments they will be treated as code
    #############################################################
    segments = []
    def mark_code(m):
        g = m.group(0)
        if g in (META, DISABLED_META ):
            segments.append((None, None, None, g))
            return m.group()
        elif g == '````':
            segments.append((None, None, None, ''))
            return m.group()
        else:
            c = m.group('c') or ''
            p = m.group('p') or ''
            if 'code' in allowed and not c in allowed['code']: c = ''
            code = m.group('t').replace('!`!','`')
            segments.append((code, c, p, m.group(0)))
        return META
    text = regex_code.sub(mark_code, text)

    #############################################################
    # replace all blocks marked with [[...]] with LINK
    # store them into links they will be treated as link
    #############################################################
    links = []
    def mark_link(m):
        links.append( None if m.group() == LINK
                         else m.group('s') )
        return LINK
    text = regex_link.sub(mark_link, text)
    text = escape(text)

    if protolinks:
        text = regex_proto.sub(lambda m: protolinks(*m.group('p','k')), text)

    if autolinks:
        text = replace_autolinks(text,autolinks)

    #############################################################
    # normalize spaces
    #############################################################
    strings=text.split('\n')

    def parse_title(t, s): #out, lev, etags, tag, s):
        hlevel=str(len(t))
        out.extend(etags[::-1])
        out.append("<h%s>%s"%(hlevel,s))
        etags[:]=["</h%s>%s"%(hlevel,pp)]
        lev=0
        ltags[:]=[]
        tlev[:]=[]
        return (lev, 'h')

    def parse_list(t, p, s, tag, lev, mtag, lineno):
        lent=len(t)
        if lent<lev: # current item level < previous item level
            while ltags[-1]>lent:
                ltags.pop()
                out.append(etags.pop())
            lev=lent
            tlev[lev:]=[]

        if lent>lev: # current item level > previous item level
            if lev==0: # previous line is not a list (paragraph or title)
                out.extend(etags[::-1])
                ltags[:]=[]
                tlev[:]=[]
                etags[:]=[]
            if pend and mtag == '.': # paragraph in a list:
                out.append(etags.pop())
                ltags.pop()
            for i in xrange(lent-lev):
                out.append('<'+tag+'>'+pp)
                etags.append('</'+tag+'>'+pp)
                lev+=1
                ltags.append(lev)
                tlev.append(tag)
        elif lent == lev:
            if tlev[-1] != tag:
                # type of list is changed (ul<=>ol):
                for i in xrange(ltags.count(lent)):
                    ltags.pop()
                    out.append(etags.pop())
                tlev[-1]=tag
                out.append('<'+tag+'>'+pp)
                etags.append('</'+tag+'>'+pp)
                ltags.append(lev)
            else:
                if ltags.count(lev)>1:
                    out.append(etags.pop())
                    ltags.pop()
        mtag='l'
        out.append('<li>')
        etags.append('</li>'+pp)
        ltags.append(lev)
        if s[:1] == '-':
            (s, mtag, lineno) = parse_table_or_blockquote(s, mtag, lineno)
        if p and mtag=='l':
            (lev,mtag,lineno)=parse_point(t, s, lev, '', lineno)
        else:
            out.append(s)

        return (lev, mtag, lineno)

    def parse_point(t, s, lev, mtag, lineno):
        """ paragraphs in lists """
        lent=len(t)
        if lent>lev:
            return parse_list(t, '.', s, 'ul', lev, mtag, lineno)
        elif lent<lev:
            while ltags[-1]>lent:
                ltags.pop()
                out.append(etags.pop())
            lev=lent
            tlev[lev:]=[]
            mtag=''
        elif lent==lev:
            if pend and mtag == '.':
                out.append(etags.pop())
                ltags.pop()
        if br and mtag in ('l','.'):
            out.append(br)
        if s == META:
           mtag = ''
        else:
            mtag = '.'
            if s[:1] == '-':
               (s, mtag, lineno) = parse_table_or_blockquote(s, mtag, lineno)
            if mtag == '.':
                out.append(pbeg)
                if pend:
                    etags.append(pend)
                    ltags.append(lev)
        out.append(s)
        return (lev, mtag, lineno)

    def parse_table_or_blockquote(s, mtag, lineno):
        # check next line. If next line :
        # - is empty -> this is an <hr /> tag
        # - consists '|' -> table
        # - consists other characters -> blockquote
        if (lineno+1 >= strings_len or
            not(s.count('-') == len(s) and len(s)>3)):
           return (s, mtag, lineno)

        lineno+=1
        s = strings[lineno].strip()
        if s:
            if '|' in s:
                # table
                tout=[]
                thead=[]
                tbody=[]
                rownum=0
                t_id = ''
                t_cls = ''

                # parse table:
                while lineno < strings_len:
                    s = strings[lineno].strip()
                    if s[:1] == '=':
                        # header or footer
                        if s.count('=')==len(s) and len(s)>3:  
                            if not thead: # if thead list is empty:
                                thead = tout
                            else:
                                tbody.extend(tout)
                            tout = []
                            rownum=0
                            lineno+=1
                            continue

                    m = regex_tq.match(s)
                    if m:
                        t_cls = m.group('c') or ''
                        t_id = m.group('p') or ''
                        break

                    if rownum % 2:
                       tr = '<tr class="even">'
                    else:
                       tr = '<tr class="first">' if rownum == 0 else '<tr>'
                    tout.append(tr + ''.join(['<td%s>%s</td>' % (
                                    ' class="num"' 
                                    if regex_num.match(f) else '',
                                    f.strip()
                                    ) for f in s.split('|')])+'</tr>'+pp)
                    rownum+=1
                    lineno+=1

                t_cls = ' class="%s%s"'%(class_prefix, t_cls) \
                    if t_cls and t_cls != 'id' else ''
                t_id  = ' id="%s%s"'%(id_prefix, t_id) if t_id else ''
                s = ''
                if thead:
                    s += '<thead>'+pp+''.join([l for l in thead])+'</thead>'+pp
                if not tbody: # tbody strings are in tout list
                    tbody = tout
                    tout = []
                if tbody: # if tbody list is not empty:
                    s += '<tbody>'+pp+''.join([l for l in tbody])+'</tbody>'+pp
                if tout: # tfoot is not empty:
                    s += '<tfoot>'+pp+''.join([l for l in tout])+'</tfoot>'+pp
                s = '<table%s%s>%s%s</table>%s' % (t_cls, t_id, pp, s, pp)
                mtag='t'
            else:
                # parse blockquote:
                bq_begin=lineno
                t_mode = False # embedded table
                t_cls = ''
                t_id = ''

                # search blockquote closing line:
                while lineno < strings_len:
                    s = strings[lineno].strip()
                    if not t_mode:
                        m = regex_tq.match(s)
                        if m:
                            if (lineno+1 == strings_len or 
                                '|' not in strings[lineno+1]):
                               t_cls = m.group('c') or ''
                               t_id = m.group('p') or ''
                               break

                        if regex_bq_headline.match(s):
                            if (lineno+1 < strings_len and 
                                strings[lineno+1].strip()):
                                    t_mode = True
                            lineno+=1
                            continue
                    elif regex_tq.match(s):
                        t_mode=False
                        lineno+=1
                        continue

                    lineno+=1

                t_cls = ' class="%s%s"'%(class_prefix,t_cls) \
                    if t_cls and t_cls != 'id' else ''
                t_id  = ' id="%s%s"'%(id_prefix,t_id) \
                    if t_id else ''
                
                s = '<blockquote%s%s>%s</blockquote>%s' \
                         % (t_cls,
                            t_id,
                            '\n'.join(strings[bq_begin:lineno]),pp)
                mtag='q'
        else:
            s = '<hr />'
            lineno-=1
            mtag='q'
        return (s, 'q', lineno)

    if sep == 'p':
      pbeg = "<p>"
      pend = "</p>"+pp
      br = ''
    else:
      pbeg = pend = ''
      br = "<br />"+pp if sep=='br' else ''

    lev = 0    # nesting level of lists
    c0 = ''    # first character of current line
    out = []   # list of processed lines
    etags = [] # trailing tags
    ltags = [] # level# correspondent to trailing tag
    tlev = []  # list of tags for each level ('ul' or 'ol')
    mtag = ''  # marked tag (~last tag) ('l','.','h','p','t'). Used to set <br/>
               # and to avoid <p></p> around tables and blockquotes
    lineno = 0
    strings_len = len(strings)
    while lineno < strings_len:
        s0 = strings[lineno][:1]
        s = strings[lineno].strip()
        """ #     +     -     .             ---------------------
            ##    ++    --    ..   -------  field | field | field  <-title
            ###   +++   ---   ...  quote    =====================
            ####  ++++  ----  .... -------  field | field | field  <-body
            ##### +++++ ----- .....         ---------------------:class[id]
        """
        pc0=c0 # first character of previous line
        c0=s[:1]
        if c0: # for non empty strings
            if c0 in "#+-.": # first character is one of: # + - .
                (t1,t2,p,ss) = regex_list.findall(s)[0]
                # t1 - tag ("###")
                # t2 - tag ("+++", "---", "...")
                # p - paragraph point ('.')->for "++." or "--."
                # ss - other part of string
                if t1 or t2:
                    # headers and lists:
                    if c0 == '#': # headers
                        (lev, mtag) = parse_title(t1, ss)
                        lineno+=1
                        continue
                    elif c0 == '+': # ordered list
                        (lev, mtag, lineno)= parse_list(t2, p, ss, 'ol', lev, mtag, lineno)
                        lineno+=1
                        continue
                    elif c0 == '-': # unordered list, table or blockquote
                        if p or ss:
                            (lev, mtag, lineno) = parse_list(t2, p, ss, 'ul', lev, mtag, lineno)
                            lineno+=1
                            continue
                        else:
                            (s, mtag, lineno) = parse_table_or_blockquote(s, mtag, lineno)
                    elif lev>0: # and c0 == '.' # paragraph in lists
                        (lev, mtag, lineno) = parse_point(t2, ss, lev, mtag, lineno)
                        lineno+=1
                        continue

            if lev == 0 and (mtag == 'q' or s == META):
                # new paragraph
                pc0=''

            if pc0 == '' or (mtag != 'p' and s0 not in (' ','\t')):
                # paragraph
                out.extend(etags[::-1])
                etags=[]
                ltags=[]
                tlev=[]
                lev=0
                if br and mtag == 'p': out.append(br)
                if mtag != 'q' and s != META:
                   if pend: etags=[pend]
                   out.append(pbeg)
                   mtag = 'p'
                else:
                   mtag = ''
                out.append(s)
            else:
                if lev>0 and mtag=='.' and s == META:
                    out.append(etags.pop())
                    ltags.pop()
                    out.append(s)
                    mtag = ''
                else:
                    out.append(' '+s)
        lineno+=1
    out.extend(etags[::-1])
    text = ''.join(out)

    #############################################################
    # do strong,em,del
    #############################################################
    text = regex_strong.sub('<strong>\g<t></strong>', text)
    text = regex_del.sub('<del>\g<t></del>', text)
    text = regex_em.sub('<em>\g<t></em>', text)

    #############################################################
    # deal with images, videos, audios and links
    #############################################################
    def sub_media(m):
        t,a,k,p,w = m.group('t','a','k','p','w')
        if not k:
            return m.group(0)
        k = escape(k)
        t = t or ''
        style = 'width:%s' % w if w else ''
        title = ' title="%s"' % escape(a).replace(META, DISABLED_META) if a else ''
        p_begin = p_end = ''
        if p == 'center':
            p_begin = '<p style="text-align:center">'
            p_end = '</p>'+pp
        elif p == 'blockleft':
            p_begin = '<p style="text-align:left">'
            p_end = '</p>'+pp
        elif p == 'blockright':
            p_begin = '<p style="text-align:right">'
            p_end = '</p>'+pp
        elif p in ('left','right'):
            style = ('float:%s' % p)+(';%s' % style if style else '')
        if t and regex_auto.match(t):
            p_begin = p_begin + '<a href="%s">' % t
            p_end = '</a>' + p_end
            t = ''
        if style:
            style = ' style="%s"' % style
        if p in ('video','audio'):
            t = render(t, {}, {}, 'br', URL, environment, latex,
                       autolinks, protolinks, class_prefix, id_prefix, pretty_print)
            return '<%(p)s controls="controls"%(title)s%(style)s><source src="%(k)s" />%(t)s</%(p)s>' \
                    % dict(p=p, title=title, style=style, k=k, t=t)
        alt = ' alt="%s"'%escape(t).replace(META, DISABLED_META) if t else ''
        return '%(begin)s<img src="%(k)s"%(alt)s%(title)s%(style)s />%(end)s' \
                % dict(begin=p_begin, k=k, alt=alt, title=title, style=style, end=p_end)

    def sub_link(m):
        t,a,k,p = m.group('t','a','k','p')
        if not k and not t:
            return m.group(0)
        t = t or ''
        a = escape(a) if a else ''
        if k:
            if '#' in k and not ':' in k.split('#')[0]: 
                # wikipage, not external url
                k=k.replace('#','#'+id_prefix)
            k = escape(k)
            title = ' title="%s"' % a.replace(META, DISABLED_META) if a else ''
            target = ' target="_blank"' if p == 'popup' else ''
            t = render(t, {}, {}, 'br', URL, environment, latex, None,
                       None, class_prefix, id_prefix, pretty_print) if t else k
            return '<a href="%(k)s"%(title)s%(target)s>%(t)s</a>' \
                   % dict(k=k, title=title, target=target, t=t)
        if t == 'NEWLINE' and not a:
            return '<br />'+pp
        return '<span class="anchor" id="%s">%s</span>' % (
            escape(id_prefix+t),
            render(a, {},{},'br', URL,
                   environment, latex, autolinks,
                   protolinks, class_prefix,
                   id_prefix, pretty_print))
    
    parts = text.split(LINK)
    text = parts[0]
    for i,s in enumerate(links):
        if s == None:
            html = LINK
        else:
            html = regex_media_level2.sub(sub_media, s)
            if html == s:
                html = regex_link_level2.sub(sub_link, html)
            if html == s:
                # return unprocessed string as a signal of an error
                html = '[[%s]]'%s
        text += html + parts[i+1]

    #############################################################
    # process all code text
    #############################################################
    def expand_meta(m):
        code,b,p,s = segments.pop(0)
        if code==None or m.group() == DISABLED_META:
            return escape(s)
        if b in extra:
            if code[:1]=='\n': code=code[1:]
            if code[-1:]=='\n': code=code[:-1]
            if p:
                return str(extra[b](code,p))
            else:
                return str(extra[b](code))
        elif b=='cite':
            return '['+','.join('<a href="#%s" class="%s">%s</a>' \
                                   % (id_prefix+d,b,d) \
                                   for d in escape(code).split(','))+']'
        elif b=='latex':
            return LATEX % urllib.quote(code)
        elif b in html_colors:
            return '<span style="color: %s">%s</span>' \
                  % (b, render(code, {}, {}, 'br', URL, environment, latex,
                      autolinks, protolinks, class_prefix, id_prefix, pretty_print))
        elif b in ('c', 'color') and p:
             c=p.split(':')
             fg='color: %s;' % c[0] if c[0] else ''
             bg='background-color: %s;' % c[1] if len(c)>1 and c[1] else ''
             return '<span style="%s%s">%s</span>' \
                 % (fg, bg, render(code, {}, {}, 'br', URL, environment, latex,
                    autolinks, protolinks, class_prefix, id_prefix, pretty_print))
        cls = ' class="%s%s"'%(class_prefix,b) if b and b != 'id' else ''
        id  = ' id="%s%s"'%(id_prefix,escape(p)) if p else ''
        beg=(code[:1]=='\n')
        end=[None,-1][code[-1:]=='\n']
        if beg and end:
            return '<pre><code%s%s>%s</code></pre>%s' % (cls, id, escape(code[1:-1]), pp)
        return '<code%s%s>%s</code>' % (cls, id, escape(code[beg:end]))
    text = regex_expand_meta.sub(expand_meta, text)

    if environment:
        text = replace_components(text,environment)

    return text.translate(ttab_out)


def markmin2html(text, extra={}, allowed={}, sep='p',
                 autolinks='default', protolinks='default',
                 class_prefix='', id_prefix='markmin_', pretty_print=False):
    return render(text, extra, allowed, sep,
                  autolinks=autolinks, protolinks=protolinks,
                  class_prefix=class_prefix, id_prefix=id_prefix,
                  pretty_print=pretty_print)

def run_doctests():
    import doctest
    doctest.testmod()

if __name__ == '__main__':
    import sys
    import doctest
    from textwrap import dedent

    html=dedent("""
         <!doctype html>
         <html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">
         <head>
         <meta http-equiv="content-type" content="text/html; charset=utf-8" />
         %(style)s
         <title>%(title)s</title>
         </head>
         <body>
         %(body)s
         </body>
         </html>""")[1:]

    if sys.argv[1:2] == ['-h']:
        style=dedent("""
              <style>
                blockquote { background-color: #FFFAAE; padding: 7px; }
                table { border-collapse: collapse; }
                thead td { border-bottom: 1px solid; }
                tfoot td { border-top: 1px solid; }
                .tableclass1 { background-color: lime; }
                .tableclass1 thead { color: yellow; background-color: green; }
                .tableclass1 tfoot { color: yellow; background-color: green; }
                .tableclass1 .even td { background-color: #80FF7F; }
                .tableclass1 .first td {border-top: 1px solid; }

                td.num { text-align: right; }
                pre { background-color: #E0E0E0; padding: 5px; }
              </style>""")[1:]

        print html % dict(title="Markmin markup language",
                          style=style,
                          body=markmin2html(__doc__, pretty_print=True))
    elif sys.argv[1:2] == ['-t']:
        from timeit import Timer
        loops=1000
        ts = Timer("markmin2html(__doc__)","from markmin2html import markmin2html")
        print 'timeit "markmin2html(__doc__)":'
        t = min([ts.timeit(loops) for i in range(3)])
        print "%s loops, best of 3: %.3f ms per loop" % (loops, t/1000*loops)
    elif len(sys.argv) > 1:
        fargv = open(sys.argv[1],'r')
        try:
            markmin_text=fargv.read()

            # embed css file from second parameter into html file
            if len(sys.argv) > 2:
                if sys.argv[2].startswith('@'):
                    markmin_style = '<link rel="stylesheet" href="'+sys.argv[2][1:]+'"/>'
                else:
                    fargv2 = open(sys.argv[2],'r')
                    try:
                        markmin_style = "<style>\n" + fargv2.read() + "</style>"
                    finally:
                        fargv2.close()
            else:
                markmin_style = ""

            print html % dict(title=sys.argv[1], style=markmin_style,
                              body=markmin2html(markmin_text, pretty_print=True))
        finally:
            fargv.close()

    else:
        print "Usage: "+sys.argv[0]+" -h | -t | file.markmin [file.css|@path_to/css]"
        print "where: -h  - print __doc__"
        print "       -t  - timeit __doc__ (for testing purpuse only)"
        print "       file.markmin  [file.css] - process file.markmin + built in file.css (optional)"
        print "       file.markmin  [@path_to/css] - process file.markmin + link path_to/css (optional)"
        run_doctests()
        
