import sys
import re


def cleancss(text):
    text = re.compile('\s+').sub(' ', text)
    text = re.compile('\s*(?P<a>,|:)\s*').sub('\g<a> ', text)
    text = re.compile('\s*;\s*').sub(';\n    ', text)
    text = re.compile('\s*\{\s*').sub(' {\n    ', text)
    text = re.compile('\s*\}\s*').sub('\n}\n\n', text)
    return text


def cleanhtml(text):
    text = text.lower()
    r = re.compile('\<script.+?/script\>', re.DOTALL)
    scripts = r.findall(text)
    text = r.sub('<script />', text)
    r = re.compile('\<style.+?/style\>', re.DOTALL)
    styles = r.findall(text)
    text = r.sub('<style />', text)
    text = re.compile(
        '<(?P<tag>(input|meta|link|hr|br|img|param))(?P<any>[^\>]*)\s*(?<!/)>')\
        .sub('<\g<tag>\g<any> />', text)
    text = text.replace('\n', ' ')
    text = text.replace('>', '>\n')
    text = text.replace('<', '\n<')
    text = re.compile('\s*\n\s*').sub('\n', text)
    lines = text.split('\n')
    (indent, newlines) = (0, [])
    for line in lines:
        if line[:2] == '</': indent = indent - 1
        newlines.append(indent * '  ' + line)
        if not line[:2] == '</' and line[-1:] == '>' and \
            not line[-2:] in ['/>', '->']: indent = indent + 1
    text = '\n'.join(newlines)
    text = re.compile(
        '\<div(?P<a>( .+)?)\>\s+\</div\>').sub('<div\g<a>></div>', text)
    text = re.compile('\<a(?P<a>( .+)?)\>\s+(?P<b>[\w\s\(\)\/]+?)\s+\</a\>').sub('<a\g<a>>\g<b></a>', text)
    text = re.compile('\<b(?P<a>( .+)?)\>\s+(?P<b>[\w\s\(\)\/]+?)\s+\</b\>').sub('<b\g<a>>\g<b></b>', text)
    text = re.compile('\<i(?P<a>( .+)?)\>\s+(?P<b>[\w\s\(\)\/]+?)\s+\</i\>').sub('<i\g<a>>\g<b></i>', text)
    text = re.compile('\<span(?P<a>( .+)?)\>\s+(?P<b>[\w\s\(\)\/]+?)\s+\</span\>').sub('<span\g<a>>\g<b></span>', text)
    text = re.compile('\s+\<br(?P<a>.*?)\/\>').sub('<br\g<a>/>', text)
    text = re.compile('\>(?P<a>\s+)(?P<b>[\.\,\:\;])').sub('>\g<b>\g<a>', text)
    text = re.compile('\n\s*\n').sub('\n', text)
    for script in scripts:
        text = text.replace('<script />', script, 1)
    for style in styles:
        text = text.replace('<style />', cleancss(style), 1)
    return text


def read_file(filename):
    f = open(filename, 'r')
    try:
        return f.read()
    finally:
        f.close()

for file in sys.argv[1:]:
    data = read_file(file)
    open(file+'.bak2', 'w').write(data)
    if file[-4:] == '.css':
        data = cleancss(data)
    if file[-5:] == '.html':
        data = cleanhtml(data)
    open(file, 'w').write(data)
