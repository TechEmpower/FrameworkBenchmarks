"""
This file is part of the web2py Web Framework
Copyrighted by Massimo Di Pierro <mdipierro@cs.depaul.edu>
License: LGPLv3 (http://www.gnu.org/licenses/lgpl.html)
"""
import datetime
import decimal
from gluon.storage import Storage
from gluon.html import TAG, XmlComponent, xmlescape
from gluon.languages import lazyT
import gluon.contrib.rss2 as rss2

try:
    # try external module
    import simplejson as json_parser
except ImportError:
    try:
        # try stdlib (Python >= 2.6)
        import json as json_parser
    except:
        # fallback to pure-Python module
        import gluon.contrib.simplejson as json_parser

# simplejson >= 2.1.3 needs use_decimal = False
# to stringify decimals
decimal_false_option = json_parser.__version__.split('.') >= ['2', '1', '3']

have_yaml = True
try:
    import yaml as yamlib
except ImportError:
    have_yaml = False


def cast_keys(o, cast=str, encoding="utf-8"):
    """
    Builds a new object with <cast> type keys.
    Use this function if you are in Python < 2.6.5
    This avoids syntax errors when unpacking dictionary arguments.

    Args:
        o: is the object input
        cast:  (defaults to str) is an object type or function
            which supports conversion such as:

                converted = cast(o)

        encoding: (defaults to utf-8) is the encoding for unicode
            keys. This is not used for custom cast functions

    """

    if isinstance(o, (dict, Storage)):
        if isinstance(o, dict):
            newobj = dict()
        else:
            newobj = Storage()
        for k, v in o.items():
            if (cast == str) and isinstance(k, unicode):
                key = k.encode(encoding)
            else:
                key = cast(k)
            newobj[key] = cast_keys(v, cast=cast, encoding=encoding)
    elif isinstance(o, (tuple, set, list)):
        newobj = []
        for item in o:
            newobj.append(cast_keys(item, cast=cast, encoding=encoding))
        if isinstance(o, tuple):
            newobj = tuple(newobj)
        elif isinstance(o, set):
            newobj = set(newobj)
    else:
        # no string cast (unknown object)
        newobj = o
    return newobj


def loads_json(o, unicode_keys=True, **kwargs):
    # deserialize a json string
    result = json_parser.loads(o, **kwargs)
    if not unicode_keys:
        # filter non-str keys in dictionary objects
        result = cast_keys(result,
                           encoding=kwargs.get("encoding", "utf-8"))
    return result


def custom_json(o):
    if hasattr(o, 'custom_json') and callable(o.custom_json):
        return o.custom_json()
    if isinstance(o, (datetime.date,
                      datetime.datetime,
                      datetime.time)):
        return o.isoformat()[:19].replace('T', ' ')
    elif isinstance(o, (int, long)):
        return int(o)
    elif isinstance(o, decimal.Decimal):
        return str(o)
    elif isinstance(o, lazyT):
        return str(o)
    elif isinstance(o, XmlComponent):
        return str(o)
    elif isinstance(o, set):
        return list(o)
    elif hasattr(o, 'as_list') and callable(o.as_list):
        return o.as_list()
    elif hasattr(o, 'as_dict') and callable(o.as_dict):
        return o.as_dict()
    else:
        raise TypeError(repr(o) + " is not JSON serializable")


def xml_rec(value, key, quote=True):
    if hasattr(value, 'custom_xml') and callable(value.custom_xml):
        return value.custom_xml()
    elif isinstance(value, (dict, Storage)):
        return TAG[key](*[TAG[k](xml_rec(v, '', quote))
                          for k, v in value.items()])
    elif isinstance(value, list):
        return TAG[key](*[TAG.item(xml_rec(item, '', quote)) for item in value])
    elif hasattr(value, 'as_list') and callable(value.as_list):
        return str(xml_rec(value.as_list(), '', quote))
    elif hasattr(value, 'as_dict') and callable(value.as_dict):
        return str(xml_rec(value.as_dict(), '', quote))
    else:
        return xmlescape(value, quote)


def xml(value, encoding='UTF-8', key='document', quote=True):
    return ('<?xml version="1.0" encoding="%s"?>' % encoding) + str(xml_rec(value, key, quote))


def json(value, default=custom_json):
    if decimal_false_option:
        value = json_parser.dumps(value, default=default, use_decimal=False)
    else:
        value = json_parser.dumps(value, default=default)

    # replace JavaScript incompatible spacing
    # http://timelessrepo.com/json-isnt-a-javascript-subset
    return value.replace(ur'\u2028', '\\u2028').replace(ur'\2029', '\\u2029')


def csv(value):
    return ''


def ics(events, title=None, link=None, timeshift=0, calname=True,
        **ignored):
    title = title or '(unknown)'
    if link and not callable(link):
        link = lambda item, prefix=link: prefix.replace(
            '[id]', str(item['id']))
    s = 'BEGIN:VCALENDAR'
    s += '\nVERSION:2.0'
    if not calname is False:
        s += '\nX-WR-CALNAME:%s' % (calname or title)
    s += '\nSUMMARY:%s' % title
    s += '\nPRODID:Generated by web2py'
    s += '\nCALSCALE:GREGORIAN'
    s += '\nMETHOD:PUBLISH'
    for item in events:
        s += '\nBEGIN:VEVENT'
        s += '\nUID:%s' % item['id']
        if link:
            s += '\nURL:%s' % link(item)
        shift = datetime.timedelta(seconds=3600 * timeshift)
        start = item['start_datetime'] + shift
        stop = item['stop_datetime'] + shift
        s += '\nDTSTART:%s' % start.strftime('%Y%m%dT%H%M%S')
        s += '\nDTEND:%s' % stop.strftime('%Y%m%dT%H%M%S')
        s += '\nSUMMARY:%s' % item['title']
        s += '\nEND:VEVENT'
    s += '\nEND:VCALENDAR'
    return s

def safe_encode(text):
    if not isinstance(text, (str, unicode)):
        text = str(text)
    try:
        text = text.encode('utf8','replace')
    except ValueError:
        new_text = ''
        for c in text:
            try:
                new_text += c.encode('utf8')
            except:
                new_text += '?'
        text = new_text
    return text

def rss(feed):
    if not 'entries' in feed and 'items' in feed:
        feed['entries'] = feed['items']

    def safestr(obj, key, default=''):
        return safe_encode(obj.get(key,''))

    now = datetime.datetime.now()
    rss = rss2.RSS2(title=safestr(feed,'title'),
                    link=safestr(feed,'link'),
                    description=safestr(feed,'description'),
                    lastBuildDate=feed.get('created_on', now),
                    items=[rss2.RSSItem(
                           title=safestr(entry,'title','(notitle)'),
                           link=safestr(entry,'link'),
                           description=safestr(entry,'description'),
                           pubDate=entry.get('created_on', now)
                           ) for entry in feed.get('entries', [])])
    return rss.to_xml(encoding='utf8')


def yaml(data):
    if have_yaml:
        return yamlib.dump(data)
    else:
        raise ImportError("No YAML serializer available")


def loads_yaml(data):
    if have_yaml:
        return yamlib.load(data)
    else:
        raise ImportError("No YAML serializer available")
