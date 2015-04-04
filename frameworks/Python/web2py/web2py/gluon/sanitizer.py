#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
| From http://aspn.activestate.com/ASPN/Cookbook/Python/Recipe/496942
| Submitter: Josh Goldfoot (other recipes)
| Last Updated: 2006/08/05
| Version: 1.0

Cross-site scripting (XSS) defense
-----------------------------------
"""

from HTMLParser import HTMLParser
from cgi import escape
from urlparse import urlparse
from formatter import AbstractFormatter
from htmlentitydefs import entitydefs
from xml.sax.saxutils import quoteattr

__all__ = ['sanitize']


def xssescape(text):
    """Gets rid of < and > and & and, for good measure, :"""

    return escape(text, quote=True).replace(':', '&#58;')


class XssCleaner(HTMLParser):

    def __init__(
        self,
        permitted_tags=[
            'a',
            'b',
            'blockquote',
            'br/',
            'i',
            'li',
            'ol',
            'ul',
            'p',
            'cite',
            'code',
            'pre',
            'img/',
        ],
        allowed_attributes={'a': ['href', 'title'], 'img': ['src', 'alt'
                                                            ], 'blockquote': ['type']},
        strip_disallowed=False
    ):

        HTMLParser.__init__(self)
        self.result = ''
        self.open_tags = []
        self.permitted_tags = [i for i in permitted_tags if i[-1] != '/']
        self.requires_no_close = [i[:-1] for i in permitted_tags
                                  if i[-1] == '/']
        self.permitted_tags += self.requires_no_close
        self.allowed_attributes = allowed_attributes

        # The only schemes allowed in URLs (for href and src attributes).
        # Adding "javascript" or "vbscript" to this list would not be smart.

        self.allowed_schemes = ['http', 'https', 'ftp', 'mailto']

        #to strip or escape disallowed tags?
        self.strip_disallowed = strip_disallowed
        self.in_disallowed = False

    def handle_data(self, data):
        if data and not self.in_disallowed:
            self.result += xssescape(data)

    def handle_charref(self, ref):
        if self.in_disallowed:
            return
        elif len(ref) < 7 and (ref.isdigit() or ref == 'x27'): # x27 is a special case for apostrophe
            self.result += '&#%s;' % ref
        else:
            self.result += xssescape('&#%s' % ref)

    def handle_entityref(self, ref):
        if self.in_disallowed:
            return
        elif ref in entitydefs:
            self.result += '&%s;' % ref
        else:
            self.result += xssescape('&%s' % ref)

    def handle_comment(self, comment):
        if self.in_disallowed:
            return
        elif comment:
            self.result += xssescape('<!--%s-->' % comment)

    def handle_starttag(
        self,
        tag,
        attrs
    ):
        if tag not in self.permitted_tags:
            if self.strip_disallowed:
                self.in_disallowed = True
            else:
                self.result += xssescape('<%s>' % tag)
        else:
            bt = '<' + tag
            if tag in self.allowed_attributes:
                attrs = dict(attrs)
                self.allowed_attributes_here = [x for x in
                                                self.allowed_attributes[tag] if x in attrs
                                                and len(attrs[x]) > 0]
                for attribute in self.allowed_attributes_here:
                    if attribute in ['href', 'src', 'background']:
                        if self.url_is_acceptable(attrs[attribute]):
                            bt += ' %s="%s"' % (attribute,
                                                attrs[attribute])
                    else:
                        bt += ' %s=%s' % (xssescape(attribute),
                                          quoteattr(attrs[attribute]))
            if bt == '<a' or bt == '<img':
                return
            if tag in self.requires_no_close:
                bt += ' /'
            bt += '>'
            self.result += bt
            if tag not in self.requires_no_close: self.open_tags.insert(0, tag)

    def handle_endtag(self, tag):
        bracketed = '</%s>' % tag
        if tag not in self.permitted_tags:
            if self.strip_disallowed:
                self.in_disallowed = False
            else:
                self.result += xssescape(bracketed)
        elif tag in self.open_tags:
            self.result += bracketed
            self.open_tags.remove(tag)

    def url_is_acceptable(self, url):
        """
        Accepts relative, absolute, and mailto urls
        """

        parsed = urlparse(url)
        return (parsed[0] in self.allowed_schemes and '.' in parsed[1]) \
            or (parsed[0] in self.allowed_schemes and '@' in parsed[2]) \
            or (parsed[0] == '' and parsed[2].startswith('/'))

    def strip(self, rawstring, escape=True):
        """
        Returns the argument stripped of potentially harmful
        HTML or Javascript code

        @type escape: boolean
        @param escape: If True (default) it escapes the potentially harmful
          content, otherwise remove it
        """

        if not isinstance(rawstring, str):
            return str(rawstring)
        for tag in self.requires_no_close:
            rawstring = rawstring.replace("<%s/>" % tag, "<%s />" % tag)
        if not escape:
            self.strip_disallowed = True
        self.result = ''
        self.feed(rawstring)
        for endtag in self.open_tags:
            if endtag not in self.requires_no_close:
                self.result += '</%s>' % endtag
        return self.result

    def xtags(self):
        """
        Returns a printable string informing the user which tags are allowed
        """

        tg = ''
        for x in sorted(self.permitted_tags):
            tg += '<' + x
            if x in self.allowed_attributes:
                for y in self.allowed_attributes[x]:
                    tg += ' %s=""' % y
            tg += '> '
        return xssescape(tg.strip())


def sanitize(text, permitted_tags=[
        'a',
        'b',
        'blockquote',
        'br/',
        'i',
        'li',
        'ol',
        'ul',
        'p',
        'cite',
        'code',
        'pre',
        'img/',
        'h1', 'h2', 'h3', 'h4', 'h5', 'h6',
        'table', 'tbody', 'thead', 'tfoot', 'tr', 'td', 'div',
        'strong', 'span',
],
    allowed_attributes={
        'a': ['href', 'title'],
        'img': ['src', 'alt'],
        'blockquote': ['type'],
        'td': ['colspan'],
    },
        escape=True):
    if not isinstance(text, basestring):
        return str(text)
    return XssCleaner(permitted_tags=permitted_tags,
                      allowed_attributes=allowed_attributes).strip(text, escape)
