# coding: utf-8

import re


def minify(response):
    def _replace(match):
        match = match.group()
        # save whole <pre>, <textarea> tags, and opening <!-- (so it doesn't break <script>)
        # otherwise, replace all whitespace with a single space character
        return match if match.startswith(('<pre', '<textarea', '<!--')) else ' '

    cpat = re.compile(
        r'\s+|<pre(.*?)</pre>|<textarea(.*?)</textarea>|<!--\s', re.DOTALL)
    return cpat.sub(_replace, response)
