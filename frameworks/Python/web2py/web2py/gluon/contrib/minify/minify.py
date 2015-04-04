#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
High-level CSS and JS minification class for web2py.
Called by response.include_files()
Created by: Ross Peoples <ross.peoples@gmail.com>
Modified by: Massimo Di Pierro <massimo.dipierro@gmail.com>
"""

import cssmin
import jsmin
import os
import hashlib
import re


def read_binary_file(filename):
    f = open(filename, 'rb')
    data = f.read()
    f.close()
    return data


def write_binary_file(filename, data):
    f = open(filename, 'wb')
    f.write(data)
    f.close()


def fix_links(css, static_path):
    return re.sub(r'url\((["\'])\.\./', 'url(\\1' + static_path, css)


def minify(files, path_info, folder, optimize_css, optimize_js,
           ignore_concat=[],
           ignore_minify=['/jquery.js', '/anytime.js']):

    """
    Input:
    files: is a list of URLs to JS and CSS files (not repeated)
    path_info: is the URL of a temp static folder
    folder: is the application folder
    optimize_css: is a string of the form 'concat|minify|inline'
    optimize_js: is a string of the form 'concat|minify|inline'
    (minify requires concat, inline requires concat also)

    Returns a new list of:
    - filename (absolute or relative, css or js, actual or temporary) or
    - ('css:inline','...css..')
    - ('js:inline','...js..')
    """
    optimize_css = optimize_css or ''
    optimize_js = optimize_js or ''
    concat_css = 'concat' in optimize_css
    minify_css = 'minify' in optimize_css
    inline_css = 'inline' in optimize_css
    concat_js = 'concat' in optimize_js
    minify_js = 'minify' in optimize_js
    inline_js = 'inline' in optimize_js
    static_path, temp = path_info.rsplit('/', 1)
    new_files = []
    css = []
    js = []
    processed = []
    for k, filename in enumerate(files):
        if not filename.startswith('/') or \
                any(filename.endswith(x)
                    for x in ignore_concat):
            new_files.append(filename)
            continue

        abs_filename = os.path.join(
            folder, 'static', filename[len(static_path) + 1:])

        if filename.lower().endswith('.css'):
            processed.append(filename)
            spath_info, sfilename = \
                path_info.split('/'), filename.split('/')
            u = 0
            for i, a in enumerate(sfilename):
                try:
                    if a != spath_info[i]:
                        u = i
                        break
                except:
                    pass
            if concat_css:
                contents = read_binary_file(abs_filename)
                replacement = '/'.join(spath_info[:u]) + '/'
                contents = fix_links(contents, replacement)
                if minify_css:
                    css.append(cssmin.cssmin(contents))
                else:
                    css.append(contents)
            else:
                css.append(filename)
        elif filename.lower().endswith('.js'):
            processed.append(filename)
            if concat_js:
                contents = read_binary_file(abs_filename)

                if minify_js and \
                        not filename.endswith('.min.js') and \
                        not any(filename.endswith(x)
                                for x in ignore_minify):
                    js.append(jsmin.jsmin(contents))
                else:
                    js.append(contents)
            else:
                js.append(filename)
    dest_key = hashlib.md5(repr(processed)).hexdigest()
    if css and concat_css:
        css = '\n\n'.join(contents for contents in css)
        if not inline_css:
            temppath = os.path.join(folder, 'static', temp)
            if not os.path.exists(temppath):
                os.mkdir(temppath)
            dest = "compressed_%s.css" % dest_key
            tempfile = os.path.join(temppath, dest)
            write_binary_file(tempfile, css)
            css = path_info + '/%s' % dest
            new_files.append(css)
        else:
            new_files.append(('css:inline', css))
    else:
        new_files += css
    if js and concat_js:
        js = '\n'.join(contents for contents in js)
        if inline_js:
            js = ('js:inline', js)
        else:
            temppath = os.path.join(folder, 'static', temp)
            if not os.path.exists(temppath):
                os.mkdir(temppath)
            dest = "compressed_%s.js" % dest_key
            tempfile = os.path.join(folder, 'static', temp, dest)
            write_binary_file(tempfile, js)
            js = path_info + '/%s' % dest
        new_files.append(js)
    else:
        new_files += js
    return new_files
