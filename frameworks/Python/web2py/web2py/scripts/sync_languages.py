#!/usr/bin/env python
# -*- coding: utf-8 -*-

# TODO: Comment this code

import sys
import shutil
import os

from gluon.languages import findT

sys.path.insert(0, '.')

def sync_language(d, data):
    ''' this function makes sure a translated string will be prefered over an untranslated
    string when syncing languages between apps. when both are translated, it prefers the
    latter app, as did the original script
    '''

    for key in data:
        # if this string is not in the allready translated data, add it
        if key not in d:
            d[key] = data[key]
        # see if there is a translated string in the original list, but not in the new list
        elif (
            ((d[key] != '') or (d[key] != key)) and
            ((data[key] == '') or (data[key] == key))
        ):
            d[key] = d[key]
        # any other case (wether there is or there isn't a translated string)
        else:
            d[key] = data[key]

    return d

def sync_main(file, apps):
    d = {}
    for app in apps:
        path = 'applications/%s/' % app
        findT(path, file)
        langfile = open(os.path.join(path, 'languages', '%s.py' % file))
        try:
            data = eval(langfile.read())
        finally:
            langfile.close()

        d = sync_language(d, data)


    path = 'applications/%s/' % apps[-1]
    file1 = os.path.join(path, 'languages', '%s.py' % file)

    f = open(file1, 'w')
    try:
        f.write('# coding: utf8\n')
        f.write('{\n')
        keys = d.keys()
        keys.sort()
        for key in keys:
            f.write("'''%s''':'''%s''',\n" % (key.replace("'", "\\'"), str(d[key].replace("'", "\\'"))))
        f.write('}\n')
    finally:
        f.close()

    oapps = reversed(apps[:-1])
    for app in oapps:
        path2 = 'applications/%s/' % app
        file2 = os.path.join(path2, 'languages', '%s.py' % file)
        if file1 != file2:
            shutil.copyfile(file1, file2)

if __name__ == "__main__":

    file = sys.argv[1]
    apps = sys.argv[2:]

    sync_main(file, apps)
