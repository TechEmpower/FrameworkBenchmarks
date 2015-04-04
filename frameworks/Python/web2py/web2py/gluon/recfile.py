#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
| This file is part of the web2py Web Framework
| Copyrighted by Massimo Di Pierro <mdipierro@cs.depaul.edu>
| License: LGPLv3 (http://www.gnu.org/licenses/lgpl.html)

Generates names for cache and session files
--------------------------------------------
"""
import os, uuid


def generate(filename, depth=2, base=512):
    if os.path.sep in filename:
        path, filename = os.path.split(filename)
    else:
        path = None
    dummyhash = sum(ord(c)*256**(i % 4) for i, c in enumerate(filename)) % base**depth
    folders = []
    for level in range(depth-1, -1, -1):
        code, dummyhash = divmod(dummyhash, base**level)
        folders.append("%03x" % code)
    folders.append(filename)
    if path:
        folders.insert(0, path)
    return os.path.join(*folders)


def exists(filename, path=None):
    if os.path.exists(filename):
        return True
    if path is None:
        path, filename = os.path.split(filename)
    fullfilename = os.path.join(path, generate(filename))
    if os.path.exists(fullfilename):
        return True
    return False


def remove(filename, path=None):
    if os.path.exists(filename):
        return os.unlink(filename)
    if path is None:
        path, filename = os.path.split(filename)
    fullfilename = os.path.join(path, generate(filename))
    if os.path.exists(fullfilename):
        return os.unlink(fullfilename)
    raise IOError


def open(filename, mode="r", path=None):
    if not path:
        path, filename = os.path.split(filename)
    fullfilename = None
    if not mode.startswith('w'):
        fullfilename = os.path.join(path, filename)
        if not os.path.exists(fullfilename):
            fullfilename = None
    if not fullfilename:
        fullfilename = os.path.join(path, generate(filename))
        if mode.startswith('w') and not os.path.exists(os.path.dirname(fullfilename)):
            os.makedirs(os.path.dirname(fullfilename))
    return file(fullfilename, mode)


def test():
    if not os.path.exists('tests'):
        os.mkdir('tests')
    for k in range(20):
        filename = os.path.join('tests', str(uuid.uuid4()) + '.test')
        open(filename, "w").write('test')
        assert open(filename, "r").read() == 'test'
        if exists(filename):
            remove(filename)

if __name__ == '__main__':
    test()
