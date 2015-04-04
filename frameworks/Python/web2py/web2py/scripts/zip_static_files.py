#!/usr/bin/env python
# -*- coding: utf-8 -*-

## launch with python web2py.py -S myapp -R scripts/zip_static_files.py


import os
import gzip


def zip_static(filelist=[]):
    tsave = 0
    for fi in filelist:
        extension = os.path.splitext(fi)
        extension = len(extension) > 1 and extension[1] or None
        if not extension or extension not in ALLOWED_EXTS:
            print 'skipping %s' % os.path.basename(fi)
            continue
        fstats = os.stat(fi)
        atime, mtime = fstats.st_atime, fstats.st_mtime
        gfi = fi + '.gz'
        if os.path.isfile(gfi):
            zstats = os.stat(gfi)
            zatime, zmtime = zstats.st_atime, zstats.st_mtime
            if zatime == atime and zmtime == mtime:
                print 'skipping %s, already gzipped to the latest version' % os.path.basename(fi)
                continue
        print 'gzipping %s to %s' % (
            os.path.basename(fi), os.path.basename(gfi))
        f_in = open(fi, 'rb')
        f_out = gzip.open(gfi, 'wb')
        f_out.writelines(f_in)
        f_out.close()
        f_in.close()
        os.utime(gfi, (atime, mtime))
        saved = fstats.st_size - os.stat(gfi).st_size
        tsave += saved

    print 'saved %s KB' % (int(tsave) / 1000.0)

if __name__ == '__main__':
    ALLOWED_EXTS = ['.css', '.js']
    static_path = os.path.abspath(os.path.join(request.folder, 'static'))
    filelist = []
    for root, dir, files in os.walk(static_path):
        for file in files:
            filelist.append(os.path.join(root, file))

    zip_static(filelist)
