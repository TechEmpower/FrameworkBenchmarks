#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
sessions2trash.py

Run this script in a web2py environment shell e.g. python web2py.py -S app
If models are loaded (-M option) auth.settings.expiration is assumed
for sessions without an expiration. If models are not loaded, sessions older
than 60 minutes are removed. Use the --expiration option to override these
values.

Typical usage:

    # Delete expired sessions every 5 minutes
    nohup python web2py.py -S app -M -R scripts/sessions2trash.py &

    # Delete sessions older than 60 minutes regardless of expiration,
    # with verbose output, then exit.
    python web2py.py -S app -M -R scripts/sessions2trash.py -A -o -x 3600 -f -v

    # Delete all sessions regardless of expiry and exit.
    python web2py.py -S app -M -R scripts/sessions2trash.py -A -o -x 0

    # Delete session in a module (move to the modules folder)
    from sessions2trash import single_loop
    def delete_sessions():
        single_loop(auth.settings.expiration)

"""

from __future__ import with_statement

import sys
import os
sys.path.append(os.path.join(*__file__.split(os.sep)[:-2] or ['.']))

from gluon import current
from gluon.storage import Storage
from optparse import OptionParser
import cPickle
import datetime
import os
import stat
import time

EXPIRATION_MINUTES = 60
SLEEP_MINUTES = 5
VERSION = 0.3


class SessionSet(object):
    """Class representing a set of sessions"""

    def __init__(self, expiration, force, verbose):
        self.expiration = expiration
        self.force = force
        self.verbose = verbose

    def get(self):
        """Get session files/records."""
        raise NotImplementedError

    def trash(self):
        """Trash expired sessions."""
        now = datetime.datetime.now()
        for item in self.get():
            status = 'OK'
            last_visit = item.last_visit_default()

            try:
                session = item.get()
                if session.auth:
                    if session.auth.expiration and not self.force:
                        self.expiration = session.auth.expiration
                    if session.auth.last_visit:
                        last_visit = session.auth.last_visit
            except:
                pass

            age = 0
            if last_visit:
                age = total_seconds(now - last_visit)

            if age > self.expiration or not self.expiration:
                item.delete()
                status = 'trashed'

            if self.verbose > 1:
                print 'key: %s' % str(item)
                print 'expiration: %s seconds' % self.expiration
                print 'last visit: %s' % str(last_visit)
                print 'age: %s seconds' % age
                print 'status: %s' % status
                print ''
            elif self.verbose > 0:
                print('%s %s' % (str(item), status))


class SessionSetDb(SessionSet):
    """Class representing a set of sessions stored in database"""

    def __init__(self, expiration, force, verbose):
        SessionSet.__init__(self, expiration, force, verbose)

    def get(self):
        """Return list of SessionDb instances for existing sessions."""
        sessions = []
        table = current.response.session_db_table
        if table:
            for row in table._db(table.id > 0).select():
                sessions.append(SessionDb(row))
        return sessions


class SessionSetFiles(SessionSet):
    """Class representing a set of sessions stored in flat files"""

    def __init__(self, expiration, force, verbose):
        SessionSet.__init__(self, expiration, force, verbose)

    def cleanup_empty_folders(self, root_path):
        for path, dirs, files in os.walk(root_path, topdown=False):
            for d in dirs:
                dd = os.path.join(path, d)
                if not os.listdir(dd):
                    os.rmdir(dd)

    def get(self):
        """Return list of SessionFile instances for existing sessions."""
        root_path = os.path.join(current.request.folder, 'sessions')
        for path, dirs, files in os.walk(root_path, topdown=False):
            for x in files:
                yield SessionFile(os.path.join(path, x))
        self.cleanup_empty_folders(root_path)


class SessionDb(object):
    """Class representing a single session stored in database"""

    def __init__(self, row):
        self.row = row

    def delete(self):
        table = current.response.session_db_table
        self.row.delete_record()
        table._db.commit()

    def get(self):
        session = Storage()
        session.update(cPickle.loads(self.row.session_data))
        return session

    def last_visit_default(self):
        if isinstance(self.row.modified_datetime, datetime.datetime):
            return self.row.modified_datetime
        else:
            try:
                return datetime.datetime.strptime(self.row.modified_datetime, '%Y-%m-%d %H:%M:%S.%f')
            except:
                print 'failed to retrieve last modified time (value: %s)' % self.row.modified_datetime

    def __str__(self):
        return self.row.unique_key


class SessionFile(object):
    """Class representing a single session stored as a flat file"""

    def __init__(self, filename):
        self.filename = filename

    def delete(self):
        try:
            os.unlink(self.filename)
        except:
            pass

    def get(self):
        session = Storage()
        with open(self.filename, 'rb+') as f:
            session.update(cPickle.load(f))
        return session

    def last_visit_default(self):
        return datetime.datetime.fromtimestamp(
            os.stat(self.filename)[stat.ST_MTIME])

    def __str__(self):
        return self.filename


def total_seconds(delta):
    """
    Adapted from Python 2.7's timedelta.total_seconds() method.

    Args:
        delta: datetime.timedelta instance.
    """
    return (delta.microseconds + (delta.seconds + (delta.days * 24 * 3600)) *
            10 ** 6) / 10 ** 6

def single_loop(expiration=None, force=False, verbose=False):
    if expiration is None:
        try:
            expiration = auth.settings.expiration
        except:
            expiration = EXPIRATION_MINUTES * 60

    set_files = SessionSetFiles(expiration, force, verbose)
    set_files.trash()
    set_db = SessionSetDb(expiration, force, verbose)
    set_db.trash()


def main():
    """Main processing."""

    usage = '%prog [options]' + '\nVersion: %s' % VERSION
    parser = OptionParser(usage=usage)

    parser.add_option('-f', '--force',
                      action='store_true', dest='force', default=False,
                      help=('Ignore session expiration. '
                            'Force expiry based on -x option or auth.settings.expiration.')
                      )
    parser.add_option('-o', '--once',
                      action='store_true', dest='once', default=False,
                      help='Delete sessions, then exit.',
                      )
    parser.add_option('-s', '--sleep',
                      dest='sleep', default=SLEEP_MINUTES * 60, type="int",
                      help='Number of seconds to sleep between executions. Default 300.',
                      )
    parser.add_option('-v', '--verbose',
                      default=0, action='count',
                      help="print verbose output, a second -v increases verbosity")
    parser.add_option('-x', '--expiration',
                      dest='expiration', default=None, type="int",
                      help='Expiration value for sessions without expiration (in seconds)',
                      )

    (options, unused_args) = parser.parse_args()

    expiration = options.expiration

    while True:
        single_loop(expiration, options.force, options.verbose)

        if options.once:
            break
        else:
            if options.verbose:
                print 'Sleeping %s seconds' % (options.sleep)
            time.sleep(options.sleep)

if __name__ == '__main__':
    main()
