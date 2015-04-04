#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
| This file is part of the web2py Web Framework
| Created by Attila Csipa <web2py@csipa.in.rs>
| Modified by Massimo Di Pierro <mdipierro@cs.depaul.edu>
| License: LGPLv3 (http://www.gnu.org/licenses/lgpl.html)

Cron-style interface
"""

import sys
import os
import threading
import logging
import time
import sched
import re
import datetime
import platform
import portalocker
import fileutils
try:
    import cPickle as pickle
except:
    import pickle
from gluon.settings import global_settings

logger = logging.getLogger("web2py.cron")
_cron_stopping = False
_cron_subprocs = []


def absolute_path_link(path):
    """
    Returns an absolute path for the destination of a symlink

    """
    if os.path.islink(path):
        link = os.readlink(path)
        if not os.path.isabs(link):
            link = os.path.join(os.path.dirname(path), link)
    else:
        link = os.path.abspath(path)
    return link


def stopcron():
    """Graceful shutdown of cron"""
    global _cron_stopping
    _cron_stopping = True
    while _cron_subprocs:
        proc = _cron_subprocs.pop()
        if proc.poll() is None:
            try:
                proc.terminate()
            except:
                import traceback
                traceback.print_exc()


class extcron(threading.Thread):

    def __init__(self, applications_parent, apps=None):
        threading.Thread.__init__(self)
        self.setDaemon(False)
        self.path = applications_parent
        self.apps = apps
        # crondance(self.path, 'external', startup=True, apps=self.apps)

    def run(self):
        if not _cron_stopping:
            logger.debug('external cron invocation')
            crondance(self.path, 'external', startup=False, apps=self.apps)


class hardcron(threading.Thread):

    def __init__(self, applications_parent):
        threading.Thread.__init__(self)
        self.setDaemon(True)
        self.path = applications_parent
        crondance(self.path, 'hard', startup=True)

    def launch(self):
        if not _cron_stopping:
            logger.debug('hard cron invocation')
            crondance(self.path, 'hard', startup=False)

    def run(self):
        s = sched.scheduler(time.time, time.sleep)
        logger.info('Hard cron daemon started')
        while not _cron_stopping:
            now = time.time()
            s.enter(60 - now % 60, 1, self.launch, ())
            s.run()


class softcron(threading.Thread):

    def __init__(self, applications_parent):
        threading.Thread.__init__(self)
        self.path = applications_parent
        # crondance(self.path, 'soft', startup=True)

    def run(self):
        if not _cron_stopping:
            logger.debug('soft cron invocation')
            crondance(self.path, 'soft', startup=False)


class Token(object):

    def __init__(self, path):
        self.path = os.path.join(path, 'cron.master')
        if not os.path.exists(self.path):
            fileutils.write_file(self.path, '', 'wb')
        self.master = None
        self.now = time.time()

    def acquire(self, startup=False):
        """
        Returns the time when the lock is acquired or
        None if cron already running

        lock is implemented by writing a pickle (start, stop) in cron.master
        start is time when cron job starts and stop is time when cron completed
        stop == 0 if job started but did not yet complete
        if a cron job started within less than 60 seconds, acquire returns None
        if a cron job started before 60 seconds and did not stop,
        a warning is issue "Stale cron.master detected"
        """
        if sys.platform == 'win32':
            locktime = 59.5
        else:
            locktime = 59.99
        if portalocker.LOCK_EX is None:
            logger.warning('WEB2PY CRON: Disabled because no file locking')
            return None
        self.master = open(self.path, 'rb+')
        try:
            ret = None
            portalocker.lock(self.master, portalocker.LOCK_EX)
            try:
                (start, stop) = pickle.load(self.master)
            except:
                (start, stop) = (0, 1)
            if startup or self.now - start > locktime:
                ret = self.now
                if not stop:
                    # this happens if previous cron job longer than 1 minute
                    logger.warning('WEB2PY CRON: Stale cron.master detected')
                logger.debug('WEB2PY CRON: Acquiring lock')
                self.master.seek(0)
                pickle.dump((self.now, 0), self.master)
                self.master.flush()
        finally:
            portalocker.unlock(self.master)
        if not ret:
            # do this so no need to release
            self.master.close()
        return ret

    def release(self):
        """
        Writes into cron.master the time when cron job was completed
        """
        if not self.master.closed:
            portalocker.lock(self.master, portalocker.LOCK_EX)
            logger.debug('WEB2PY CRON: Releasing cron lock')
            self.master.seek(0)
            (start, stop) = pickle.load(self.master)
            if start == self.now:  # if this is my lock
                self.master.seek(0)
                pickle.dump((self.now, time.time()), self.master)
            portalocker.unlock(self.master)
            self.master.close()


def rangetolist(s, period='min'):
    retval = []
    if s.startswith('*'):
        if period == 'min':
            s = s.replace('*', '0-59', 1)
        elif period == 'hr':
            s = s.replace('*', '0-23', 1)
        elif period == 'dom':
            s = s.replace('*', '1-31', 1)
        elif period == 'mon':
            s = s.replace('*', '1-12', 1)
        elif period == 'dow':
            s = s.replace('*', '0-6', 1)
    m = re.compile(r'(\d+)-(\d+)/(\d+)')
    match = m.match(s)
    if match:
        for i in range(int(match.group(1)), int(match.group(2)) + 1):
            if i % int(match.group(3)) == 0:
                retval.append(i)
    return retval


def parsecronline(line):
    task = {}
    if line.startswith('@reboot'):
        line = line.replace('@reboot', '-1 * * * *')
    elif line.startswith('@yearly'):
        line = line.replace('@yearly', '0 0 1 1 *')
    elif line.startswith('@annually'):
        line = line.replace('@annually', '0 0 1 1 *')
    elif line.startswith('@monthly'):
        line = line.replace('@monthly', '0 0 1 * *')
    elif line.startswith('@weekly'):
        line = line.replace('@weekly', '0 0 * * 0')
    elif line.startswith('@daily'):
        line = line.replace('@daily', '0 0 * * *')
    elif line.startswith('@midnight'):
        line = line.replace('@midnight', '0 0 * * *')
    elif line.startswith('@hourly'):
        line = line.replace('@hourly', '0 * * * *')
    params = line.strip().split(None, 6)
    if len(params) < 7:
        return None
    daysofweek = {'sun': 0, 'mon': 1, 'tue': 2, 'wed': 3, 'thu': 4,
                  'fri': 5, 'sat': 6}
    for (s, id) in zip(params[:5], ['min', 'hr', 'dom', 'mon', 'dow']):
        if not s in [None, '*']:
            task[id] = []
            vals = s.split(',')
            for val in vals:
                if val != '-1' and '-' in val and '/' not in val:
                    val = '%s/1' % val
                if '/' in val:
                    task[id] += rangetolist(val, id)
                elif val.isdigit() or val == '-1':
                    task[id].append(int(val))
                elif id == 'dow' and val[:3].lower() in daysofweek:
                    task[id].append(daysofweek(val[:3].lower()))
    task['user'] = params[5]
    task['cmd'] = params[6]
    return task


class cronlauncher(threading.Thread):

    def __init__(self, cmd, shell=True):
        threading.Thread.__init__(self)
        if platform.system() == 'Windows':
            shell = False
        self.cmd = cmd
        self.shell = shell

    def run(self):
        import subprocess
        global _cron_subprocs
        if isinstance(self.cmd, (list, tuple)):
            cmd = self.cmd
        else:
            cmd = self.cmd.split()
        proc = subprocess.Popen(cmd,
                                stdin=subprocess.PIPE,
                                stdout=subprocess.PIPE,
                                stderr=subprocess.PIPE,
                                shell=self.shell)
        _cron_subprocs.append(proc)
        (stdoutdata, stderrdata) = proc.communicate()
        try:
            _cron_subprocs.remove(proc)
        except ValueError:
            pass
        if proc.returncode != 0:
            logger.warning(
                'WEB2PY CRON Call returned code %s:\n%s' %
                (proc.returncode, stdoutdata + stderrdata))
        else:
            logger.debug('WEB2PY CRON Call returned success:\n%s'
                         % stdoutdata)


def crondance(applications_parent, ctype='soft', startup=False, apps=None):
    apppath = os.path.join(applications_parent, 'applications')
    cron_path = os.path.join(applications_parent)
    token = Token(cron_path)
    cronmaster = token.acquire(startup=startup)
    if not cronmaster:
        return
    now_s = time.localtime()
    checks = (('min', now_s.tm_min),
              ('hr', now_s.tm_hour),
              ('mon', now_s.tm_mon),
              ('dom', now_s.tm_mday),
              ('dow', (now_s.tm_wday + 1) % 7))

    if apps is None:
        apps = [x for x in os.listdir(apppath)
                if os.path.isdir(os.path.join(apppath, x))]

    full_apath_links = set()

    for app in apps:
        if _cron_stopping:
            break
        apath = os.path.join(apppath, app)

        # if app is a symbolic link to other app, skip it
        full_apath_link = absolute_path_link(apath)
        if full_apath_link in full_apath_links:
            continue
        else:
            full_apath_links.add(full_apath_link)

        cronpath = os.path.join(apath, 'cron')
        crontab = os.path.join(cronpath, 'crontab')
        if not os.path.exists(crontab):
            continue
        try:
            cronlines = fileutils.readlines_file(crontab, 'rt')
            lines = [x.strip() for x in cronlines if x.strip(
            ) and not x.strip().startswith('#')]
            tasks = [parsecronline(cline) for cline in lines]
        except Exception, e:
            logger.error('WEB2PY CRON: crontab read error %s' % e)
            continue

        for task in tasks:
            if _cron_stopping:
                break
            if sys.executable.lower().endswith('pythonservice.exe'):
                _python_exe = os.path.join(sys.exec_prefix, 'python.exe')
            else:
                _python_exe = sys.executable
            commands = [_python_exe]
            w2p_path = fileutils.abspath('web2py.py', gluon=True)
            if os.path.exists(w2p_path):
                commands.append(w2p_path)
            if applications_parent != global_settings.gluon_parent:
                commands.extend(('-f', applications_parent))
            citems = [(k in task and not v in task[k]) for k, v in checks]
            task_min = task.get('min', [])
            if not task:
                continue
            elif not startup and task_min == [-1]:
                continue
            elif task_min != [-1] and reduce(lambda a, b: a or b, citems):
                continue
            logger.info('WEB2PY CRON (%s): %s executing %s in %s at %s'
                        % (ctype, app, task.get('cmd'),
                           os.getcwd(), datetime.datetime.now()))
            action, command, models = False, task['cmd'], ''
            if command.startswith('**'):
                (action, models, command) = (True, '', command[2:])
            elif command.startswith('*'):
                (action, models, command) = (True, '-M', command[1:])
            else:
                action = False

            if action and command.endswith('.py'):
                commands.extend(('-J',                # cron job
                                 models,              # import models?
                                 '-S', app,           # app name
                                 '-a', '"<recycle>"',  # password
                                 '-R', command))      # command
            elif action:
                commands.extend(('-J',                  # cron job
                                 models,                # import models?
                                 '-S', app + '/' + command,  # app name
                                 '-a', '"<recycle>"'))  # password
            else:
                commands = command

            # from python docs:
            # You do not need shell=True to run a batch file or
            # console-based executable.
            shell = False

            try:
                cronlauncher(commands, shell=shell).start()
            except Exception, e:
                logger.warning(
                    'WEB2PY CRON: Execution error for %s: %s'
                    % (task.get('cmd'), e))
    token.release()
