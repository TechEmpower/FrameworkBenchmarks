# Copyright (C) 2012, Eduardo Silva <edsiper@gmail.com>
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

import os
import sys
import shutil
import commands

from utils import *

PROTOCOL_HTTPS = 0
PROTOCOL_GIT   = 1

class GitProject(object):
    # Project and repository details
    project_name = None
    version  = 'master'
    https_repo = None      # 'https://github.com/monkey/monkey.git'
    git_repo   = None      # 'git@github.com:monkey/monkey.git'

    # Internal usage variables
    recent_update   = False
    recent_master   = False
    recent_snapshot = False

    def __init__(self, project_name, https_repo, git_repo):
        self.project_name = project_name
        self.https_repo = https_repo
        self.git_repo   = git_repo

    def set_protocol(self, protocol):
        if protocol == PROTOCOL_HTTPS:
            self.protocol = self.https_repo
        elif protocol == PROTOCOL_GIT:
            self.protocol = self.git_repo

    def setup(self, version, https_repo, git_repo):
        if version is not None:
            self.version = version

        if https_repo is not None:
            self.https_repo = https_repo

        if git_repo is not None:
            self.git_repo = git_repo

    def clone(self, to):
        cmd = "git clone " + self.protocol + " " + to
        execute("%s: cloning source code" % (self.project_name), cmd)

    def update(self, to):
        if self.recent_update is True:
            return

        self.master()

        cpath = os.getcwd()
        os.chdir(to)
        cmd = "git pull"
        execute("GIT %s: updating" % (self.project_name), cmd)
        os.chdir(cpath)

        self.recent_update = True

    def remove(self, path):
        cmd = "rm -rf %s" % path
        print "[+] Deleting %s code..." % (self.project_name),
        try:
                shutil.rmtree(path)
                print "\t[OK]"
        except:
                print "\t[FAILED]"

    def run(self, cmd):
        return commands.getstatusoutput(cmd)

    def print_line(self, l):
        print l,
        sys.stdout.flush()

    # The home method must be implemented by the subclass
    def home(self):
        pass

    # Return the current head hash
    def master(self):
        home = self.home()
        if os.path.exists(home) is False:
            pass

        if self.recent_master is True:
            return

        cmd = 'git checkout master'
        cpath = os.getcwd()
        os.chdir(home)
        execute("GIT %s: checkout master" % (self.project_name), cmd)
        os.chdir(cpath)

        self.recent_master = True

    def archive_to(self, path):
        cmd =  'git checkout-index -a -f --prefix=%s/' % (path)

        if self.version:
            v = self.version
            if v.startswith('commit') or v.startswith('tag') or v.startswith('branch'):
                arr = v.split('@')
                cmd = 'git checkout %s' % (arr[1])

        cpath = os.getcwd()
        os.chdir(self.home())
        execute('%-12s: archive' % ('GIT %s' % self.project_name), cmd)

    def check_reference(self, ref):
        cmd = 'git show-ref %s' % ref
        ret = commands.getstatusoutput(cmd)
        if os.WEXITSTATUS(ret[0]) == 0:
            return True

        return False

    # A snapshot takes the value of the 'version' key and set the GIT repository
    # to the specified point, a few examples:
    #
    #    version = master
    #    version = commit@dc2d07d8ac991392aaf8de8ea82cc60257a15670
    #    version = branch@my_devel_branch
    #    version = tag@v1.5
    #
    def snapshot(self):
        if self.recent_snapshot is True:
            return

        cmd = 'git checkout %s' % (self.version)
        cpath = os.getcwd()
        os.chdir(self.home())

        # Validate GIT reference
        ref = self.check_reference(self.version)
        if ref is False:
            fail_msg("Error: invalid API level, aborting.")
            sys.exit(1)

        cmd = 'git stash && git checkout master && %s' % (cmd)
        ghead = 'GIT %s' % self.project_name
        execute("%-12s: switch HEAD to '%s'" % (ghead, self.version), cmd)
        os.chdir(cpath)

        self.recent_snapshot = True
