#!/usr/bin/python

# You can pass several parameters on the command line
# (more info by running this with option --help)
# or you can modify the default values here
# (more info in WebKit.Launch):
import os

workDir = None
webwareDir = os.path.dirname(os.path.normpath(os.getcwd()))
libraryDirs = ['Lib']
runProfile = False
logFile = None
pidFile = None
user = None
group = None

import sys
sys.path.insert(0, webwareDir)

from WebKit import Launch

Launch.workDir = workDir
Launch.webwareDir = webwareDir
Launch.libraryDirs = libraryDirs
Launch.runProfile = runProfile
Launch.logFile = logFile
Launch.pidFile = pidFile
Launch.user = user
Launch.group = group

if __name__ == '__main__':
    Launch.main()
