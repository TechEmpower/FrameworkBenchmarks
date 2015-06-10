import re
import os
import sys
import subprocess
import platform

from threading import Thread
from Queue import Queue, Empty

class NonBlockingStreamReader:
  '''
  Enables calling readline in a non-blocking manner with a blocking stream, 
  such as the ones returned from subprocess.Popen

  Originally written by Eyal Arubas, who granted permission to use this inside TFB
  See http://eyalarubas.com/python-subproc-nonblock.html
  '''
  def __init__(self, stream, eof_message = None):
    '''
    stream: the stream to read from.
            Usually a process' stdout or stderr.
    eof_message: A message to print to stdout as soon
      as the stream's end is reached. Useful if you
      want to track the exact moment a stream terminates
    '''

    self._s = stream
    self._q = Queue()
    self._eof_message = eof_message
    self._poisonpill = 'MAGIC_POISONPILL_STRING'

    def _populateQueue(stream, queue):
      while True:
        line = stream.readline()
        if line: # 'data\n' or '\n'
          queue.put(line)
        else:    # '' e.g. EOF
          if self._eof_message:
            sys.stdout.write(self._eof_message + '\n')
          queue.put(self._poisonpill)
          return

    self._t = Thread(target = _populateQueue,
            args = (self._s, self._q))
    self._t.daemon = True
    self._t.start()

  def readline(self, timeout = None):
    try:
      line = self._q.get(block = timeout is not None,
        timeout = timeout)
      if line == self._poisonpill: 
        raise EndOfStream
      return line
    except Empty:
      return None

class EndOfStream(Exception): pass

# Replaces all text found using the regular expression to_replace with the supplied replacement.
def replace_text(file, to_replace, replacement):
    with open(file, "r") as conf:
        contents = conf.read()
    replaced_text = re.sub(to_replace, replacement, contents)
    with open(file, "w") as f:
        f.write(replaced_text)

# Replaces the current process environment with the one found in 
# config file. Retains a few original vars (HOME,PATH, etc) by default. 
# Optionally allows specification of a command to be run before loading
# the environment, to allow the framework to set environment variables
# Note: This command *cannot* print to stdout!
#
# Note: This will not replace the sudo environment (e.g. subprocess.check_call("sudo <command>")). 
# If you must use sudo, consider sudo sh -c ". <config> && your_command"
def replace_environ(config=None, root=None, print_result=False, command='true'):

    if platform.system().lower() == 'windows':

        pass

    else:
    
        # Clean up our current environment, preserving some important items
        mini_environ = {}
        for envname in ['HOME', 'PATH', 'LANG', 'USER', 'LD_LIBRARY_PATH', 'PYTHONPATH', 'FWROOT', 'TRAVIS']:
          if envname in os.environ:
            mini_environ[envname] = os.environ[envname]
        for key in os.environ:
          if key.startswith(('TFB_', 'TRAVIS_')):    # Any TFB_* and TRAVIS_* variables are preserved
            mini_environ[key] = os.environ[key]
        os.environ.clear()

        # Use FWROOT if explicitely provided
        if root is not None: 
          mini_environ['FWROOT']=root
        

        # Run command, source config file, and store resulting environment
        setup_env = "%s && . %s && env" % (command, config)
        env = ""
        try:
            env = subprocess.check_output(setup_env, shell=True, env=mini_environ,
           executable='/bin/bash')
        except subprocess.CalledProcessError:
            # Ensure that an error here does not crash the toolset
            print "CRITICAL: Loading %s returned non-zero exit" % config
            for key,value in mini_environ.iteritems():
                os.environ[key]=value
            return
        for line in env.split('\n'):
            try:
                key, value = line.split('=', 1)
                # If we already have this TFB_ variable, do not overwrite
                if key.startswith('TFB_') and key in mini_environ:
                    os.environ[key]=mini_environ[key]
                else:
                    os.environ[key]=value    
            except Exception:
                if not line: # Don't warn for empty line
                    continue 
                print "WARN: Line '%s' from '%s' is not an environment variable" % (line, config)
                continue
        if print_result:
            out = subprocess.check_output('env', shell=True, executable='/bin/bash')
            print "Environment after loading %s" %config
            print out

# Queries the shell for the value of FWROOT
def get_fwroot():

    if platform.system().lower() == 'windows':

        fwroot = "C:\FrameworkBenchmarks"
        return fwroot

    else:
    
        try:
            # Use printf to avoid getting a newline
            # Redirect to avoid stderr printing
            fwroot = subprocess.check_output('printf $FWROOT 2> /dev/null', shell=True, executable='/bin/bash')
            return fwroot
        except subprocess.CalledProcessError:
            # Make a last-guess effort ;-)
            return os.getcwd();

# Turns absolute path into path relative to FWROOT
# Assumes path is underneath FWROOT, not above
# 
# Useful for clean presentation of paths 
# e.g. /foo/bar/benchmarks/go/install.sh
# v.s. FWROOT/go/install.sh 
def path_relative_to_root(path):
    # Requires bash shell parameter expansion
    return subprocess.check_output("D=%s && printf \"${D#%s}\""%(path, get_fwroot()), shell=True, executable='/bin/bash')
