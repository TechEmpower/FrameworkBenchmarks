import os
import subprocess
from collections import namedtuple

import setup_util

Command = namedtuple('Command', ['command', 'wait_for_exit'])

def set_database_host(args):
  database_host = args.database_host or 'localhost'
  database_file = os.path.join(os.path.dirname(os.path.realpath(__file__)), 'config/database.yml')
  setup_util.replace_text(database_file, "  host:.*", "  host: " + database_host)

def run(commands, logfile, errfile):
  cwd = os.path.basename(os.path.normpath(os.path.dirname(os.path.realpath(__file__))))
  try:
    for command in commands:      
      if command.wait_for_exit:
        subprocess.check_call(command.command, shell=True, cwd=cwd, stderr=errfile, stdout=logfile)
      else:
        subprocess.Popen(command.command, shell=True, cwd=cwd, stderr=errfile, stdout=logfile)
  except subprocess.CalledProcessError:
    return 1
  return 0

def stop(partial_command, logfile, errfile):
  return run([Command('sudo pkill --signal 15 -f "' + partial_command + '"', True)], logfile, errfile)