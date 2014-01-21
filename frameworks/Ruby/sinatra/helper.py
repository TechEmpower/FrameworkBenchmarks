import os
import subprocess
from collections import namedtuple

import setup_util

Command = namedtuple('Command', ['command', 'wait_for_exit'])

def set_database_host(args):
  database_host = args.database_host or 'localhost'
  database_file = os.path.join(args.troot, 'config', 'database.yml')
  setup_util.replace_text(database_file, "  host:.*", "  host: " + database_host)

def run(commands, logfile, errfile, cwd):
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
  p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
  out, err = p.communicate()
  for line in out.splitlines():
    if partial_command in line and 'run-tests' not in line:
      pid = int(line.split(None, 2)[1])
      os.kill(pid, 15)
  return 0