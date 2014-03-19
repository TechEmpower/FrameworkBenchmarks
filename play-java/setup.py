import os
import signal
import subprocess

dirname = projname = 'play-java'
is_windows = args.os.lower() == "windows"
cmd_suffix = '.bat' if is_windows else ''

def start(args, logfile, errfile):
  kill_running_process() # Kill the running process and delete the 
                         # RUNNING_PID file (if any). With any luck no 
                         # new process has picked up the same PID.

  subprocess.call(['sbt'+cmd_suffix,"stage"], stdin=subprocess.PIPE, cwd=dirname, stderr=errfile, stdout=logfile)
  subprocess.Popen([os.path.join("target","universal","stage","bin",projname+cmd_suffix)], shell=True, stdin=subprocess.PIPE, cwd=dirname, stderr=errfile, stdout=logfile)
  return 0

def stop(logfile, errfile):
  kill_running_process()  
  return 0

def kill_running_process():
  pidfile = os.path.join(dirname,"target","universal","stage","RUNNING_PID")
  try:
    with open(pidfile) as f:
      pid = int(f.read())
      os.kill(pid, signal.SIGTERM)
  except:
    pass

  try:
    os.remove(pidfile)
  except OSError:
    pass