import setup_util
import subprocess
import os

def start(args, logfile, errfile):
  kill_running_process() # Kill the running process and delete the 
                         # RUNNING_PID file (if any). With any luck no 
                         # new process has picked up the same PID.

  play_cmd = "play"
  if args.os.lower() == "windows":
    play_cmd = "play.bat"
  
  setup_util.replace_text("play-scala/conf/application.conf", "jdbc:mysql:\/\/.*:3306", "jdbc:mysql://" + args.database_host + ":3306")
  subprocess.Popen([play_cmd,"start"], stdin=subprocess.PIPE, cwd="play-scala", stderr=errfile, stdout=logfile)
  return 0

def stop(logfile, errfile):
  kill_running_process()  
  return 0

def kill_running_process():
  try:
    with open("./play-scala/RUNNING_PID") as f:
      pid = int(f.read())
      os.kill(pid, 9)
  except:
  	pass

  try:
    os.remove("play-scala/RUNNING_PID")
  except OSError:
    pass  
