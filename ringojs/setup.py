
import subprocess
import sys
import setup_util
import os

def start(args, logfile, errfile):
  setup_util.replace_text("ringojs/ringo-main.js", "dbHost = '.*';", "dbHost = '" + args.database_host + "';")

  try:
    subprocess.check_call("sudo rm -rf /usr/share/ringojs/packages/*", shell=True, stderr=errfile, stdout=logfile)
    subprocess.check_call("sudo ringo-admin install oberhamsi/sql-ringojs-client", shell=True, stderr=errfile, stdout=logfile)
    subprocess.Popen("ringo --production -J-server -J-Xmx1g -J-Xms1g ringo-main.js", shell=True, cwd="ringojs", stderr=errfile, stdout=logfile)
    return 0
  except subprocess.CalledProcessError:
    return 1
def stop(logfile, errfile):
  p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
  out, err = p.communicate()
  for line in out.splitlines():
    if 'ringo-main.js' in line:
      pid = int(line.split(None, 2)[1])
      try:
        os.kill(pid, 15)
      except OSError:
        pass
  return 0
