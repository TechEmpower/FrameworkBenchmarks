
import subprocess
import sys
import setup_util
import os

def start(args, logfile):
  setup_util.replace_text("ringojs-convenient/app/models.js", "dbHost = '.*';", "dbHost = '" + args.database_host + "';")

  try:

    subprocess.check_call("sudo rm -rf /usr/share/ringojs/packages/*", shell=True)
    subprocess.check_call("sudo ringo-admin install oberhamsi/sql-ringojs-client", shell=True)
    subprocess.check_call("sudo ringo-admin install grob/ringo-sqlstore", shell=True)
    subprocess.check_call("sudo ringo-admin install ringo/stick", shell=True)
    subprocess.check_call("sudo ringo-admin install oberhamsi/reinhardt", shell=True)

    subprocess.check_call("sudo mkdir -p /usr/share/ringojs/packages/ringo-sqlstore/jars/", shell=True)
    subprocess.check_call("sudo cp /usr/share/ringojs//packages/sql-ringojs-client/jars/mysql.jar /usr/share/ringojs/packages/ringo-sqlstore/jars/", shell=True)
    subprocess.Popen("ringo --production -Dserver -DXmx=512m -DXms=512m ringo-main.js", shell=True, cwd="ringojs-convenient")
    return 0
  except subprocess.CalledProcessError:
    return 1
def stop(logfile):
  p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
  out, err = p.communicate()
  for line in out.splitlines():
    if 'ringo-main.js' in line:
      pid = int(line.split(None, 2)[1])
      try:
        os.kill(pid, 9)
      except OSError:
        pass
  return 0
