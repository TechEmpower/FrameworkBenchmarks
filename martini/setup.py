import subprocess
import sys
import os
import setup_util

def start(args, logfile, errfile):
  setup_util.replace_text("martini/src/app/app.go", "tcp\(.*:3306\)", "tcp(" + args.database_host + ":3306)")
  if os.name == 'nt':
    subprocess.call("set GOPATH=C:\\FrameworkBenchmarks\\martini&& go get ./...", shell=True, cwd="martini", stderr=errfile, stdout=logfile)
    subprocess.Popen("setup.bat", shell=True, cwd="martini", stderr=errfile, stdout=logfile) 
    return 0
  os.environ["GOPATH"] = os.path.expanduser('~/FrameworkBenchmarks/martini')
  # get the martini dependencies
  subprocess.call("go get -u github.com/martini/martini", shell=True, cwd="martini", env=env, stderr=errfile, stdout=logfile)
  subprocess.call("go get -u github.com/martini-contrib/render", shell=True, cwd="martini", env=env, stderr=errfile, stdout=logfile)
  subprocess.call("go get ./...", shell=True, cwd="martini", stderr=errfile, stdout=logfile) 
  subprocess.call("go run src/app/app.go src/app/models.go".rsplit(" "), shell=True, cwd="martini", stderr=errfile, stdout=logfile)
  return 0
def stop(logfile, errfile):
  if os.name == 'nt':
    subprocess.call("taskkill /f /im go.exe > NUL", shell=True, stderr=errfile, stdout=logfile)
    subprocess.call("taskkill /f /im app.exe > NUL", shell=True, stderr=errfile, stdout=logfile)
    return 0
  p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
  out, err = p.communicate()
  for line in out.splitlines():
    if 'app' in line:
      pid = int(line.split(None, 2)[1])
      os.kill(pid, 15)
  return 0
