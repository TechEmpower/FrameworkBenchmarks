import subprocess
import sys
import os
import setup_util

def start(args, logfile, errfile):
  setup_util.replace_text("go/src/hello/hello.go", "tcp\(.*:3306\)", "tcp(" + args.database_host + ":3306)")
  if os.name == 'nt':
    #subprocess.call("rmdir /s /q pkg\\windows_amd64", shell=True, cwd="go")
    #subprocess.call("rmdir /s /q src\\github.com", shell=True, cwd="go")
    #subprocess.call("del /s /q /f bin\\hello.exe", shell=True, cwd="go")
    subprocess.call("set GOPATH=C:\\FrameworkBenchmarks\\gorail&& go get ./...", shell=True, cwd="go", stderr=errfile, stdout=logfile)
    subprocess.Popen("setup.bat", shell=True, cwd="go", stderr=errfile, stdout=logfile) 
    return 0
  os.environ["GOPATH"] = os.path.expanduser('~/FrameworkBenchmarks/gorail')
  subprocess.call("go get -u github.com/gorail/core", shell=True, cwd="gorail", env=env, stderr=errfile, stdout=logfile)
  subprocess.call("go get ./...", shell=True, cwd="go", stderr=errfile, stdout=logfile) 
  subprocess.Popen("go run src/hello/hello.go".rsplit(" "), cwd="go", stderr=errfile, stdout=logfile)
  return 0
def stop(logfile, errfile):
  if os.name == 'nt':
    subprocess.call("taskkill /f /im go.exe > NUL", shell=True, stderr=errfile, stdout=logfile)
    subprocess.call("taskkill /f /im hello.exe > NUL", shell=True, stderr=errfile, stdout=logfile)
    return 0
  p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
  out, err = p.communicate()
  for line in out.splitlines():
    if 'hello' in line:
      pid = int(line.split(None, 2)[1])
      os.kill(pid, 15)
  return 0
