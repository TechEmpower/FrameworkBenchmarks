import subprocess
import sys
import os
import setup_util

def start(args):
  setup_util.replace_text("go/src/hello/hello.go", "tcp\(.*:3306\)", "tcp(" + args.database_host + ":3306)")
  if os.name == 'nt':
    #subprocess.call("rmdir /s /q pkg\\windows_amd64", shell=True, cwd="go")
    #subprocess.call("rmdir /s /q src\\github.com", shell=True, cwd="go")
    #subprocess.call("del /s /q /f bin\\hello.exe", shell=True, cwd="go")
    subprocess.call("set GOPATH=C:\\FrameworkBenchmarks\\go&& go get ./...", shell=True, cwd="go")
    subprocess.Popen("setup.bat", shell=True, cwd="go") 
    return 0
  subprocess.call("go get ./...", shell=True, cwd="go") 
  subprocess.Popen("go run src/hello/hello.go".rsplit(" "), cwd="go")
  return 0
def stop():
  if os.name == 'nt':
    subprocess.call("taskkill /f /im go.exe > NUL", shell=True)
    subprocess.call("taskkill /f /im hello.exe > NUL", shell=True)
    return 0
  p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
  out, err = p.communicate()
  for line in out.splitlines():
    if 'hello' in line:
      pid = int(line.split(None, 2)[1])
      os.kill(pid, 9)
  return 0
