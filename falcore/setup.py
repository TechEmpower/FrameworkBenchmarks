import subprocess
import sys
import os
import setup_util

def start(args, logfile, errfile):
  setup_util.replace_text("falcore/src/framework_benchmarks/falcore.go", "tcp\(.*:3306\)", "tcp(" + args.database_host + ":3306)")
  if os.name == 'nt':
    #subprocess.call("rmdir /s /q pkg\\windows_amd64", shell=True, cwd="go")
    #subprocess.call("rmdir /s /q src\\github.com", shell=True, cwd="go")
    #subprocess.call("del /s /q /f bin\\hello.exe", shell=True, cwd="go")
    subprocess.call("set GOPATH=C:\\FrameworkBenchmarks\\falcore&& go get ./...", shell=True, cwd="falcore", stderr=errfile, stdout=logfile)
    subprocess.Popen("setup.bat", shell=True, cwd="falcore", stderr=errfile, stdout=logfile) 
    return 0
  os.environ["GOPATH"] = os.path.expanduser('~/FrameworkBenchmarks/falcore')
  subprocess.call("go get ./...", shell=True, cwd="falcore", stderr=errfile, stdout=logfile) 
  subprocess.Popen("go run src/framework_benchmarks/falcore.go".rsplit(" "), cwd="falcore", stderr=errfile, stdout=logfile)
  return 0
def stop(logfile, errfile):
  if os.name == 'nt':
    subprocess.call("taskkill /f /im go.exe > NUL", shell=True, stderr=errfile, stdout=logfile)
    subprocess.call("taskkill /f /im falcore.exe > NUL", shell=True, stderr=errfile, stdout=logfile)
    return 0
  p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
  out, err = p.communicate()
  for line in out.splitlines():
    if 'falcore' in line and 'run-tests' not in line:
      pid = int(line.split(None, 2)[1])
      os.kill(pid, 9)
  return 0
