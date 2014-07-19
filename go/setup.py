import subprocess
import sys
import os
import setup_util
from test_runner import TestRunner

class Bar(TestRunner):

  def start(self):
    setup_util.replace_text("go/src/hello/hello.go", "tcp\(.*:3306\)", "tcp(" + self.database_host + ":3306)")
    if os.name == 'nt':
      #subprocess.call("rmdir /s /q pkg\\windows_amd64", shell=True, cwd="go")
      #subprocess.call("rmdir /s /q src\\github.com", shell=True, cwd="go")
      #subprocess.call("del /s /q /f bin\\hello.exe", shell=True, cwd="go")
      subprocess.call("set GOPATH=C:\\FrameworkBenchmarks\\go&& go get ./...", shell=True, cwd="go", stderr=errfile, stdout=logfile)
      subprocess.Popen("setup.bat", shell=True, cwd="go", stderr=errfile, stdout=logfile)
      return 0
    
    self.sh("which go")
    self.sh("rm -rf src/github.com")
    self.sh("ls src/github.com/go-sql-driver/mysql")
    self.sh("go get ./...")
    self.sh("ls src/github.com/go-sql-driver/mysql")
    self.sh_async("go run -x -v src/hello/hello.go")
    return 0

  def stop(self):
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
