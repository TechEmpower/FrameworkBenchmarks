import subprocess
import sys
import os
import setup_util
from test_runner import TestRunner

class Go(TestRunner):

  def start(self):
    setup_util.replace_text("go/src/hello/hello.go", "tcp\(.*:3306\)", "tcp(" + self.database_host + ":3306)")
    if os.name == 'nt':
      #subprocess.call("rmdir /s /q pkg\\windows_amd64", shell=True, cwd="go")
      #subprocess.call("rmdir /s /q src\\github.com", shell=True, cwd="go")
      #subprocess.call("del /s /q /f bin\\hello.exe", shell=True, cwd="go")
      self.sh("go get ./...")
      self.sh("setup.bat")
      return 0
    
    self.sh("echo Debugging database connectivity")
    self.sh("mysql --host=%s --password=benchmarkdbpass --user=benchmarkdbuser --execute=\"show databases;\"" % self.database_host)
    self.sh("mysql --host=%s --password=benchmarkdbpass --user=benchmarkdbuser --execute=\"show create database hello_world;\"" % self.database_host)
    
    self.sh("go get ./...")
    self.pid = self.sh_async("go run -x -v src/hello/hello.go")
    return 0

  def stop(self):
    if os.name == 'nt':
      subprocess.call("taskkill /f /im go.exe > NUL", shell=True, stderr=errfile, stdout=logfile)
      subprocess.call("taskkill /f /im hello.exe > NUL", shell=True, stderr=errfile, stdout=logfile)
      return 0
    # Kill off the entire go process group
    self.sh_pkill(self.pid)
    return 0
