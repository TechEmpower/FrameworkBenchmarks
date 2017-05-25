
import subprocess
import sys
import setup_util
import os

def start(args):
  if os.name != 'nt':
    return 1
  
  try:
    setup_util.replace_text("aspnet-katana/src/Web.config", "localhost", args.database_host)
    subprocess.check_call("powershell -File setup_owinhost.ps1 start", cwd="aspnet-katana/src")
    return 0
  except subprocess.CalledProcessError:
    return 1

def stop():
  if os.name != 'nt':
    return 0
  if os.name == 'nt':
    subprocess.Popen("taskkill /f /im OwinHost.exe > NUL", shell=True)
    return 0