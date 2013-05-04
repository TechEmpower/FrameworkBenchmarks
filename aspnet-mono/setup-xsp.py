import subprocess
import sys
import setup_util
import os

def start(args):
  setup_util.replace_text("aspnet-mono/project/Benchmarks.Mono.AspNet/Web.config", "localhost", args.database_host)

  try:
    subprocess.check_call("rm -rf bin obj", shell=True, cwd="aspnet-mono/project/Benchmarks.Mono.AspNet")
    subprocess.check_call("xbuild", shell=True, cwd="aspnet-mono/project")
    subprocess.Popen("xsp4 --nonstop", shell=True, cwd="aspnet-mono/project/Benchmarks.Mono.AspNet")
    return 0
  except subprocess.CalledProcessError:
    return 1

def stop():
  p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
  out, err = p.communicate()
  for line in out.splitlines():
    if 'xsp4' in line:
      pid = int(line.split(None, 2)[1])
      try:
        os.kill(pid, 9)
      except OSError:
        pass
  return 0