import subprocess
import sys
import os
import setup_util 

def start(args, logfile, errfile):
  setup_util.replace_text("cpoll_cppsp/www/connectioninfo.H", "\\#define BENCHMARK_DB_HOST \".*\"", "#define BENCHMARK_DB_HOST \"" + args.database_host + "\"")
  subprocess.check_call("make clean", shell=True, cwd="cpoll_cppsp", stderr=errfile, stdout=logfile)
  subprocess.check_call("make", shell=True, cwd="cpoll_cppsp", stderr=errfile, stdout=logfile)
  subprocess.Popen("./run_application \"$(pwd)\"/www -g g++-4.8 -m /forcedynamic.cppsm", shell=True, cwd="cpoll_cppsp", stderr=errfile, stdout=logfile);
  return 0

def stop(logfile, errfile):
  p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
  out, err = p.communicate()
  for line in out.splitlines():
    if 'cppsp_standalone' in line:
      pid = int(line.split(None, 2)[1])
      os.kill(pid, 9)
  return 0
