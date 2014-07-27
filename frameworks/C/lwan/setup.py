import subprocess
import sys
import os

def start(args, logfile, errfile):
  subprocess.call('rm -rf ${LWAN_BUILD}', shell=True, stderr=errfile, stdout=logfile)
  subprocess.call('mkdir -p ${LWAN_BUILD}', shell=True, stderr=errfile, stdout=logfile)
  subprocess.call('cmake ${LWAN_ROOT} -DCMAKE_BUILD_TYPE=Release && make techempower',
      cwd=os.environ['LWAN_BUILD'], shell=True, stderr=errfile, stdout=logfile)

  db_dir = os.path.join(os.environ['LWAN_ROOT'], 'techempower')
  exe_path = os.path.join(os.environ['LWAN_BUILD'], 'techempower', 'techempower')
  subprocess.Popen(exe_path, cwd=db_dir, stderr=errfile, stdout=logfile, shell=True)

  return 0

def stop(logfile, errfile):
  p = subprocess.Popen(['pgrep', 'techempower'], stdout=subprocess.PIPE)
  out, err = p.communicate()
  for line in out.splitlines():
      pid = int(line)
      os.kill(pid, 2)
  return 0
