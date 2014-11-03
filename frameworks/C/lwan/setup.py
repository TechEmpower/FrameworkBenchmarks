import subprocess
import sys
import os

def get_env_for_database(args):
  if args.database == 'MySQL':
    return {
      'USE_MYSQL': '1',
      'MYSQL_USER': 'benchmarkdbuser',
      'MYSQL_PASS': 'benchmarkdbpass',
      'MYSQL_HOST': args.database_host,
      'MYSQL_DB': 'hello_world'
    }
  return None

def start(args, logfile, errfile):
  subprocess.call('rm -rf ${LWAN_BUILD}', shell=True, stderr=errfile, stdout=logfile)
  subprocess.call('mkdir -p ${LWAN_BUILD}', shell=True, stderr=errfile, stdout=logfile)
  subprocess.call('cmake ${LWAN_ROOT} -DCMAKE_BUILD_TYPE=Release && make techempower',
      cwd=os.environ['LWAN_BUILD'], shell=True, stderr=errfile, stdout=logfile)

  db_dir = os.path.join(os.environ['LWAN_ROOT'], 'techempower')
  exe_path = os.path.join(os.environ['LWAN_BUILD'], 'techempower', 'techempower')

  subprocess.Popen(exe_path, cwd=db_dir, stderr=errfile, stdout=logfile, shell=True,
      env = get_env_for_database(args))

  return 0

def stop(logfile, errfile):
  p = subprocess.Popen(['pgrep', 'techempower'], stdout=subprocess.PIPE)
  out, err = p.communicate()
  for line in out.splitlines():
      pid = int(line)
      os.kill(pid, 2)
  return 0
