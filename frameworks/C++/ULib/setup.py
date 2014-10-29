import os
import sys
import time
import setup_util
import subprocess
import multiprocessing

def get_env_for_database(args):
  database = 'mysql'       if args.database == 'MySQL' else 'sqlite'
  dbname   = 'hello_world' if args.database == 'MySQL' else os.environ['ULIB_ROOT'] + '/db/%.*s'
  return {
      'ORM_DRIVER': database,
      'ORM_OPTION': "host=" + args.database_host + " user=benchmarkdbuser password=benchmarkdbpass dbname=" + dbname,
      'UMEMPOOL': '135,0,0,34,8465,129,-17,-22,41'
    }

def start(args, logfile, errfile):
  fwroot = args.fwroot
  try:
    # ulib_root = subprocess.check_output('printf $ULIB_ROOT', shell=True, stderr=errfile)
    ulib_root = os.environ['ULIB_ROOT']
    fcfg = ulib_root + "/benchmark.cfg"

    # 1. Change ULib Server configuration

    # I don't understand if the two approach are different...
    #threads = str(args.max_threads)
    PROCS = str(multiprocessing.cpu_count())
    setup_util.replace_text(fcfg, "PREFORK_CHILD .*", "PREFORK_CHILD " + PROCS)

    fprg = ulib_root + "/bin/userver_tcp"

    # 2. Start ULib Server (userver_tcp)
    logfile.write("ULib: trying to start server %s -c %s\n" % (fprg, fcfg))

    # Run in the background, but keep stdout/stderr for easy debugging
    subprocess.Popen( "%s -c %s" % (fprg, fcfg), shell=True, stdout=logfile, stderr=errfile, env = get_env_for_database(args))
    # subprocess.Popen("UTRACE=\"0 50M\" " + fprg + " -c " + fcfg, shell=True, stdout=logfile, stderr=errfile, env = get_env_for_database(args))

    logfile.write("ULib: server STARTED\n")
    return 0
  except subprocess.CalledProcessError:
    return 1

def stop(logfile, errfile):
  try:
    logfile.write( "ULib: setup.py STOP\n")

    # Stop ULib Server (userver_tcp)
    subprocess.check_call("kill -TERM $( cat $ULIB_ROOT/userver_tcp.pid )", shell=True, stderr=errfile, stdout=logfile)
    time.sleep(3);
    p = subprocess.Popen(['pgrep', 'userver_tcp'], stdout=subprocess.PIPE)
    out, err = p.communicate()
    for line in out.splitlines():
       if 'userver_tcp' in line:
         pid = int(line.split(None, 2)[1])
         os.kill(pid, 9)
    subprocess.call("rm -f $ULIB_ROOT/userver_tcp.pid", shell=True, stderr=errfile, stdout=logfile)
    return 0
  except subprocess.CalledProcessError:
    return 1
