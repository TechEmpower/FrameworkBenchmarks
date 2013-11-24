import subprocess
import os

def start(args, logfile, errfile):
  subprocess.check_call("urweb bench", shell=True, cwd="UrWeb", stderr=errfile, stdout=logfile)

  threads = str(args.max_threads)
  conn_string = ('dbname=hello_world '
                'user=benchmarkdbuser '
                'password=benchmarkdbpass '
                'host=' + args.database_host)
  env = {'URWEB_PQ_CON': conn_string}
  subprocess.Popen("./bench.exe -t " + threads,
                   env=env, shell=True, cwd="UrWeb", stderr=errfile, stdout=logfile)
  return 0

def stop(logfile, errfile):
  p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
  out, err = p.communicate()
  for line in out.splitlines():
    if 'bench.exe' in line:
      try:
        pid = int(line.split(None, 2)[1])
        os.kill(pid, 9)
      except OSError:
        pass
  return 0
