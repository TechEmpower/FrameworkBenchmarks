import subprocess
import os

def start(args, logfile, errfile):
  conn_string = ('dbname=hello_world '
                 'user=benchmarkdbuser '
                 'password=benchmarkdbpass '
                 'host=' + args.database_host)

  subprocess.check_call("urweb -dbms mysql -db \"" + conn_string + "\" bench", shell=True, cwd="urweb", stderr=errfile, stdout=logfile)

  threads = str(args.max_threads * 2)
  subprocess.Popen("./bench.exe -q -k -t " + threads,
                   shell=True, cwd="urweb", stderr=errfile, stdout=logfile)
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
