import subprocess
import sys
import setup_util
from os.path import expanduser
import os
import getpass

def start(args, logfile, errfile):
  setup_util.replace_text("plack/app.psgi", "localhost", args.database_host)
  setup_util.replace_text("plack/nginx.conf", "USR", getpass.getuser())
  setup_util.replace_text("plack/nginx.conf", "server unix:.*\/FrameworkBenchmarks/plack", "server unix:" + args.troot)
  try:
    subprocess.Popen("start_server --backlog=16384 --pid-file=$TROOT/app.pid --path=$TROOT/app.sock -- plackup -E production -s Starlet --max-keepalive-reqs 1000 --max-reqs-per-child 50000 --min-reqs-per-child 40000 --max-workers=" + str(args.max_threads+4) + " -a $TROOT/app.psgi", shell=True, cwd="plack", stderr=errfile, stdout=logfile)
    subprocess.check_call("sudo /usr/local/nginx/sbin/nginx -c $TROOT/nginx.conf", shell=True,stderr=errfile, stdout=logfile)
    return 0
  except subprocess.CalledProcessError:
    return 1

def stop(logfile, errfile):
   try:
     subprocess.Popen("kill -TERM $(ps --ppid `cat app.pid` -o pid --no-header)", shell=True, cwd="plack", stderr=errfile, stdout=logfile)
     # TE - There was an issue on the EC2 machines where this, for reasons unknown,
     # was not sufficient in cleanly ending plack. In fact, the above would not 
     # successfully kill the starter process which would result in this 'stop' call
     # to report success but leave port 8080 bound to a plackup instance. We tried
     # adding a 'nuke' approach which detects the test's port still being bound
     # after calling stop and then directly kills those pids a few times to try and
     # cleanly release any/all ports (over 6000).
     # Why this only happens on EC2 is just a guess, but somehow the server seems
     # overwhelmed by the sheer volume of requests from the client and gets into
     # a deadlock state. Calling "kill -15 [pid]" against the server process does
     # nothing; so we needed a general way to kill all the processes that were 
     # spawned by the original process. For plack, this was as simple as the next
     # subprocess.Popen call (killall -s 9 plackup), but to do this generally is
     # a bit more difficult.
 
     # TE - In general, no test should ever be forced to use the KILL sigterm;
     # TERM should be sufficient. However, in this case it seems that the plack
     # server gets into a deadlock state and will not respond to a TERM sigterm.
     subprocess.Popen("killall -s 9 plackup")
     return 0
   except subprocess.CalledProcessError:
     return 1
