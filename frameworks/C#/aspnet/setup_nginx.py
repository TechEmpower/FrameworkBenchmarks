import subprocess
import sys
import setup_util
import os

def start(args, logfile, errfile):
  if os.name == 'nt':
    return 1
  
  setup_util.replace_text("aspnet/src/Web.config", "localhost", args.database_host)

  try:
    # build
    subprocess.check_call("rm -rf bin obj", shell=True, cwd="aspnet", stderr=errfile, stdout=logfile)
    subprocess.check_call("xbuild /p:Configuration=Release", shell=True, cwd="aspnet/src", stderr=errfile, stdout=logfile)
    subprocess.check_call("sudo chown -R $USER:$USER $MONO_ROOT", shell=True, stderr=errfile, stdout=logfile)
    
    # nginx
    workers = 'worker_processes ' + str(args.max_threads) + ';'
    subprocess.check_call('echo "upstream mono {\n' + ';\n'.join('\tserver 127.0.0.1:' + str(port) for port in range(9001, 9001 + args.max_threads)) + ';\n}" > $TROOT/nginx.upstream.conf', shell=True, stderr=errfile, stdout=logfile);
    subprocess.check_call('sudo /usr/local/nginx/sbin/nginx -c $TROOT/nginx.conf -g "' + workers + '"', shell=True, stderr=errfile, stdout=logfile)
    
    # fastcgi
    for port in range(9001, 9001 + args.max_threads):
      # /usr/local/bin/fastcgi-mono-server4
      subprocess.Popen("MONO_OPTIONS=--gc=sgen fastcgi-mono-server4 /applications=/:. /socket=tcp:127.0.0.1:" + str(port) + " &", shell=True, cwd="aspnet", stderr=errfile, stdout=logfile)
    return 0
  except subprocess.CalledProcessError:
    return 1

def stop(logfile, errfile):
  if os.name == 'nt':
    return 0
  
  subprocess.check_call("sudo /usr/local/nginx/sbin/nginx -c $TROOT/nginx.conf -s stop", shell=True, stderr=errfile, stdout=logfile)
  subprocess.check_call("rm -f $TROOT/nginx.upstream.conf", shell=True, stderr=errfile, stdout=logfile)
  #
  # stop mono
  #
  p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
  out, err = p.communicate()
  for line in out.splitlines():
    if 'mono-server' in line and not 'run-ci' in line and not 'run-tests' in line:
      pid = int(line.split(None, 2)[1])
      os.kill(pid, 15)
  return 0
