import subprocess
import sys
import setup_util
import os

root = os.getcwd() + "/servicestack"
app = root + "/src"

def start(args, logfile, errfile):
  if os.name == 'nt':
    return 1
  
  setup_util.replace_text(app + "/Web.config", "localhost", args.database_host)

  try:
    # build
    subprocess.check_call("rm -rf bin obj", shell=True, cwd=app, stderr=errfile, stdout=logfile)
    subprocess.check_call("xbuild /p:Configuration=Release", shell=True, cwd=app, stderr=errfile, stdout=logfile)
    subprocess.check_call("sudo chown -R $USER:$USER /usr/local/etc/mono", shell=True, stderr=errfile, stdout=logfile)
    
    # nginx
    workers = 'worker_processes ' + str(args.max_threads) + ';'
    subprocess.check_call('echo "upstream mono {\n' + ';\n'.join('\tserver 127.0.0.1:' + str(port) for port in range(9001, 9001 + args.max_threads)) + ';\n}" > ' + root + '/nginx.upstream.conf', shell=True, stderr=errfile, stdout=logfile);
    subprocess.check_call('/usr/local/nginx/sbin/nginx -c ' + root + '/nginx.conf -g "' + workers + '"', shell=True, stderr=errfile, stdout=logfile)
    
    # fastcgi
    for port in range(9001, 9001 + args.max_threads):
      subprocess.Popen("MONO_OPTIONS=--gc=sgen fastcgi-mono-server4 /applications=/:. /socket=tcp:127.0.0.1:" + str(port) + " &", shell=True, cwd=app, stderr=errfile, stdout=logfile)
    return 0
  except subprocess.CalledProcessError:
    return 1

def stop(logfile, errfile):
  if os.name == 'nt':
    return 0
  
  #
  # stop mono
  #
  p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
  out, err = p.communicate()
  for line in out.splitlines():
    if 'mono-server' in line:
      pid = int(line.split(None, 2)[1])
      os.kill(pid, 9)

  #
  # stop nginx
  #
  p2 = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
  out, err = p2.communicate()
  for line in out.splitlines():
    if 'sbin/nginx' in line:
      pid = int(line.split(None, 2)[1])
      os.kill(pid, 15)

  return 0

