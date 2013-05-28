import subprocess
import sys
import setup_util
import os

root = os.getcwd() + "/aspnet"
app = root + "/src"

def start(args):
  if os.name == 'nt':
    return 1
  
  setup_util.replace_text(app + "/Web.config", "localhost", args.database_host)

  try:
    # build
    subprocess.check_call("rm -rf bin obj", shell=True, cwd=app)
    subprocess.check_call("xbuild /p:Configuration=Release", shell=True, cwd=app)
    subprocess.check_call("sudo chown -R ubuntu:ubuntu /usr/local/etc/mono", shell=True)
    
    # nginx
    workers = 'worker_processes ' + str(args.max_threads) + ';'
    subprocess.check_call('echo "upstream mono {\n' + ';\n'.join('\tserver 127.0.0.1:' + str(port) for port in range(9001, 9001 + args.max_threads)) + ';\n}" > ' + root + '/nginx.upstream.conf', shell=True);
    subprocess.check_call('sudo /usr/local/nginx/sbin/nginx -c ' + root + '/nginx.conf -g "' + workers + '"', shell=True)
    
    # fastcgi
    for port in range(9001, 9001 + args.max_threads):
      subprocess.Popen("fastcgi-mono-server4 /applications=/:. /socket=tcp:127.0.0.1:" + str(port) + " &", shell=True, cwd=app)
    return 0
  except subprocess.CalledProcessError:
    return 1

def stop():
  if os.name == 'nt':
    return 0
  
  subprocess.check_call("sudo /usr/local/nginx/sbin/nginx -c " + root + "/nginx.conf -s stop", shell=True)
  subprocess.check_call("rm -f " + root + "/nginx.upstream.conf", shell=True)
  subprocess.check_call("pkill -9 mono", shell=True)
  return 0
