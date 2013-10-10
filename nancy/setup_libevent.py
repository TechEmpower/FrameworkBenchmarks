import subprocess
import sys
import setup_util
import os

root = os.getcwd() + "/nancy"
app = root + "/src"

def start(args):
  if os.name == 'nt':
    return 1
  
  setup_util.replace_text(app + "/Web.config", "localhost", args.database_host)

  try:
    # build
    subprocess.check_call("rm -rf bin obj", shell=True, cwd=app)
    subprocess.check_call("xbuild /p:Configuration=Release", shell=True, cwd=app)
    
    # nginx
    workers = 'worker_processes ' + str(args.max_threads) + ';'
    subprocess.check_call('echo "upstream mono {\n' + ';\n'.join('\tserver 127.0.0.1:' + str(port) for port in range(9001, 9001 + args.max_threads)) + ';\n}" > ' + root + '/nginx.upstream.conf', shell=True);
    subprocess.check_call('sudo /usr/local/nginx/sbin/nginx -c ' + root + '/nginx.conf.libevent -g "' + workers + '"', shell=True)
    
    # fastcgi
    os.environ['MONO_GC_PARAMS']="nursery-size=16m"
    for port in range(9001, 9001 + args.max_threads):
      subprocess.Popen("mono-sgen -O=all LibeventHost/bin/Release/LibeventHost.exe 127.0.0.1 " + str(port) + " " + args.database_host + " &", shell=True, cwd=app)
    return 0
  except subprocess.CalledProcessError:
    return 1

def stop():
  if os.name == 'nt':
    return 0
  
  subprocess.check_call("sudo /usr/local/nginx/sbin/nginx -c " + root + "/nginx.conf -s stop", shell=True)
  subprocess.check_call("rm -f " + root + "/nginx.upstream.conf", shell=True)
  subprocess.check_call("pkill -9 mono-sgen", shell=True)
  return 0