import subprocess
import sys
import setup_util
import os

DART_SDK = os.environ('DART_SDK')

def start(args):
  setup_util.replace_text('dart/postgresql.yaml', 'host: .*', 'host: ' + args.database_host)
  try:
    #
    # install dart dependencies
    #
    subprocess.Popen(DART_SDK + 'bin/pub install', shell=True, cwd='dart')
    #
    # start dart servers
    #
    for port in range(9001, 9001 + args.max_threads)
      subprocess.Popen(DART_SDK + 'bin/dart server.dart -a 127.0.0.1 -p ' + str(port) + ' -d ' + str(args.max_concurrency / args.max_threads), shell=True, cwd='dart')
    #
    # create nginx configuration
    #
    conf = []
    conf.append('worker_processes ' + str(args.max_threads) + ';')
    conf.append('error_log /dev/null crit;')
    conf.append('events {')
    conf.append('    worker_connections ' + str(4 * args.max_concurrency / args.max_threads) + ';')
    conf.append('}')
    conf.append('http {')
    conf.append('    access_log off;')
    conf.append('    include mime.types;')
    conf.append('    default_type application/octet-stream;')
    conf.append('    sendfile on;')
    conf.append('    upstream dart_cluster {')
    for port in range(9001, 9001 + args.max_threads)
      conf.append('        server 127.0.0.1:' + str(port) + ';')
    conf.append('    }')
    conf.append('    server {')
    conf.append('        listen 8080;')
    conf.append('        location / {')
    conf.append('            proxy_pass http://dart_cluster;')
    conf.append('            proxy_http_version 1.1;')
    conf.append('        }')
    conf.append('    }')
    conf.append('}')
    #
    # write nginx configuration to disk
    #
    os.remove('dart/nginx.conf')
    with open('dart/nginx.conf', 'w') as f:
      f.write('\n'.join(conf))
    #
    # start nginx
    #
    subprocess.Popen('nginx -c `pwd`/nginx.conf', shell=True, cwd='dart');
    return 0
  except subprocess.CalledProcessError:
    return 1

def stop():
  #
  # stop nginx
  #
  subprocess.Popen('nginx -c `pwd`/nginx.conf -s stop', shell=True, cwd='dart')
  os.remove('dart/nginx.conf')
  #
  # stop dart servers
  #
  p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
  out, err = p.communicate()
  for line in out.splitlines():
    if 'dart' in line:
      pid = int(line.split(None, 2)[1])
      os.kill(pid, 9)
  return 0
