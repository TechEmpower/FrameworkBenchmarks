import subprocess

def start(args, logfile, errfile):
  subprocess.Popen('sudo /usr/local/nginx_mruby/sbin/nginx -c $TROOT/nginx.conf -g "worker_processes ' + str((args.max_threads)) + ';"', shell=True, cwd="ngx_mruby", stderr=errfile, stdout=logfile)

  return 0

def stop(logfile, errfile):
  subprocess.Popen('sudo /usr/local/nginx_mruby/sbin/nginx -c $TROOT/nginx.conf -s stop', shell=True, cwd="ngx_mruby", stderr=errfile, stdout=logfile)

  return 0
