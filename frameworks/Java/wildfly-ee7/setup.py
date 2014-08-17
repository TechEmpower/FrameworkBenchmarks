import subprocess
import sys
import setup_util
import os

def start(args, logfile, errfile):
  wildfly_env = os.environ.copy()
  wildfly_env['JAVA_OPTS'] = "-Xms2g -Xmx2g -XX:MaxPermSize=256m -XX:+UseG1GC -XX:MaxGCPauseMillis=25 -verbosegc -Xloggc:/tmp/wildfly_gc.log"
  try:
    subprocess.check_call("mvn clean initialize package -Pbenchmark -Ddatabase.host=" + args.database_host, shell=True, cwd="wildfly-ee7", stderr=errfile, stdout=logfile)
    subprocess.Popen("target/wildfly-8.1.0.Final/bin/standalone.sh -b 0.0.0.0", shell=True, env=wildfly_env, cwd="wildfly-ee7", stderr=errfile, stdout=logfile)
    return 0
  except subprocess.CalledProcessError:
    return 1
def stop(logfile, errfile):
  try:
    subprocess.check_call("target/wildfly-8.1.0.Final/bin/jboss-cli.sh --connect --command=:shutdown", shell=True, cwd="wildfly-ee7", stderr=errfile, stdout=logfile)
    return 0
  except subprocess.CalledProcessError:
    return 1
