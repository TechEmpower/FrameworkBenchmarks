import setup_util
import subprocess

def start(args, logfile, errfile):
  setup_util.replace_text("play-java-ebean/conf/application.conf", "jdbc:mysql:\/\/.*:3306", "jdbc:mysql://" + args.database_host + ":3306")
  subprocess.Popen(["play","start"], stdin=subprocess.PIPE, cwd="play-java-ebean", stderr=errfile, stdout=logfile)
  return 0

def stop(logfile, errfile):
  p = subprocess.Popen(["play","stop"], cwd="play-java-ebean", stderr=errfile, stdout=logfile)
  p.communicate()
  return 0
