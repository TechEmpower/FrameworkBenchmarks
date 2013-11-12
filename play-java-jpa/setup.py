import setup_util
import subprocess

def start(args, logfile):
  setup_util.replace_text("play-java-jpa/conf/application.conf", "jdbc:mysql:\/\/.*:3306", "jdbc:mysql://" + args.database_host + ":3306")
  subprocess.Popen(["play","start"], stdin=subprocess.PIPE, cwd="play-java-jpa")
  return 0

def stop(logfile):
  p = subprocess.Popen(["play","stop"], cwd="play-java-jpa")
  p.communicate()
  return 0
