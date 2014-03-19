import setup_util
import subprocess

def start(args, logfile, errfile):
  subprocess.Popen(["play","start"], stdin=subprocess.PIPE, cwd="play-java", stderr=errfile, stdout=logfile)
  return 0

def stop(logfile, errfile):
  p = subprocess.Popen(["play","stop"], cwd="play-java", stderr=errfile, stdout=logfile)
  p.communicate()
  return 0
