import subprocess
import os
import os.path
import time
import traceback
import sys
import glob
import logging
import setup_util

from benchmark.utils import gather_tests

class Installer:

  ############################################################
  # install_software
  ############################################################
  def install_software(self):
    linux_install_root = self.fwroot + "/toolset/setup/linux"
    imode = self.benchmarker.install

    script_vars = {
      'TFB_DBHOST': self.benchmarker.database_host
    }
    l=[]
    for k,v in script_vars.iteritems():
      l.append("export %s=%s" % (k,v))
    script_vars_str = "\n".join(l) + "\n\n"
    
    if imode == 'all' or imode == 'database':
      print("\nINSTALL: Installing database software\n")   
      self.__run_command("cd .. && " + self.benchmarker.database_sftp_string(batch_file="../config/database_sftp_batch"), True)
      with open (linux_install_root + "/database.sh", "r") as myfile:
        print("\nINSTALL: %s" % self.benchmarker.database_ssh_string)
        p = subprocess.Popen(self.benchmarker.database_ssh_string.split(" ") +
                             ["bash"], stdin=subprocess.PIPE)
        remote_script = myfile.read()
        p.communicate(script_vars_str + remote_script)
        returncode = p.returncode
        if returncode != 0:
          self.__install_error("status code %s running subprocess '%s'." % (returncode, self.benchmarker.database_ssh_string))
      print("\nINSTALL: Finished installing database software\n")

    if imode == 'all' or imode == 'client':
      print("\nINSTALL: Installing client software\n")    
      with open (linux_install_root + "/client.sh", "r") as myfile:
        remote_script=myfile.read()
        print("\nINSTALL: %s" % self.benchmarker.client_ssh_string)
        p = subprocess.Popen(self.benchmarker.client_ssh_string.split(" ") + ["bash"], stdin=subprocess.PIPE)
        p.communicate(remote_script)
        returncode = p.returncode
        if returncode != 0:
          self.__install_error("status code %s running subprocess '%s'." % (returncode, self.benchmarker.client_ssh_string))
      print("\nINSTALL: Finished installing client software\n")
  ############################################################
  # End install_software
  ############################################################

  ############################################################
  # __install_error
  ############################################################
  def __install_error(self, message):
    print("\nINSTALL ERROR: %s\n" % message)
    if self.benchmarker.install_error_action == 'abort':
      sys.exit("Installation aborted.")
  ############################################################
  # End __install_error
  ############################################################

  ############################################################
  # __run_command
  ############################################################
  def __run_command(self, command, send_yes=False, cwd=None):
    if cwd is None: 
        cwd = self.install_dir

    if send_yes:
      command = "yes yes | " + command
        
    rel_cwd = setup_util.path_relative_to_root(cwd)
    print("INSTALL: %s (cwd=$FWROOT/%s)" % (command, rel_cwd))

    try:
      subprocess.check_call(command, shell=True, cwd=cwd, executable='/bin/bash')
    except:
      exceptionType, exceptionValue, exceptionTraceBack = sys.exc_info()
      error_message = "".join(traceback.format_exception_only(exceptionType, exceptionValue))
      self.__install_error(error_message)
  ############################################################
  # End __run_command
  ############################################################

  ############################################################
  # __init__(benchmarker)
  ############################################################
  def __init__(self, benchmarker, install_strategy):
    self.benchmarker = benchmarker
    self.install_dir = "installs"
    self.fwroot = benchmarker.fwroot
    self.strategy = install_strategy
    
    # setup logging
    logging.basicConfig(stream=sys.stderr, level=logging.INFO)

    try:
      os.mkdir(self.install_dir)
    except OSError:
      pass
  ############################################################
  # End __init__
  ############################################################

# vim: sw=2
