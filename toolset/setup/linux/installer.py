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

    if imode == 'all' or imode == 'server':
      self.__install_server_software()

    if imode == 'all' or imode == 'database':
      print("\nINSTALL: Installing database software\n")   
      self.__run_command("cd .. && " + self.benchmarker.database_sftp_string(batch_file="../config/database_sftp_batch"), True)
      with open (linux_install_root + "/database.sh", "r") as myfile:
        remote_script=myfile.read().format(database_host=self.benchmarker.database_host)
        print("\nINSTALL: %s" % self.benchmarker.database_ssh_string)
        p = subprocess.Popen(self.benchmarker.database_ssh_string.split(" ") + ["bash"], stdin=subprocess.PIPE)
        p.communicate(remote_script)
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
  # __install_server_software
  ############################################################
  def __install_server_software(self):
    print("\nINSTALL: Installing server software (strategy=%s)\n"%self.strategy)
    # Install global prerequisites (requires sudo)
    bash_functions_path='$FWROOT/toolset/setup/linux/bash_functions.sh'
    prereq_path='$FWROOT/toolset/setup/linux/prerequisites.sh'
    self.__run_command(". %s && . %s" % (bash_functions_path, prereq_path))
    self.__run_command("sudo chown -R %s:%s %s" % (self.benchmarker.runner_user,
      self.benchmarker.runner_user, os.path.join(self.fwroot, self.install_dir)))

    tests = gather_tests(include=self.benchmarker.test, 
      exclude=self.benchmarker.exclude,
      benchmarker=self.benchmarker)
    
    dirs = [t.directory for t in tests]

    # Locate all installation files
    install_files = glob.glob("%s/*/install.sh" % self.fwroot)
    install_files.extend(glob.glob("%s/frameworks/*/*/install.sh" % self.fwroot))

    # Run install for selected tests
    for test_install_file in install_files:
      test_dir = os.path.dirname(test_install_file)
      test_rel_dir = os.path.relpath(test_dir, self.fwroot)
      logging.debug("Considering install of %s (%s, %s)", test_install_file, test_rel_dir, test_dir)

      if test_dir not in dirs:
        continue

      logging.info("Running installation for directory %s (cwd=%s)", test_dir, test_dir)

      # Collect the tests in this directory
      # local_tests = [t for t in tests if t.directory == test_dir]

      # Find installation directory 
      #   e.g. FWROOT/installs or FWROOT/installs/pertest/<test-name>
      test_install_dir="%s/%s" % (self.fwroot, self.install_dir)
      if self.strategy is 'pertest':
        test_install_dir="%s/pertest/%s" % (test_install_dir, test_dir)
      if not os.path.exists(test_install_dir):
        os.makedirs(test_install_dir)
      
      # Move into the proper working directory
      previousDir = os.getcwd()
      os.chdir(test_dir)

      # Load profile for this installation
      profile="%s/bash_profile.sh" % test_dir
      if not os.path.exists(profile):
        profile="$FWROOT/config/benchmark_profile"
      else:
        logging.info("Loading environment from %s (cwd=%s)", profile, test_dir)
      setup_util.replace_environ(config=profile, 
        command='export TROOT=%s && export IROOT=%s' %
        (test_dir, test_install_dir))

      # Run test installation script
      #   FWROOT - Path of the FwBm root
      #   IROOT  - Path of this test's install directory
      #   TROOT  - Path to this test's directory 
      # Note: Cannot use ''' for newlines here or the script
      # passed to `bash -c` will fail.
      self.__run_command('sudo -u %s -E -H bash -c "export TROOT=%s && export IROOT=%s && source %s && source %s"' % 
        (self.benchmarker.runner_user, test_dir, test_install_dir, 
          bash_functions_path, test_install_file),
          cwd=test_install_dir)

      # Move back to previous directory
      os.chdir(previousDir)

    self.__run_command("sudo apt-get -yq autoremove");    

    print("\nINSTALL: Finished installing server software\n")
  ############################################################
  # End __install_server_software
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
  def __run_command(self, command, send_yes=False, cwd=None, retry=False):
    if cwd is None: 
        cwd = self.install_dir

    if retry:
      max_attempts = 5
    else:
      max_attempts = 1
    attempt = 1
    delay = 0
    if send_yes:
      command = "yes yes | " + command
        
    rel_cwd = setup_util.path_relative_to_root(cwd)
    print("INSTALL: %s (cwd=$FWROOT/%s)" % (command, rel_cwd))

    while attempt <= max_attempts:
      error_message = ""
      try:

        # Execute command.
        subprocess.check_call(command, shell=True, cwd=cwd, executable='/bin/bash')
        break  # Exit loop if successful.
      except:
        exceptionType, exceptionValue, exceptionTraceBack = sys.exc_info()
        error_message = "".join(traceback.format_exception_only(exceptionType, exceptionValue))

      # Exit if there are no more attempts left.
      attempt += 1
      if attempt > max_attempts:
        break

      # Delay before next attempt.
      if delay == 0:
        delay = 5
      else:
        delay = delay * 2
      print("Attempt %s/%s starting in %s seconds." % (attempt, max_attempts, delay))
      time.sleep(delay)

    if error_message:
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
