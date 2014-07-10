import subprocess
import os
import os.path
import time
import traceback
import sys
import glob
import logging

class Installer:

  ############################################################
  # install_software
  ############################################################
  def install_software(self):
    if self.benchmarker.install == 'all' or self.benchmarker.install == 'server':
        self.__install_server_software()

    if self.benchmarker.install == 'all' or self.benchmarker.install == 'database':
        self.__install_database_software()

    if self.benchmarker.install == 'all' or self.benchmarker.install == 'client':
        self.__install_client_software()

  ############################################################
  # End install_software
  ############################################################

  ############################################################
  # __install_server_software
  ############################################################
  def __install_server_software(self):
    print("\nINSTALL: Installing server software\n")
    
    bash_functions_path='../toolset/setup/linux/bash_functions.sh'
    prereq_path='../toolset/setup/linux/prerequisites.sh'
    self.__run_command(". %s && . %s" % (bash_functions_path, prereq_path))

    # Pull in benchmarker include and exclude list
    exclude = self.benchmarker.exclude
    include = self.benchmarker.test
    if exclude == None:
        exclude = []

    # Assume we are running from FrameworkBenchmarks
    install_files = glob.glob('*/install.sh')

    for install_file in install_files:
        test = os.path.dirname(install_file)
        
        if test in exclude:
            logging.debug("%s has been excluded", test)
            continue
        elif include is not None and test not in include:
            logging.debug("%s not in include list", test)
            continue
        else:
            logging.debug("Running installer for %s", test)
            bash_functions_path="../toolset/setup/linux/bash_functions.sh"
            self.__run_command(". %s && . ../%s" % (bash_functions_path, install_file))

    self.__run_command("sudo apt-get -y autoremove");
    

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
  # __install_database_software
  ############################################################
  def __install_database_software(self):
    print("\nINSTALL: Installing database software\n")
 
    self.__run_command("cd .. && " + self.benchmarker.database_sftp_string(batch_file="../config/database_sftp_batch"), True)

    remote_script = """

    ##############################
    # Prerequisites
    ##############################
    sudo apt-get -y update
    sudo apt-get -y install build-essential git libev-dev libpq-dev libreadline6-dev postgresql
    sudo sh -c "echo '*               -    nofile          65535' >> /etc/security/limits.conf"

    sudo mkdir -p /ssd
    sudo mkdir -p /ssd/log

    ##############################
    # MySQL
    ##############################
    sudo sh -c "echo mysql-server mysql-server/root_password_again select secret | debconf-set-selections"
    sudo sh -c "echo mysql-server mysql-server/root_password select secret | debconf-set-selections"

    sudo apt-get -y install mysql-server

    sudo stop mysql
    # disable checking of disk size
    sudo cp mysql /etc/init.d/mysql
    sudo chmod +x /etc/init.d/mysql
    sudo cp mysql.conf /etc/init/mysql.conf
    # use the my.cnf file to overwrite /etc/mysql/my.cnf
    sudo mv /etc/mysql/my.cnf /etc/mysql/my.cnf.orig
    sudo mv my.cnf /etc/mysql/my.cnf

    sudo cp -R -p /var/lib/mysql /ssd/
    sudo cp -R -p /var/log/mysql /ssd/log
    sudo cp usr.sbin.mysqld /etc/apparmor.d/
    sudo /etc/init.d/apparmor reload
    sudo start mysql

    # Insert data
    mysql -uroot -psecret < create.sql

    ##############################
    # Postgres
    ##############################
    sudo useradd benchmarkdbuser -p benchmarkdbpass
    sudo -u postgres psql template1 < create-postgres-database.sql
    sudo -u benchmarkdbuser psql hello_world < create-postgres.sql

    sudo -u postgres -H /etc/init.d/postgresql stop
    sudo mv postgresql.conf /etc/postgresql/9.3/main/postgresql.conf
    sudo mv pg_hba.conf /etc/postgresql/9.3/main/pg_hba.conf

    sudo cp -R -p /var/lib/postgresql/9.3/main /ssd/postgresql
    sudo -u postgres -H /etc/init.d/postgresql start
    sudo mv 60-postgresql-shm.conf /etc/sysctl.d/60-postgresql-shm.conf

    ##############################
    # MongoDB
    ##############################
    sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 7F0CEB10
    sudo cp 10gen.list /etc/apt/sources.list.d/10gen.list
    sudo apt-get -y update
    sudo apt-get -y remove mongodb-clients
    sudo apt-get -y install mongodb-10gen

    sudo stop mongodb
    sudo mv /etc/mongodb.conf /etc/mongodb.conf.orig
    sudo mv mongodb.conf /etc/mongodb.conf
    sudo cp -R -p /var/lib/mongodb /ssd/
    sudo cp -R -p /var/log/mongodb /ssd/log/
    sudo start mongodb
    """
    
    print("\nINSTALL: %s" % self.benchmarker.database_ssh_string)
    p = subprocess.Popen(self.benchmarker.database_ssh_string.split(" "), stdin=subprocess.PIPE)
    p.communicate(remote_script)
    returncode = p.returncode
    if returncode != 0:
      self.__install_error("status code %s running subprocess '%s'." % (returncode, self.benchmarker.database_ssh_string))

    print("\nINSTALL: Finished installing database software\n")
  ############################################################
  # End __install_database_software
  ############################################################

  ############################################################
  # __install_client_software
  ############################################################
  def __install_client_software(self):
    print("\nINSTALL: Installing client software\n")

    remote_script = """

    ##############################
    # Prerequisites
    ##############################
    sudo apt-get -y update
    sudo apt-get -y install build-essential git libev-dev libpq-dev libreadline6-dev 
    sudo sh -c "echo '*               -    nofile          65535' >> /etc/security/limits.conf"


    ##############################
    # wrk
    ##############################

    git clone https://github.com/wg/wrk.git
    cd wrk
    make
    sudo cp wrk /usr/local/bin
    cd ~
    
    #############################
    # pipeline.lua
    #############################
cat << EOF | tee pipeline.lua
init = function(args)
  wrk.init(args)
  local r = {}
  local depth = tonumber(args[1]) or 1
  for i=1,depth do
    r[i] = wrk.format()
  end
  req = table.concat(r)
end

request = function()
  return req
end
EOF
    """
    
    print("\nINSTALL: %s" % self.benchmarker.client_ssh_string)
    p = subprocess.Popen(self.benchmarker.client_ssh_string.split(" "), stdin=subprocess.PIPE)
    p.communicate(remote_script)
    returncode = p.returncode
    if returncode != 0:
      self.__install_error("status code %s running subprocess '%s'." % (returncode, self.benchmarker.client_ssh_string))

    print("\nINSTALL: Finished installing client software\n")
  ############################################################
  # End __install_client_software
  ############################################################

  ############################################################
  # __path_exists
  ############################################################
  def __path_exists(self, path, cwd=None):
    full_path = os.path.join(cwd or self.install_dir, path)

    if os.path.exists(full_path):
        print("\nEXISTS: %s " % full_path)
        return True

    print("\nNOT_EXISTS: %s" % full_path)
    return False
  ############################################################
  # End __path_exists
  ############################################################

  ############################################################
  # __run_command
  ############################################################
  def __run_command(self, command, send_yes=False, cwd=None, retry=False):
    try:
      cwd = os.path.join(self.install_dir, cwd)
    except AttributeError:
      cwd = self.install_dir

    if retry:
      max_attempts = 5
    else:
      max_attempts = 1
    attempt = 1
    delay = 0
    if send_yes:
      command = "yes yes | " + command
        

    print("\nINSTALL: %s (cwd=%s)" % (command, cwd))

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
  # __bash_from_string
  # Runs bash -c "command" in install_dir.
  ############################################################
  def __bash_from_string(self, command):
    self.__run_command('bash -c "%s"' % command)
  ############################################################
  # End __bash_from_string
  ############################################################

  ############################################################
  # __download
  # Downloads a file from a URI.
  ############################################################
  def __download(self, uri, filename=""):
    if filename:
      if os.path.exists(filename):
        return
      filename_option = "-O %s " % filename
    else:
      filename_option = ""
    command = "wget -nv --no-check-certificate --trust-server-names %s%s" % (filename_option, uri)
    self.__run_command(command, retry=True)
  ############################################################
  # End __download
  ############################################################

  ############################################################
  # __init__(benchmarker)
  ############################################################
  def __init__(self, benchmarker):
    self.benchmarker = benchmarker
    self.install_dir = "installs"
    
    # setup logging
    logging.basicConfig(stream=sys.stderr, level=logging.DEBUG)

    try:
      os.mkdir(self.install_dir)
    except OSError:
      pass
  ############################################################
  # End __init__
  ############################################################

# vim: sw=2
