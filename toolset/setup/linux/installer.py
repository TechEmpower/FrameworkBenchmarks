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
    print("\nINSTALL: Installing server software (strategy=%s)\n"%self.strategy)
    # Install global prerequisites
    bash_functions_path='$FWROOT/toolset/setup/linux/bash_functions.sh'
    prereq_path='$FWROOT/toolset/setup/linux/prerequisites.sh'
    self.__run_command(". %s && . %s" % (bash_functions_path, prereq_path))

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

      if test_dir not in dirs:
        continue
              
      logging.info("Running installation for directory %s", test_dir)

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
        logging.warning("Directory %s does not have a bash_profile"%test_dir)
        profile="$FWROOT/config/benchmark_profile"
      else:
        logging.info("Loading environment from %s", profile)
      setup_util.replace_environ(config=profile, 
        command='export TROOT=%s && export IROOT=%s' %
        (test_dir, test_install_dir))

      # Run test installation script
      #   FWROOT - Path of the FwBm root
      #   IROOT  - Path of this test's install directory
      #   TROOT  - Path to this test's directory 
      self.__run_command('''
        export TROOT=%s && 
        export IROOT=%s && 
        source %s && 
        source %s''' % 
        (test_dir, test_install_dir, 
          bash_functions_path, test_install_file),
          cwd=test_install_dir)

      # Move back to previous directory
      os.chdir(previousDir)

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
    sudo apt-get -y install build-essential git libev-dev libpq-dev libreadline6-dev postgresql redis-server
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
    sudo apt-get -y install mongodb-org

    sudo stop mongodb
    sudo mv /etc/mongodb.conf /etc/mongodb.conf.orig
    sudo mv mongodb.conf /etc/mongodb.conf
    sudo cp -R -p /var/lib/mongodb /ssd/
    sudo cp -R -p /var/log/mongodb /ssd/log/
    sudo start mongodb

    ##############################
    # Apache Cassandra
    ##############################
    sudo apt-get install -qqy openjdk-7-jdk
    export CASS_V=2.0.7
    wget http://archive.apache.org/dist/cassandra/$CASS_V/apache-cassandra-$CASS_V-bin.tar.gz
    tar xzf apache-cassandra-$CASS_V-bin.tar.gz
    rm apache-cassandra-*-bin.tar.gz
    fuser -k -TERM /ssd/log/cassandra/system.log
    sleep 5
    sudo rm -rf /ssd/cassandra /ssd/log/cassandra
    sudo mkdir -p /ssd/cassandra /ssd/log/cassandra
    sudo chown tfb:tfb /ssd/cassandra /ssd/log/cassandra
    sed -i "s/^.*seeds:.*/          - seeds: \"%s\"/" cassandra/cassandra.yaml
    sed -i "s/^listen_address:.*/listen_address: %s/" cassandra/cassandra.yaml
    sed -i "s/^rpc_address:.*/rpc_address: %s/" cassandra/cassandra.yaml
    cp cassandra/cassandra.yaml apache-cassandra-$CASS_V/conf
    cp cassandra/log4j-server.properties apache-cassandra-$CASS_V/conf
    pushd apache-cassandra-$CASS_V
    nohup ./bin/cassandra
    sleep 10
    cat ../cassandra/create-keyspace.cql | ./bin/cqlsh $TFB_DATABASE_HOST
    python ../cassandra/db-data-gen.py | ./bin/cqlsh $TFB_DATABASE_HOST
    popd

    ##############################
    # Redis
    ##############################
    sudo service redis-server stop
    sudo cp redis.conf /etc/redis/redis.conf
    sudo service redis-server start
    bash create-redis.sh

    """ % (self.benchmarker.database_host, self.benchmarker.database_host, self.benchmarker.database_host)
    
    print("\nINSTALL: %s" % self.benchmarker.database_ssh_string)
    p = subprocess.Popen(self.benchmarker.database_ssh_string.split(" ") + ["bash"], stdin=subprocess.PIPE)
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
    git checkout 205a1960c8b8de5f500bb143863ae293456b7add
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
    print("INSTALL: %s (cwd=$FWROOT%s)" % (command, rel_cwd))

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
  def __init__(self, benchmarker, install_strategy):
    self.benchmarker = benchmarker
    self.install_dir = "installs"
    self.fwroot = benchmarker.fwroot
    self.strategy = install_strategy
    
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
