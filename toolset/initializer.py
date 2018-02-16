import subprocess, os
from setup.linux import setup_util

DEVNULL = open(os.devnull, 'w')

def initialize(args):
  fwroot = setup_util.get_fwroot()
  dbuser = args.database_user
  dbhost = args.database_host
  dbiden = args.database_identity_file
  cluser = args.client_user
  clhost = args.client_host
  cliden = args.client_identity_file
  aphost = args.server_host

  # test ssh connections to all the machines
  client_conn = __check_connection(cluser, clhost, cliden, aphost)
  database_conn = __check_connection(dbuser, dbhost, dbiden, aphost)

  conn_success = client_conn and database_conn
  if not conn_success and not args.quiet:
    return __print_failure()
  
  # set up client machine
  if not __init_client(fwroot, cluser, clhost, cliden, args.quiet) and not args.quiet:
    return __print_failure()


  # set up database software
  if not __init_database(fwroot, dbuser, dbhost, dbiden, args.quiet) and not args.quiet:
    return __print_failure()

def __print_failure():
  print("""
-------------------------------------------------------------------------------
  This wizard is intended to help configure the required software on all the
  machines in the ecosystem specified in benchmark.cfg.

  Note: It is expected that you have already set up passwordless-sudo on all
  of the machines (app, database, client) as well as identity file based 
  authentication and hostname setup in your hosts file. 
  More information on this required setup can be found at:

  frameworkbenchmarks.readthedocs.io/en/latest/Development/Installation-Guide/

  Please ensure that your benchmark.cfg is correctly configured as well as all
  of the machines (app, database, client).
-------------------------------------------------------------------------------""")

def __ssh_string(user, host, identity_file):
  return ["ssh", "-T", "-o", "StrictHostKeyChecking=no", "%s@%s" % (user, host), "-i", identity_file]
  
def __check_connection(user, host, identity_file, app_host):
  ''' 
  Checks that the given user and host are accessible via ssh with the given
  identity file and have the the following permissions:
    1. passwordless sudo
    2. ability to ssh back to app machine
  '''
  client_conn = True
  try:
    p = subprocess.Popen(__ssh_string(user, host, identity_file), 
      stdin=subprocess.PIPE, stdout=DEVNULL, stderr=DEVNULL)
    p.communicate("ssh -T -o StrictHostKeyChecking=no %s" % app_host)
    if p.returncode:
      client_conn = False
  except Exception as e:
    client_conn = False
  return client_conn

def __init_client(fwroot, user, host, identity_file, quiet):
  '''
  Initializes and configures the software required to run the suite on the 
  client machine.
  '''
  if not quiet:
    print("INSTALL: Installing client software")
  with open (os.path.join(fwroot, "toolset", "setup", "linux", "client.sh"), "r") as myfile:
    remote_script=myfile.read()
    if quiet:
      p = subprocess.Popen(__ssh_string(user, host, identity_file), 
        stdin=subprocess.PIPE, stdout=DEVNULL, stderr=DEVNULL)
    else:
      p = subprocess.Popen(__ssh_string(user, host, identity_file), 
        stdin=subprocess.PIPE)
    p.communicate(remote_script)
    return p.returncode == 0

def __init_database(fwroot, user, host, identity_file, quiet):
  '''
  Initializes and configures the software required to run the suite on the
  database machine.
  '''
  if not quiet:
    print("INSTALL: Installing database software")
  with open(os.path.join(fwroot, "toolset", "setup", "linux", "database.sh"), "r") as myfile:
    remote_script=myfile.read()
    if quiet:
      p = subprocess.Popen(__ssh_string(user, host, identity_file), 
        stdin=subprocess.PIPE, stdout=DEVNULL, stderr=DEVNULL)
    else:
      p = subprocess.Popen(__ssh_string(user, host, identity_file), 
        stdin=subprocess.PIPE)
    p.communicate(remote_script)
    return p.returncode == 0