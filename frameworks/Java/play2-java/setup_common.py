import os, setup_util, signal, subprocess

# Create start and stop functions for the Play project with the given dir
# and install them in the given module's globals.
def make_setup_for_dir(module_globals, subtest_name):

  def start(args, logfile, errfile):
    kill_running_process(logfile)

    subtest_dir = get_subtest_dir()
    install_dir = os.environ['IROOT']

    is_windows = os.name == "nt"
    cmd_suffix = '.bat' if is_windows else ''
    sbt_cmd = os.path.join(install_dir, 'sbt', 'bin', 'sbt'+cmd_suffix)
    app_cmd = os.path.join(subtest_dir, 'target','universal','stage','bin',subtest_name+cmd_suffix)

    setup_util.replace_text(
      os.path.join(subtest_dir,'conf','application.conf'),
      "jdbc:mysql:\/\/.*:3306", "jdbc:mysql://" + args.database_host + ":3306")
    logfile.write('Staging app: '+sbt_cmd+' stage\n')
    subprocess.call(
      [sbt_cmd, 'stage'],
      stdin=subprocess.PIPE, cwd=subtest_dir, stderr=errfile, stdout=logfile)
    logfile.write('Starting app: '+app_cmd+'\n')
    subprocess.Popen(
      [app_cmd],
      shell=True, stdin=subprocess.PIPE, cwd=subtest_dir, stderr=errfile, stdout=logfile)
    return 0

  def stop(logfile, errfile):
    kill_running_process(logfile)  
    return 0

  # Install the start and stop functions in the calling module
  module_globals['start'] = start
  module_globals['stop'] = stop

  def get_subtest_dir():
    test_dir = os.environ['TROOT']
    return os.path.join(test_dir, subtest_name)

  # Kill the running process and delete the RUNNING_PID file (if any).
  def kill_running_process(logfile):
    subtest_dir = get_subtest_dir()
    pidfile = os.path.join(subtest_dir,"target","universal","stage","RUNNING_PID")
    if not os.path.exists(pidfile):
      logfile.write('No PID file: {}\n'.format(pidfile))
      return
    logfile.write('Reading and deleting PID file: {}\n'.format(pidfile))
    with open(pidfile) as f:
      pid = int(f.read())
    os.remove(pidfile)
    logfile.write('Sending SIGTERM to process: {}\n'.format(pid))
    os.kill(pid, signal.SIGTERM)
