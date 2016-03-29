#!/usr/bin/env python
import argparse
import ConfigParser
import sys
import os
import multiprocessing
import itertools
import copy
import subprocess
from pprint import pprint 
from benchmark.benchmarker import Benchmarker
from setup.linux.unbuffered import Unbuffered
from setup.linux import setup_util
from ast import literal_eval

# Enable cross-platform colored output
from colorama import init
init()

class StoreSeqAction(argparse.Action):
  '''Helper class for parsing a sequence from the command line'''
  def __init__(self, option_strings, dest, nargs=None, **kwargs):
     super(StoreSeqAction, self).__init__(option_strings, dest, type=str, **kwargs)
  def __call__(self, parser, namespace, values, option_string=None):
    setattr(namespace, self.dest, self.parse_seq(values))
  def parse_seq(self, argument):
    result = argument.split(',')
    sequences = [x for x in result if ":" in x]
    for sequence in sequences:
      try:
        (start,step,end) = sequence.split(':')
      except ValueError: 
        print "  Invalid: %s" % sequence
        print "  Requires start:step:end, e.g. 1:2:10"
        raise
      result.remove(sequence)
      result = result + range(int(start), int(end), int(step))
    return [abs(int(item)) for item in result]


###################################################################################################
# Main
###################################################################################################
def main(argv=None):
    ''' Runs the program. There are three ways to pass arguments 
    1) environment variables TFB_*
    2) configuration file benchmark.cfg
    3) command line flags
    In terms of precedence, 3 > 2 > 1, so config file trumps environment variables
    but command line flags have the final say
    '''
    # Do argv default this way, as doing it in the functional declaration sets it at compile time
    if argv is None:
        argv = sys.argv

    # Enable unbuffered output so messages will appear in the proper order with subprocess output.
    sys.stdout=Unbuffered(sys.stdout)

    # Update python environment
    # 1) Ensure the current directory (which should be the benchmark home directory) is in the path so that the tests can be imported.
    sys.path.append('.')
    # 2) Ensure toolset/setup/linux is in the path so that the tests can "import setup_util".
    sys.path.append('toolset/setup/linux')

    # Update environment for shell scripts
    fwroot = setup_util.get_fwroot()
    if not fwroot: 
        fwroot = os.getcwd()
    setup_util.replace_environ(config='config/benchmark_profile', root=fwroot)
    print "FWROOT is %s"%setup_util.get_fwroot()

    conf_parser = argparse.ArgumentParser(
        description=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter,
        add_help=False)
    conf_parser.add_argument('--conf_file', default='benchmark.cfg', metavar='FILE', help='Optional configuration file to provide argument defaults. All config options can be overridden using the command line.')
    args, remaining_argv = conf_parser.parse_known_args()

    try:
        with open (args.conf_file):
            config = ConfigParser.SafeConfigParser()
            config.read([os.getcwd() + '/' + args.conf_file])
            defaults = dict(config.items("Defaults"))
            # Convert strings into proper python types
            for k,v in defaults.iteritems():
                try:
                    defaults[k] = literal_eval(v)
                except Exception:
                    pass
    except IOError:
        if args.conf_file != 'benchmark.cfg':
            print 'Configuration file not found!'
        defaults = { "client-host":"localhost"}

    ##########################################################
    # Set up default values
    ##########################################################        
    serverHost = os.environ.get('TFB_SERVER_HOST')
    clientHost = os.environ.get('TFB_CLIENT_HOST')
    clientUser = os.environ.get('TFB_CLIENT_USER')
    clientIden = os.environ.get('TFB_CLIENT_IDENTITY_FILE')
    runnerUser = os.environ.get('TFB_RUNNER_USER')
    databaHost = os.getenv('TFB_DATABASE_HOST', clientHost)
    databaUser = os.getenv('TFB_DATABASE_USER', clientUser)
    dbIdenFile = os.getenv('TFB_DATABASE_IDENTITY_FILE', clientIden)
    maxThreads = 8
    try:
        maxThreads = multiprocessing.cpu_count()
    except Exception:
        pass

    ##########################################################
    # Set up argument parser
    ##########################################################
    parser = argparse.ArgumentParser(description="Install or run the Framework Benchmarks test suite.",
        parents=[conf_parser],
        formatter_class=argparse.ArgumentDefaultsHelpFormatter,
        epilog='''If an argument includes (type int-sequence), then it accepts integer lists in multiple forms. 
        Using a single number e.g. 5 will create a list [5]. Using commas will create a list containing those 
        values e.g. 1,3,6 creates [1, 3, 6]. Using three colon-separated numbers of start:step:end will create a 
        list, using the semantics of python's range function, e.g. 1:3:15 creates [1, 4, 7, 10, 13] while 
        0:1:5 creates [0, 1, 2, 3, 4]
        ''')

    # SSH options
    parser.add_argument('-s', '--server-host', default=serverHost, help='The application server.')
    parser.add_argument('-c', '--client-host', default=clientHost, help='The client / load generation server.')
    parser.add_argument('-u', '--client-user', default=clientUser, help='The username to use for SSH to the client instance.')
    parser.add_argument('-r', '--runner-user', default=runnerUser, help='The user to run each test as.')
    parser.add_argument('-i', '--client-identity-file', dest='client_identity_file', default=clientIden,
                        help='The key to use for SSH to the client instance.')
    parser.add_argument('-d', '--database-host', default=databaHost,
                        help='The database server.  If not provided, defaults to the value of --client-host.')
    parser.add_argument('--database-user', default=databaUser,
                        help='The username to use for SSH to the database instance.  If not provided, defaults to the value of --client-user.')
    parser.add_argument('--database-identity-file', default=dbIdenFile, dest='database_identity_file',
                        help='The key to use for SSH to the database instance.  If not provided, defaults to the value of --client-identity-file.')
    parser.add_argument('-p', dest='password_prompt', action='store_true', help='Prompt for password')
    
    
    # Install options
    parser.add_argument('--install', choices=['client', 'database', 'server', 'all'], default=None,
                        help='Runs installation script(s) before continuing on to execute the tests.')
    parser.add_argument('--install-error-action', choices=['abort', 'continue'], default='continue', help='action to take in case of error during installation')
    parser.add_argument('--install-strategy', choices=['unified', 'pertest'], default='unified', 
        help='''Affects : With unified, all server software is installed into a single directory. 
        With pertest each test gets its own installs directory, but installation takes longer''')
    parser.add_argument('--install-only', action='store_true', default=False, help='Do not run benchmark or verification, just install and exit')
    parser.add_argument('--clean', action='store_true', default=False, help='Removes the results directory')
    parser.add_argument('--clean-all', action='store_true', dest='clean_all', default=False, help='Removes the results and installs directories')

    # Test options
    parser.add_argument('--test', nargs='+', help='names of tests to run')
    parser.add_argument('--exclude', nargs='+', help='names of tests to exclude')
    parser.add_argument('--type', choices=['all', 'json', 'db', 'query', 'fortune', 'update', 'plaintext'], default='all', help='which type of test to run')
    parser.add_argument('-m', '--mode', choices=['benchmark', 'verify'], default='benchmark', help='verify mode will only start up the tests, curl the urls and shutdown')
    parser.add_argument('--list-tests', action='store_true', default=False, help='lists all the known tests that can run')
    parser.add_argument('--list-test-metadata', action='store_true', default=False, help='writes all the test metadata as a JSON file in the results directory')
    parser.add_argument('--os', choices=['linux', 'windows'], default='linux', help='The operating system of the application/framework server (the one running' +
                        'this binary')
    parser.add_argument('--database-os', choices=['linux', 'windows'], default='linux', help='The operating system of the database server.')

    # Benchmark options
    parser.add_argument('--concurrency-levels', default=[8, 16, 32, 64, 128, 256], help='Runs wrk benchmarker with different concurrency value (type int-sequence)', action=StoreSeqAction)
    parser.add_argument('--query-levels', default=[1, 5,10,15,20], help='Database queries requested per HTTP connection, used during query test (type int-sequence)', action=StoreSeqAction) 
    parser.add_argument('--threads', default=maxThreads, help='Run wrk benchmarker with this many threads. This should probably be the number of cores for your client system', type=int)
    parser.add_argument('--duration', default=15, help='Time in seconds that each test should run for.')
    parser.add_argument('--sleep', type=int, default=60, help='the amount of time to sleep after starting each test to allow the server to start up.')

    # Misc Options
    parser.add_argument('--parse', help='Parses the results of the given timestamp and merges that with the latest results')
    parser.add_argument('-v', '--verbose', action='store_true', default=False, help='Causes the configuration to print before any other commands are executed.')
    parser.set_defaults(**defaults) # Must do this after add, or each option's default will override the configuration file default
    args = parser.parse_args(remaining_argv)

    # Verify and massage options
    if args.client_user is None:
      print 'Usernames (e.g. --client-user, --runner-user, and --database-user) are required!'
      print 'The system will SSH into the client and the database for the install stage'
      print 'Aborting'
      exit(1)

    if args.runner_user is None:
      print 'Usernames (e.g. --client-user, --runner-user, and --database-user) are required!'
      print 'The system will run each test as the runner-user'
      print 'Aborting'
      exit(1)        

    if args.database_user is None:
      args.database_user = args.client_user

    if args.database_host is None:
      args.database_host = args.client_host

    if args.verbose:
        print 'Configuration options: '
        pprint(vars(args))

    benchmarker = Benchmarker(vars(args))

    # Run the benchmarker in the specified mode
    #   Do not use benchmarker variables for these checks, 
    #   they are either str or bool based on the python version
    if args.list_tests:
      benchmarker.run_list_tests()
    elif args.list_test_metadata:
      benchmarker.run_list_test_metadata()
    elif args.parse != None:
      benchmarker.parse_timestamp()
    elif not args.install_only:
      return benchmarker.run()

if __name__ == "__main__":
    sys.exit(main())
