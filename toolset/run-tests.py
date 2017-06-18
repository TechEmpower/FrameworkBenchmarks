#!/usr/bin/env python
import argparse
import ConfigParser
import socket
import sys
import time
import os
import platform
import multiprocessing
import itertools
import copy
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
                print("  Invalid: {!s}".format(sequence))
                print("  Requires start:step:end, e.g. 1:2:10")
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
    os.environ['FWROOT'] = setup_util.get_fwroot()
    os.environ['IROOT'] = os.environ['FWROOT'] + '/installs'
    # 'Ubuntu', '14.04', 'trusty' respectively
    os.environ['TFB_DISTRIB_ID'], os.environ['TFB_DISTRIB_RELEASE'], os.environ['TFB_DISTRIB_CODENAME'] = platform.linux_distribution()
    # App server cpu count
    os.environ['CPU_COUNT'] = str(multiprocessing.cpu_count())

    print("FWROOT is {!s}.".format(os.environ['FWROOT']))

    conf_parser = argparse.ArgumentParser(
        description=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter,
        add_help=False)
    conf_parser.add_argument(
        '--conf_file', default='benchmark.cfg', metavar='FILE',
        help='Optional configuration file to provide argument defaults. All config options can be overridden using the command line.')
    args, remaining_argv = conf_parser.parse_known_args()

    defaults = {}
    try:
        if not os.path.exists(os.path.join(os.environ['FWROOT'], args.conf_file)) and not os.path.exists(os.path.join(os.environ['FWROOT'] + 'benchmark.cfg')):
            print("No config file found. Aborting!")
            exit(1)
        with open (os.path.join(os.environ['FWROOT'], args.conf_file)):
            config = ConfigParser.SafeConfigParser()
            config.read([os.path.join(os.environ['FWROOT'], args.conf_file)])
            defaults.update(dict(config.items("Defaults")))
            # Convert strings into proper python types
            for k, v in defaults.iteritems():
                try:
                    defaults[k] = literal_eval(v)
                except Exception:
                    pass
    except IOError:
        print("Configuration file not found!")
        exit(1)

    ##########################################################
    # Set up default values
    ##########################################################

    # Verify and massage options
    if defaults['client_user'] is None or defaults['client_host'] is None:
        print("client_user and client_host are required!")
        print("Please check your configuration file.")
        print("Aborting!")
        exit(1)

    if defaults['database_user'] is None:
        defaults['database_user'] = defaults['client_user']
    if defaults['database_host'] is None:
        defaults['database_host'] = defaults['client_host']
    if defaults['server_host'] is None:
        defaults['server_host'] = defaults['client_host']
    if defaults['ulimit'] is None:
        defaults['ulimit'] = 200000

    os.environ['ULIMIT'] = str(defaults['ulimit'])

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

    # Install options
    parser.add_argument('--clean', action='store_true', default=False, help='Removes the results directory')
    parser.add_argument('--clean-all', action='store_true', dest='clean_all', default=False, help='Removes the results and installs directories')

    # Test options
    parser.add_argument('--test', nargs='+', help='names of tests to run')
    parser.add_argument('--test-dir', nargs='+', dest='test_dir', help='name of framework directory containing all tests to run')
    parser.add_argument('--exclude', nargs='+', help='names of tests to exclude')
    parser.add_argument('--type', choices=['all', 'json', 'db', 'query', 'cached_query', 'fortune', 'update', 'plaintext'], default='all', help='which type of test to run')
    parser.add_argument('-m', '--mode', choices=['benchmark', 'verify', 'debug'], default='benchmark', help='verify mode will only start up the tests, curl the urls and shutdown. debug mode will skip verification and leave the server running.')
    parser.add_argument('--list-tests', action='store_true', default=False, help='lists all the known tests that can run')

    # Benchmark options
    parser.add_argument('--duration', default=15, help='Time in seconds that each test should run for.')
    parser.add_argument('--sleep', type=int, default=60, help='the amount of time to sleep after starting each test to allow the server to start up.')

    # Misc Options
    parser.add_argument('--results-name', help='Gives a name to this set of results, formatted as a date', default='(unspecified, datetime = %Y-%m-%d %H:%M:%S)')
    parser.add_argument('--results-environment', help='Describes the environment in which these results were gathered', default='(unspecified, hostname = %s)' % socket.gethostname())
    parser.add_argument('--results-upload-uri', default=None, help='A URI where the in-progress results.json file will be POSTed periodically')
    parser.add_argument('--parse', help='Parses the results of the given timestamp and merges that with the latest results')
    parser.add_argument('-v', '--verbose', action='store_true', default=False, help='Causes the configuration to print before any other commands are executed.')
    parser.add_argument('--quiet', action='store_true', default=False, help='Only print a limited set of messages to stdout, keep the bulk of messages in log files only')
    parser.add_argument('--clear-tmp', action='store_true', default=False, help='Clears files written to /tmp after each framework\'s tests complete.')
    parser.set_defaults(**defaults) # Must do this after add, or each option's default will override the configuration file default
    args = parser.parse_args(remaining_argv)

    benchmarker = Benchmarker(vars(args))

    # Run the benchmarker in the specified mode
    #   Do not use benchmarker variables for these checks,
    #   they are either str or bool based on the python version
    if args.list_tests:
        benchmarker.run_list_tests()
    elif args.parse != None:
        benchmarker.parse_timestamp()
    else:
        return benchmarker.run()

if __name__ == "__main__":
    sys.exit(main())
