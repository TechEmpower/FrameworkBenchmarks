import argparse
import ConfigParser
import socket
import sys
import os
import platform
import multiprocessing
import signal
from toolset.benchmark.benchmarker import Benchmarker
from toolset.utils.scaffolding import Scaffolding
from toolset.utils.initializer import initialize
from toolset.utils import cleaner
from toolset.utils.results_helper import Results
from toolset.utils.benchmark_config import BenchmarkConfig
from toolset.utils import docker_helper
from toolset.utils.metadata_helper import gather_tests
from toolset.utils.output_helper import log
from ast import literal_eval

# Enable cross-platform colored output
from colorama import init, Fore
init()


class StoreSeqAction(argparse.Action):
    '''
    Helper class for parsing a sequence from the command line
    '''

    def __init__(self, option_strings, dest, nargs=None, **kwargs):
        super(StoreSeqAction, self).__init__(
            option_strings, dest, type=str, **kwargs)

    def __call__(self, parser, namespace, values, option_string=None):
        setattr(namespace, self.dest, self.parse_seq(values))

    def parse_seq(self, argument):
        result = argument.split(',')
        sequences = [x for x in result if ":" in x]
        for sequence in sequences:
            try:
                (start, step, end) = sequence.split(':')
            except ValueError:
                log("  Invalid: {!s}".format(sequence), color=Fore.RED)
                log("  Requires start:step:end, e.g. 1:2:10", color=Fore.RED)
                raise
            result.remove(sequence)
            result = result + range(int(start), int(end), int(step))
        return [abs(int(item)) for item in result]


def __stop(signal, frame):
    '''
    Method called on SIGTERM to stop all running containers 
    '''
    docker_helper.stop()
    sys.exit(0)


signal.signal(signal.SIGTERM, __stop)


###################################################################################################
# Main
###################################################################################################
def main(argv=None):
    '''
    Runs the program. There are three ways to pass arguments
    1) environment variables TFB_*
    2) configuration file benchmark.cfg
    3) command line flags
    In terms of precedence, 3 > 2 > 1, so config file trumps environment variables
    but command line flags have the final say
    '''
    # Do argv default this way, as doing it in the functional declaration sets it at compile time
    if argv is None:
        argv = sys.argv

    # 'Ubuntu', '14.04', 'trusty' respectively
    os.environ['TFB_DISTRIB_ID'], os.environ[
        'TFB_DISTRIB_RELEASE'], os.environ[
            'TFB_DISTRIB_CODENAME'] = platform.linux_distribution()
    # App server cpu count
    os.environ['CPU_COUNT'] = str(multiprocessing.cpu_count())

    conf_parser = argparse.ArgumentParser(
        description=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter,
        add_help=False)
    conf_parser.add_argument(
        '--conf_file',
        default='benchmark.cfg',
        metavar='FILE',
        help=
        'Optional configuration file to provide argument defaults. All config options can be overridden using the command line.'
    )
    args, remaining_argv = conf_parser.parse_known_args()

    defaults = {}
    try:
        if not os.path.exists(
                os.path.join(
                    os.environ['FWROOT'],
                    args.conf_file)) and not os.path.exists(
                        os.path.join(os.environ['FWROOT'] + 'benchmark.cfg')):
            log("No config file found. Aborting!", color=Fore.RED)
            exit(1)
        with open(os.path.join(os.environ['FWROOT'], args.conf_file)):
            config = ConfigParser.SafeConfigParser()
            config.read([os.path.join(os.environ['FWROOT'], args.conf_file)])
            defaults.update(dict(config.items("Defaults")))
            # Convert strings into proper python types
            for k, v in defaults.items():
                try:
                    defaults[k] = literal_eval(v)
                except Exception:
                    pass
    except IOError:
        log("Configuration file not found!", color=Fore.RED)
        exit(1)

    ##########################################################
    # Set up default values
    ##########################################################

    # Verify and massage options
    if defaults['client_user'] is None or defaults['client_host'] is None:
        log("client_user and client_host are required!", color=Fore.RED)
        log("Please check your configuration file.", color=Fore.RED)
        log("Aborting!", color=Fore.RED)
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
    parser = argparse.ArgumentParser(
        description="Install or run the Framework Benchmarks test suite.",
        parents=[conf_parser],
        formatter_class=argparse.ArgumentDefaultsHelpFormatter,
        epilog=
        '''If an argument includes (type int-sequence), then it accepts integer lists in multiple forms.
        Using a single number e.g. 5 will create a list [5]. Using commas will create a list containing those
        values e.g. 1,3,6 creates [1, 3, 6]. Using three colon-separated numbers of start:step:end will create a
        list, using the semantics of python's range function, e.g. 1:3:15 creates [1, 4, 7, 10, 13] while
        0:1:5 creates [0, 1, 2, 3, 4]
        ''')

    # Install options
    parser.add_argument(
        '--init',
        action='store_true',
        default=False,
        help='Initializes the benchmark environment')

    # Suite options
    parser.add_argument(
        '--build',
        nargs='+',
        help='Builds the dockerfile(s) for the given test(s)')
    parser.add_argument(
        '--clean',
        action='store_true',
        default=False,
        help='Removes the results directory')
    parser.add_argument(
        '--new',
        action='store_true',
        default=False,
        help='Initialize a new framework test')
    parser.add_argument(
        '-v',
        '--verbose',
        action='store_true',
        default=False,
        help=
        'Causes the configuration to print before any other commands are executed.'
    )
    parser.add_argument(
        '--quiet',
        action='store_true',
        default=False,
        help=
        'Only print a limited set of messages to stdout, keep the bulk of messages in log files only'
    )
    parser.add_argument(
        '--results-name',
        help='Gives a name to this set of results, formatted as a date',
        default='(unspecified, datetime = %Y-%m-%d %H:%M:%S)')
    parser.add_argument(
        '--results-environment',
        help='Describes the environment in which these results were gathered',
        default='(unspecified, hostname = %s)' % socket.gethostname())
    parser.add_argument(
        '--results-upload-uri',
        default=None,
        help=
        'A URI where the in-progress results.json file will be POSTed periodically'
    )
    parser.add_argument(
        '--parse',
        help=
        'Parses the results of the given timestamp and merges that with the latest results'
    )

    # Test options
    parser.add_argument('--test', nargs='+', help='names of tests to run')
    parser.add_argument(
        '--test-dir',
        nargs='+',
        dest='test_dir',
        help='name of framework directory containing all tests to run')
    parser.add_argument(
        '--test-lang',
        nargs='+',
        dest='test_lang',
        help='name of language directory containing all tests to run')
    parser.add_argument(
        '--exclude', nargs='+', help='names of tests to exclude')
    parser.add_argument(
        '--type',
        choices=[
            'all', 'json', 'db', 'query', 'cached_query', 'fortune', 'update',
            'plaintext'
        ],
        default='all',
        help='which type of test to run')
    parser.add_argument(
        '-m',
        '--mode',
        choices=['benchmark', 'verify', 'debug'],
        default='benchmark',
        help=
        'verify mode will only start up the tests, curl the urls and shutdown. debug mode will skip verification and leave the server running.'
    )
    parser.add_argument(
        '--list-tests',
        action='store_true',
        default=False,
        help='lists all the known tests that can run')

    # Benchmark options
    parser.add_argument(
        '--duration',
        default=15,
        help='Time in seconds that each test should run for.')
    parser.add_argument(
        '--sleep',
        type=int,
        default=60,
        help=
        'the amount of time to sleep after starting each test to allow the server to start up.'
    )

    parser.set_defaults(**defaults)
    # Must do this after add, or each option's default will override the configuration file default
    args = parser.parse_args(remaining_argv)

    config = BenchmarkConfig(vars(args))
    results = Results(config)

    if config.new:
        Scaffolding()

    elif config.init:
        initialize(config)

    elif config.build:
        docker_helper.build(config, config.build)

    elif config.clean:
        cleaner.clean(results)
        docker_helper.clean(config)

    elif config.list_tests:
        all_tests = gather_tests(benchmarker_config=config)

        for test in all_tests:
            log(test.name)

    elif config.parse != None:
        # TODO: broken
        all_tests = gather_tests(benchmarker_config=config)

        for test in all_tests:
            test.parse_all()

        results.parse(all_tests)

    else:
        benchmarker = Benchmarker(config, results)
        if not benchmarker.run():
            return 1

    return 0


if __name__ == "__main__":
    sys.exit(main())
