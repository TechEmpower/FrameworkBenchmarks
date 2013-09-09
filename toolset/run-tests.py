#!/usr/bin/env python
import argparse
import sys
from benchmark.benchmarker import Benchmarker
from setup.linux.unbuffered import Unbuffered

###################################################################################################
# Main
###################################################################################################

# Enable unbuffered output so messages will appear in the proper order with subprocess output.
sys.stdout=Unbuffered(sys.stdout)

# Ensure the current directory (which should be the benchmark home directory) is in the path so that the tests can be imported.
sys.path.append('.')

# Ensure toolset/setup/linux is in the path so that the tests can "import setup_util".
sys.path.append('toolset/setup/linux')

##########################################################
# Set up argument parser
##########################################################
parser = argparse.ArgumentParser(description='Run the Framework Benchmarking test suite.')
parser.add_argument('-s', '--server-host', default='localhost', help='The application server.')
parser.add_argument('-c', '--client-host', default='localhost', help='The client / load generation server.')
parser.add_argument('-u', '--client-user', help='The username to use for SSH to the client instance.')
parser.add_argument('-i', '--client-identity-file', dest='client_identity_file', help='The key to use for SSH to the client instance.')
parser.add_argument('-d', '--database-host', help='The database server.  If not provided, defaults to the value of --client-host.')
parser.add_argument('--database-user', help='The username to use for SSH to the database instance.  If not provided, defaults to the value of --client-user.')
parser.add_argument('--database-identity-file', dest='database_identity_file', help='The key to use for SSH to the database instance.  If not provided, defaults to the value of --client-identity-file.')
parser.add_argument('-p', dest='password_prompt', action='store_true')
parser.add_argument('--install-software', action='store_true', help='runs the installation script before running the rest of the commands')
parser.add_argument('--install', choices=['client', 'database', 'server', 'all'], default='all', help='Allows you to only install the server, client, or database software')
parser.add_argument('--install-error-action', choices=['abort', 'continue'], default='continue', help='action to take in case of error during installation')
parser.add_argument('--test', nargs='+', help='names of tests to run')
parser.add_argument('--exclude', nargs='+', help='names of tests to exclude')
parser.add_argument('--type', choices=['all', 'json', 'db', 'query', 'fortune', 'update', 'plaintext'], default='all', help='which type of test to run')
parser.add_argument('-m', '--mode', choices=['benchmark', 'verify'], default='benchmark', help='verify mode will only start up the tests, curl the urls and shutdown')
parser.add_argument('--list-tests', action='store_true', default=False, help='lists all the known tests that can run')
parser.add_argument('--list-test-metadata', action='store_true', default=False, help='writes all the test metadata as a JSON file in the results directory')
parser.add_argument('--max-concurrency', default=256, help='the maximum concurrency that the tests will run at. The query tests will run at this concurrency', type=int)
parser.add_argument('--max-queries', default=20, help='The maximum number of queries to run during the query test', type=int)
parser.add_argument('--query-interval', default=5, type=int)
parser.add_argument('--max-threads', default=8, help='The max number of threads to run weight at, this should be set to the number of cores for your system.', type=int)
parser.add_argument('--duration', default=60, help='Time in seconds that each test should run for.')
parser.add_argument('--starting-concurrency', default=8, type=int)
parser.add_argument('--sleep', type=int, default=60, help='the amount of time to sleep after starting each test to allow the server to start up.')
parser.add_argument('--parse', help='Parses the results of the given timestamp and merges that with the latest results')
parser.add_argument('--name', default="ec2", help='The name to give this test. Results will be placed in a folder using this name.')
parser.add_argument('--application-os', choices=['linux', 'windows'], default='linux', help='The operating system of the application server.')
parser.add_argument('--database-os', choices=['linux', 'windows'], default='linux', help='The operating system of the database server.')
args = parser.parse_args()

benchmarker = Benchmarker(vars(args))

# Run the benchmarker in the specified mode
if benchmarker.list_tests:
  benchmarker.run_list_tests()
elif benchmarker.list_test_metadata:
  benchmarker.run_list_test_metadata()
elif benchmarker.parse != None:
  benchmarker.parse_timestamp()
else:
  benchmarker.run()
