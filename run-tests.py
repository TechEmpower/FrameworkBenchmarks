#!/usr/bin/env python
import argparse
from benchmarker import Benchmarker

###################################################################################################
# Main
###################################################################################################

##########################################################
# Set up argument parser
##########################################################
parser = argparse.ArgumentParser(description='Run the Framework Benchmarking test suite.')
parser.add_argument('-s', '--server-host', default='localhost')
parser.add_argument('-c', '--client-host', default='localhost')
parser.add_argument('-u', '--client-user', default='ubuntu')
parser.add_argument('-d', '--database-host')
parser.add_argument('-i', dest='identity_file', help='ssh key to ssh from the server instance tothe client instance.')
parser.add_argument('-p', dest='password_prompt', action='store_true')
parser.add_argument('--install-software', action='store_true', help='runs the installation script before running the rest of the commands')
parser.add_argument('--test', nargs='+', help='names of tests to run')
parser.add_argument('--exclude', nargs='+', help='names of tests to exclude')
parser.add_argument('--type', choices=['all', 'json', 'db', 'query'], default='all', help='which type of test to run')
parser.add_argument('-m', '--mode', choices=['benchmark', 'verify'], default='benchmark', help='verify mode will only start up the tests, curl the urls and shutdown')
parser.add_argument('--list-tests', action='store_true', default=False, help='lists all the known tests that can run')
parser.add_argument('--next-sort', action='store_true', default=False, help='displatys the next value that can be used as a sort value')
parser.add_argument('--max-concurrency', default=256, help='the maximum concurrency that the tests will run at. The query tests will run at this concurrency', type=int)
parser.add_argument('--max-queries', default=20, help='The maximum number of queries to run during the query test', type=int)
parser.add_argument('--query-interval', default=5, type=int)
parser.add_argument('--max-threads', default=8, help='The max number of threads to run weight at, this shoul dbe set to the number of cores for your system.', type=int)
parser.add_argument('--number-of-runs', default=100000, help='the number of requests to make per test run.')
parser.add_argument('--starting-concurrency', default=8, type=int)
parser.add_argument('--sleep', type=int, default=60, help='the amount of time to sleep after starting each test to allow the server to start up.')
parser.add_argument('--parse', help='Parses the results of the given timestamp and merges that with the latest results')
parser.add_argument('--name', default="ec2", help='The name to give this test. Results will be placed in a folder using this name.')
args = parser.parse_args()

benchmarker = Benchmarker(vars(args))

# Run the benchmarker in the specified mode
if benchmarker.list_tests:
  benchmarker.run_list_tests()
elif benchmarker.next_sort:
  benchmarker.next_sort_value()
elif benchmarker.parse != None:
  benchmarker.parse_timestamp()
else:
  benchmarker.run()
