from toolset.benchmark.test_types import *
from toolset.utils.output_helper import QuietOutputStream

import os
import time


class BenchmarkConfig:
    def __init__(self, args):
        '''
        Configures this BenchmarkConfig given the arguments provided.
        '''

        # Map type strings to their objects
        types = dict()
        types['json'] = JsonTestType(self)
        types['db'] = DBTestType(self)
        types['query'] = QueryTestType(self)
        types['fortune'] = FortuneTestType(self)
        types['update'] = UpdateTestType(self)
        types['plaintext'] = PlaintextTestType(self)
        types['cached_query'] = CachedQueryTestType(self)

        # Turn type into a map instead of a string
        if args.type == 'all':
            self.types = types
        else:
            self.types = {args.type: types[args.type]}

        self.duration = args.duration
        self.exclude = args.exclude
        self.publish = args.publish
        self.build = args.build
        self.quiet = args.quiet
        self.server_host = args.server_host
        self.database_host = args.database_host
        self.client_host = args.client_host
        self.new = args.new
        self.clean = args.clean
        self.mode = args.mode
        self.list_tests = args.list_tests
        self.max_concurrency = max(args.concurrency_levels)
        self.concurrency_levels = args.concurrency_levels
        self.cached_query_levels = args.cached_query_levels
        self.pipeline_concurrency_levels = args.pipeline_concurrency_levels
        self.query_levels = args.query_levels
        self.parse = args.parse
        self.results_environment = args.results_environment
        self.results_name = args.results_name
        self.results_upload_uri = args.results_upload_uri
        self.test = args.test
        self.test_dir = args.test_dir
        self.test_lang = args.test_lang
        self.network_mode = args.network_mode
        self.server_docker_host = None
        self.database_docker_host = None
        self.client_docker_host = None
        self.network = None

        if self.network_mode is None:
            self.network = 'tfb'
            self.server_docker_host = "unix://var/run/docker.sock"
            self.database_docker_host = "unix://var/run/docker.sock"
            self.client_docker_host = "unix://var/run/docker.sock"
        else:
            self.network = None
            # The only other supported network_mode is 'host', and that means
            # that we have a tri-machine setup, so we need to use tcp to
            # communicate with docker.
            self.server_docker_host = "tcp://%s:2375" % self.server_host
            self.database_docker_host = "tcp://%s:2375" % self.database_host
            self.client_docker_host = "tcp://%s:2375" % self.client_host

        self.quiet_out = QuietOutputStream(self.quiet)

        self.start_time = time.time()

        # Remember root directory
        self.fwroot = os.getenv('FWROOT')

        if hasattr(self, 'parse') and self.parse != None:
            self.timestamp = self.parse
        else:
            self.timestamp = time.strftime("%Y%m%d%H%M%S", time.localtime())

        self.run_test_timeout_seconds = 7200
