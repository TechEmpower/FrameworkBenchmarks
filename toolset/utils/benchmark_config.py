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
        if args['type'] == 'all':
            args['types'] = types
        else:
            args['types'] = {args['type']: types[args['type']]}
        del args['type']

        args['max_concurrency'] = max(args['concurrency_levels'])
        if 'pipeline_concurrency_levels' not in args:
            args['pipeline_concurrency_levels'] = [256, 1024, 4096, 16384]

        self.quiet = False
        self.client_host = ""
        self.database_host = ""
        self.parse = False
        self.new = False
        self.init = False
        self.build = False
        self.clean = False
        self.list_tests = False
        self.concurrency_levels = []
        self.pipeline_concurrency_levels = []
        self.network_mode = None
        self.network = None
        self.server_docker_host = None
        self.database_docker_host = None

        self.__dict__.update(args)

        if self.network_mode is None:
            self.network_mode = None
            self.network = 'tfb'

        self.quiet_out = QuietOutputStream(self.quiet)

        self.start_time = time.time()

        # Remember root directory
        self.fwroot = os.getenv('FWROOT')

        if hasattr(self, 'parse') and self.parse != None:
            self.timestamp = self.parse
        else:
            self.timestamp = time.strftime("%Y%m%d%H%M%S", time.localtime())

        self.run_test_timeout_seconds = 7200
