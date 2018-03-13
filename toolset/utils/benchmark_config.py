from toolset.utils import setup_util
from toolset.benchmark.test_types import *
from toolset.utils.output_helper import QuietOutputStream

import logging
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
        self.client_user = ""
        self.client_host = ""
        self.client_identity_file = ""
        self.database_user = ""
        self.database_host = ""
        self.database_identity_file = ""
        self.parse = False
        self.new = False
        self.init = False
        self.build = False
        self.clean = False
        self.list_tests = False
        self.concurrency_levels = []
        self.pipeline_concurrency_levels = []

        self.__dict__.update(args)

        self.quiet_out = QuietOutputStream(self.quiet)

        self.start_time = time.time()

        # setup logging
        logging.basicConfig(stream=self.quiet_out, level=logging.INFO)

        # setup some additional variables
        if self.database_user == None: self.database_user = self.client_user
        if self.database_host == None: self.database_host = self.client_host
        if self.database_identity_file == None:
            self.database_identity_file = self.client_identity_file

        # Remember root directory
        self.fwroot = setup_util.get_fwroot()

        if hasattr(self, 'parse') and self.parse != None:
            self.timestamp = self.parse
        else:
            self.timestamp = time.strftime("%Y%m%d%H%M%S", time.localtime())

        # Setup the ssh commands
        self.client_ssh_command = [
            'ssh', '-T', 'o', 'StrictHostKeyChecking=no',
            self.client_user + "@" + self.client_host
        ]
        if self.client_identity_file != None:
            self.client_ssh_command.extend(['-i', self.client_identity_file])

        self.database_ssh_command = [
            'ssh', '-T', '-o', 'StrictHostKeyChecking=no',
            self.database_user + "@" + self.database_host
        ]
        if self.database_identity_file != None:
            self.database_ssh_command.extend(
                ['-i', self.database_identity_file])

        self.run_test_timeout_seconds = 7200
