from toolset.utils.output_helper import QuietOutputStream
from toolset.test_types import test_types

import os
import time


class BenchmarkConfig:
    def __init__(self, args):
        '''
        Configures this BenchmarkConfig given the arguments provided.
        '''

        # Map type strings to their objects
        types = {}
        for type in test_types:
            types[type] = test_types[type](self)

        # Turn type into a map instead of a list of strings
        if 'all' in args.type:
            self.types = types
        else:
            self.types = {t: types[t] for t in args.type}

        self.duration = args.duration
        self.exclude = args.exclude
        self.quiet = args.quiet
        self.server_host = args.server_host
        self.database_host = args.database_host
        self.client_host = args.client_host
        self.audit = args.audit
        self.new = args.new
        self.mode = args.mode
        self.list_tests = args.list_tests
        self.list_tag = args.list_tag
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
        self.tag = args.tag
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

        # Remember directories
        self.fw_root = os.getenv('FWROOT')
        self.db_root = os.path.join(self.fw_root, "toolset", "databases")
        self.lang_root = os.path.join(self.fw_root, "frameworks")
        self.results_root = os.path.join(self.fw_root, "results")
        self.wrk_root = os.path.join(self.fw_root, "toolset", "wrk")
        self.scaffold_root = os.path.join(self.fw_root, "toolset", "scaffolding")

        if hasattr(self, 'parse') and self.parse is not None:
            self.timestamp = self.parse
        else:
            self.timestamp = time.strftime("%Y%m%d%H%M%S", time.localtime())

        self.run_test_timeout_seconds = 7200
