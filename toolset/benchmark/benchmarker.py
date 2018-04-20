from toolset.utils.output_helper import log, FNULL
from toolset.utils.metadata_helper import gather_tests, gather_remaining_tests
from toolset.utils import docker_helper

import os
import subprocess
import traceback
import socket
import time
import json
import shlex
from pprint import pprint

from colorama import Fore


class Benchmarker:
    def __init__(self, config, results):
        '''
        Initialize the benchmarker.
        '''
        self.config = config
        self.results = results

    ##########################################################################################
    # Public methods
    ##########################################################################################

    def run(self):
        '''
        This process involves setting up the client/server machines
        with any necessary change. Then going through each test,
        running their docker build and run, verifying the URLs, and
        running benchmarks against them.
        '''
        # Generate metadata
        self.__run_list_test_metadata()

        # Get a list of all known  tests that we can run.
        all_tests = gather_remaining_tests(self.config, self.results)

        any_failed = False
        # Run tests
        log("Running Tests...", border='=')
        docker_helper.build_wrk(self.config)

        with open(os.path.join(self.results.directory, 'benchmark.log'),
                  'w') as benchmark_log:
            for test in all_tests:
                log("Running Test: %s" % test.name, border='-')
                with self.config.quiet_out.enable():
                    if not self.__run_test(test, benchmark_log):
                        any_failed = True
                # Load intermediate result from child process
                self.results.load()

        # Parse results
        if self.config.mode == "benchmark":
            log("Parsing Results ...", border='=')
            self.results.parse(all_tests)

        self.results.set_completion_time()
        self.results.upload()
        self.results.finish()

        return any_failed

    ##########################################################################################
    # Private methods
    ##########################################################################################

    def __run_list_test_metadata(self):
        '''
        Prints the metadata for all the available tests
        '''
        all_tests = gather_tests(benchmarker_config=self.config)
        all_tests_json = json.dumps(map(lambda test: {
          "name": test.name,
          "approach": test.approach,
          "classification": test.classification,
          "database": test.database,
          "framework": test.framework,
          "language": test.language,
          "orm": test.orm,
          "platform": test.platform,
          "webserver": test.webserver,
          "os": test.os,
          "database_os": test.database_os,
          "display_name": test.display_name,
          "notes": test.notes,
          "versus": test.versus
        }, all_tests))

        with open(
                os.path.join(self.results.directory, "test_metadata.json"),
                "w") as f:
            f.write(all_tests_json)

    def __run_test(self, test, benchmark_log):
        '''
        Runs the given test, verifies that the webapp is accepting requests,
        optionally benchmarks the webapp, and ultimately stops all services
        started for this test.
        '''
        log_prefix = "%s: " % test.name

        # If the test is in the excludes list, we skip it
        if self.config.exclude != None and test.name in self.config.exclude:
            message = "Test {name} has been added to the excludes list. Skipping.".format(name=test.name)
            self.results.write_intermediate(test.name, message)
            log(message,
                prefix=log_prefix,
                file=benchmark_log)
            return False

        database_container = None
        try:
            if self.__is_port_bound(test.port):
                time.sleep(60)

            if self.__is_port_bound(test.port):
                # We gave it our all
                message = "Error: Port %s is not available, cannot start %s" % (test.port, test.name)
                self.results.write_intermediate(test.name, message)
                log(message,
                    prefix=log_prefix,
                    file=benchmark_log,
                    color=Fore.RED)
                return False

            # Start database container
            if test.database.lower() != "none":
                database_container = docker_helper.start_database(
                    self.config, test.database.lower())
                if database_container is None:
                    message = "ERROR: Problem building/running database container"
                    self.results.write_intermediate(test.name, message)
                    log(message,
                        prefix=log_prefix,
                        file=benchmark_log,
                        color=Fore.RED)
                    return False

            # Start webapp
            container = test.start()
            if container is None:
                docker_helper.stop(self.config, container, database_container,
                                   test)
                message = "ERROR: Problem starting {name}".format(name=test.name)
                self.results.write_intermediate(test.name, message)
                log(message,
                    prefix=log_prefix,
                    file=benchmark_log,
                    color=Fore.RED)
                return False

            slept = 0
            max_sleep = 60
            accepting_requests = False
            while not accepting_requests and slept < max_sleep:
                if not docker_helper.server_container_exists(self.config, container.id):
                    break
                accepting_requests = test.is_accepting_requests()
                time.sleep(1)
                slept += 1

            if not accepting_requests:
                docker_helper.stop(self.config, container, database_container,
                                   test)
                message = "ERROR: Framework is not accepting requests from client machine"
                self.results.write_intermediate(test.name, message)
                log(message,
                    prefix=log_prefix,
                    file=benchmark_log,
                    color=Fore.RED)
                return False

            # Debug mode blocks execution here until ctrl+c
            if self.config.mode == "debug":
                log("Entering debug mode. Server has started. CTRL-c to stop.",
                    prefix=log_prefix,
                    file=benchmark_log,
                    color=Fore.YELLOW)
                while True:
                    time.sleep(1)

            # Verify URLs
            log("Verifying framework URLs", prefix=log_prefix)
            passed_verify = test.verify_urls()

            # Benchmark this test
            if self.config.mode == "benchmark":
                log("Benchmarking %s" % test.name,
                    file=benchmark_log,
                    border='-')
                self.__benchmark(test, benchmark_log)

            # Stop this test
            docker_helper.stop(self.config, container, database_container,
                               test)

            # Save results thus far into the latest results directory
            self.results.write_intermediate(test.name,
                                            time.strftime(
                                                "%Y%m%d%H%M%S",
                                                time.localtime()))

            # Upload the results thus far to another server (optional)
            self.results.upload()

            if self.config.mode == "verify" and not passed_verify:
                log("Failed verify!",
                    prefix=log_prefix,
                    file=benchmark_log,
                    color=Fore.RED)
                return False
        except (OSError, IOError, subprocess.CalledProcessError) as e:
            tb = traceback.format_exc()
            self.results.write_intermediate(test.name,
                                            "error during test: " + str(e))
            log("Subprocess Error %s" % test.name,
                file=benchmark_log,
                border='-',
                color=Fore.RED)
            log(tb, prefix=log_prefix, file=benchmark_log)
            return False

        return True

    def __benchmark(self, framework_test, benchmark_log):
        '''
        Runs the benchmark for each type of test that it implements
        '''

        def benchmark_type(test_type):
            log("BENCHMARKING %s ... " % test_type.upper(), file=benchmark_log)

            test = framework_test.runTests[test_type]
            test.setup_out(benchmark_log)
            raw_file = self.results.get_raw_file(framework_test.name,
                                                 test_type)
            if not os.path.exists(raw_file):
                # Open to create the empty file
                with open(raw_file, 'w'):
                    pass

            if not test.failed:
                # Begin resource usage metrics collection
                self.__begin_logging(framework_test, test_type)

                script = self.config.types[test_type].get_script_name()
                script_variables = self.config.types[
                    test_type].get_script_variables(
                        test.name, "http://%s:%s%s" % (self.config.server_host,
                                                       framework_test.port,
                                                       test.get_url()))

                docker_helper.benchmark(self.config, script, script_variables,
                                        raw_file)

                # End resource usage metrics collection
                self.__end_logging()

            results = self.results.parse_test(framework_test, test_type)
            log("Benchmark results:", file=benchmark_log)
            # TODO move into log somehow
            pprint(results)

            self.results.report_benchmark_results(framework_test, test_type,
                                                  results['results'])
            log("Complete", file=benchmark_log)

        for test_type in framework_test.runTests:
            benchmark_type(test_type)

    def __begin_logging(self, framework_test, test_type):
        '''
        Starts a thread to monitor the resource usage, to be synced with the
        client's time.
        TODO: MySQL and InnoDB are possible. Figure out how to implement them.
        '''
        output_file = "{file_name}".format(
            file_name=self.results.get_stats_file(framework_test.name,
                                                  test_type))
        dstat_string = "dstat -Tafilmprs --aio --fs --ipc --lock --raw --socket --tcp \
                                      --raw --socket --tcp --udp --unix --vm --disk-util \
                                      --rpc --rpcd --output {output_file}".format(
            output_file=output_file)
        cmd = shlex.split(dstat_string)
        self.subprocess_handle = subprocess.Popen(
            cmd, stdout=FNULL, stderr=subprocess.STDOUT)

    def __end_logging(self):
        '''
        Stops the logger thread and blocks until shutdown is complete.
        '''
        self.subprocess_handle.terminate()
        self.subprocess_handle.communicate()

    def __is_port_bound(self, port):
        '''
        Check if the requested port is available. If it isn't available, then a
        previous test probably didn't shutdown properly.
        '''
        port = int(port)
        s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        try:
            # Try to bind to all IP addresses, this port
            s.bind(("", port))
            # If we get here, we were able to bind successfully,
            # which means the port is free.
        except socket.error:
            # If we get an exception, it might be because the port is still bound
            # which would be bad, or maybe it is a privileged port (<1024) and we
            # are not running as root, or maybe the server is gone, but sockets are
            # still in TIME_WAIT (SO_REUSEADDR). To determine which scenario, try to
            # connect.
            try:
                s.connect(("127.0.0.1", port))
                # If we get here, we were able to connect to something, which means
                # that the port is still bound.
                return True
            except socket.error:
                # An exception means that we couldn't connect, so a server probably
                # isn't still running on the port.
                pass
        finally:
            s.close()

        return False
