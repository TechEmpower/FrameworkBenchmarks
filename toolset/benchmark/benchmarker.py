from toolset.utils.output_helper import log, FNULL
from toolset.utils.docker_helper import DockerHelper
from toolset.utils.time_logger import TimeLogger
from toolset.utils.metadata import Metadata
from toolset.utils.results import Results
from toolset.utils.audit import Audit

import os
import subprocess
import traceback
import sys
import time
import shlex
from pprint import pprint

from colorama import Fore
import numbers


class Benchmarker:
    def __init__(self, config):
        '''
        Initialize the benchmarker.
        '''
        self.config = config
        self.time_logger = TimeLogger()
        self.metadata = Metadata(self)
        self.audit = Audit(self)

        # a list of all tests for this run
        self.tests = self.metadata.tests_to_run()

        self.results = Results(self)
        self.docker_helper = DockerHelper(self)

        self.last_test = False

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
        self.metadata.list_test_metadata()

        any_failed = False
        # Run tests
        log("Running Tests...", border='=')

        # build wrk and all databases needed for current run
        self.docker_helper.build_wrk()
        self.docker_helper.build_databases()

        with open(os.path.join(self.results.directory, 'benchmark.log'),
                  'w') as benchmark_log:
            for test in self.tests:
                if self.tests.index(test) + 1 == len(self.tests):
                    self.last_test = True
                log("Running Test: %s" % test.name, border='-')
                with self.config.quiet_out.enable():
                    if not self.__run_test(test, benchmark_log):
                        any_failed = True
                # Load intermediate result from child process
                self.results.load()

        # Parse results
        if self.config.mode == "benchmark":
            log("Parsing Results ...", border='=')
            self.results.parse(self.tests)

        self.results.set_completion_time()
        self.results.upload()
        self.results.finish()

        return any_failed

    def stop(self, signal=None, frame=None):
        log("Shutting down (may take a moment)")
        self.docker_helper.stop()
        sys.exit(0)

    ##########################################################################################
    # Private methods
    ##########################################################################################

    def __exit_test(self, success, prefix, file, message=None):
        if message:
            log(message,
                prefix=prefix,
                file=file,
                color=Fore.RED if success else '')
        self.time_logger.log_test_end(log_prefix=prefix, file=file)
        if self.config.mode == "benchmark":
            # Sleep for 60 seconds to ensure all host connects are closed
            log("Clean up: Sleep 60 seconds...", prefix=prefix, file=file)
            time.sleep(60)
            # After benchmarks are complete for all test types in this test,
            # let's clean up leftover test images (techempower/tfb.test.test-name)
            self.docker_helper.clean()

        return success

    def __run_test(self, test, benchmark_log):
        '''
        Runs the given test, verifies that the webapp is accepting requests,
        optionally benchmarks the webapp, and ultimately stops all services
        started for this test.
        '''

        log_prefix = "%s: " % test.name
        # Start timing the total test duration
        self.time_logger.mark_test_start()

        if self.config.mode == "benchmark":
            log("Benchmarking %s" % test.name,
                file=benchmark_log,
                border='-')

        # If the test is in the excludes list, we skip it
        if self.config.exclude and test.name in self.config.exclude:
            message = "Test {name} has been added to the excludes list. Skipping.".format(
                name=test.name)
            self.results.write_intermediate(test.name, message)
            self.results.upload()
            return self.__exit_test(
                success=False,
                message=message,
                prefix=log_prefix,
                file=benchmark_log)

        database_container = None
        try:
            # Start database container
            if test.database.lower() != "none":
                self.time_logger.mark_starting_database()
                database_container = self.docker_helper.start_database(
                    test.database.lower())
                if database_container is None:
                    message = "ERROR: Problem building/running database container"
                    self.results.write_intermediate(test.name, message)
                    self.results.upload()
                    return self.__exit_test(
                        success=False,
                        message=message,
                        prefix=log_prefix,
                        file=benchmark_log)
                self.time_logger.mark_started_database()

            # Start webapp
            container = test.start()
            self.time_logger.mark_test_starting()
            if container is None:
                message = "ERROR: Problem starting {name}".format(
                    name=test.name)
                self.results.write_intermediate(test.name, message)
                self.results.upload()
                return self.__exit_test(
                    success=False,
                    message=message,
                    prefix=log_prefix,
                    file=benchmark_log)

            max_time = time.time() + 60
            while True:
                accepting_requests = test.is_accepting_requests()
                if accepting_requests \
                        or time.time() >= max_time \
                        or not self.docker_helper.server_container_exists(container.id):
                    break
                time.sleep(1)

            if hasattr(test, 'wait_before_sending_requests') and isinstance(test.wait_before_sending_requests, numbers.Integral) and test.wait_before_sending_requests > 0:
                time.sleep(test.wait_before_sending_requests)

            if not accepting_requests:
                message = "ERROR: Framework is not accepting requests from client machine"
                self.results.write_intermediate(test.name, message)
                self.results.upload()
                return self.__exit_test(
                    success=False,
                    message=message,
                    prefix=log_prefix,
                    file=benchmark_log)

            self.time_logger.mark_test_accepting_requests()

            # Debug mode blocks execution here until ctrl+c
            if self.config.mode == "debug":
                msg = "Entering debug mode. Server http://localhost:%s has started. CTRL-c to stop.\r\n" % test.port
                msg = msg + "From outside vagrant: http://localhost:%s" % (int(test.port) + 20000)
                log(msg,
                    prefix=log_prefix,
                    file=benchmark_log,
                    color=Fore.YELLOW)
                while True:
                    time.sleep(1)

            # Verify URLs and audit
            log("Verifying framework URLs", prefix=log_prefix)
            self.time_logger.mark_verify_start()
            passed_verify = test.verify_urls()
            self.audit.audit_test_dir(test.directory)

            # Benchmark this test
            if self.config.mode == "benchmark":
                self.time_logger.mark_benchmarking_start()
                self.__benchmark(test, benchmark_log)
                self.time_logger.log_benchmarking_end(
                    log_prefix=log_prefix, file=benchmark_log)

            # Log test timing stats
            self.time_logger.log_build_flush(benchmark_log)
            self.time_logger.log_database_start_time(log_prefix, benchmark_log)
            self.time_logger.log_test_accepting_requests(
                log_prefix, benchmark_log)
            self.time_logger.log_verify_end(log_prefix, benchmark_log)

            # Save results thus far into the latest results directory
            self.results.write_intermediate(test.name,
                                            time.strftime(
                                                "%Y%m%d%H%M%S",
                                                time.localtime()))

            # Upload the results thus far to another server (optional)
            self.results.upload()

            if self.config.mode == "verify" and not passed_verify:
                return self.__exit_test(
                    success=False,
                    message="Failed verify!",
                    prefix=log_prefix,
                    file=benchmark_log)
        except Exception as e:
            tb = traceback.format_exc()
            self.results.write_intermediate(test.name,
                                            "error during test: " + str(e))
            self.results.upload()
            log(tb, prefix=log_prefix, file=benchmark_log)
            return self.__exit_test(
                success=False,
                message="Error during test: %s" % test.name,
                prefix=log_prefix,
                file=benchmark_log)
        finally:
            self.docker_helper.stop()

        return self.__exit_test(
            success=True, prefix=log_prefix, file=benchmark_log)

    def __benchmark(self, framework_test, benchmark_log):
        '''
        Runs the benchmark for each type of test that it implements
        '''

        def benchmark_type(test_type):
            log("BENCHMARKING %s ... " % test_type.upper(), file=benchmark_log)

            test = framework_test.runTests[test_type]
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

                self.docker_helper.benchmark(script, script_variables,
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
        dstat_string = "dstat -Tafilmprs --aio --fs --ipc --lock --socket --tcp \
                                      --raw --udp --unix --vm --disk-util \
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

