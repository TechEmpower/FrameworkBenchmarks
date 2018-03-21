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
        running their setup script, verifying the URLs, and
        running benchmarks against them.
        '''
        # Generate metadata
        self.__run_list_test_metadata()

        # Get a list of all known  tests that we can run.
        all_tests = gather_remaining_tests(self.config, self.results)

        # Setup client/server
        log(
            "Preparing Server, Database, and Client ...", border='=')
        with self.config.quiet_out.enable():
            self.__setup_server()
            self.__setup_database()
            self.__setup_client()

        # Run tests
        success = True
        log("Running Tests...", border='=')
        with open(os.path.join(self.results.directory, 'benchmark.log'),
                  'w') as benchmark_log:
            for test in all_tests:
                log("Running Test: %s" % test.name, border='-')
                with self.config.quiet_out.enable():
                    success = self.__run_test(test, benchmark_log) and success
                # Load intermediate result from child process
                self.results.load()

        # Parse results
        if self.config.mode == "benchmark":
            log("Parsing Results ...", border='=')
            self.results.parse(all_tests)

        self.results.set_completion_time()
        self.results.upload()
        self.results.finish()

        return success

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

    def __setup_server(self):
        '''
        Makes any necessary changes to the server that should be
        made before running the tests. This involves setting kernal
        settings to allow for more connections, or more file
        descriptiors
        
        http://redmine.lighttpd.net/projects/weighttp/wiki#Troubleshooting
        '''
        try:
            subprocess.call(
                ['sudo', 'sysctl', '-w', 'net.ipv4.tcp_max_syn_backlog=65535'],
                stdout=FNULL,
                stderr=subprocess.STDOUT)
            subprocess.call(
                ['sudo', 'sysctl', '-w', 'net.core.somaxconn=65535'],
                stdout=FNULL,
                stderr=subprocess.STDOUT)
            subprocess.call(
                ['sudo', 'sysctl', 'net.ipv4.tcp_tw_reuse=1'],
                stdout=FNULL,
                stderr=subprocess.STDOUT)
            subprocess.call(
                ['sudo', 'sysctl', 'net.ipv4.tcp_tw_recycle=1'],
                stdout=FNULL,
                stderr=subprocess.STDOUT)
            subprocess.call(
                ['sudo', 'sysctl', '-w', 'kernel.shmmax=134217728'],
                stdout=FNULL,
                stderr=subprocess.STDOUT)
            subprocess.call(
                ['sudo', 'sysctl', '-w', 'kernel.shmall=2097152'],
                stdout=FNULL,
                stderr=subprocess.STDOUT)

            with open(os.path.join(self.results.directory, 'sysctl.txt'),
                      'w') as f:
                f.write(subprocess.check_output(['sudo', 'sysctl', '-a']))
        except subprocess.CalledProcessError:
            return False

    def __setup_database(self):
        '''
        Makes any necessary changes to the database machine that should be made 
        before running the tests. Is very similar to the server setup, but may also 
        include database specific changes.

        Explanations:
        net.ipv4.tcp_max_syn_backlog, net.core.somaxconn, kernel.sched_autogroup_enabled: http://tweaked.io/guide/kernel/
        ulimit -n: http://www.cyberciti.biz/faq/linux-increase-the-maximum-number-of-open-files/
        net.ipv4.tcp_tw_*: http://www.linuxbrigade.com/reduce-time_wait-socket-connections/
        kernel.shm*: http://seriousbirder.com/blogs/linux-understanding-shmmax-and-shmall-settings/
        For kernel.sem: https://access.redhat.com/documentation/en-US/Red_Hat_Enterprise_Linux/5/html/Tuning_and_Optimizing_Red_Hat_Enterprise_Linux_for_Oracle_9i_and_10g_Databases/chap-Oracle_9i_and_10g_Tuning_Guide-Setting_Semaphores.html
        '''
        command = list(self.config.database_ssh_command)
        command.extend([
            """
            sudo sysctl -w net.ipv4.tcp_max_syn_backlog=65535
            sudo sysctl -w net.core.somaxconn=65535
            sudo sysctl -w kernel.sched_autogroup_enabled=0
            sudo -s ulimit -n 65535
            sudo sysctl net.ipv4.tcp_tw_reuse=1
            sudo sysctl net.ipv4.tcp_tw_recycle=1
            sudo sysctl -w kernel.shmmax=2147483648
            sudo sysctl -w kernel.shmall=2097152
            sudo sysctl -w kernel.sem="250 32000 256 512"
            """
        ])
        subprocess.check_call(command, stdout=FNULL, stderr=subprocess.STDOUT)
        # TODO - print kernel configuration to file
        # echo "Printing kernel configuration:" && sudo sysctl -a

    def __setup_client(self):
        '''
        Makes any necessary changes to the client machine that should be made 
        before running the tests. Is very similar to the server setup, but may also 
        include client specific changes.
        '''
        command = list(self.config.client_ssh_command)
        command.extend([
            """
            sudo sysctl -w net.ipv4.tcp_max_syn_backlog=65535
            sudo sysctl -w net.core.somaxconn=65535
            sudo -s ulimit -n 65535
            sudo sysctl net.ipv4.tcp_tw_reuse=1
            sudo sysctl net.ipv4.tcp_tw_recycle=1
            sudo sysctl -w kernel.shmmax=2147483648
            sudo sysctl -w kernel.shmall=2097152
            """
        ])
        subprocess.check_call(command, stdout=FNULL, stderr=subprocess.STDOUT)

    def __run_test(self, test, benchmark_log):
        '''
        Runs the given test, verifies that the webapp is accepting requests,
        optionally benchmarks the webapp, and ultimately stops all services
        started for this test.
        '''
        log_prefix = "%s: " % test.name

        if test.os.lower() != self.config.os.lower() or test.database_os.lower(
        ) != self.config.database_os.lower():
            log("OS or Database OS specified in benchmark_config.json does not match the current environment. Skipping.",
                log_prefix, benchmark_log)
            return False

        # If the test is in the excludes list, we skip it
        if self.config.exclude != None and test.name in self.config.exclude:
            log("Test {name} has been added to the excludes list. Skipping.".
                format(name=test.name),
                log_prefix,
                benchmark_log)
            return False

        database_container_id = None
        try:
            if self.__is_port_bound(test.port):
                time.sleep(60)

            if self.__is_port_bound(test.port):
                # We gave it our all
                self.results.write_intermediate(test.name, "port " + str(
                    test.port) + " is not available before start")
                log("Error: Port %s is not available, cannot start %s" %
                       (test.port, test.name),
                    prefix=log_prefix, file=benchmark_log, color=Fore.RED)
                return False

            # Start database container
            if test.database.lower() != "none":
                database_container_id = docker_helper.start_database(
                    self.config, test.database.lower())
                if not database_container_id:
                    self.results.write_intermediate(test.name,
                                                    "ERROR: Problem starting")
                    log("ERROR: Problem building/running database container",
                        prefix=log_prefix, file=benchmark_log, color=Fore.RED)
                    return False

            # Start webapp
            result = test.start(database_container_id)
            if result != 0:
                docker_helper.stop(self.config, database_container_id, test)
                self.results.write_intermediate(test.name,
                                                "ERROR: Problem starting")
                log("ERROR: Problem starting {name}".format(name=test.name),
                    prefix=log_prefix, file=benchmark_log, color=Fore.RED)
                return False

            slept = 0
            max_sleep = 60
            while not test.is_running() and slept < max_sleep:
                if not docker_helper.successfully_running_containers(
                        test.get_docker_files(), benchmark_log):
                    docker_helper.stop(self.config, database_container_id,
                                       test)
                    log("ERROR: One or more expected docker container exited early",
                        prefix=log_prefix, file=benchmark_log, color=Fore.RED)
                    return False
                time.sleep(1)
                slept += 1

            # Debug mode blocks execution here until ctrl+c
            if self.config.mode == "debug":
                log("Entering debug mode. Server has started. CTRL-c to stop.",
                    prefix=log_prefix, file=benchmark_log, color=Fore.YELLOW)
                while True:
                    time.sleep(1)

            # Verify URLs
            log("Verifying framework URLs", prefix=log_prefix)
            passed_verify = test.verify_urls()

            # Benchmark this test
            if self.config.mode == "benchmark":
                log("Benchmarking %s" % test.name,
                    file=benchmark_log, border='-')
                self.__benchmark(test, benchmark_log)

            # Stop this test
            docker_helper.stop(self.config, database_container_id, test)

            # Remove contents of  /tmp folder
            try:
                subprocess.check_call(
                    'sudo rm -rf /tmp/*',
                    shell=True,
                    stderr=benchmark_log,
                    stdout=benchmark_log)
            except Exception:
                log("Error: Could not empty /tmp",
                    file=benchmark_log, color=Fore.RED)

            # Save results thus far into the latest results directory
            self.results.write_intermediate(test.name,
                                            time.strftime(
                                                "%Y%m%d%H%M%S",
                                                time.localtime()))

            # Upload the results thus far to another server (optional)
            self.results.upload()

            if self.config.mode == "verify" and not passed_verify:
                log("Failed verify!",
                    prefix=log_prefix, file=benchmark_log, color=Fore.RED)
                return False
        except KeyboardInterrupt:
            docker_helper.stop(self.config, database_container_id, test)
        except (OSError, IOError, subprocess.CalledProcessError) as e:
            tb = traceback.format_exc()
            self.results.write_intermediate(
                test.name, "error during test setup: " + str(e))
            log("Subprocess Error %s" % test.name,
                file=benchmark_log, border='-', color=Fore.RED)
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
                remote_script = self.config.types[test_type].get_remote_script(
                    self.config, test.name, test.get_url(),
                    framework_test.port)

                # Begin resource usage metrics collection
                self.__begin_logging(framework_test, test_type)

                # Run the benchmark
                with open(raw_file, 'w') as raw_file:
                    p = subprocess.Popen(
                        self.config.client_ssh_command,
                        stdin=subprocess.PIPE,
                        stdout=raw_file,
                        stderr=raw_file)
                    p.communicate(remote_script)

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
