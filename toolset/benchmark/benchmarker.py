from toolset.setup.linux import setup_util

from toolset.benchmark import framework_test
from toolset.utils.output_helper import header
from toolset.utils.metadata_helper import gather_frameworks, gather_tests, gather_remaining_tests
from toolset.utils.Results import Results

import os
import shutil
import stat
import requests
import subprocess
import traceback
import pprint
import csv
import sys
import logging
import socket
import threading
import textwrap
import docker
import shutil
import time
import json
from pprint import pprint

from contextlib import contextmanager

from multiprocessing import Process


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

    def run_list_test_metadata(self):
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

    def run(self):
        '''
        This process involves setting up the client/server machines
        with any necessary change. Then going through each test,
        running their setup script, verifying the URLs, and
        running benchmarks against them.
        '''
        # Generate metadata
        self.run_list_test_metadata()

        # Get a list of all known  tests that we can run.
        all_tests = gather_remaining_tests(self.config, self.results)

        # Setup client/server
        print(
            header(
                "Preparing Server, Database, and Client ...",
                top='=',
                bottom='='))
        with self.config.quiet_out.enable():
            self.__setup_server()
            self.__setup_database()
            self.__setup_client()

        # Run tests
        print(header("Running Tests...", top='=', bottom='='))
        result = self.__run_tests(all_tests)

        # Parse results
        if self.config.mode == "benchmark":
            print(header("Parsing Results ...", top='=', bottom='='))
            self.results.parse(all_tests)

        self.results.set_completion_time()
        self.results.upload()
        self.results.finish()
        return result

    def generate_url(self, url, port):
        '''
        Generates a fully qualified URL for accessing a test url
        '''
        return self.config.server_host + ":" + str(port) + url

    ##########################################################################################
    # Private methods
    ##########################################################################################

    def __setup_server(self):
        '''
        Makes any necessary changes to the server that should be
        made before running the tests. This involves setting kernal
        settings to allow for more connections, or more file
        descriptiors
        
        http://redmine.lighttpd.net/projects/weighttp/wiki#Troubleshooting
        '''
        try:
            if os.name == 'nt':
                return True
            subprocess.call(
                ['sudo', 'sysctl', '-w', 'net.ipv4.tcp_max_syn_backlog=65535'],
                stdout=self.config.quiet_out,
                stderr=subprocess.STDOUT)
            subprocess.call(
                ['sudo', 'sysctl', '-w', 'net.core.somaxconn=65535'],
                stdout=self.config.quiet_out,
                stderr=subprocess.STDOUT)
            subprocess.call(
                ['sudo', 'sysctl', 'net.ipv4.tcp_tw_reuse=1'],
                stdout=self.config.quiet_out,
                stderr=subprocess.STDOUT)
            subprocess.call(
                ['sudo', 'sysctl', 'net.ipv4.tcp_tw_recycle=1'],
                stdout=self.config.quiet_out,
                stderr=subprocess.STDOUT)
            subprocess.call(
                ['sudo', 'sysctl', '-w', 'kernel.shmmax=134217728'],
                stdout=self.config.quiet_out,
                stderr=subprocess.STDOUT)
            subprocess.call(
                ['sudo', 'sysctl', '-w', 'kernel.shmall=2097152'],
                stdout=self.config.quiet_out,
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
        p = subprocess.Popen(
            self.config.database_ssh_string,
            stdin=subprocess.PIPE,
            shell=True,
            stdout=self.config.quiet_out,
            stderr=subprocess.STDOUT)
        p.communicate("""
            sudo sysctl -w net.ipv4.tcp_max_syn_backlog=65535
            sudo sysctl -w net.core.somaxconn=65535
            sudo sysctl -w kernel.sched_autogroup_enabled=0
            sudo -s ulimit -n 65535
            sudo sysctl net.ipv4.tcp_tw_reuse=1
            sudo sysctl net.ipv4.tcp_tw_recycle=1
            sudo sysctl -w kernel.shmmax=2147483648
            sudo sysctl -w kernel.shmall=2097152
            sudo sysctl -w kernel.sem="250 32000 256 512"
        """)
        # TODO - print kernel configuration to file
        # echo "Printing kernel configuration:" && sudo sysctl -a

    def __setup_database_container(self, database):
        '''
        Sets up a container for the given database and port, and starts said docker 
        container.
        '''

        def __is_hex(s):
            try:
                int(s, 16)
            except ValueError:
                return False
            return len(s) % 2 == 0

        p = subprocess.Popen(
            self.config.database_ssh_string,
            stdin=subprocess.PIPE,
            shell=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT)
        out = p.communicate("docker images  -q %s" % database)[0]
        dbid = ''
        if len(out.splitlines()) > 0:
            dbid = out.splitlines()[len(out.splitlines()) - 1]

        # If the database image exists, then dbid will look like
        # fe12ca519b47, and we do not want to rebuild if it exists
        if len(dbid) != 12 and not __is_hex(dbid):

            def __scp_string(files):
                scpstr = ["scp", "-i", self.config.database_identity_file]
                for file in files:
                    scpstr.append(file)
                scpstr.append("%s@%s:~/%s/" %
                              (self.config.database_user,
                               self.config.database_host, database))
                return scpstr

            p = subprocess.Popen(
                self.config.database_ssh_string,
                shell=True,
                stdin=subprocess.PIPE,
                stdout=self.config.quiet_out,
                stderr=subprocess.STDOUT)
            p.communicate("mkdir -p %s" % database)
            dbpath = os.path.join(self.config.fwroot, "toolset", "setup",
                                  "linux", "docker", "databases", database)
            dbfiles = ""
            for dbfile in os.listdir(dbpath):
                dbfiles += "%s " % os.path.join(dbpath, dbfile)
            p = subprocess.Popen(
                __scp_string(dbfiles.split()),
                stdin=subprocess.PIPE,
                stdout=self.config.quiet_out,
                stderr=subprocess.STDOUT)
            p.communicate()
            p = subprocess.Popen(
                self.config.database_ssh_string,
                shell=True,
                stdin=subprocess.PIPE,
                stdout=self.config.quiet_out,
                stderr=subprocess.STDOUT)
            p.communicate("docker build -f ~/%s/%s.dockerfile -t %s ~/%s" %
                          (database, database, database, database))
            if p.returncode != 0:
                return None

        p = subprocess.Popen(
            self.config.database_ssh_string,
            stdin=subprocess.PIPE,
            shell=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT)
        out = p.communicate(
            "docker run -d --rm --network=host %s" % database)[0]
        return out.splitlines()[len(out.splitlines()) - 1]

    def __setup_client(self):
        '''
        Makes any necessary changes to the client machine that should be made 
        before running the tests. Is very similar to the server setup, but may also 
        include client specific changes.
        '''
        p = subprocess.Popen(
            self.config.client_ssh_string,
            stdin=subprocess.PIPE,
            shell=True,
            stdout=self.config.quiet_out,
            stderr=subprocess.STDOUT)
        p.communicate("""
            sudo sysctl -w net.ipv4.tcp_max_syn_backlog=65535
            sudo sysctl -w net.core.somaxconn=65535
            sudo -s ulimit -n 65535
            sudo sysctl net.ipv4.tcp_tw_reuse=1
            sudo sysctl net.ipv4.tcp_tw_recycle=1
            sudo sysctl -w kernel.shmmax=2147483648
            sudo sysctl -w kernel.shmall=2097152
        """)

    def __run_tests(self, tests):
        '''
        Calls each test passed in tests to __run_test in a separate process. 
        Each test is given a set amount of time and if kills the child process 
        (and subsequently all of its child processes).
        '''
        if len(tests) == 0:
            return 0

        logging.debug("Start __run_tests.")
        logging.debug("__name__ = %s", __name__)

        error_happened = False
        if self.config.os.lower() == 'windows':
            logging.debug("Executing __run_tests on Windows")
            for test in tests:
                with open(self.config.current_benchmark,
                          'w') as benchmark_resume_file:
                    benchmark_resume_file.write(test.name)
                with self.config.quiet_out.enable():
                    if self.__run_test(test) != 0:
                        error_happened = True
        else:
            logging.debug("Executing __run_tests on Linux")

            # These features do not work on Windows
            for test in tests:
                print(header("Running Test: %s" % test.name))
                with open(self.config.current_benchmark,
                          'w') as benchmark_resume_file:
                    benchmark_resume_file.write(test.name)
                with self.config.quiet_out.enable():
                    test_process = Process(
                        target=self.__run_test,
                        name="Test Runner (%s)" % test.name,
                        args=(test, ))
                    test_process.start()
                    test_process.join(self.config.run_test_timeout_seconds)
                self.results.load(
                )  # Load intermediate result from child process
                if (test_process.is_alive()):
                    logging.debug(
                        "Child process for {name} is still alive. Terminating.".
                        format(name=test.name))
                    self.results.write_intermediate(
                        test.name, "__run_test timeout (=" +
                        str(self.config.run_test_timeout_seconds) +
                        " seconds)")
                    test_process.terminate()
                    test_process.join()
                if test_process.exitcode != 0:
                    error_happened = True

        if os.path.isfile(self.config.current_benchmark):
            os.remove(self.config.current_benchmark)
        logging.debug("End __run_tests.")

        if error_happened:
            return 1
        return 0

    def __run_test(self, test):
        '''
        Ensures that the system has all necessary software to run the tests. 
        This does not include that software for the individual test, but covers 
        software such as curl and weighttp that are needed.
        '''
        logDir = os.path.join(self.results.directory, test.name.lower())
        try:
            os.makedirs(logDir)
        except Exception:
            pass
        with open(os.path.join(logDir, 'out.txt'), 'w') as out:

            if test.os.lower() != self.config.os.lower(
            ) or test.database_os.lower() != self.config.database_os.lower():
                out.write(
                    "OS or Database OS specified in benchmark_config.json does not match the current environment. Skipping.\n"
                )
                return sys.exit(0)

            # If the test is in the excludes list, we skip it
            if self.config.exclude != None and test.name in self.config.exclude:
                out.write(
                    "Test {name} has been added to the excludes list. Skipping.\n".
                    format(name=test.name))
                return sys.exit(0)

            out.write(
                "test.os.lower() = {os}  test.database_os.lower() = {dbos}\n".
                format(os=test.os.lower(), dbos=test.database_os.lower()))
            out.write("self.results.frameworks != None: {val}\n".format(
                val=str(self.results.frameworks != None)))
            out.write("test.name: {name}\n".format(name=str(test.name)))
            out.write("self.results.completed.: {completed}\n".format(
                completed=str(self.results.completed)))
            if self.results.frameworks != None and test.name in self.results.completed:
                out.write(
                    'Framework {name} found in latest saved data. Skipping.\n'.
                    format(name=str(test.name)))
                print(
                    'WARNING: Test {test} exists in the results directory; this must be removed before running a new test.\n'.
                    format(test=str(test.name)))
                return sys.exit(1)
            out.flush()

            out.write(header("Beginning %s" % test.name, top='='))
            out.flush()

            ##########################
            # Start this test
            ##########################
            out.write(header("Starting %s" % test.name))
            out.flush()
            database_container_id = None
            try:
                if self.__is_port_bound(test.port):
                    time.sleep(60)

                if self.__is_port_bound(test.port):
                    # We gave it our all
                    self.results.write_intermediate(test.name, "port " + str(
                        test.port) + " is not available before start")
                    out.write(
                        header(
                            "Error: Port %s is not available, cannot start %s"
                            % (test.port, test.name)))
                    out.flush()
                    print("Error: Unable to recover port, cannot start test")
                    return sys.exit(1)

                ##########################
                # Start database container
                ##########################
                if test.database != "None":
                    database_container_id = self.__setup_database_container(
                        test.database.lower())
                    if not database_container_id:
                        out.write(
                            "ERROR: Problem building/running database container"
                        )
                        out.flush()
                        self.results.write_intermediate(
                            test.name, "ERROR: Problem starting")
                        return sys.exit(1)

                ##########################
                # Start webapp
                ##########################
                result = test.start(out)
                if result != 0:
                    self.__stop_test(database_container_id, test, out)
                    time.sleep(5)
                    out.write("ERROR: Problem starting {name}\n".format(
                        name=test.name))
                    out.flush()
                    self.results.write_intermediate(test.name,
                                                    "ERROR: Problem starting")
                    return sys.exit(1)

                logging.info("Sleeping %s seconds to ensure framework is ready"
                             % self.config.sleep)
                time.sleep(self.config.sleep)

                ##########################
                # Verify URLs
                ##########################
                if self.config.mode == "debug":
                    logging.info(
                        "Entering debug mode. Server has started. CTRL-c to stop."
                    )
                    while True:
                        time.sleep(1)
                else:
                    logging.info("Verifying framework URLs")
                    passed_verify = test.verify_urls(logDir)

                ##########################
                # Benchmark this test
                ##########################
                if self.config.mode == "benchmark":
                    logging.info("Benchmarking")
                    out.write(header("Benchmarking %s" % test.name))
                    out.flush()
                    test.benchmark(logDir)

                ##########################
                # Stop this test
                ##########################
                self.__stop_test(database_container_id, test, out)
                self.__stop_database(database_container_id, out)

                out.write(header("Stopped %s" % test.name))
                out.flush()

                ##########################################################
                # Remove contents of  /tmp folder
                ##########################################################
                try:
                    subprocess.check_call(
                        'sudo rm -rf /tmp/*',
                        shell=True,
                        stderr=out,
                        stdout=out)
                except Exception:
                    out.write(header("Error: Could not empty /tmp"))

                ##########################################################
                # Remove apt sources to avoid pkg errors and collisions
                ##########################################################
                os.system("sudo rm -rf /etc/apt/sources.list.d/*")

                ##########################################################
                # Save results thus far into the latest results directory
                ##########################################################

                out.write(header("Saving results through %s" % test.name))
                out.flush()
                self.results.write_intermediate(test.name,
                                                time.strftime(
                                                    "%Y%m%d%H%M%S",
                                                    time.localtime()))

                ##########################################################
                # Upload the results thus far to another server (optional)
                ##########################################################

                self.results.upload()

                if self.config.mode == "verify" and not passed_verify:
                    print("Failed verify!")
                    return sys.exit(1)
            except KeyboardInterrupt:
                self.__stop_test(database_container_id, test, out)
                self.__stop_database(database_container_id, out)
            except (OSError, IOError, subprocess.CalledProcessError):
                self.results.write_intermediate(
                    test.name, "<setup.py> raised an exception")
                out.write(header("Subprocess Error %s" % test.name))
                traceback.print_exc(file=out)
                out.flush()
                out.close()
                return sys.exit(1)

            out.close()
            return sys.exit(0)

    def __stop_database(self, database_container_id, out):
        '''
        Attempts to stop the running database container.
        '''
        if database_container_id:
            p = subprocess.Popen(
                self.config.database_ssh_string,
                stdin=subprocess.PIPE,
                shell=True,
                stdout=self.config.quiet_out,
                stderr=subprocess.STDOUT)
            p.communicate("docker stop %s" % database_container_id)

    def __stop_test(self, database_container_id, test, out):
        '''
        Attempts to stop the running test container.
        '''
        client = docker.from_env()
        # Stop all the containers
        for container in client.containers.list():
            if container.status == "running" and container.id != database_container_id:
                container.stop()
        # Remove only the tfb/test image for this test
        client.images.remove("tfb/test/%s" % test.name, force=True)
        client.images.prune()

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
