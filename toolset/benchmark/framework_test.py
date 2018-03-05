from toolset.benchmark.fortune_html_parser import FortuneHTMLParser
from toolset.setup.linux import setup_util

import importlib
import os
import subprocess
import socket
import time
from pprint import pprint
import sys
import traceback
import json
import logging
import multiprocessing
import docker
from collections import OrderedDict
from requests import ConnectionError
from threading import Thread
from threading import Event

from toolset.utils.output_helper import header
from toolset.utils.docker_helper import gather_docker_dependencies
from toolset.utils.docker_helper import find_docker_file

# Cross-platform colored text
from colorama import Fore, Back, Style
from datetime import datetime
from datetime import timedelta


class FrameworkTest:
    def __init__(self, name, directory, benchmarker_config, results, runTests,
                 args):
        '''
        Constructor
        '''
        self.name = name
        self.directory = directory
        self.benchmarker_config = benchmarker_config
        self.results = results
        self.runTests = runTests
        self.fwroot = benchmarker_config.fwroot
        self.approach = ""
        self.classification = ""
        self.database = ""
        self.framework = ""
        self.language = ""
        self.orm = ""
        self.platform = ""
        self.webserver = ""
        self.os = ""
        self.database_os = ""
        self.display_name = ""
        self.notes = ""
        self.port = ""
        self.versus = ""
        self.docker_files = None

        # setup logging
        logging.basicConfig(stream=sys.stderr, level=logging.INFO)

        # Used in setup.sh scripts for consistency with
        # the bash environment variables
        self.troot = self.directory

        self.__dict__.update(args)

    ##########################################################################################
    # Public Methods
    ##########################################################################################

    def start(self, out):
        '''
        Start the test using its setup file
        '''

        def tee_output(prefix, line):
            # Needs to be one atomic write
            # Explicitly use UTF-8 as it's the most common framework output
            # TODO improve encoding handling
            line = prefix.encode('utf-8') + line

            # Log to current terminal
            sys.stdout.write(line)
            sys.stdout.flush()

            out.write(line)
            out.flush()

        prefix = "Setup %s: " % self.name

        # Build the test docker file based on the test name
        # then build any additional docker files specified in the benchmark_config
        # Note - If you want to be able to stream the output of the build process you have
        # to use the low level API:
        #  https://docker-py.readthedocs.io/en/stable/api.html#module-docker.api.build

        prev_line = os.linesep

        def handle_build_output(line):
            if line.startswith('{"stream":'):
                line = json.loads(line)
                line = line[line.keys()[0]].encode('utf-8')
                if prev_line.endswith(os.linesep):
                    tee_output(prefix, line)
                else:
                    tee_output("", line)
                self.prev_line = line

        docker_buildargs = {
            'CPU_COUNT': str(multiprocessing.cpu_count()),
            'MAX_CONCURRENCY': str(
                max(self.benchmarker_config.concurrency_levels))
        }

        test_docker_files = ["%s.dockerfile" % self.name]
        if self.docker_files is not None:
            if type(self.docker_files) is list:
                test_docker_files.extend(self.docker_files)
            else:
                raise Exception(
                    "docker_files in benchmark_config.json must be an array")

        for test_docker_file in test_docker_files:
            deps = list(
                reversed(
                    gather_docker_dependencies(
                        os.path.join(self.directory, test_docker_file))))

            docker_dir = os.path.join(setup_util.get_fwroot(), "toolset",
                                      "setup", "linux", "docker")

            for dependency in deps:
                docker_file = os.path.join(self.directory,
                                           dependency + ".dockerfile")
                if not docker_file or not os.path.exists(docker_file):
                    docker_file = find_docker_file(docker_dir,
                                                   dependency + ".dockerfile")
                if not docker_file:
                    tee_output(
                        prefix,
                        "Docker build failed; %s could not be found; terminating\n"
                        % (dependency + ".dockerfile"))
                    return 1

                # Build the dependency image
                try:
                    for line in docker.APIClient(
                            base_url='unix://var/run/docker.sock').build(
                                path=os.path.dirname(docker_file),
                                dockerfile="%s.dockerfile" % dependency,
                                tag="tfb/%s" % dependency,
                                buildargs=docker_buildargs,
                                forcerm=True):
                        handle_build_output(line)
                except Exception as e:
                    tee_output(prefix,
                               "Docker dependency build failed; terminating\n")
                    print(e)
                    return 1

        # Build the test images
        for test_docker_file in test_docker_files:
            try:
                for line in docker.APIClient(
                        base_url='unix://var/run/docker.sock').build(
                            path=self.directory,
                            dockerfile=test_docker_file,
                            tag="tfb/test/%s" % test_docker_file.replace(
                                ".dockerfile", ""),
                            buildargs=docker_buildargs,
                            forcerm=True):
                    handle_build_output(line)
            except Exception as e:
                tee_output(prefix, "Docker build failed; terminating\n")
                print(e)
                return 1

        # Run the Docker container
        client = docker.from_env()

        for test_docker_file in test_docker_files:
            try:

                def watch_container(container, prefix):
                    for line in container.logs(stream=True):
                        tee_output(prefix, line)

                container = client.containers.run(
                    "tfb/test/%s" % test_docker_file.replace(
                        ".dockerfile", ""),
                    network_mode="host",
                    privileged=True,
                    stderr=True,
                    detach=True)

                prefix = "Server %s: " % self.name
                watch_thread = Thread(
                    target=watch_container, args=(container, prefix))
                watch_thread.daemon = True
                watch_thread.start()

            except Exception as e:
                tee_output(
                    prefix,
                    "Running docker cointainer: %s failed" % test_docker_file)
                print(e)
                return 1

        return 0

    def verify_urls(self, logPath):
        '''
        Verifys each of the URLs for this test. THis will sinply curl the URL and 
        check for it's return status. For each url, a flag will be set on this 
        object for whether or not it passed.
        Returns True if all verifications succeeded
        '''
        result = True

        def verify_type(test_type):
            verificationPath = os.path.join(logPath, test_type)
            try:
                os.makedirs(verificationPath)
            except OSError:
                pass
            with open(os.path.join(verificationPath, 'verification.txt'),
                      'w') as verification:
                test = self.runTests[test_type]
                test.setup_out(verification)
                verification.write(header("VERIFYING %s" % test_type.upper()))

                base_url = "http://%s:%s" % (
                    self.benchmarker_config.server_host, self.port)

                try:
                    # Verifies headers from the server. This check is made from the
                    # App Server using Pythons requests module. Will do a second check from
                    # the client to make sure the server isn't only accepting connections
                    # from localhost on a multi-machine setup.
                    results = test.verify(base_url)

                    # Now verify that the url is reachable from the client machine, unless
                    # we're already failing
                    if not any(result == 'fail'
                               for (result, reason, url) in results):
                        p = subprocess.call(
                            [
                                "ssh", "TFB-client",
                                "curl -sSf %s" % base_url + test.get_url()
                            ],
                            shell=False,
                            stdout=subprocess.PIPE,
                            stderr=subprocess.PIPE)
                        if p is not 0:
                            results = [(
                                'fail',
                                "Server did not respond to request from client machine.",
                                base_url)]
                            logging.warning(
                                """This error usually means your server is only accepting
                requests from localhost.""")
                except ConnectionError as e:
                    results = [('fail', "Server did not respond to request",
                                base_url)]
                    logging.warning(
                        "Verifying test %s for %s caused an exception: %s",
                        test_type, self.name, e)
                except Exception as e:
                    results = [('fail', """Caused Exception in TFB
            This almost certainly means your return value is incorrect,
            but also that you have found a bug. Please submit an issue
            including this message: %s\n%s""" % (e, traceback.format_exc()),
                                base_url)]
                    logging.warning(
                        "Verifying test %s for %s caused an exception: %s",
                        test_type, self.name, e)
                    traceback.format_exc()

                test.failed = any(
                    result == 'fail' for (result, reason, url) in results)
                test.warned = any(
                    result == 'warn' for (result, reason, url) in results)
                test.passed = all(
                    result == 'pass' for (result, reason, url) in results)

                def output_result(result, reason, url):
                    specific_rules_url = "http://frameworkbenchmarks.readthedocs.org/en/latest/Project-Information/Framework-Tests/#specific-test-requirements"
                    color = Fore.GREEN
                    if result.upper() == "WARN":
                        color = Fore.YELLOW
                    elif result.upper() == "FAIL":
                        color = Fore.RED

                    verification.write((
                        "   " + color + "%s" + Style.RESET_ALL + " for %s\n") %
                                       (result.upper(), url))
                    print("   {!s}{!s}{!s} for {!s}\n".format(
                        color, result.upper(), Style.RESET_ALL, url))
                    if reason is not None and len(reason) != 0:
                        for line in reason.splitlines():
                            verification.write("     " + line + '\n')
                            print("     " + line)
                        if not test.passed:
                            verification.write(
                                "     See %s\n" % specific_rules_url)
                            print("     See {!s}\n".format(specific_rules_url))

                [output_result(r1, r2, url) for (r1, r2, url) in results]

                if test.failed:
                    self.results.report_verify_results(self, test_type, 'fail')
                elif test.warned:
                    self.results.report_verify_results(self, test_type, 'warn')
                elif test.passed:
                    self.results.report_verify_results(self, test_type, 'pass')
                else:
                    raise Exception(
                        "Unknown error - test did not pass,warn,or fail")

                verification.flush()

        result = True
        for test_type in self.runTests:
            verify_type(test_type)
            if self.runTests[test_type].failed:
                result = False

        return result