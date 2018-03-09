import os
import subprocess
import sys
import traceback
import logging
from requests import ConnectionError

from toolset.utils.output_helper import header
from toolset.utils import docker_helper

# Cross-platform colored text
from colorama import Fore, Style


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
        Start the test implementation
        '''
        test_docker_files = ["%s.dockerfile" % self.name]
        if self.docker_files is not None:
            if type(self.docker_files) is list:
                test_docker_files.extend(self.docker_files)
            else:
                raise Exception(
                    "docker_files in benchmark_config.json must be an array")

        result = docker_helper.build(self.benchmarker_config, [self.name], out)
        if result != 0:
            return result

        return docker_helper.run(self.benchmarker_config, test_docker_files, out)

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
                                "ssh", self.benchmarker_config.client_host,
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