import os
import traceback
from requests import ConnectionError, Timeout

from toolset.utils.output_helper import log

# Cross-platform colored text
from colorama import Fore, Style


class FrameworkTest:
    def __init__(self, name, directory, benchmarker, runTests,
                 args):
        '''
        Constructor
        '''
        self.name = name
        self.directory = directory
        self.benchmarker = benchmarker
        self.runTests = runTests
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

        self.__dict__.update(args)

    ##########################################################################################
    # Public Methods
    ##########################################################################################

    def start(self):
        '''
        Start the test implementation
        '''
        test_log_dir = os.path.join(self.benchmarker.results.directory, self.name.lower())
        build_log_dir = os.path.join(test_log_dir, 'build')
        run_log_dir = os.path.join(test_log_dir, 'run')

        try:
            os.makedirs(build_log_dir)
        except OSError:
            pass
        try:
            os.makedirs(run_log_dir)
        except OSError:
            pass

        result = self.benchmarker.docker_helper.build(self, build_log_dir)
        if result != 0:
            return None

        return self.benchmarker.docker_helper.run(self, run_log_dir)

    def is_accepting_requests(self):
        '''
        Determines whether this test implementation is up and accepting
        requests.
        '''
        test_type = None
        for any_type in self.runTests:
            test_type = any_type
            break

        url = "http://%s:%s%s" % (self.benchmarker.config.server_host,
                                  self.port,
                                  self.runTests[test_type].get_url())

        return self.benchmarker.docker_helper.test_client_connection(url)

    def verify_urls(self):
        '''
        Verifys each of the URLs for this test. This will simply curl the URL and
        check for it's return status. For each url, a flag will be set on this
        object for whether or not it passed.
        Returns True if all verifications succeeded
        '''
        log_path = os.path.join(self.benchmarker.results.directory, self.name.lower())
        result = True

        def verify_type(test_type):
            verificationPath = os.path.join(log_path, test_type)
            try:
                os.makedirs(verificationPath)
            except OSError:
                pass
            with open(os.path.join(verificationPath, 'verification.txt'),
                      'w') as verification:
                test = self.runTests[test_type]
                log("VERIFYING %s" % test_type.upper(),
                    file=verification,
                    border='-',
                    color=Fore.WHITE + Style.BRIGHT)

                base_url = "http://%s:%s" % (
                    self.benchmarker.config.server_host, self.port)

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
                        self.benchmarker.docker_helper.test_client_connection(
                            base_url + test.get_url())
                except ConnectionError as e:
                    results = [('fail', "Server did not respond to request",
                                base_url)]
                    log("Verifying test %s for %s caused an exception: %s" %
                        (test_type, self.name, e),
                        color=Fore.RED)
                except Timeout as e:
                    results = [('fail', "Connection to server timed out",
                                base_url)]
                    log("Verifying test %s for %s caused an exception: %s" %
                        (test_type, self.name, e),
                        color=Fore.RED)
                except Exception as e:
                    results = [('fail', """Caused Exception in TFB
            This almost certainly means your return value is incorrect,
            but also that you have found a bug. Please submit an issue
            including this message: %s\n%s""" % (e, traceback.format_exc()),
                                base_url)]
                    log("Verifying test %s for %s caused an exception: %s" %
                        (test_type, self.name, e),
                        color=Fore.RED)
                    traceback.format_exc()

                test.failed = any(
                    result == 'fail' for (result, reason, url) in results)
                test.warned = any(
                    result == 'warn' for (result, reason, url) in results)
                test.passed = all(
                    result == 'pass' for (result, reason, url) in results)

                def output_result(result, reason, url):
                    specific_rules_url = "https://github.com/TechEmpower/FrameworkBenchmarks/wiki/Project-Information-Framework-Tests-Overview#specific-test-requirements"
                    color = Fore.GREEN
                    if result.upper() == "WARN":
                        color = Fore.YELLOW
                    elif result.upper() == "FAIL":
                        color = Fore.RED

                    log("   {!s}{!s}{!s} for {!s}".format(
                        color, result.upper(), Style.RESET_ALL, url),
                        file=verification)
                    if reason is not None and len(reason) != 0:
                        for line in reason.splitlines():
                            log("     " + line, file=verification)
                        if not test.passed:
                            log("     See {!s}".format(specific_rules_url),
                                file=verification)

                [output_result(r1, r2, url) for (r1, r2, url) in results]

                if test.failed:
                    test.output_headers_and_body()
                    self.benchmarker.results.report_verify_results(self, test_type, 'fail')
                elif test.warned:
                    test.output_headers_and_body()
                    self.benchmarker.results.report_verify_results(self, test_type, 'warn')
                elif test.passed:
                    self.benchmarker.results.report_verify_results(self, test_type, 'pass')
                else:
                    raise Exception(
                        "Unknown error - test did not pass,warn,or fail")

        result = True
        for test_type in self.runTests:
            verify_type(test_type)
            if self.runTests[test_type].failed:
                result = False

        return result
