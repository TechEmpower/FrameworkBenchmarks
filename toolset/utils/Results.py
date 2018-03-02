from toolset.utils.metadata_helper import gather_remaining_tests, gather_frameworks
from toolset.utils.output_helper import header

import os
import logging
import subprocess
import uuid
import time
import json
import requests
import threading
from datetime import datetime

# Cross-platform colored text
from colorama import Fore, Back, Style


class Results:
    def __init__(self, config):
        '''
        Constructor
        '''
        self.config = config
        self.directory = os.path.join(self.config.fwroot, "results",
                                      self.config.timestamp)
        try:
            os.makedirs(self.directory)
        except OSError:
            pass
        self.file = os.path.join(self.directory, "results.json")

        try:
            with open(os.path.join(self.directory, 'results.json'), 'r') as f:
                # Load json file into results object
                # TODO: fix this
                self.results = json.load(f)
        except IOError:
            logging.warn("results.json for test not found.")

        self.uuid = str(uuid.uuid4())
        self.name = datetime.now().strftime(self.config.results_name)
        self.environmentDescription = self.config.results_environment
        try:
            self.git = dict()
            self.git['commitId'] = self.__get_git_commit_id()
            self.git['repositoryUrl'] = self.__get_git_repository_url()
            self.git['branchName'] = self.__get_git_branch_name()
        except Exception as e:
            logging.debug(
                'Could not read local git repository, which is fine. The error was: %s',
                e)
            self.git = None
        self.startTime = int(round(time.time() * 1000))
        self.completionTime = None
        self.concurrencyLevels = self.config.concurrency_levels
        self.pipelineConcurrencyLevels = self.config.pipeline_concurrency_levels
        self.queryIntervals = self.config.query_levels
        self.cachedQueryIntervals = self.config.cached_query_levels
        self.frameworks = [
            t.name for t in gather_remaining_tests(self.config, self)
        ]
        self.duration = self.config.duration
        self.rawData = dict()
        self.rawData['json'] = dict()
        self.rawData['db'] = dict()
        self.rawData['query'] = dict()
        self.rawData['fortune'] = dict()
        self.rawData['update'] = dict()
        self.rawData['plaintext'] = dict()
        self.rawData['cached_query'] = dict()
        self.completed = dict()
        self.succeeded = dict()
        self.succeeded['json'] = []
        self.succeeded['db'] = []
        self.succeeded['query'] = []
        self.succeeded['fortune'] = []
        self.succeeded['update'] = []
        self.succeeded['plaintext'] = []
        self.succeeded['cached_query'] = []
        self.failed = dict()
        self.failed['json'] = []
        self.failed['db'] = []
        self.failed['query'] = []
        self.failed['fortune'] = []
        self.failed['update'] = []
        self.failed['plaintext'] = []
        self.failed['cached_query'] = []
        self.verify = dict()

    #############################################################################
    # PUBLIC FUNCTIONS
    #############################################################################

    def parse(self, tests):
        '''
        Ensures that the system has all necessary software to run
        the tests. This does not include that software for the individual
        test, but covers software such as curl and weighttp that
        are needed.
        '''
        # Run the method to get the commmit count of each framework.
        self.__count_commits()
        # Call the method which counts the sloc for each framework
        self.__count_sloc()

        # Time to create parsed files
        # Aggregate JSON file
        with open(self.file, "w") as f:
            f.write(json.dumps(self.results, indent=2))

    def write_intermediate(self, test_name, status_message):
        '''
        Writes the intermediate results for the given test_name and status_message
        '''
        self.completed[test_name] = status_message
        self.__write_results()

    def set_completion_time(self):
        '''
        Sets the completionTime for these results and writes the results
        '''
        self.completionTime = int(round(time.time() * 1000))
        self.__write_results()

    def upload(self):
        '''
        Attempts to upload the results.json to the configured results_upload_uri
        '''
        if self.config.results_upload_uri != None:
            try:
                requests.post(
                    self.config.results_upload_uri,
                    headers={'Content-Type': 'application/json'},
                    data=json.dumps(self, indent=2))
            except (Exception):
                logging.error("Error uploading results.json")

    def load(self):
        '''
        Load the results.json file
        '''
        try:
            with open(self.file) as f:
                self.__dict__.update(json.load(f))
        except (ValueError, IOError):
            pass

    def get_output_file(self, test_name, test_type):
        '''
        Returns the output file for this test_name and test_type
        Example: fwroot/results/timestamp/test_type/test_name/raw.txt
        '''
        path = os.path.join(self.directory, self.config.timestamp, test_name,
                            test_type, "raw.txt")
        try:
            os.makedirs(os.path.dirname(path))
        except OSError:
            pass
        return path

    def get_stats_file(self, test_name, test_type):
        '''
        Returns the stats file name for this test_name and
        Example: fwroot/results/timestamp/test_type/test_name/stats.txt
        '''
        path = os.path.join(self.directory, self.config.timestamp, test_name,
                            test_type, "stats.txt")
        try:
            os.makedirs(os.path.dirname(path))
        except OSError:
            pass
        return path

    def report_verify_results(self, framework, test, result):
        '''
        Used by FrameworkTest to add verification details to our results
        
        TODO: Technically this is an IPC violation - we are accessing
        the parent process' memory from the child process
        '''
        if framework.name not in self.verify.keys():
            self.verify[framework.name] = dict()
        self.verify[framework.name][test] = result

    def report_benchmark_results(self, framework, test, results):
        '''
        Used by FrameworkTest to add benchmark data to this
        
        TODO: Technically this is an IPC violation - we are accessing
        the parent process' memory from the child process
        '''
        if test not in self.rawData.keys():
            self.rawData[test] = dict()

        # If results has a size from the parse, then it succeeded.
        if results:
            self.rawData[test][framework.name] = results

            # This may already be set for single-tests
            if framework.name not in self.succeeded[test]:
                self.succeeded[test].append(framework.name)
        else:
            # This may already be set for single-tests
            if framework.name not in self.failed[test]:
                self.failed[test].append(framework.name)

    def finish(self):
        '''
        Finishes these results.
        '''
        if not self.config.parse:
            tests = gather_remaining_tests(self.config, self)
            # Normally you don't have to use Fore.BLUE before each line, but
            # Travis-CI seems to reset color codes on newline (see travis-ci/travis-ci#2692)
            # or stream flush, so we have to ensure that the color code is printed repeatedly
            prefix = Fore.CYAN
            for line in header(
                    "Verification Summary", top='=', bottom='').split('\n'):
                print(prefix + line)
            for test in tests:
                print(prefix + "| Test: {!s}".format(test.name))
                if test.name in self.verify.keys():
                    for test_type, result in self.verify[
                            test.name].iteritems():
                        if result.upper() == "PASS":
                            color = Fore.GREEN
                        elif result.upper() == "WARN":
                            color = Fore.YELLOW
                        else:
                            color = Fore.RED
                        print(prefix + "|       " + test_type.ljust(13) +
                              ' : ' + color + result.upper())
                else:
                    print(prefix + "|      " + Fore.RED +
                          "NO RESULTS (Did framework launch?)")
            print(prefix + header('', top='', bottom='=') + Style.RESET_ALL)

        print("Time to complete: " +
              str(int(time.time() - self.config.start_time)) + " seconds")
        print("Results are saved in " + self.directory)

    #############################################################################
    # PRIVATE FUNCTIONS
    #############################################################################

    def __to_jsonable(self):
        '''
        Returns a dict suitable for jsonification
        '''
        toRet = dict()

        toRet['uuid'] = self.uuid
        toRet['name'] = self.name
        toRet['environmentDescription'] = self.environmentDescription
        toRet['git'] = self.git
        toRet['startTime'] = self.startTime
        toRet['completionTime'] = self.completionTime
        toRet['concurrencyLevels'] = self.concurrencyLevels
        toRet['pipelineConcurrencyLevels'] = self.pipelineConcurrencyLevels
        toRet['queryIntervals'] = self.queryIntervals
        toRet['cachedQueryIntervals'] = self.cachedQueryIntervals
        toRet['frameworks'] = self.frameworks
        toRet['duration'] = self.duration
        toRet['rawData'] = self.rawData
        toRet['completed'] = self.completed
        toRet['succeeded'] = self.succeeded
        toRet['failed'] = self.failed
        toRet['verify'] = self.verify

        return toRet

    def __write_results(self):
        try:
            with open(self.file, 'w') as f:
                f.write(json.dumps(self.__to_jsonable(), indent=2))
        except (IOError):
            logging.error("Error writing results.json")

    def __count_sloc(self):
        '''
        Counts the significant lines of code for all tests and stores in results.
        '''
        frameworks = gather_frameworks(self.config.test, self.config.exclude,
                                       self.config)

        jsonResult = {}
        for framework, testlist in frameworks.items():
            if not os.path.exists(
                    os.path.join(testlist[0].directory, "source_code")):
                logging.warn(
                    "Cannot count lines of code for %s - no 'source_code' file",
                    framework)
                continue

            # Unfortunately the source_code files use lines like
            # ./cpoll_cppsp/www/fortune_old instead of
            # ./www/fortune_old
            # so we have to back our working dir up one level
            wd = os.path.dirname(testlist[0].directory)

            try:
                command = "cloc --list-file=%s/source_code --yaml" % testlist[
                    0].directory

                if os.path.exists(
                        os.path.join(testlist[0].directory, "cloc_defs.txt")):
                    command += " --read-lang-def %s" % os.path.join(
                        testlist[0].directory, "cloc_defs.txt")
                    logging.info("Using custom cloc definitions for %s",
                                 framework)

                # Find the last instance of the word 'code' in the yaml output. This should
                # be the line count for the sum of all listed files or just the line count
                # for the last file in the case where there's only one file listed.
                command = command + "| grep code | tail -1 | cut -d: -f 2"
                logging.debug("Running \"%s\" (cwd=%s)", command, wd)
                lineCount = subprocess.check_output(
                    command, cwd=wd, shell=True)
                jsonResult[framework] = int(lineCount)
            except subprocess.CalledProcessError:
                continue
            except ValueError as ve:
                logging.warn(
                    "Unable to get linecount for %s due to error '%s'",
                    framework, ve)
        self.rawData['slocCounts'] = jsonResult

    def __count_commits(self):
        frameworks = gather_frameworks(self.config.test, self.config.exclude,
                                       self.config)

        def count_commit(directory, jsonResult):
            command = "git rev-list HEAD -- " + directory + " | sort -u | wc -l"
            try:
                commitCount = subprocess.check_output(command, shell=True)
                jsonResult[framework] = int(commitCount)
            except subprocess.CalledProcessError:
                pass

        # Because git can be slow when run in large batches, this
        # calls git up to 4 times in parallel. Normal improvement is ~3-4x
        # in my trials, or ~100 seconds down to ~25
        # This is safe to parallelize as long as each thread only
        # accesses one key in the dictionary
        threads = []
        jsonResult = {}
        # t1 = datetime.now()
        for framework, testlist in frameworks.items():
            directory = testlist[0].directory
            t = threading.Thread(
                target=count_commit, args=(directory, jsonResult))
            t.start()
            threads.append(t)
            # Git has internal locks, full parallel will just cause contention
            # and slowness, so we rate-limit a bit
            if len(threads) >= 4:
                threads[0].join()
                threads.remove(threads[0])

        # Wait for remaining threads
        for t in threads:
            t.join()
        # t2 = datetime.now()
        # print "Took %s seconds " % (t2 - t1).seconds

        self.rawData['commitCounts'] = jsonResult
        self.config.commits = jsonResult

    def __get_git_commit_id(self):
        '''
        Get the git commit id for this benchmark
        '''
        return subprocess.check_output(["git", "rev-parse", "HEAD"]).strip()

    def __get_git_repository_url(self):
        '''
        Gets the git repository url for this benchmark
        '''
        return subprocess.check_output(
            ["git", "config", "--get", "remote.origin.url"]).strip()

    def __get_git_branch_name(self):
        '''
        Gets the git branch name for this benchmark
        '''
        return subprocess.check_output(
            'git rev-parse --abbrev-ref HEAD', shell=True).strip()
