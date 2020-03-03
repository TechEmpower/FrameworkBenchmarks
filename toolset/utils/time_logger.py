import time
from colorama import Fore

from toolset.utils.output_helper import log


class TimeLogger:
    '''
    Class for keeping track of and logging execution times
    for suite actions
    '''

    def __init__(self):
        self.start = time.time()

        self.benchmarking_start = 0
        self.benchmarking_total = 0
        self.database_starting = 0
        self.database_started = 0
        self.build_start = 0
        self.build_total = 0
        self.test_started = 0
        self.accepting_requests = 0
        self.test_start = 0
        self.test_total = 0
        self.verify_start = 0
        self.verify_total = 0
        self.build_logs = []

    @staticmethod
    def output(sec):
        output = ""
        h = sec // 3600
        m = (sec // 60) % 60
        s = sec % 60
        if h > 0:
            output = "%sh" % h
        if m > 0:
            output = output + "%sm " % m
        output = output + "%ss" % s
        return output

    def mark_starting_database(self):
        self.database_starting = time.time()

    def mark_started_database(self):
        self.database_started = int(time.time() - self.database_starting)

    def log_database_start_time(self, log_prefix, file):
        log("Time starting database: %s" % TimeLogger.output(
            self.database_started),
            prefix=log_prefix,
            file=file,
            color=Fore.YELLOW)

    def mark_benchmarking_start(self):
        self.benchmarking_start = time.time()

    def log_benchmarking_end(self, log_prefix, file):
        total = int(time.time() - self.benchmarking_start)
        self.benchmarking_total = self.benchmarking_total + total
        log("Benchmarking time: %s" % TimeLogger.output(total),
            prefix=log_prefix,
            file=file,
            color=Fore.YELLOW)

    def mark_build_start(self):
        self.build_start = time.time()

    def time_since_start(self):
        return time.time() - self.build_start

    def log_build_end(self, log_prefix, file):
        total = int(time.time() - self.build_start)
        self.build_total = self.build_total + total
        log_str = "Build time: %s" % TimeLogger.output(total)
        self.build_logs.append({'log_prefix': log_prefix, 'str': log_str})
        log(log_str, prefix=log_prefix, file=file, color=Fore.YELLOW)

    def log_build_flush(self, file):
        for b_log in self.build_logs:
            log(b_log['str'],
                prefix=b_log['log_prefix'],
                file=file,
                color=Fore.YELLOW)
        self.build_logs = []

    def mark_test_starting(self):
        self.test_started = time.time()

    def mark_test_accepting_requests(self):
        self.accepting_requests = int(time.time() - self.test_started)

    def log_test_accepting_requests(self, log_prefix, file):
        log("Time until accepting requests: %s" % TimeLogger.output(
            self.accepting_requests),
            prefix=log_prefix,
            file=file,
            color=Fore.YELLOW)

    def mark_test_start(self):
        self.test_start = time.time()

    def log_test_end(self, log_prefix, file):
        total = int(time.time() - self.test_start)
        log("Total test time: %s" % TimeLogger.output(total),
            prefix=log_prefix,
            file=file,
            color=Fore.YELLOW)
        log("Total time building so far: %s" % TimeLogger.output(
            self.build_total),
            prefix="tfb: ",
            file=file,
            color=Fore.YELLOW)
        log("Total time verifying so far: %s" % TimeLogger.output(
            self.verify_total),
            prefix="tfb: ",
            file=file,
            color=Fore.YELLOW)
        if self.benchmarking_total > 0:
            log("Total time benchmarking so far: %s" % TimeLogger.output(
                self.benchmarking_total),
                prefix="tfb: ",
                file=file,
                color=Fore.YELLOW)
        running_time = int(time.time() - self.start)
        log("Total execution time so far: %s" %
            TimeLogger.output(running_time),
            prefix="tfb: ",
            file=file,
            color=Fore.YELLOW)

    def mark_verify_start(self):
        self.verify_start = time.time()

    def log_verify_end(self, log_prefix, file):
        total = int(time.time() - self.verify_start)
        self.verify_total = self.verify_total + total
        log("Verify time: %s" % TimeLogger.output(total),
            prefix=log_prefix,
            file=file,
            color=Fore.YELLOW)
