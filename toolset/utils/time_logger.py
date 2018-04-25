import time
from colorama import Fore, Style

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
        self.build_start = 0
        self.build_total = 0
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

    def log_benchmarking_start(self):
        self.benchmarking_start = time.time()

    def log_benchmarking_end(self, log_prefix, file):
        total = int(time.time() - self.benchmarking_start)
        self.benchmarking_total = self.benchmarking_total + total
        log("Total benchmarking time: %s" % TimeLogger.output(total),
            prefix=log_prefix,
            file=file,
            color=Fore.YELLOW)

    def log_build_start(self):
        self.build_start = time.time()

    def log_build_end(self, log_prefix, file):
        total = int(time.time() - self.build_start)
        self.build_total = self.build_total + total
        log_str = "Total build time: %s" % TimeLogger.output(total)
        self.build_logs.append({'log_prefix': log_prefix, 'str': log_str})
        log(log_str,
            prefix=log_prefix,
            file=file,
            color=Fore.YELLOW)

    def log_build_flush(self, file):
        for b_log in self.build_logs:
            log(b_log['str'],
                prefix=b_log['log_prefix'],
                file=file,
                color=Fore.YELLOW)
        self.build_logs = []

    def log_test_start(self):
        self.test_start = time.time()

    def log_test_end(self, log_prefix, file):
        total = int(time.time() - self.test_start)
        log("Total test time: %s" % TimeLogger.output(total),
            prefix=log_prefix,
            file=file,
            color=Fore.YELLOW)
        log("Total time building so far: %s"
            % TimeLogger.output(self.build_total),
            prefix="tfb: ",
            file=file,
            color=Fore.YELLOW)
        log("Total time verifying so far: %s"
            % TimeLogger.output(self.verify_total),
            prefix="tfb: ",
            file=file,
            color=Fore.YELLOW)
        if self.benchmarking_total > 0:
            log("Total time benchmarking so far: %s"
                % TimeLogger.output(self.benchmarking_total),
                prefix="tfb: ",
                file=file,
                color=Fore.YELLOW)
        running_time = int(time.time() - self.start)
        log("Total execution time so far: %s"
            % TimeLogger.output(running_time),
            prefix="tfb: ",
            file=file,
            color=Fore.YELLOW)

    def log_verify_start(self):
        self.verify_start = time.time()

    def log_verify_end(self, log_prefix, file):
        self.log_build_flush(file)
        total = int(time.time() - self.verify_start)
        self.verify_total = self.verify_total + total
        log("Total verify time: %s" % TimeLogger.output(total),
            prefix=log_prefix,
            file=file,
            color=Fore.YELLOW)
