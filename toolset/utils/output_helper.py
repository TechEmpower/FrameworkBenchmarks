import os, sys
from contextlib import contextmanager


def header(message, top='-', bottom='-'):
    '''
    Generates a clean header
    '''
    topheader = (top * 80)[:80]
    bottomheader = (bottom * 80)[:80]
    result = ""
    if topheader != "":
        result += "%s" % topheader
    if message != "":
        if result == "":
            result = "  %s" % message
        else:
            result += "%s  %s" % (os.linesep, message)
    if bottomheader != "":
        if result == "":
            result = "%s" % bottomheader
        else:
            result += "%s%s" % (os.linesep, bottomheader)
    return result + os.linesep


class QuietOutputStream:
    def __init__(self, is_quiet):
        self.is_quiet = is_quiet
        self.null_out = open(os.devnull, 'w')

    def fileno(self):
        with self.enable():
            return sys.stdout.fileno()

    def write(self, message):
        with self.enable():
            sys.stdout.write(message)

    @contextmanager
    def enable(self):
        if self.is_quiet:
            old_out = sys.stdout
            old_err = sys.stderr
            try:
                sys.stdout = self.null_out
                sys.stderr = self.null_out
                yield
            finally:
                sys.stdout = old_out
                sys.stderr = old_err
        else:
            yield
