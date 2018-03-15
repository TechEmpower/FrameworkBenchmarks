import os, sys
from contextlib import contextmanager

# Enable cross-platform colored output
from colorama import init


def header(message,
           top='-',
           bottom='-',
           log_file=None,
           quiet=False,
           color=None):
    '''
    Generates a clean header with the given message and top/bottom text breaks.
    Optionally, a log file can be provided to log this header to, as well.
    '''
    topheader = (top * 80)[:80]
    bottomheader = (bottom * 80)[:80]
    result = ""
    if topheader != "":
        if color is not None:
            result += color
        result += "%s" % topheader
    if message != "":
        if color is not None:
            result += color

        if result == "":
            result = "  %s" % message
        else:
            result += "%s  %s" % (os.linesep, message)
    if bottomheader != "":
        if color is not None:
            result += color
        result += "%s%s" % (os.linesep, bottomheader)
    log(result + os.linesep, None, log_file, True, quiet)


def log(log_text=None,
        prefix=None,
        log_file=None,
        allow_newlines=False,
        quiet=False):
    '''
    Logs the given text and optional prefix to stdout (if quiet is False) and
    to an optional log file. By default, we strip out newlines in order to 
    print our lines correctly, but you can override this functionality if you
    want to print multi-line output.
    '''
    if not allow_newlines:
        log_text = log_text.splitlines()[0] + os.linesep

    if not log_text:
        return

    if not allow_newlines and log_text.splitlines()[0] is '':
        return

    if not quiet:
        init()
        if prefix is not None:
            sys.stdout.write(prefix + log_text)
        else:
            sys.stdout.write(log_text)
        sys.stdout.flush()

    if log_file is not None:
        init(strip=True)
        log_file.write(log_text)
        log_file.flush()


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
