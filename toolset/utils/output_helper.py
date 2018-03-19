# -*- coding: utf-8 -*-
import os, sys, re
from contextlib import contextmanager

# RegExp for stripping color codes
seq = re.compile(r'\x1B\[\d+m')

FNULL = open(os.devnull, 'w')


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
    if not log_text:
        return

    if not allow_newlines:
        log_text = log_text.splitlines()[0] + os.linesep

        if log_text.splitlines()[0].strip() is '':
            return

    try:
        if not quiet:
            if prefix is not None:
                sys.stdout.write(prefix + log_text)
            else:
                sys.stdout.write(log_text)
            sys.stdout.flush()

        if log_file is not None:
            log_file.write(seq.sub('', log_text))
            log_file.flush()
    except:
        pass


def log_error(exception=None, prefix=None, log_file=None, quiet=False):
    '''
    Logs the given exception
    '''
    log(exception + os.linesep, prefix, log_file, True, quiet)


class QuietOutputStream:
    '''
    Provides an output stream which either writes to stdout or nothing 
    depending on the is_quiet param.
    '''

    def __init__(self, is_quiet):
        self.is_quiet = is_quiet

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
                sys.stdout = FNULL
                sys.stderr = FNULL
                yield
            finally:
                sys.stdout = old_out
                sys.stderr = old_err
        else:
            yield
