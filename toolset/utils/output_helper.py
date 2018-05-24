# -*- coding: utf-8 -*-
import os, sys, re

from colorama import Style
from contextlib import contextmanager

# RegExp for stripping color codes
seq = re.compile(r'\x1B\[\d+m')

FNULL = open(os.devnull, 'w')

# To prevent the entire disk from being consumed, refuse to
# append more lines to a log file once it's grown too large.
# Logs that hit this limit are probably repeating the same
# message endlessly anyway.
TOO_MANY_BYTES = 50 * 1024 * 1024


def log(log_text=None, **kwargs):
    '''
    Logs the given text and optional prefix to stdout (if quiet is False) and
    to an optional log file. By default, we strip out newlines in order to 
    print our lines correctly, but you can override this functionality if you
    want to print multi-line output.
    '''

    # set up some defaults
    color = kwargs.get('color', '')
    color_reset = Style.RESET_ALL if color else ''
    prefix = kwargs.get('prefix', '')
    border = kwargs.get('border')
    border_bottom = kwargs.get('border_bottom')
    file = kwargs.get('file')
    quiet = kwargs.get('quiet')

    if border is not None:
        border = color + (border * 80) + os.linesep + color_reset
        border_bottom = border if border_bottom is None else \
            color + (border_bottom * 80) + os.linesep + color_reset
    elif not log_text:
        return

    try:
        new_log_text = border or ''
        for line in log_text.splitlines():
            if line.strip() is not '':
                if prefix:
                    new_log_text += Style.DIM + prefix + Style.RESET_ALL
                new_log_text += color + line + color_reset + os.linesep
        new_log_text += border_bottom or ''

        if not quiet:
            sys.stdout.write(Style.RESET_ALL + new_log_text)
            sys.stdout.flush()

        if file is not None and os.fstat(
                file.fileno()).st_size < TOO_MANY_BYTES:
            file.write(seq.sub('', new_log_text))
            file.flush()
    except:
        pass


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
