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

class Logger:
    '''
    Logs the given text and optional prefix to stdout (if quiet is False) and
    to an optional log file. By default, we strip out newlines in order to
    print our lines correctly, but you can override this functionality if you
    want to print multi-line output.
    '''

    def __init__(self):
        self.fileLogger = FileLogger()

    def log(self, log_text=None, squash=True, **kwargs):
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

            if file is not None:
                self.fileLogger.log(file, new_log_text, squash)

        except:
            pass

class FileLogger:
    '''
    Logs text to a file
    '''

    def __init__(self):
        self.prev_text_count = 0
        self.prev_text = ''

    def write_to_file(self, file, text):
        if os.fstat(file.fileno()).st_size < TOO_MANY_BYTES:
            file.write(seq.sub('', text))
            file.flush()

    def write_prev_text(self, file):
        text = self.prev_text
        if self.prev_text_count > 1:
            text = '[%s]: %s' % (self.prev_text_count, self.prev_text)
        self.write_to_file(file, text)

    def log(self, file, text, squash):
        if not squash:
            # If we're not squashing make sure there's no prev text
            # to flush out
            if self.prev_text_count > 0:
                self.write_prev_text(file)
                self.prev_text_count = 0
                self.prev_text = ''
            # Then write the text we're not squashing
            self.write_to_file(file, text)
        # If we have matching lines, increase the counter without
        # writing anything to file
        elif self.prev_text and self.prev_text == text:
            self.prev_text_count += 1
        # If we get here, we don't have matching lines. Write the
        # previous text and store the current text
        elif self.prev_text_count > 0:
            self.write_prev_text(file)
            self.prev_text = text
            self.prev_text_count = 1
        else:
            self.prev_text = text
            self.prev_text_count = 1

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
